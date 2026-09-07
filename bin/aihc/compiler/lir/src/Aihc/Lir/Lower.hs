{-# LANGUAGE OverloadedStrings #-}

-- | Lower GC-GRIN to Lir.
--
-- Every GRIN function becomes a Lir function with the @aihc@ convention. The
-- first parameter is the machine. A pointer representation becomes @ptr@, an
-- address becomes @ptr@, and every other scalar becomes @i64@. Floats travel
-- as their bit patterns, like in the native runtime ABI.
--
-- Control transfer is explicit: a CPS transfer is a @tailcall@, a runtime
-- helper is a @call@ of an extern C function, and dynamic entries go through
-- the @backend_entry@ field of an info table. That field has the signature
-- @(ptr, ptr, ptr, T...) -> ()@ with the machine, the object, the
-- continuation, and the supplied values. The runtime defines fixed helpers
-- and common argument shapes. Other shapes remain in the module.
module Aihc.Lir.Lower
  ( LowerError (..),
    LowerOptions (..),
    LowerTarget (..),
    HostKind (..),
    UnitKind (..),
    posixTarget64,
    wasip3Target,
    lowerEntry,
    lowerModule,
    lowerProgramWith,

    -- * Building blocks for harnesses
    LowerM,
    Typed (..),
    ContinuationSpec (..),
    runLower,
    lowerUnitItems,
    continuationInfoItems,
    functionSymbol,
    functionResultTypes,
    threadDoneContinuation,
    constructorInfoSymbol,
    repType,
    beginBlock,
    emit,
    terminate,
    fresh,
    finishFunction,
    requireExtern,
    requireExternData,
    requireHelper,
    Helper (..),
    helperSymbol,
    runtimeCallSignature,
    loadSlot,
    storeSlot,
  )
where

import Aihc.Grin.Analysis (freeExprVars)
import Aihc.Grin.Cps (ContinuationFrameKind (..), continuationFrameKindCode)
import Aihc.Grin.Gc (GcGrinProgram, entryGcProgram, gcContinuationFrames, gcContinuationFunctions, gcGrinProgram, gcUpdateFunction)
import Aihc.Grin.Srt
import Aihc.Grin.Syntax
import Aihc.Lir.Syntax
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    buildAddrLiteralPool,
    executableEntryName,
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
    renderLinkedConstructorInfoSymbol,
    renderLinkedFunctionSymbol,
    renderLinkedGlobalSymbol,
    renderLinkedPartialConstructorInfoSymbol,
  )
import Control.Monad (foldM, forM, forM_, unless, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', put, runStateT)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data LowerError
  = LowerMissingFunction !FunctionName
  | LowerUnsupportedExpression !Text
  | LowerUnsupportedValue !Text
  | LowerUnsupportedRuntimeRep !GrinRep
  | LowerUnsupportedPrimitive !Text
  | LowerCpsError !Text
  deriving (Eq, Show)

data UnitKind
  = -- | A library module. Unsupported primitives become runtime failures.
    LibraryUnit
  | -- | The executable entry unit with @main@.
    ExecutableUnit
  deriving (Eq, Show)

-- | The host that starts the program and owns the IO loop.
data HostKind
  = -- | A POSIX process. The entry unit defines @main@.
    PosixHost
  | -- | A WASI P3 component. The entry unit exports the start and resume
    -- functions that the P3 driver calls.
    Wasip3Host
  deriving (Eq, Show)

-- | The properties of the target that the lowering depends on. Heap slots
-- are 8 bytes on every target; the word size decides the layout of the
-- info tables, the static reference tables, and the resume records.
data LowerTarget = LowerTarget
  { lowerWordSize :: !Int,
    lowerHost :: !HostKind
  }
  deriving (Eq, Show)

-- | A 64-bit POSIX target: Apple ARM64, Linux AMD64, and LLVM.
posixTarget64 :: LowerTarget
posixTarget64 = LowerTarget {lowerWordSize = 8, lowerHost = PosixHost}

-- | The 32-bit WASI P3 target.
wasip3Target :: LowerTarget
wasip3Target = LowerTarget {lowerWordSize = 4, lowerHost = Wasip3Host}

data LowerOptions = LowerOptions
  { lowerUnitKind :: !UnitKind,
    -- | Export every function symbol. Test harnesses use the symbols.
    lowerExposeFunctions :: !Bool,
    lowerTarget :: !LowerTarget
  }
  deriving (Eq, Show)

-- | Lower one library module.
lowerModule :: LowerTarget -> GcGrinProgram -> Either LowerError Module
lowerModule target = lowerProgramWith LowerOptions {lowerUnitKind = LibraryUnit, lowerExposeFunctions = False, lowerTarget = target}

-- | Lower the fixed executable entry unit.
lowerEntry :: LowerTarget -> Either LowerError Module
lowerEntry target = do
  gcProgram <- either (Left . LowerCpsError . T.pack . show) Right entryGcProgram
  lowerProgramWith LowerOptions {lowerUnitKind = ExecutableUnit, lowerExposeFunctions = False, lowerTarget = target} gcProgram

lowerProgramWith :: LowerOptions -> GcGrinProgram -> Either LowerError Module
lowerProgramWith options gcProgram =
  Module . snd <$> runLower options gcProgram unit
  where
    unit env = do
      lowerUnitItems env
      when (lowerUnitKind options == ExecutableUnit) $
        case lowerHost (lowerTarget options) of
          PosixHost -> lowerExecutableMain gcProgram
          Wasip3Host -> lowerWasip3Entry gcProgram

-- Types

-- | The Lir type of one runtime representation.
repType :: GrinRep -> Type
repType runtimeRep =
  case runtimeRep of
    BoxedRep _ -> Ptr
    SumRep _ -> Ptr
    AddrRep -> Ptr
    _ -> I64

-- | An operand together with its Lir type.
data Typed = Typed
  { typedOperand :: !Operand,
    typedType :: !Type
  }
  deriving (Eq, Show)

-- Environment

-- | Which of the two info tables of one constructor an object uses. A
-- constructor that still wants arguments shares a single info table across
-- every stage and records how much it holds in the object itself, so the
-- number of arguments outstanding is not part of its identity here.
data ConstructorStage
  = SaturatedConstructor
  | PartialConstructor
  deriving (Eq, Ord, Show)

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !ConstructorStage
  | ClosureRuntimeInfo !FunctionName ![GrinRep] ![[GrinRep]]
  | ThunkRuntimeInfo !FunctionName ![GrinRep]
  deriving (Eq, Ord, Show)

-- | The stage a node tag names. GRIN counts the arguments a constructor still
-- wants; zero of them means the constructor is finished.
constructorStage :: Int -> ConstructorStage
constructorStage remaining
  | remaining == 0 = SaturatedConstructor
  | otherwise = PartialConstructor

data RuntimeEnter = RuntimeEnter
  { enterTarget :: !Symbol,
    enterStored :: ![Type],
    enterSupplied :: ![Type],
    enterTargetParameters :: ![Type],
    enterPassesContinuation :: !Bool
  }

data RuntimeInfo = RuntimeInfo
  { infoSymbol :: !Symbol,
    infoLinkage :: !Linkage,
    infoIdentity :: !DataField,
    infoFields :: ![GrinRep],
    infoRemainingArity :: !Int,
    infoNext :: !(Maybe Symbol),
    infoEnter :: !(Maybe RuntimeEnter),
    infoFrameKind :: !(Maybe ContinuationFrameKind),
    infoObjectKind :: !Int,
    infoSrt :: !(Maybe Symbol)
  }

data LowerEnv = LowerEnv
  { envProgram :: !GrinProgram,
    envOptions :: !LowerOptions,
    envFunctionSymbols :: !(Map FunctionName Symbol),
    envFunctionParameters :: !(Map FunctionName [Type]),
    envContinuationFunctions :: !(Set FunctionName),
    envInfoSymbols :: !(Map RuntimeInfoKey Symbol),
    envInfos :: ![RuntimeInfo],
    envStaticReferences :: !StaticReferences,
    envSrtSymbols :: !(Map FunctionName Symbol),
    envAddrLiterals :: !(Map BS.ByteString Symbol)
  }

-- | Shared functions that lowered code tail-calls.
data Helper
  = HelperEval
  | HelperResume
  | HelperExit
  | HelperQuotRem2
  | HelperCStringLength
  | HelperContinue ![Type]
  | HelperApply ![Type]
  | -- | Continue with one value that arrives as a raw slot: the runtime
    -- does not know whether it is a pointer, so the info table decides.
    HelperContinueSlot
  | HelperApplySlot
  deriving (Eq, Ord, Show)

helperSymbol :: Helper -> Symbol
helperSymbol helper =
  Symbol $ case helper of
    HelperEval -> "aihc_lir_eval"
    HelperResume -> "aihc_lir_resume"
    HelperExit -> "aihc_lir_exit"
    HelperQuotRem2 -> "aihc_lir_quotrem2"
    HelperCStringLength -> "aihc_lir_cstring_length"
    HelperContinue shape -> "aihc_lir_continue_" <> shapeName shape
    HelperApply shape -> "aihc_lir_apply_" <> shapeName shape
    HelperContinueSlot -> "aihc_lir_continue_slot"
    HelperApplySlot -> "aihc_lir_apply_slot"

shapeName :: [Type] -> Text
shapeName = T.pack . map letter
  where
    letter ty = if ty == Ptr then 'p' else 'i'

-- | An open block under construction.
data OpenBlock = OpenBlock
  { openLabel :: !Label,
    openParameters :: ![(Var, Type)],
    openInstructionsRev :: ![Instruction]
  }

data LowerState = LowerState
  { stateNext :: !Int,
    stateTarget :: !LowerTarget,
    stateExterns :: !(Map Symbol Signature),
    stateExternData :: !(Set Symbol),
    stateHelpers :: !(Set Helper),
    -- | The pointer-bitmap array emitted for each distinct bitmap so far.
    stateBitmaps :: !(Map BS.ByteString Symbol),
    stateItemsRev :: ![Item],
    stateBlocksRev :: ![Block],
    stateOpen :: !(Maybe OpenBlock)
  }

type LowerM = StateT LowerState (Either LowerError)

failWith :: LowerError -> LowerM value
failWith = lift . Left

-- | Run a lowering action for one program and collect the emitted items, the
-- shared helpers, and the extern declarations.
runLower :: LowerOptions -> GcGrinProgram -> (LowerEnv -> LowerM value) -> Either LowerError (value, [Item])
runLower options gcProgram action = do
  let env = lowerEnvironment options gcProgram
      initial =
        LowerState
          { stateNext = 0,
            stateTarget = lowerTarget options,
            stateExterns = Map.empty,
            stateExternData = Set.empty,
            stateHelpers = Set.empty,
            stateBitmaps = Map.empty,
            stateItemsRev = [],
            stateBlocksRev = [],
            stateOpen = Nothing
          }
  (value, final) <- runStateT (action env <* generateHelpers env Set.empty) initial
  let defined =
        Set.fromList
          [ symbol
          | item <- stateItemsRev final,
            symbol <- case item of
              ItemFunction function -> [functionName function]
              ItemData dataItem -> [dataName dataItem]
              _ -> []
          ]
      externs =
        [ItemExternFunction (ExternFunction symbol signature) | (symbol, signature) <- Map.toAscList (stateExterns final), symbol `Set.notMember` defined]
          <> [ItemExternData symbol | symbol <- Set.toAscList (stateExternData final), symbol `Set.notMember` defined]
  pure (value, externs <> reverse (stateItemsRev final))

lowerEnvironment :: LowerOptions -> GcGrinProgram -> LowerEnv
lowerEnvironment options gcProgram =
  LowerEnv
    { envProgram = program,
      envOptions = options,
      envFunctionSymbols = functionSymbols,
      envFunctionParameters = functionParameters,
      envContinuationFunctions = continuationFunctions,
      envInfoSymbols = Map.fromList [(key, infoSymbol info) | (key, info) <- constructorEntries <> functionEntries],
      envInfos = map snd (constructorEntries <> functionEntries),
      envStaticReferences = staticReferences,
      envSrtSymbols = srtSymbols,
      envAddrLiterals = Map.fromList [(bytes, Symbol ("aihc_lir_addr_" <> T.pack (show index))) | (index, (bytes, _)) <- zip [0 :: Int ..] (buildAddrLiteralPool program)]
    }
  where
    program = gcGrinProgram gcProgram
    continuationFunctions = gcContinuationFunctions gcProgram
    continuationFrames = gcContinuationFrames gcProgram
    functionSymbols = Map.fromList [(grinFunctionName function, functionSymbol (grinFunctionName function)) | function <- grinFunctions program]
    functionParameters = Map.fromList [(grinFunctionName function, map (repType . grinVarRuntimeRep) (grinFunctionParameters function)) | function <- grinFunctions program]
    staticReferences = programStaticReferences program
    srtSymbols =
      Map.fromList
        [ (name, Symbol ("aihc_lir_srt_" <> T.pack (show index)))
        | (index, name) <- zip [0 :: Int ..] (Map.keys (staticReferenceTables staticReferences))
        ]
    constructorLayouts = grinConstructors program
    -- The program that declares a constructor defines its info tables even
    -- when it builds no node of its own: another module that builds one has
    -- only this program to link its node against.
    requiredConstructorInfos =
      Set.fromList
        ( concatMap declaredConstructorInfos constructorLayouts
            <> concatMap requiredNodeConstructorInfos (programNodes program)
        )
    declaredConstructorInfos (name, layouts)
      | null layouts = [ConstructorRuntimeInfo name SaturatedConstructor]
      | otherwise = [ConstructorRuntimeInfo name SaturatedConstructor, ConstructorRuntimeInfo name PartialConstructor]
    -- One constructor needs at most two info tables: the saturated object,
    -- and one shared by every stage that still wants arguments. The partial
    -- table carries the saturated one as its next stage, which is where the
    -- runtime reads the full width and the pointer map from.
    constructorEntries =
      [ ( key,
          RuntimeInfo
            { infoSymbol = symbol,
              infoLinkage = Export,
              infoIdentity = DataSymbol (constructorInfoSymbol name 0) 0,
              -- Both tables describe the saturated slots. A partial object
              -- has filled a prefix of them.
              infoFields = concat layouts,
              infoRemainingArity = if stage == SaturatedConstructor then 0 else length layouts,
              infoNext = if stage == SaturatedConstructor then Nothing else Just (constructorInfoSymbol name 0),
              infoEnter = Nothing,
              infoFrameKind = Nothing,
              infoObjectKind = runtimeInfoKeyObjectKind key,
              infoSrt = Nothing
            }
        )
      | (name, layouts) <- constructorLayouts,
        stage <- [SaturatedConstructor, PartialConstructor],
        let key = ConstructorRuntimeInfo name stage,
        let symbol = constructorStageSymbol name stage,
        key `Set.member` requiredConstructorInfos
      ]
    infoKeys =
      [ key
      | key <- Set.toAscList (Set.fromList (concatMap runtimeInfoKeyStages (programNodes program))),
        Just name <- [runtimeInfoFunctionName key],
        name `Map.member` functionSymbols
      ]
    infoSymbols = Map.fromList [(key, Symbol ("aihc_lir_info_" <> T.pack (show index))) | (index, key) <- zip [0 :: Int ..] infoKeys]
    functionEntries =
      [ ( key,
          RuntimeInfo
            { infoSymbol = symbol,
              infoLinkage = Internal,
              infoIdentity = DataCode (Just target),
              infoFields = runtimeInfoKeyFields key,
              infoRemainingArity = runtimeInfoKeyRemainingArity key,
              infoNext = runtimeInfoKeyNext key >>= (`Map.lookup` infoSymbols),
              infoEnter = runtimeEnter target name key,
              infoFrameKind = Map.lookup name continuationFrames,
              infoObjectKind = runtimeInfoKeyObjectKind key,
              infoSrt = Map.lookup name srtSymbols
            }
        )
      | (key, symbol) <- Map.toAscList infoSymbols,
        Just name <- [runtimeInfoFunctionName key],
        Just target <- [Map.lookup name functionSymbols]
      ]
    targetParameters name = Map.findWithDefault [] name functionParameters
    runtimeEnter target name key =
      case key of
        ClosureRuntimeInfo _ fields [supplied] ->
          Just
            RuntimeEnter
              { enterTarget = target,
                enterStored = map repType fields,
                enterSupplied = map repType supplied,
                enterTargetParameters = targetParameters name,
                enterPassesContinuation = name `Set.notMember` continuationFunctions
              }
        ThunkRuntimeInfo _ fields ->
          Just
            RuntimeEnter
              { enterTarget = target,
                enterStored = map repType fields,
                enterSupplied = [],
                enterTargetParameters = targetParameters name,
                enterPassesContinuation = True
              }
        _ -> Nothing

-- | The Lir symbol of one GRIN function.
functionSymbol :: FunctionName -> Symbol
functionSymbol (FunctionName name) = Symbol ("aihc_f_" <> renderLinkedFunctionSymbol name)

constructorInfoSymbol :: Text -> Int -> Symbol
constructorInfoSymbol name remaining = Symbol (renderLinkedConstructorInfoSymbol name remaining)

constructorStageSymbol :: Text -> ConstructorStage -> Symbol
constructorStageSymbol name stage =
  case stage of
    SaturatedConstructor -> constructorInfoSymbol name 0
    PartialConstructor -> Symbol (renderLinkedPartialConstructorInfoSymbol name)

globalSymbol :: Text -> Symbol
globalSymbol = Symbol . renderLinkedGlobalSymbol

-- | The Lir types of the values one GRIN function returns to its
-- continuation.
functionResultTypes :: GrinFunction -> [Type]
functionResultTypes = map repType . runtimeRepComponents . grinFunctionResultRep

-- State helpers

fresh :: Text -> LowerM Var
fresh base = do
  state <- get
  put state {stateNext = stateNext state + 1}
  pure (Var (base <> "_" <> T.pack (show (stateNext state))))

freshLabel :: Text -> LowerM Label
freshLabel base = do
  state <- get
  put state {stateNext = stateNext state + 1}
  pure (Label (base <> "_" <> T.pack (show (stateNext state))))

requireExtern :: Symbol -> [Type] -> [Type] -> LowerM ()
requireExtern symbol parameters results =
  modify' $ \state ->
    state {stateExterns = Map.insertWith (\_ old -> old) symbol (Signature parameters results CConvention) (stateExterns state)}

requireExternData :: Symbol -> LowerM ()
requireExternData symbol = modify' $ \state -> state {stateExternData = Set.insert symbol (stateExternData state)}

requireHelper :: Helper -> LowerM Symbol
requireHelper helper = do
  let symbol = helperSymbol helper
  case sharedHelperSignature helper of
    Just signature ->
      modify' $ \state -> state {stateExterns = Map.insert symbol signature (stateExterns state)}
    Nothing ->
      modify' $ \state -> state {stateHelpers = Set.insert helper (stateHelpers state)}
  pure symbol

-- | The runtime Lir unit defines these fixed signatures.
sharedHelperSignature :: Helper -> Maybe Signature
sharedHelperSignature helper =
  case helper of
    HelperEval -> shared [Ptr, Ptr, I64, Ptr, Ptr] []
    HelperResume -> shared [Ptr, Ptr] []
    HelperContinue shape | common shape -> shared (Ptr : Ptr : shape) []
    HelperApply shape | common shape -> shared (Ptr : Ptr : Ptr : shape) []
    HelperContinueSlot -> shared [Ptr, Ptr, I64] []
    HelperApplySlot -> shared [Ptr, Ptr, Ptr, I64] []
    HelperQuotRem2 -> shared [I64, I64, I64] [I64, I64]
    HelperCStringLength -> shared [Ptr] [I64]
    _ -> Nothing
  where
    common shape = shape `elem` [[], [Ptr], [I64]]
    shared parameters results = Just (Signature parameters results AihcConvention)

emitItem :: Item -> LowerM ()
emitItem item = modify' $ \state -> state {stateItemsRev = item : stateItemsRev state}

beginBlock :: Label -> [(Var, Type)] -> LowerM ()
beginBlock label parameters = do
  state <- get
  case stateOpen state of
    Just _ -> failWith (LowerUnsupportedExpression "internal: block opened inside another block")
    Nothing -> put state {stateOpen = Just (OpenBlock label parameters [])}

emit :: [Var] -> Operation -> LowerM ()
emit results operation = do
  state <- get
  case stateOpen state of
    Nothing -> failWith (LowerUnsupportedExpression "internal: instruction outside a block")
    Just open -> put state {stateOpen = Just open {openInstructionsRev = Instruction results operation : openInstructionsRev open}}

-- | Emit an operation with one fresh result.
emitValue :: Text -> Type -> Operation -> LowerM Typed
emitValue base ty operation = do
  var <- fresh base
  emit [var] operation
  pure (Typed (OperandVar var) ty)

terminate :: Terminator -> LowerM ()
terminate terminator = do
  state <- get
  case stateOpen state of
    Nothing -> failWith (LowerUnsupportedExpression "internal: terminator outside a block")
    Just open ->
      put
        state
          { stateOpen = Nothing,
            stateBlocksRev = Block (openLabel open) (openParameters open) (reverse (openInstructionsRev open)) terminator : stateBlocksRev state
          }

-- | Collect the blocks emitted since the last function into a function item.
finishFunction :: Symbol -> Linkage -> [(Var, Type)] -> [Type] -> CallingConvention -> LowerM ()
finishFunction symbol linkage parameters results convention = do
  state <- get
  when (isJust (stateOpen state)) $ failWith (LowerUnsupportedExpression "internal: function finished with an open block")
  put state {stateBlocksRev = []}
  emitItem
    ( ItemFunction
        Function
          { functionName = symbol,
            functionLinkage = linkage,
            functionParameters = parameters,
            functionResults = results,
            functionConvention = convention,
            functionBlocks = reverse (stateBlocksRev state)
          }
    )

-- Target layout

targetM :: LowerM LowerTarget
targetM = stateTarget <$> get

-- | The integer type with the width of a machine word.
wordType :: LowerTarget -> Type
wordType target = if lowerWordSize target == 8 then I64 else I32

-- | A heap slot is 8 bytes on every target. A pointer that lives in a slot
-- travels through @i64@ on a 32-bit target, so the high bytes of the slot
-- are always zero.
loadSlot :: Text -> Type -> Operand -> Integer -> LowerM Typed
loadSlot base ty object offset = do
  target <- targetM
  if lowerWordSize target == 8 || ty `notElem` [Ptr, Code]
    then emitValue base ty (Load ty (Address object offset) 8)
    else do
      word <- emitValue base I64 (Load I64 (Address object offset) 8)
      emitValue base ty (PtrFromInt (typedOperand word))

storeSlot :: Type -> Operand -> Operand -> Integer -> LowerM ()
storeSlot ty value object offset = do
  target <- targetM
  if lowerWordSize target == 8 || ty `notElem` [Ptr, Code]
    then emit [] (Store ty value (Address object offset) 8)
    else do
      word <- emitValue "slot" I64 (PtrToInt value)
      emit [] (Store I64 (typedOperand word) (Address object offset) 8)

-- | Field @index@ of an info table as an @i64@.
loadInfoWord :: Text -> Operand -> Int -> LowerM Typed
loadInfoWord base header index = do
  target <- targetM
  let offset = toInteger (lowerWordSize target * index)
  case wordType target of
    I64 -> emitValue base I64 (Load I64 (Address header offset) 8)
    narrow -> do
      value <- emitValue base narrow (Load narrow (Address header offset) 4)
      emitValue base I64 (Convert ZExt narrow (typedOperand value) I64)

-- | A @code@ field of an info table.
loadInfoCode :: Text -> Operand -> Int -> LowerM Typed
loadInfoCode base header index = do
  target <- targetM
  let width = lowerWordSize target
  emitValue base Code (Load Code (Address header (toInteger (width * index))) (toInteger width))

-- | A @ptr@ field of an info table.
loadInfoPointer :: Text -> Operand -> Int -> LowerM Typed
loadInfoPointer base header index = do
  target <- targetM
  let width = lowerWordSize target
  emitValue base Ptr (Load Ptr (Address header (toInteger (width * index))) (toInteger width))

-- | A word-sized integer field of a data object.
wordField :: LowerTarget -> Integer -> DataField
wordField target = DataInt (wordType target)

-- | A pointer stored in an 8-byte slot of a static object.
slotPointerFields :: LowerTarget -> DataField -> [DataField]
slotPointerFields target field
  | lowerWordSize target == 8 = [field]
  | otherwise = [field, DataInt I32 0]

-- | The exit-code field of the machine, at the same offset on every target.
machineExitCodeOffset :: Integer
machineExitCodeOffset = 16

-- Coercion

-- | Convert a typed operand to a type. Pointers and words convert both ways.
-- Narrow C integers widen with sign extension and narrow with truncation.
coerce :: Type -> Typed -> LowerM Operand
coerce ty (Typed operand actual)
  | ty == actual = pure operand
  | otherwise =
      case (actual, ty) of
        (Ptr, I64) -> result (PtrToInt operand)
        (I64, Ptr) -> result (PtrFromInt operand)
        (I64, I32) -> result (Convert Trunc I64 operand I32)
        (I32, I64) -> result (Convert SExt I32 operand I64)
        (Ptr, I32) -> do
          word <- result (PtrToInt operand)
          result (Convert Trunc I64 word I32)
        (I32, Ptr) -> do
          word <- result (Convert SExt I32 operand I64)
          result (PtrFromInt word)
        -- A Float# value travels as its bit pattern in the low 32 bits of an
        -- integer slot, and a Double# value as its 64-bit pattern, so the C
        -- ABI reads them as floats rather than converting the number.
        (I64, F64) -> result (Convert Bitcast I64 operand F64)
        (I64, F32) -> do
          narrow <- typedOperand <$> emitValue "coerced" I32 (Convert Trunc I64 operand I32)
          result (Convert Bitcast I32 narrow F32)
        _ -> failWith (LowerUnsupportedValue ("cannot convert " <> T.pack (show actual) <> " to " <> T.pack (show ty)))
  where
    result operation = typedOperand <$> emitValue "coerced" ty operation

coerceTo :: Type -> Typed -> LowerM Typed
coerceTo ty typed = (`Typed` ty) <$> coerce ty typed

-- Unit lowering

-- | Lower the functions, the static objects, the info tables, the enter
-- stubs, the static reference tables, and the address literals of the
-- program.
lowerUnitItems :: LowerEnv -> LowerM ()
lowerUnitItems env = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  mapM_ (lowerFunction env) (grinFunctions program)
  mapM_ (lowerStaticObject env) (programStaticObjects program)
  mapM_ lowerInfo (envInfos env)
  lowerStaticReferenceTables env
  forM_ (Map.toAscList (envAddrLiterals env)) $ \(bytes, symbol) ->
    emitItem (ItemData (DataItem symbol Internal False 1 [DataBytes bytes, DataInt I8 0]))
  where
    program = envProgram env

validateRuntimeRep :: GrinRep -> LowerM ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> failWith (LowerUnsupportedRuntimeRep runtimeRep)
    TupleRep reps -> mapM_ validateRuntimeRep reps
    SumRep reps -> mapM_ validateRuntimeRep reps
    _ -> pure ()

-- | The pointer bitmap one info table describes.
infoBitmap :: RuntimeInfo -> BS.ByteString
infoBitmap info = BS.pack [if isPointerRuntimeRep field then 1 else 0 | field <- infoFields info]

-- | The array holding one pointer bitmap, emitted once per distinct bitmap.
--
-- Info tables share an array whenever their bitmaps agree, which is often:
-- the application stages of an arity-n constructor ask between them for every
-- prefix of one layout, and those prefixes repeat across every constructor
-- built the same way. Naming the arrays after the info tables that wanted
-- them would keep one array per stage, and each such name spells out the
-- mangled constructor name -- hundreds of bytes for a wide tuple, where the
-- symbol costs far more than the handful of bytes it points at.
internBitmap :: BS.ByteString -> LowerM (Maybe Symbol)
internBitmap bytes
  | BS.null bytes = pure Nothing
  | otherwise = do
      known <- gets stateBitmaps
      case Map.lookup bytes known of
        Just symbol -> pure (Just symbol)
        Nothing -> do
          let symbol = Symbol ("aihc_lir_bitmap_" <> T.pack (show (Map.size known)))
          modify' (\state -> state {stateBitmaps = Map.insert bytes symbol (stateBitmaps state)})
          emitItem (ItemData (DataItem symbol Internal False 1 [DataBytes bytes]))
          pure (Just symbol)

lowerInfo :: RuntimeInfo -> LowerM ()
lowerInfo info = do
  target <- targetM
  let stub = Symbol (unSymbol (infoSymbol info) <> "_e")
      fields = infoFields info
      word = wordField target
  bitmap <- internBitmap (infoBitmap info)
  forM_ (infoEnter info) (lowerEnterStub stub)
  emitItem
    ( ItemData
        DataItem
          { dataName = infoSymbol info,
            dataLinkage = infoLinkage info,
            dataMutable = False,
            dataAlignment = toInteger (lowerWordSize target),
            dataFields =
              [ infoIdentity info,
                DataCode Nothing,
                word (toInteger (length fields)),
                word (toInteger (infoRemainingArity info)),
                maybe DataNull (`DataSymbol` 0) bitmap,
                maybe DataNull (`DataSymbol` 0) (infoNext info),
                DataCode (stub <$ infoEnter info),
                word (toInteger (continuationFrameKindCode (infoFrameKind info))),
                word (toInteger (infoObjectKind info)),
                maybe DataNull (`DataSymbol` 0) (infoSrt info)
              ]
          }
    )

-- | The dynamic entry of one enterable object. It loads the stored fields,
-- takes the supplied values as parameters, and tail-calls the code.
lowerEnterStub :: Symbol -> RuntimeEnter -> LowerM ()
lowerEnterStub stub enter = do
  machine <- fresh "machine"
  object <- fresh "object"
  continuation <- fresh "continuation"
  supplied <- forM (enterSupplied enter) $ \ty -> (,ty) <$> fresh "supplied"
  beginBlock (Label "entry") []
  stored <- forM (zip [0 :: Int ..] (enterStored enter)) $ \(index, ty) ->
    loadSlot "stored" ty (OperandVar object) (toInteger (8 * (index + 1)))
  let values = stored <> [Typed (OperandVar var) ty | (var, ty) <- supplied] <> [Typed (OperandVar continuation) Ptr | enterPassesContinuation enter]
      parameters = enterTargetParameters enter
  when (length parameters /= length values) $
    failWith (LowerUnsupportedExpression ("enter stub arity mismatch for " <> unSymbol (enterTarget enter)))
  arguments <- zipWithM coerce parameters values
  terminate (TailCall (enterTarget enter) (OperandVar machine : arguments))
  finishFunction stub Internal ((machine, Ptr) : (object, Ptr) : (continuation, Ptr) : supplied) [] AihcConvention

-- | A continuation object kind for an entry or a harness: the unapplied and
-- the applied info table, and the stub that enters the target function with
-- the stored fields and the supplied values.
data ContinuationSpec = ContinuationSpec
  { continuationInfo :: !Symbol,
    continuationAppliedInfo :: !Symbol,
    continuationTarget :: !Symbol,
    continuationStored :: ![Type],
    continuationSupplied :: ![Type],
    continuationFrame :: !ContinuationFrameKind
  }

continuationInfoItems :: ContinuationSpec -> LowerM ()
continuationInfoItems spec = do
  lowerInfo unapplied
  lowerInfo applied
  where
    stored = map typeRep (continuationStored spec)
    supplied = map typeRep (continuationSupplied spec)
    unapplied =
      RuntimeInfo
        { infoSymbol = continuationInfo spec,
          infoLinkage = Internal,
          infoIdentity = DataCode (Just (continuationTarget spec)),
          infoFields = stored,
          infoRemainingArity = 1,
          infoNext = Just (continuationAppliedInfo spec),
          infoEnter =
            Just
              RuntimeEnter
                { enterTarget = continuationTarget spec,
                  enterStored = continuationStored spec,
                  enterSupplied = continuationSupplied spec,
                  enterTargetParameters = continuationStored spec <> continuationSupplied spec,
                  enterPassesContinuation = False
                },
          infoFrameKind = Just (continuationFrame spec),
          infoObjectKind = runtimeObjectClosure,
          infoSrt = Nothing
        }
    applied =
      unapplied
        { infoSymbol = continuationAppliedInfo spec,
          infoFields = stored <> supplied,
          infoRemainingArity = 0,
          infoNext = Nothing,
          infoEnter = Nothing
        }
    -- Info tables carry pointer bitmaps, so recover a representation from
    -- the Lir type.
    typeRep ty = if ty == Ptr then BoxedRep Lifted else IntRep

-- | A static object has the layout of a heap object: an 8-byte header slot
-- and 8-byte field slots. A pointer occupies the low bytes of its slot.
lowerStaticObject :: LowerEnv -> StaticObject -> LowerM ()
lowerStaticObject env object = do
  target <- targetM
  header <- slotPointerFields target . (`DataSymbol` 0) <$> nodeInfoSymbol env node
  fields <- concat <$> mapM (staticField target) (grinNodeFields node)
  let applied = [wordField target (toInteger (length (grinNodeFields node))) | isPartialConstructorNode node]
      payload = if null fields && isThunk then [DataZero 8] else fields
  emitItem (ItemData (DataItem (globalSymbol (staticObjectName object)) Export True 8 (header <> applied <> payload)))
  where
    node = staticObjectNode object
    isThunk = case grinNodeTag node of
      GrinThunk {} -> True
      _ -> False
    staticField target value =
      case value of
        GrinVarValue var -> referenceGlobal target (grinVarName var)
        GrinGlobalValue name -> referenceGlobal target name
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes -> do
              symbol <- addrLiteralSymbol env bytes
              pure (slotPointerFields target (DataSymbol symbol 0))
            _ -> maybe (failWith (LowerUnsupportedValue "string literal")) (pure . (: []) . DataInt I64) (normalizedLiteralInteger literal)
    referenceGlobal target name = do
      let symbol = globalSymbol name
      requireExternData symbol
      pure (slotPointerFields target (DataSymbol symbol 0))

nodeInfoSymbol :: LowerEnv -> GrinNode -> LowerM Symbol
nodeInfoSymbol env node =
  case grinNodeTag node of
    GrinConstructor name remaining -> lookupInfo (ConstructorRuntimeInfo name (constructorStage remaining))
    GrinClosure name layouts -> lookupInfo (ClosureRuntimeInfo name fields layouts)
    GrinThunk name -> lookupInfo (ThunkRuntimeInfo name fields)
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)
    lookupInfo key =
      case Map.lookup key (envInfoSymbols env) of
        Just symbol -> pure symbol
        Nothing ->
          case key of
            ConstructorRuntimeInfo name stage -> do
              let symbol = constructorStageSymbol name stage
              requireExternData symbol
              pure symbol
            ClosureRuntimeInfo name _ _ -> failWith (LowerMissingFunction name)
            ThunkRuntimeInfo name _ -> failWith (LowerMissingFunction name)

addrLiteralSymbol :: LowerEnv -> BS.ByteString -> LowerM Symbol
addrLiteralSymbol env bytes =
  maybe (failWith (LowerUnsupportedValue "unregistered Addr# literal")) pure (Map.lookup bytes (envAddrLiterals env))

lowerStaticReferenceTables :: LowerEnv -> LowerM ()
lowerStaticReferenceTables env = do
  target <- targetM
  forM_ (Map.toList (staticReferenceTables (envStaticReferences env))) $ \(name, table) ->
    forM_ (Map.lookup name (envSrtSymbols env)) $ \symbol -> do
      let objects = map globalSymbol (srtObjects table)
          children = [child | name' <- srtChildren table, Just child <- [Map.lookup name' (envSrtSymbols env)]]
          word = wordField target
      mapM_ requireExternData objects
      emitItem
        ( ItemData
            ( DataItem
                symbol
                Internal
                True
                (toInteger (lowerWordSize target))
                ( [word 0, word (toInteger (length objects)), word (toInteger (length children))]
                    <> [DataSymbol object 0 | object <- objects]
                    <> [DataSymbol child 0 | child <- children]
                )
            )
        )

-- Functions

data FunctionCtx = FunctionCtx
  { ctxEnv :: !LowerEnv,
    ctxMachine :: !Operand,
    ctxFunctionName :: !FunctionName,
    ctxRoots :: !(Maybe Operand)
  }

type ValueEnv = Map GrinVar Typed

currentSrtSymbol :: Symbol
currentSrtSymbol = Symbol "aihc_current_srt"

lowerFunction :: LowerEnv -> GrinFunction -> LowerM ()
lowerFunction env function = do
  machine <- fresh "machine"
  parameters <- forM (grinFunctionParameters function) $ \var -> do
    lirVar <- fresh (varBase var)
    pure (var, lirVar, repType (grinVarRuntimeRep var))
  beginBlock (Label "entry") []
  -- Every function publishes its own table, so a collection never sees a
  -- table left behind by a function that has already transferred away.
  requireExternData currentSrtSymbol
  let srt = maybe (OperandLiteral LitNull) (OperandLiteral . LitSymbol) (Map.lookup (grinFunctionName function) (envSrtSymbols env))
  emit [] (Store Ptr srt (Address (OperandLiteral (LitSymbol currentSrtSymbol)) 0) 8)
  roots <-
    case maximumRoots (grinFunctionBody function) of
      0 -> pure Nothing
      count -> Just . typedOperand <$> emitValue "roots" Ptr (StackAlloc (toInteger (8 * count)) 8)
  let ctx = FunctionCtx {ctxEnv = env, ctxMachine = OperandVar machine, ctxFunctionName = grinFunctionName function, ctxRoots = roots}
      valueEnv = Map.fromList [(var, Typed (OperandVar lirVar) ty) | (var, lirVar, ty) <- parameters]
  compileExpr ctx valueEnv (grinFunctionBody function)
  finishFunction
    (functionSymbol (grinFunctionName function))
    (if lowerExposeFunctions (envOptions env) then Export else Internal)
    ((machine, Ptr) : [(lirVar, ty) | (_, lirVar, ty) <- parameters])
    []
    AihcConvention

varBase :: GrinVar -> Text
varBase var = T.filter (\character -> character /= '"' && character /= '\\') (grinVarName var)

-- | The largest root list of a reservation in the expression.
maximumRoots :: GrinExpr -> Int
maximumRoots expression =
  case expression of
    GrinBind _ (GrinEnsureHeap _ roots) body -> max (length roots) (maximumRoots body)
    GrinBind _ value body -> max (maximumRoots value) (maximumRoots body)
    GrinStoreRec _ body -> maximumRoots body
    GrinStoreRecUnchecked _ body -> maximumRoots body
    GrinCase _ _ alternatives -> maximum (0 : map (maximumRoots . grinAltRhs) alternatives)
    GrinEnsureHeap _ roots -> length roots
    _ -> 0

compileExpr :: FunctionCtx -> ValueEnv -> GrinExpr -> LowerM ()
compileExpr ctx env expression =
  case expression of
    GrinBind vars value body -> do
      env' <- compileBinding ctx env vars value
      compileExpr ctx env' body
    GrinStoreRec bindings body -> compileStoreRec "aihc_make_node" bindings body
    GrinStoreRecUnchecked bindings body -> compileStoreRec "aihc_make_node_unchecked" bindings body
    GrinCpsEval runtimeRep value continuation updateContinuation -> do
      valueOperand <- pointerValue ctx env value
      continuationOperand <- pointerValue ctx env continuation
      updateOperand <- pointerValue ctx env updateContinuation
      eval <- requireHelper HelperEval
      terminate (TailCall eval [ctxMachine ctx, valueOperand, OperandLiteral (LitInt (if isLiftedRuntimeRep runtimeRep then 1 else 0)), continuationOperand, updateOperand])
    GrinCall _ name arguments -> do
      target <- functionTarget (ctxEnv ctx) name
      parameters <- maybe (failWith (LowerMissingFunction name)) pure (Map.lookup name (envFunctionParameters (ctxEnv ctx)))
      when (length parameters /= length arguments) $ failWith (LowerUnsupportedExpression ("call arity mismatch for " <> unFunctionName name))
      values <- mapM (materialize ctx env) arguments
      operands <- zipWithM coerce parameters values
      terminate (TailCall target (ctxMachine ctx : operands))
    GrinCpsPrimitiveCall runtimeRep name arguments continuation -> compileCpsPrimitive ctx env runtimeRep name arguments continuation
    GrinCpsApply _ function arguments continuation -> do
      functionOperand <- pointerValue ctx env function
      continuationOperand <- pointerValue ctx env continuation
      values <- mapM (materialize ctx env) arguments
      apply <- requireHelper (HelperApply (map typedType values))
      terminate (TailCall apply (ctxMachine ctx : functionOperand : continuationOperand : map typedOperand values))
    GrinContinue continuation values -> do
      continuationOperand <- pointerValue ctx env continuation
      typedValues <- mapM (materialize ctx env) values
      continueTransfer ctx continuationOperand typedValues
    GrinCpsRaise exception continuation -> do
      exceptionOperand <- pointerValue ctx env exception
      continuationOperand <- pointerValue ctx env continuation
      resume <- callRuntime "aihc_raise" [Ptr, Ptr, Ptr] [Ptr] [ctxMachine ctx, exceptionOperand, continuationOperand]
      resumeTransfer ctx resume
    GrinHalt _ -> do
      entry <- callRuntime "aihc_halt" [Ptr] [Code] [ctxMachine ctx]
      terminate (TailCallIndirect entry [ctxMachine ctx] (Signature [Ptr] [] AihcConvention))
    -- A POSIX process exits at once. A WASI P3 component records the
    -- status and halts, so the driver reports it when the machine returns.
    GrinExit status -> do
      statusOperand <- materialize ctx env status >>= coerce I64
      target <- targetM
      case lowerHost target of
        PosixHost -> do
          _ <- callRuntime "aihc_exit_process" [I64] [] [statusOperand]
          terminate (Trap "unreachable")
        Wasip3Host -> do
          _ <- callRuntime "aihc_set_exit_status" [Ptr, I64] [] [ctxMachine ctx, statusOperand]
          entry <- callRuntime "aihc_halt" [Ptr] [Code] [ctxMachine ctx]
          terminate (TailCallIndirect entry [ctxMachine ctx] (Signature [Ptr] [] AihcConvention))
    GrinCase scrutinee binder alternatives -> compileCase ctx env scrutinee binder alternatives
    GrinConstant {} -> unsupported "direct-style constant return after CPS"
    GrinStore {} -> unsupported "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupported "unbound heap reservation"
    GrinStoreUnchecked {} -> unsupported "unbound unchecked store"
    GrinUpdate {} -> unsupported "direct-style update after CPS"
    GrinUpdateBlackhole {} -> unsupported "unbound blackhole update"
    GrinEval {} -> unsupported "direct-style eval after CPS"
    GrinPrimitiveCall {} -> unsupported "unbound primitive call after CPS"
    GrinApply {} -> unsupported "direct-style apply after CPS"
    GrinThrow {} -> unsupported "throw"
    GrinCatch {} -> unsupported "catch"
    GrinForeignCallExpr {} -> unsupported "unbound foreign call after CPS"
  where
    unsupported = failWith . LowerUnsupportedExpression
    compileStoreRec allocator bindings body = do
      allocated <- forM bindings $ \(var, node) -> do
        object <- allocateNode ctx allocator node
        pure (var, object)
      let env' = Map.fromList allocated `Map.union` env
      forM_ allocated $ \(var, object) ->
        for_ (lookup var bindings) (initializeFields ctx env' object)
      compileExpr ctx env' body

functionTarget :: LowerEnv -> FunctionName -> LowerM Symbol
functionTarget env name = maybe (failWith (LowerMissingFunction name)) pure (Map.lookup name (envFunctionSymbols env))

-- | The single result of one extern C call.
callRuntime :: Text -> [Type] -> [Type] -> [Operand] -> LowerM Operand
callRuntime name parameters results arguments = do
  let symbol = Symbol name
  requireExtern symbol parameters results
  case results of
    [ty] -> typedOperand <$> emitValue "result" ty (Call symbol arguments)
    _ -> do
      vars <- mapM (const (fresh "result")) results
      emit vars (Call symbol arguments)
      pure (OperandLiteral LitNull)

continueTransfer :: FunctionCtx -> Operand -> [Typed] -> LowerM ()
continueTransfer ctx continuation values = do
  continue <- requireHelper (HelperContinue (map typedType values))
  terminate (TailCall continue (ctxMachine ctx : continuation : map typedOperand values))

resumeTransfer :: FunctionCtx -> Operand -> LowerM ()
resumeTransfer ctx resume = do
  helper <- requireHelper HelperResume
  terminate (TailCall helper [ctxMachine ctx, resume])

compileCpsPrimitive :: FunctionCtx -> ValueEnv -> GrinRep -> Text -> [GrinValue] -> GrinValue -> LowerM ()
compileCpsPrimitive ctx env runtimeRep name arguments continuation =
  case nativeCpsPrimitiveCall name of
    Just runtimeCall | nativeCpsCallOperandCount runtimeCall == length arguments -> do
      continuationOperand <- pointerValue ctx env continuation
      values <- mapM (materialize ctx env) arguments
      let (parameterTypes, resultType) = cpsCallSignature name
      operands <- zipWithM coerce parameterTypes values
      let symbol = nativeCpsCallSymbol runtimeCall
          callArguments = ctxMachine ctx : operands <> [continuationOperand | nativeCpsCallPassContinuation runtimeCall]
          callParameters = Ptr : parameterTypes <> [Ptr | nativeCpsCallPassContinuation runtimeCall]
      result <- callRuntime symbol callParameters [resultType] callArguments
      case nativeCpsCallTransfer runtimeCall of
        NativeCpsEnterContinuation -> do
          let resultRep = case runtimeRepComponents runtimeRep of
                rep : _ -> rep
                [] -> IntRep
          value <- coerceTo (repType resultRep) (Typed result resultType)
          continueTransfer ctx continuationOperand [value]
        NativeCpsResumeScheduler -> resumeTransfer ctx result
    _
      | lowerUnitKind (envOptions (ctxEnv ctx)) == LibraryUnit -> do
          _ <- callRuntime "aihc_unsupported_primitive" [] [] []
          terminate (Trap "unreachable")
      | otherwise -> failWith (LowerUnsupportedExpression ("CPS primitive call " <> name))

-- | The C parameter types after the machine, and the C result type, of each
-- CPS runtime call.
cpsCallSignature :: Text -> ([Type], Type)
cpsCallSignature name =
  case name of
    "fork#" -> ([Ptr], I64)
    "newMVar#" -> ([], Ptr)
    "putMVar#" -> ([Ptr, I64], Ptr)
    "readMVar#" -> ([Ptr], Ptr)
    "takeMVar#" -> ([Ptr], Ptr)
    "yield#" -> ([], Ptr)
    _ -> ([Ptr], Ptr)

compileBinding :: FunctionCtx -> ValueEnv -> [GrinVar] -> GrinExpr -> LowerM ValueEnv
compileBinding ctx env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values -> do
          bound <- forM (zip vars values) $ \(var, value) -> do
            typed <- materialize ctx env value >>= coerceTo (repType (grinVarRuntimeRep var))
            pure (var, typed)
          pure (Map.fromList bound `Map.union` env)
    GrinStore node -> allocateAndInitialize "aihc_make_node" node
    GrinStoreUnchecked node -> allocateAndInitialize "aihc_make_node_unchecked" node
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          words' <- materialize ctx env requiredWords >>= coerce I64
          rootOperands <- mapM (pointerValue ctx env) roots
          array <-
            case (roots, ctxRoots ctx) of
              ([], _) -> pure (OperandLiteral LitNull)
              (_, Just array) -> pure array
              _ -> failWith (LowerUnsupportedExpression "internal: roots without a root array")
          forM_ (zip [0 :: Int ..] rootOperands) $ \(index, root) ->
            storeSlot Ptr root array (toInteger (8 * index))
          _ <- callRuntime "aihc_ensure_heap" [Ptr, I64, I64, Ptr] [] [ctxMachine ctx, words', OperandLiteral (LitInt (toInteger (length roots))), array]
          relocated <- forM (zip [0 :: Int ..] vars) $ \(index, var) -> do
            typed <- loadSlot (varBase var) Ptr array (toInteger (8 * index))
            pure (var, typed)
          pure (Map.fromList relocated `Map.union` env)
      | otherwise -> failWith (LowerUnsupportedExpression "heap reservation result arity")
    GrinUpdate pointer value -> update "aihc_update" False pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_update_blackhole" True pointer value
    GrinPrimitiveCall runtimeRep name arguments -> compilePrimitive ctx env vars runtimeRep name arguments
    GrinForeignCallExpr foreignCall arguments ->
      compileForeignCall ctx env foreignCall arguments >>= bindResults
    _ -> failWith (LowerUnsupportedExpression "non-direct expression remained in a CPS bind")
  where
    allocateAndInitialize allocator node = do
      object <- allocateNode ctx allocator node
      initializeFields ctx env object node
      bindResults [object]
    update symbol passMachine pointer value = do
      pointerOperand <- pointerValue ctx env pointer
      valueTyped <- materialize ctx env value
      valueOperand <- coerce Ptr valueTyped
      let arguments = [ctxMachine ctx | passMachine] <> [pointerOperand, valueOperand]
      _ <- callRuntime symbol (map (const Ptr) arguments) [] arguments
      bindResults [valueTyped]
    bindResults = bindVars env vars

-- | Bind the result variables of a direct expression, converting each value
-- to the representation of its variable.
bindVars :: ValueEnv -> [GrinVar] -> [Typed] -> LowerM ValueEnv
bindVars env vars values
  | length vars /= length values = failWith (LowerUnsupportedExpression "direct expression result arity")
  | otherwise = do
      bound <- forM (zip vars values) $ \(var, value) -> do
        typed <- coerceTo (repType (grinVarRuntimeRep var)) value
        pure (var, typed)
      pure (Map.fromList bound `Map.union` env)

allocateNode :: FunctionCtx -> Text -> GrinNode -> LowerM Typed
allocateNode ctx allocator node = do
  info <- nodeInfoSymbol (ctxEnv ctx) node
  object <-
    if isPartialConstructorNode node
      then
        -- The shared info table of an unsaturated constructor does not say
        -- how wide this stage is, so the allocator is told and writes the
        -- count into the object.
        callRuntime
          (partialAllocator allocator)
          [Ptr, Ptr, I64]
          [Ptr]
          [ctxMachine ctx, OperandLiteral (LitSymbol info), OperandLiteral (LitInt (toInteger (length (grinNodeFields node))))]
      else callRuntime allocator [Ptr, Ptr] [Ptr] [ctxMachine ctx, OperandLiteral (LitSymbol info)]
  pure (Typed object Ptr)

-- | The allocator of an unsaturated constructor, matching the checked or
-- unchecked node allocator it stands in for.
partialAllocator :: Text -> Text
partialAllocator allocator =
  case allocator of
    "aihc_make_node_unchecked" -> "aihc_make_partial_unchecked"
    _ -> "aihc_make_partial"

-- | An unsaturated constructor spends field zero on its applied count, so its
-- payload starts one slot later than every other object's.
isPartialConstructorNode :: GrinNode -> Bool
isPartialConstructorNode node =
  case grinNodeTag node of
    GrinConstructor _ remaining -> constructorStage remaining == PartialConstructor
    _ -> False

initializeFields :: FunctionCtx -> ValueEnv -> Typed -> GrinNode -> LowerM ()
initializeFields ctx env object node =
  forM_ (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    typed <- materialize ctx env field
    storeSlot (typedType typed) (typedOperand typed) (typedOperand object) (toInteger (8 * (index + 1 + payloadShift)))
  where
    payloadShift = if isPartialConstructorNode node then 1 else 0

-- | A GRIN value as a typed Lir operand.
materialize :: FunctionCtx -> ValueEnv -> GrinValue -> LowerM Typed
materialize ctx env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var env of
        Just typed -> pure typed
        Nothing -> globalReference (grinVarName var)
    GrinGlobalValue name -> globalReference name
    GrinLitValue literal ->
      case literal of
        GrinLitAddr bytes -> do
          symbol <- addrLiteralSymbol (ctxEnv ctx) bytes
          pure (Typed (OperandLiteral (LitSymbol symbol)) Ptr)
        _ -> case normalizedLiteralInteger literal of
          Just integer -> pure (Typed (OperandLiteral (LitInt integer)) I64)
          Nothing -> failWith (LowerUnsupportedValue "string literal")
  where
    globalReference name = do
      let symbol = globalSymbol name
      requireExternData symbol
      pure (Typed (OperandLiteral (LitSymbol symbol)) Ptr)

pointerValue :: FunctionCtx -> ValueEnv -> GrinValue -> LowerM Operand
pointerValue ctx env value = materialize ctx env value >>= coerce Ptr

-- | A foreign call gives one value, or none for a C procedure.
compileForeignCall :: FunctionCtx -> ValueEnv -> GrinForeignCall -> [GrinValue] -> LowerM [Typed]
compileForeignCall ctx env foreignCall arguments =
  case grinForeignCallTarget foreignCall of
    -- An address import materializes the symbol address instead of calling it.
    GrinForeignAddress
      | null arguments -> do
          let symbol = Symbol (grinForeignCallSymbol foreignCall)
          requireExternData symbol
          pure [Typed (OperandLiteral (LitSymbol symbol)) Ptr]
      | otherwise -> failWith (LowerUnsupportedExpression "address foreign import with arguments")
    GrinForeignFunction -> compileCCall ctx env False foreignCall arguments

compileRuntimeCall :: FunctionCtx -> ValueEnv -> NativeRuntimeCall -> [GrinValue] -> LowerM Typed
compileRuntimeCall ctx env runtimeCall arguments = do
  results <- compileCCall ctx env (nativeRuntimeCallPassMachine runtimeCall) (nativeRuntimeCallForeignCall runtimeCall) arguments
  case results of
    [result] -> pure result
    _ -> failWith (LowerUnsupportedExpression "runtime call without a result")

compileCCall :: FunctionCtx -> ValueEnv -> Bool -> GrinForeignCall -> [GrinValue] -> LowerM [Typed]
compileCCall ctx env passMachine foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      (parameters, results) = runtimeCallSignature passMachine signature
  when (length arguments /= length (grinForeignArgumentTypes signature)) $ failWith (LowerUnsupportedExpression "foreign call arity mismatch")
  values <- mapM (materialize ctx env) arguments
  operands <- zipWithM coerce (drop (fromEnum passMachine) parameters) values
  resultOperand <- callRuntime (grinForeignCallSymbol foreignCall) parameters results ([ctxMachine ctx | passMachine] <> operands)
  case results of
    [result] -> (: []) <$> extendForeignResult (grinForeignResultType signature) (Typed resultOperand result)
    _ -> pure []

-- | The Lir signature of a C runtime or foreign function.
runtimeCallSignature :: Bool -> GrinForeignSignature -> ([Type], [Type])
runtimeCallSignature passMachine signature =
  ([Ptr | passMachine] <> map foreignType (grinForeignArgumentTypes signature), maybeToList (foreignResultType (grinForeignResultType signature)))

-- | The C ABI passes integers narrower than 32 bits extended to 32 bits, and
-- GRIN keeps them extended to 64 bits, so their low 32 bits are the C value.
foreignType :: GrinForeignType -> Type
foreignType ty =
  case ty of
    GrinForeignInt -> I64
    GrinForeignInt8 -> I32
    GrinForeignInt16 -> I32
    GrinForeignInt32 -> I32
    GrinForeignInt64 -> I64
    GrinForeignWord -> I64
    GrinForeignWord8 -> I32
    GrinForeignWord16 -> I32
    GrinForeignWord32 -> I32
    GrinForeignWord64 -> I64
    GrinForeignFloat -> F32
    GrinForeignDouble -> F64
    GrinForeignAddr -> Ptr
    GrinForeignVoid -> I32

foreignResultType :: GrinForeignType -> Maybe Type
foreignResultType ty =
  case ty of
    GrinForeignVoid -> Nothing
    _ -> Just (foreignType ty)

-- | Extend a narrow C result to 64 bits from its own width, because the high
-- bits of a narrow result register are unspecified.
extendForeignResult :: GrinForeignType -> Typed -> LowerM Typed
extendForeignResult foreignTy (Typed operand actual) =
  case (foreignTy, actual) of
    (GrinForeignInt8, I32) -> extend SExt I8
    (GrinForeignInt16, I32) -> extend SExt I16
    (GrinForeignInt32, I32) -> emitValue "foreign_result" I64 (Convert SExt I32 operand I64)
    (GrinForeignWord8, I32) -> extend ZExt I8
    (GrinForeignWord16, I32) -> extend ZExt I16
    (GrinForeignWord32, I32) -> emitValue "foreign_result" I64 (Convert ZExt I32 operand I64)
    -- The float result returns to the integer slot it came from.
    (GrinForeignDouble, F64) -> emitValue "foreign_result" I64 (Convert Bitcast F64 operand I64)
    (GrinForeignFloat, F32) -> do
      bits <- emitValue "foreign_bits" I32 (Convert Bitcast F32 operand I32)
      emitValue "foreign_result" I64 (Convert ZExt I32 (typedOperand bits) I64)
    _ -> pure (Typed operand actual)
  where
    extend op narrowTy = do
      narrowed <- emitValue "foreign_narrow" narrowTy (Convert Trunc I32 operand narrowTy)
      emitValue "foreign_result" I64 (Convert op narrowTy (typedOperand narrowed) I64)

-- Primitives

compilePrimitive :: FunctionCtx -> ValueEnv -> [GrinVar] -> GrinRep -> Text -> [GrinValue] -> LowerM ValueEnv
compilePrimitive ctx env vars runtimeRep name arguments =
  case (name, arguments) of
    (_, [left, right])
      | Just op <- lookup name binaryPrimitives -> do
          leftOperand <- word left
          rightOperand <- word right
          result <- emitValue "result" I64 (Binary op I64 leftOperand rightOperand)
          bind [result]
      | Just op <- lookup name comparisonPrimitives -> do
          leftOperand <- word left
          rightOperand <- word right
          flag <- emitValue "flag" I1 (Compare op I64 leftOperand rightOperand)
          result <- widen flag
          bind [result]
      | Just op <- lookup name addressComparisonPrimitives -> do
          leftOperand <- pointerValue ctx env left
          rightOperand <- pointerValue ctx env right
          flag <- emitValue "flag" I1 (Compare op Ptr leftOperand rightOperand)
          result <- widen flag
          bind [result]
      | Just (op, ty) <- lookup name floatBinaryPrimitives -> do
          leftOperand <- floatOperand ty left
          rightOperand <- floatOperand ty right
          result <- emitValue "result" ty (FloatBinary op ty leftOperand rightOperand)
          bits <- floatBits ty result
          bind [bits]
      | Just (op, ty) <- lookup name floatComparisonPrimitives -> do
          leftOperand <- floatOperand ty left
          rightOperand <- floatOperand ty right
          flag <- emitValue "flag" I1 (Compare op ty leftOperand rightOperand)
          result <- widen flag
          bind [result]
    ("compareInt#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      less <- emitValue "less" I1 (Compare LtS I64 leftOperand rightOperand) >>= widen
      greater <- emitValue "greater" I1 (Compare GtS I64 leftOperand rightOperand) >>= widen
      result <- emitValue "result" I64 (Binary Sub I64 (typedOperand greater) (typedOperand less))
      bind [result]
    ("not#", [value]) -> do
      operand <- word value
      result <- emitValue "result" I64 (Binary Xor I64 operand (OperandLiteral (LitInt (-1))))
      bind [result]
    ("addIntC#", [left, right]) -> signedCarry Add left right
    ("subIntC#", [left, right]) -> signedCarry Sub left right
    ("addWordC#", [left, right]) -> unsignedCarry AddCarry left right
    ("subWordC#", [left, right]) -> unsignedCarry SubBorrow left right
    ("timesWord2#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      low <- fresh "low"
      high <- fresh "high"
      emit [low, high] (Wide MulWideU I64 leftOperand rightOperand)
      bind [Typed (OperandVar high) I64, Typed (OperandVar low) I64]
    -- The high half is necessary when it is not the sign extension of the low
    -- half.
    ("timesInt2#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      low <- fresh "low"
      high <- fresh "high"
      emit [low, high] (Wide MulWideS I64 leftOperand rightOperand)
      sign <- emitValue "sign" I64 (Binary ShrS I64 (OperandVar low) (OperandLiteral (LitInt 63)))
      needed <- emitValue "needed" I1 (Compare Ne I64 (OperandVar high) (typedOperand sign))
      neededWord <- widen needed
      bind [neededWord, Typed (OperandVar high) I64, Typed (OperandVar low) I64]
    ("quotRemWord#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      quotient <- emitValue "quotient" I64 (Binary DivU I64 leftOperand rightOperand)
      remainder <- emitValue "remainder" I64 (Binary RemU I64 leftOperand rightOperand)
      bind [quotient, remainder]
    ("quotRemWord2#", [high, low, divisor]) -> do
      operands <- mapM word [high, low, divisor]
      helper <- requireHelper HelperQuotRem2
      quotient <- fresh "quotient"
      remainder <- fresh "remainder"
      emit [quotient, remainder] (Call helper operands)
      bind [Typed (OperandVar quotient) I64, Typed (OperandVar remainder) I64]
    ("nullAddr#", []) -> bind [Typed (OperandLiteral LitNull) Ptr]
    ("realWorld#", [])
      | null vars && null (runtimeRepComponents runtimeRep) -> pure env
    ("plusAddr#", [address, offset]) -> do
      base <- pointerValue ctx env address
      delta <- word offset
      result <- emitValue "address" Ptr (PtrAdd base delta)
      bind [result]
    ("minusAddr#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      result <- emitValue "result" I64 (Binary Sub I64 leftOperand rightOperand)
      bind [result]
    ("addr2Int#", [address]) -> do
      operand <- word address
      bind [Typed operand I64]
    ("int2Addr#", [value]) -> do
      operand <- pointerValue ctx env value
      bind [Typed operand Ptr]
    ("cstringLength#", [address]) -> do
      operand <- pointerValue ctx env address
      helper <- requireHelper HelperCStringLength
      result <- emitValue "length" I64 (Call helper [operand])
      bind [result]
    -- The collector uses explicit root lists, thus touch# keeps no value
    -- alive and gives no code.
    ("touch#", [_])
      | null vars -> pure env
    ("float2Double#", [value]) -> do
      operand <- floatOperand F32 value
      result <- emitValue "result" F64 (Convert FpExt F32 operand F64)
      bits <- floatBits F64 result
      bind [bits]
    ("double2Float#", [value]) -> do
      operand <- floatOperand F64 value
      result <- emitValue "result" F32 (Convert FpTrunc F64 operand F32)
      bits <- floatBits F32 result
      bind [bits]
    (_, [address, index])
      | Just (ty, scale) <- lookup name addressLoadPrimitives -> do
          target <- addressElement address index scale
          value <- emitValue "value" ty (Load ty (Address target 0) 1)
          result <- if ty == I64 then pure value else emitValue "value" I64 (Convert ZExt ty (typedOperand value) I64)
          bind [result]
    (_, [address, index, value])
      | Just (ty, scale) <- lookup name addressStorePrimitives -> do
          target <- addressElement address index scale
          operand <- word value
          narrow <- if ty == I64 then pure operand else typedOperand <$> emitValue "narrow" ty (Convert Trunc I64 operand ty)
          emit [] (Store ty narrow (Address target 0) 1)
          bind []
    (_, [value])
      | name `elem` identityPrimitives -> do
          typed <- materialize ctx env value
          bind [typed]
      | Just (ty, op) <- lookup name narrowPrimitives -> do
          operand <- word value
          narrow <- emitValue "narrow" ty (Convert Trunc I64 operand ty)
          result <- emitValue "result" I64 (Convert op ty (typedOperand narrow) I64)
          bind [result]
      | Just shift <- lookup name byteSwapPrimitives -> do
          operand <- word value
          result <- byteSwap shift operand
          bind [result]
      | Just op <- lookup name bitCountPrimitives -> do
          operand <- word value
          result <- emitValue "count" I64 (Unary op I64 operand)
          bind [result]
      | Just (op, ty) <- lookup name floatUnaryPrimitives -> do
          operand <- floatOperand ty value
          result <- emitValue "result" ty (FloatUnary op ty operand)
          bits <- floatBits ty result
          bind [bits]
      | Just ty <- lookup name intToFloatPrimitives -> do
          operand <- word value
          result <- emitValue "result" ty (Convert IToFS I64 operand ty)
          bits <- floatBits ty result
          bind [bits]
      | Just ty <- lookup name floatToIntPrimitives -> do
          operand <- floatOperand ty value
          result <- emitValue "result" I64 (Convert FToIS ty operand I64)
          bind [result]
    ("casMutVar#", [reference, expected, replacement])
      | Just swapCall <- nativeRuntimePrimitiveCall "casMutVar#",
        Just readCall <- nativeRuntimePrimitiveCall "readMutVar#" -> do
          flag <- compileRuntimeCall ctx env swapCall [reference, expected, replacement]
          current <- compileRuntimeCall ctx env readCall [reference]
          bind [flag, current]
    _
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          result <- compileRuntimeCall ctx env runtimeCall arguments
          case nativeRuntimeCallResultCount runtimeCall of
            0 | null vars -> pure env
            1 -> bind [result]
            _ -> failWith (LowerUnsupportedExpression ("runtime primitive result arity " <> name))
      | lowerUnitKind (envOptions (ctxEnv ctx)) == LibraryUnit -> do
          _ <- callRuntime "aihc_unsupported_primitive" [] [] []
          bind [zeroValue (repType (grinVarRuntimeRep var)) | var <- vars]
      | otherwise -> failWith (LowerUnsupportedPrimitive name)
  where
    bind = bindVars env vars
    word value = materialize ctx env value >>= coerce I64
    widen (Typed flag _) = emitValue "wide" I64 (Convert ZExt I1 flag I64)
    zeroValue ty = Typed (OperandLiteral (if ty == Ptr then LitNull else LitInt 0)) ty
    -- The address of element @index@ of the given width. Every access uses
    -- alignment one, because the source can give an unaligned address.
    addressElement address index scale = do
      base <- pointerValue ctx env address
      offset <- word index
      scaled <-
        if scale == 1
          then pure offset
          else typedOperand <$> emitValue "offset" I64 (Binary Mul I64 offset (OperandLiteral (LitInt scale)))
      typedOperand <$> emitValue "address" Ptr (PtrAdd base scaled)
    -- A Float# value travels as its bit pattern in the low 32 bits and a
    -- Double# value as its 64-bit pattern.
    floatOperand ty value = do
      operand <- word value
      case ty of
        F32 -> do
          narrow <- emitValue "bits" I32 (Convert Trunc I64 operand I32)
          typedOperand <$> emitValue "float" F32 (Convert Bitcast I32 (typedOperand narrow) F32)
        _ -> typedOperand <$> emitValue "double" F64 (Convert Bitcast I64 operand F64)
    floatBits ty (Typed operand _) =
      case ty of
        F32 -> do
          bits <- emitValue "bits" I32 (Convert Bitcast F32 operand I32)
          emitValue "result" I64 (Convert ZExt I32 (typedOperand bits) I64)
        _ -> emitValue "result" I64 (Convert Bitcast F64 operand I64)
    -- A byte swap of a narrow value moves the value to the high bytes first,
    -- thus one 64-bit swap gives every width.
    byteSwap shift operand = do
      shifted <-
        if shift == 0
          then pure operand
          else typedOperand <$> emitValue "shifted" I64 (Binary Shl I64 operand (OperandLiteral (LitInt shift)))
      result <- foldM swapStage shifted [32, 16, 8]
      pure (Typed result I64)
    swapStage value stage = do
      let mask = OperandLiteral (LitInt (byteSwapMask stage))
      high <- emitValue "swapped" I64 (Binary ShrU I64 value (OperandLiteral (LitInt stage)))
      highPart <- emitValue "swapped" I64 (Binary And I64 (typedOperand high) mask)
      lowPart <- emitValue "swapped" I64 (Binary And I64 value mask)
      low <- emitValue "swapped" I64 (Binary Shl I64 (typedOperand lowPart) (OperandLiteral (LitInt stage)))
      typedOperand <$> emitValue "swapped" I64 (Binary Or I64 (typedOperand highPart) (typedOperand low))
    signedCarry op left right = do
      leftOperand <- word left
      rightOperand <- word right
      result <- emitValue "result" I64 (Binary op I64 leftOperand rightOperand)
      -- Signed overflow: the sign of the result differs from both operands
      -- for addition, and from the left operand and the negated right operand
      -- for subtraction.
      firstBits <- emitValue "bits" I64 (Binary Xor I64 (if op == Add then typedOperand result else leftOperand) (if op == Add then leftOperand else rightOperand))
      secondBits <- emitValue "bits" I64 (Binary Xor I64 (if op == Add then typedOperand result else leftOperand) (if op == Add then rightOperand else typedOperand result))
      overflow <- emitValue "overflow" I64 (Binary And I64 (typedOperand firstBits) (typedOperand secondBits))
      flag <- emitValue "flag" I64 (Binary ShrU I64 (typedOperand overflow) (OperandLiteral (LitInt 63)))
      bind [result, flag]
    unsignedCarry op left right = do
      leftOperand <- word left
      rightOperand <- word right
      result <- fresh "result"
      carry <- fresh "carry"
      emit [result, carry] (Wide op I64 leftOperand rightOperand)
      flag <- widen (Typed (OperandVar carry) I1)
      bind [Typed (OperandVar result) I64, flag]

-- | The mask of one stage of a 64-bit byte swap. The mask keeps every other
-- block of @stage@ bits.
byteSwapMask :: Integer -> Integer
byteSwapMask stage = sum [(2 ^ stage - 1) * 2 ^ (2 * stage * block) | block <- [0 .. 64 `div` (2 * stage) - 1]]

binaryPrimitives :: [(Text, BinaryOp)]
binaryPrimitives =
  [ ("+#", Add),
    ("-#", Sub),
    ("*#", Mul),
    ("plusWord#", Add),
    ("minusWord#", Sub),
    ("timesWord#", Mul),
    ("quotWord#", DivU),
    ("remWord#", RemU),
    ("and#", And),
    ("or#", Or),
    ("xor#", Xor),
    ("uncheckedShiftL#", Shl),
    ("uncheckedShiftRL#", ShrU)
  ]

comparisonPrimitives :: [(Text, CompareOp)]
comparisonPrimitives =
  [ ("<#", LtS),
    ("==#", Eq),
    (">#", GtS),
    (">=#", GeS),
    ("<=#", LeS),
    ("/=#", Ne),
    ("eqWord#", Eq),
    ("neWord#", Ne),
    ("ltWord#", LtU),
    ("leWord#", LeU),
    ("gtWord#", GtU),
    ("geWord#", GeU),
    ("eqWord64#", Eq),
    ("neWord64#", Ne),
    ("ltWord64#", LtU),
    ("leWord64#", LeU),
    ("gtWord64#", GtU),
    ("geWord64#", GeU)
  ]

-- | Comparisons of two addresses. An address compares as an unsigned number.
addressComparisonPrimitives :: [(Text, CompareOp)]
addressComparisonPrimitives =
  [ ("eqAddr#", Eq),
    ("neAddr#", Ne),
    ("ltAddr#", LtU),
    ("leAddr#", LeU),
    ("gtAddr#", GtU),
    ("geAddr#", GeU)
  ]

-- | Reads of memory at an address. Each entry gives the width of the value
-- and the size of one index step in bytes.
addressLoadPrimitives :: [(Text, (Type, Integer))]
addressLoadPrimitives =
  [ ("indexWord8OffAddr#", (I8, 1)),
    ("readWord8OffAddr#", (I8, 1)),
    ("indexWord16OffAddr#", (I16, 2)),
    ("readWord16OffAddr#", (I16, 2)),
    ("indexWord32OffAddr#", (I32, 4)),
    ("readWord32OffAddr#", (I32, 4)),
    ("indexWord64OffAddr#", (I64, 8)),
    ("readWord64OffAddr#", (I64, 8)),
    ("indexWord8OffAddrAsWord16#", (I16, 1)),
    ("readWord8OffAddrAsWord16#", (I16, 1)),
    ("indexWord8OffAddrAsWord32#", (I32, 1)),
    ("readWord8OffAddrAsWord32#", (I32, 1)),
    ("indexWord8OffAddrAsWord64#", (I64, 1)),
    ("readWord8OffAddrAsWord64#", (I64, 1)),
    -- A Float# value travels as its bit pattern in the low 32 bits and a
    -- Double# value as its 64-bit pattern, thus the float accessors reuse
    -- the word accessors of the same width.
    ("indexWord8OffAddrAsFloat#", (I32, 1)),
    ("readWord8OffAddrAsFloat#", (I32, 1)),
    ("indexWord8OffAddrAsDouble#", (I64, 1)),
    ("readWord8OffAddrAsDouble#", (I64, 1))
  ]

-- | Writes of memory at an address, with the same widths and index steps as
-- the reads.
addressStorePrimitives :: [(Text, (Type, Integer))]
addressStorePrimitives =
  [ ("writeWord8OffAddr#", (I8, 1)),
    ("writeWord16OffAddr#", (I16, 2)),
    ("writeWord32OffAddr#", (I32, 4)),
    ("writeWord64OffAddr#", (I64, 8)),
    ("writeWord8OffAddrAsWord16#", (I16, 1)),
    ("writeWord8OffAddrAsWord32#", (I32, 1)),
    ("writeWord8OffAddrAsWord64#", (I64, 1)),
    ("writeWord8OffAddrAsFloat#", (I32, 1)),
    ("writeWord8OffAddrAsDouble#", (I64, 1))
  ]

-- | Conversions to a narrow integer. The result keeps the width of a word.
-- A word narrows without a sign and an integer keeps its sign.
narrowPrimitives :: [(Text, (Type, ConvertOp))]
narrowPrimitives =
  [ ("wordToWord8#", (I8, ZExt)),
    ("wordToWord16#", (I16, ZExt)),
    ("wordToWord32#", (I32, ZExt)),
    ("intToInt8#", (I8, SExt)),
    ("intToInt16#", (I16, SExt)),
    ("intToInt32#", (I32, SExt))
  ]

-- | Byte swaps. The value moves left by the given number of bits first, thus
-- one 64-bit swap gives the result of every width.
-- | The bit counts of a @Word#@. Lir has one operation for each, so no
-- target calls the runtime for them.
bitCountPrimitives :: [(Text, UnaryOp)]
bitCountPrimitives =
  [ ("clz#", Clz),
    ("ctz#", Ctz),
    ("popCnt#", Popcount)
  ]

byteSwapPrimitives :: [(Text, Integer)]
byteSwapPrimitives =
  [ ("byteSwap16#", 48),
    ("byteSwap32#", 32),
    ("byteSwap64#", 0),
    ("byteSwap#", 0)
  ]

floatBinaryPrimitives :: [(Text, (FloatBinaryOp, Type))]
floatBinaryPrimitives =
  [ ("plusFloat#", (FAdd, F32)),
    ("minusFloat#", (FSub, F32)),
    ("timesFloat#", (FMul, F32)),
    ("+##", (FAdd, F64)),
    ("-##", (FSub, F64)),
    ("*##", (FMul, F64)),
    ("divideFloat#", (FDiv, F32)),
    ("/##", (FDiv, F64))
  ]

floatUnaryPrimitives :: [(Text, (FloatUnaryOp, Type))]
floatUnaryPrimitives =
  [ ("negateFloat#", (FNeg, F32)),
    ("fabsFloat#", (FAbs, F32)),
    ("negateDouble#", (FNeg, F64)),
    ("fabsDouble#", (FAbs, F64)),
    ("sqrtFloat#", (FSqrt, F32)),
    ("sqrtDouble#", (FSqrt, F64))
  ]

floatComparisonPrimitives :: [(Text, (CompareOp, Type))]
floatComparisonPrimitives =
  [ ("gtFloat#", (FGt, F32)),
    ("ltFloat#", (FLt, F32)),
    ("eqFloat#", (Eq, F32)),
    (">##", (FGt, F64)),
    ("<##", (FLt, F64)),
    ("==##", (Eq, F64))
  ]

intToFloatPrimitives :: [(Text, Type)]
intToFloatPrimitives =
  [ ("int2Float#", F32),
    ("int2Double#", F64)
  ]

-- | Conversions of a float to an integer. GHC gives no result outside the
-- range of an integer. Lir has no undefined behavior, thus the conversion
-- traps there.
floatToIntPrimitives :: [(Text, Type)]
floatToIntPrimitives =
  [ ("float2Int#", F32),
    ("double2Int#", F64)
  ]

identityPrimitives :: [Text]
identityPrimitives =
  [ "int2Word#",
    "word2Int#",
    "word8ToWord#",
    "word32ToWord#",
    "word64ToWord#",
    "wordToWord64#",
    "word16ToWord#",
    "ord#",
    "chr#",
    "unsafeFreezeArray#",
    "unsafeThawArray#",
    "unsafeFreezeByteArray#",
    "unsafeThawByteArray#",
    "castFloatToWord32#",
    "castWord32ToFloat#",
    "castDoubleToWord64#",
    "castWord64ToDouble#",
    "int8ToInt#",
    "int16ToInt#",
    "int32ToInt#",
    "intToInt64#",
    "int64ToInt#"
  ]

-- Case

compileCase :: FunctionCtx -> ValueEnv -> GrinValue -> GrinVar -> [GrinAlt] -> LowerM ()
compileCase ctx env scrutinee binder alternatives = do
  typed <- materialize ctx env scrutinee
  binderValue <- coerceTo (repType (grinVarRuntimeRep binder)) typed
  let env' = Map.insert binder binderValue env
      isPointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
  targets <- forM alternatives $ \alternative -> do
    label <- freshLabel "alt"
    pure (alternative, label)
  let defaultTargets = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  fallback <-
    case defaultTargets of
      label : _ -> pure label
      [] -> freshLabel "no_match"
  if isPointer
    then do
      header <- loadSlot "header" Ptr (typedOperand typed) 0
      identity <- loadInfoPointer "identity" (typedOperand header) 0
      checks <- forM [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt] $ \(alternative, label) ->
        case grinAltCon alternative of
          GrinDataAlt name -> do
            let symbol = constructorInfoSymbol name 0
            unless (Map.member (ConstructorRuntimeInfo name SaturatedConstructor) (envInfoSymbols (ctxEnv ctx))) (requireExternData symbol)
            pure (symbol, label)
          _ -> failWith (LowerUnsupportedExpression "literal case on a lifted value")
      pointerChecks identity checks fallback
    else do
      wordValue <- coerce I64 typed
      cases <- forM [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt] $ \(alternative, label) ->
        case grinAltCon alternative of
          GrinLitAlt literal ->
            case normalizedLiteralInteger literal of
              Just integer -> pure (SwitchCase integer (Target label []))
              Nothing -> failWith (LowerUnsupportedValue "string case alternative")
          _ -> failWith (LowerUnsupportedExpression "constructor case on an unboxed value")
      terminate (Switch I64 wordValue (firstCases cases) (Just (Target fallback [])))
  when (null defaultTargets) $ do
    beginBlock fallback []
    _ <- callRuntime "aihc_no_match" [] [] []
    terminate (Trap "no matching case alternative")
  forM_ targets $ \(alternative, label) -> do
    beginBlock label []
    env'' <- bindAlternative alternative typed env'
    compileExpr ctx env'' (grinAltRhs alternative)
  where
    pointerChecks identity checks fallback =
      case checks of
        [] -> terminate (Jump (Target fallback []))
        (symbol, label) : rest -> do
          matches <- emitValue "matches" I1 (Compare Eq Ptr (typedOperand identity) (OperandLiteral (LitSymbol symbol)))
          next <- if null rest then pure fallback else freshLabel "check"
          terminate (Branch (typedOperand matches) (Target label []) (Target next []))
          unless (null rest) $ do
            beginBlock next []
            pointerChecks identity rest fallback
    firstCases = go Set.empty
      where
        go _ [] = []
        go seen (switchCase : rest)
          | switchCaseValue switchCase `Set.member` seen = go seen rest
          | otherwise = switchCase : go (Set.insert (switchCaseValue switchCase) seen) rest
    bindAlternative alternative typed env' =
      case grinAltCon alternative of
        GrinDataAlt _ -> do
          let live = freeExprVars (grinAltRhs alternative)
          bound <- forM [(index, field) | (index, field) <- zip [0 :: Int ..] (grinAltBinders alternative), field `Set.member` live] $ \(index, field) -> do
            let ty = repType (grinVarRuntimeRep field)
            value <- loadSlot (varBase field) ty (typedOperand typed) (toInteger (8 * (index + 1)))
            pure (field, value)
          pure (Map.fromList bound `Map.union` env')
        GrinLitAlt _ -> pure env'
        GrinDefaultAlt -> do
          bound <- forM (grinAltBinders alternative) $ \field -> do
            value <- coerceTo (repType (grinVarRuntimeRep field)) typed
            pure (field, value)
          pure (Map.fromList bound `Map.union` env')

-- Executable entry

-- | The @main@ of the executable and the special continuations: the top
-- continuation applies the evaluated entry, the final continuation halts,
-- the update continuation is the GC-GRIN update function, and the thread
-- done continuation returns to the scheduler.
lowerExecutableMain :: GcGrinProgram -> LowerM ()
lowerExecutableMain gcProgram = do
  entryItems gcProgram
  argc <- fresh "argc"
  argv <- fresh "argv"
  beginBlock (Label "entry") []
  _ <- callRuntime "aihc_program_arguments_initialize" [I32, Ptr] [] [OperandVar argc, OperandVar argv]
  _ <- startMachine
  terminate (Return [OperandLiteral (LitInt 0)])
  finishFunction (Symbol "main") Export [(argc, I32), (argv, Ptr)] [I32] CConvention

-- | The WASI P3 entry unit. The P3 driver initializes the program
-- arguments, calls @aihc_lir_program_start@, and calls
-- @aihc_lir_program_resume@ with the scheduler resumption of every
-- completed IO request. Both return one when the program has halted and zero
-- when every thread waits for IO. The machine is published in the C global
-- @aihc_machine@ for the driver.
lowerWasip3Entry :: GcGrinProgram -> LowerM ()
lowerWasip3Entry gcProgram = do
  entryItems gcProgram
  requireExternData wasmMachineSymbol
  do
    beginBlock (Label "entry") []
    machine <- startMachine
    emit [] (Store Ptr machine (Address (OperandLiteral (LitSymbol wasmMachineSymbol)) 0) 4)
    finished <- loadFinished
    terminate (Return [finished])
    finishFunction (Symbol "aihc_lir_program_start") Export [] [I32] CConvention
  do
    resume <- fresh "resume"
    beginBlock (Label "entry") []
    machine <- emitValue "machine" Ptr (Load Ptr (Address (OperandLiteral (LitSymbol wasmMachineSymbol)) 0) 4)
    helper <- requireHelper HelperResume
    emit [] (Call helper [typedOperand machine, OperandVar resume])
    finished <- loadFinished
    terminate (Return [finished])
    finishFunction (Symbol "aihc_lir_program_resume") Export [(resume, Ptr)] [I32] CConvention
  where
    loadFinished = do
      flag <- emitValue "finished" I64 (Load I64 (Address (OperandLiteral (LitSymbol finishedSymbol)) 0) 8)
      typedOperand <$> emitValue "finished" I32 (Convert Trunc I64 (typedOperand flag) I32)

wasmMachineSymbol :: Symbol
wasmMachineSymbol = Symbol "aihc_machine"

-- | The word the exit function sets when the machine halts.
finishedSymbol :: Symbol
finishedSymbol = Symbol "aihc_lir_finished"

-- | The special continuations of an executable: the top continuation
-- applies the evaluated entry, the final continuation halts, the update
-- continuation is the GC-GRIN update function, and the thread done
-- continuation returns to the scheduler.
entryItems :: GcGrinProgram -> LowerM ()
entryItems gcProgram = do
  requireExternData (globalSymbol executableEntryName)
  continuationInfoItems (ContinuationSpec finalInfo (Symbol "aihc_lir_final_applied_info") finalTarget [] [Ptr] ContinuationFrameStop)
  continuationInfoItems (ContinuationSpec topInfo (Symbol "aihc_lir_top_applied_info") topTarget [Ptr] [Ptr] ContinuationFrameNormal)
  continuationInfoItems (ContinuationSpec updateInfo (Symbol "aihc_lir_update_applied_info") (functionSymbol (gcUpdateFunction gcProgram)) [Ptr, Ptr] [Ptr] ContinuationFrameUpdate)
  continuationInfoItems (ContinuationSpec threadDoneInfo (Symbol "aihc_lir_thread_done_applied_info") threadDoneTarget [] [Ptr] ContinuationFrameStop)
  emitItem (ItemData (DataItem finishedSymbol Internal True 8 [DataInt I64 0]))
  -- The top continuation applies the evaluated entry action to no arguments
  -- with the final continuation.
  do
    machine <- fresh "machine"
    final <- fresh "final"
    result <- fresh "result"
    beginBlock (Label "entry") []
    apply <- requireHelper (HelperApply [])
    terminate (TailCall apply [OperandVar machine, OperandVar result, OperandVar final])
    finishFunction topTarget Internal [(machine, Ptr), (final, Ptr), (result, Ptr)] [] AihcConvention
  do
    machine <- fresh "machine"
    value <- fresh "value"
    beginBlock (Label "entry") []
    entry <- callRuntime "aihc_halt" [Ptr] [Code] [OperandVar machine]
    terminate (TailCallIndirect entry [OperandVar machine] (Signature [Ptr] [] AihcConvention))
    finishFunction finalTarget Internal [(machine, Ptr), (value, Ptr)] [] AihcConvention
  threadDoneContinuation threadDoneTarget
  where
    finalTarget = Symbol "aihc_lir_final_continuation"
    topTarget = Symbol "aihc_lir_top_continuation"
    threadDoneTarget = Symbol "aihc_lir_thread_done_continuation"

finalInfo, topInfo, updateInfo, threadDoneInfo :: Symbol
finalInfo = Symbol "aihc_lir_final_info"
topInfo = Symbol "aihc_lir_top_info"
updateInfo = Symbol "aihc_lir_update_info"
threadDoneInfo = Symbol "aihc_lir_thread_done_info"

-- | Create the machine and its continuations and evaluate the entry. The
-- call returns when the machine halts or when every thread waits for IO.
startMachine :: LowerM Operand
startMachine = do
  target <- targetM
  exit <- requireHelper HelperExit
  let entryGlobal = globalSymbol executableEntryName
  machine <- callRuntime "aihc_machine_new" [I64] [Ptr] [OperandLiteral (LitInt 0)]
  _ <- callRuntime "aihc_ensure_heap" [Ptr, I64, I64, Ptr] [] [machine, OperandLiteral (LitInt 7), OperandLiteral (LitInt 0), OperandLiteral LitNull]
  final <- callRuntime "aihc_make_node_unchecked" [Ptr, Ptr] [Ptr] [machine, OperandLiteral (LitSymbol finalInfo)]
  top <- callRuntime "aihc_make_node_unchecked" [Ptr, Ptr] [Ptr] [machine, OperandLiteral (LitSymbol topInfo)]
  storeSlot Ptr final top 8
  update <- callRuntime "aihc_make_node_unchecked" [Ptr, Ptr] [Ptr] [machine, OperandLiteral (LitSymbol updateInfo)]
  storeSlot Ptr top update 8
  storeSlot Ptr (OperandLiteral (LitSymbol entryGlobal)) update 16
  threadDone <- callRuntime "aihc_make_node_unchecked" [Ptr, Ptr] [Ptr] [machine, OperandLiteral (LitSymbol threadDoneInfo)]
  _ <- callRuntime "aihc_set_thread_done_continuation" [Ptr, Ptr] [] [machine, threadDone]
  -- The halt path returns through the exit function to the caller.
  emit [] (Store Code (OperandLiteral (LitSymbol exit)) (Address machine machineExitCodeOffset) (toInteger (lowerWordSize target)))
  emit [] (Store I64 (OperandLiteral (LitInt 0)) (Address (OperandLiteral (LitSymbol finishedSymbol)) 0) 8)
  eval <- requireHelper HelperEval
  emit [] (Call eval [machine, OperandLiteral (LitSymbol entryGlobal), OperandLiteral (LitInt 1), top, update])
  pure machine

-- | The continuation that a finished thread enters. Exported for harnesses
-- that build their own entry.
threadDoneContinuation :: Symbol -> LowerM ()
threadDoneContinuation target = do
  machine <- fresh "machine"
  value <- fresh "value"
  beginBlock (Label "entry") []
  resume <- callRuntime "aihc_thread_done" [Ptr] [Ptr] [OperandVar machine]
  helper <- requireHelper HelperResume
  terminate (TailCall helper [OperandVar machine, resume])
  finishFunction target Internal [(machine, Ptr), (value, Ptr)] [] AihcConvention

-- Helpers

-- | Generate every requested helper. A helper can request further helpers,
-- so repeat until no request is new.
generateHelpers :: LowerEnv -> Set Helper -> LowerM ()
generateHelpers env done = do
  requested <- stateHelpers <$> get
  let pending = Set.toAscList (requested `Set.difference` done)
  unless (null pending) $ do
    mapM_ (generateHelper env) pending
    generateHelpers env (done `Set.union` Set.fromList pending)

generateHelper :: LowerEnv -> Helper -> LowerM ()
generateHelper env helper =
  case helper of
    -- The exit function records that the machine halted and returns to the
    -- function that started the machine.
    HelperExit -> do
      machine <- fresh "machine"
      beginBlock (Label "entry") []
      when (lowerUnitKind (envOptions env) == ExecutableUnit) $
        emit [] (Store I64 (OperandLiteral (LitInt 1)) (Address (OperandLiteral (LitSymbol finishedSymbol)) 0) 8)
      terminate (Return [])
      finishFunction symbol Internal [(machine, Ptr)] [] AihcConvention
    HelperContinue shape -> do
      machine <- fresh "machine"
      continuation <- fresh "continuation"
      values <- forM shape $ \ty -> (,ty) <$> fresh "value"
      beginBlock (Label "entry") []
      terminate (Jump (Target (Label "loop") [OperandVar continuation]))
      current <- fresh "current"
      beginBlock (Label "loop") [(current, Ptr)]
      header <- loadHeader (OperandVar current)
      kind <- loadInfoWord "kind" header infoObjectKindIndex
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt runtimeObjectIndirection)))
      terminate (Branch (typedOperand isIndirection) (Target (Label "indirection") []) (Target (Label "enter") []))
      beginBlock (Label "indirection") []
      next <- loadSlot "next" Ptr (OperandVar current) 8
      terminate (Jump (Target (Label "loop") [typedOperand next]))
      beginBlock (Label "enter") []
      entry <- loadInfoCode "entry" header infoBackendEntryIndex
      terminate
        ( TailCallIndirect
            (typedOperand entry)
            (OperandVar machine : OperandVar current : OperandLiteral LitNull : [OperandVar var | (var, _) <- values])
            (Signature (Ptr : Ptr : Ptr : shape) [] AihcConvention)
        )
      finishFunction symbol Internal ((machine, Ptr) : (continuation, Ptr) : values) [] AihcConvention
    HelperApply shape -> do
      target <- targetM
      let word = toInteger (lowerWordSize target)
      machine <- fresh "machine"
      function <- fresh "function"
      continuation <- fresh "continuation"
      values <- forM shape $ \ty -> (,ty) <$> fresh "value"
      beginBlock (Label "entry") []
      arguments <- if null shape then pure (OperandLiteral LitNull) else typedOperand <$> emitValue "arguments" Ptr (StackAlloc (toInteger (8 * length shape)) 8)
      continuationSlot <- typedOperand <$> emitValue "slot" Ptr (StackAlloc word word)
      terminate (Jump (Target (Label "loop") [OperandVar function]))
      current <- fresh "current"
      beginBlock (Label "loop") [(current, Ptr)]
      header <- loadHeader (OperandVar current)
      kind <- loadInfoWord "kind" header infoObjectKindIndex
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt runtimeObjectIndirection)))
      terminate (Branch (typedOperand isIndirection) (Target (Label "indirection") []) (Target (Label "apply") []))
      beginBlock (Label "indirection") []
      next <- loadSlot "next" Ptr (OperandVar current) 8
      terminate (Jump (Target (Label "loop") [typedOperand next]))
      beginBlock (Label "apply") []
      arity <- loadInfoWord "arity" header infoRemainingArityIndex
      isClosure <- emitValue "closure" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt (toInteger runtimeObjectClosure))))
      isSaturated <- emitValue "saturated" I1 (Compare Eq I64 (typedOperand arity) (OperandLiteral (LitInt 1)))
      isFast <- emitValue "fast" I1 (Binary And I1 (typedOperand isClosure) (typedOperand isSaturated))
      terminate (Branch (typedOperand isFast) (Target (Label "fast") []) (Target (Label "slow") []))
      beginBlock (Label "fast") []
      entry <- loadInfoCode "entry" header infoBackendEntryIndex
      terminate
        ( TailCallIndirect
            (typedOperand entry)
            (OperandVar machine : OperandVar current : OperandVar continuation : [OperandVar var | (var, _) <- values])
            (Signature (Ptr : Ptr : Ptr : shape) [] AihcConvention)
        )
      beginBlock (Label "slow") []
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        storeSlot ty (OperandVar var) arguments (toInteger (8 * index))
      -- The continuation slot is a C pointer variable, not a heap slot.
      emit [] (Store Ptr (OperandVar continuation) (Address continuationSlot 0) word)
      applied <- callRuntime "aihc_apply_slow" [Ptr, Ptr, I64, Ptr, Ptr] [Ptr] [OperandVar machine, OperandVar current, OperandLiteral (LitInt (toInteger (length shape))), arguments, continuationSlot]
      adjusted <- emitValue "adjusted" Ptr (Load Ptr (Address continuationSlot 0) word)
      continue <- requireHelper (HelperContinue [Ptr])
      terminate (TailCall continue [OperandVar machine, typedOperand adjusted, applied])
      finishFunction symbol Internal ((machine, Ptr) : (function, Ptr) : (continuation, Ptr) : values) [] AihcConvention
    _ -> failWith (LowerUnsupportedExpression "internal: shared helper requested a local definition")
  where
    symbol = helperSymbol helper
    loadHeader object = typedOperand <$> loadSlot "header" Ptr object 0

infoRemainingArityIndex, infoBackendEntryIndex, infoObjectKindIndex :: Int
infoRemainingArityIndex = 3
infoBackendEntryIndex = 6
infoObjectKindIndex = 8

runtimeObjectNode, runtimeObjectClosure, runtimeObjectThunk, runtimeObjectPartialConstructor :: Int
runtimeObjectNode = 0
runtimeObjectClosure = 1
runtimeObjectThunk = 2
runtimeObjectPartialConstructor = 3

runtimeObjectIndirection :: Integer
runtimeObjectIndirection = 4

-- Program queries

programNodes :: GrinProgram -> [GrinNode]
programNodes program = map snd (grinGlobals program) <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression =
  case expression of
    GrinBind _ value body -> exprNodes value <> exprNodes body
    GrinStore node -> [node]
    GrinStoreUnchecked node -> [node]
    GrinStoreRec bindings body -> map snd bindings <> exprNodes body
    GrinStoreRecUnchecked bindings body -> map snd bindings <> exprNodes body
    GrinCase _ _ alternatives -> concatMap (exprNodes . grinAltRhs) alternatives
    _ -> []

programRuntimeReps :: GrinProgram -> [GrinRep]
programRuntimeReps program =
  concatMap (concat . snd) (grinConstructors program)
    <> concatMap (map grinValueRuntimeRep . grinNodeFields) (programNodes program)
    <> concatMap functionReps (grinFunctions program)
  where
    functionReps function = grinFunctionResultRep function : map grinVarRuntimeRep (grinFunctionParameters function) <> exprReps (grinFunctionBody function)
    exprReps expression =
      case expression of
        GrinBind vars value body -> map grinVarRuntimeRep vars <> exprReps value <> exprReps body
        GrinStoreRec bindings body -> map (grinVarRuntimeRep . fst) bindings <> exprReps body
        GrinStoreRecUnchecked bindings body -> map (grinVarRuntimeRep . fst) bindings <> exprReps body
        GrinCase value binder alternatives -> grinValueRuntimeRep value : grinVarRuntimeRep binder : concatMap (\alternative -> map grinVarRuntimeRep (grinAltBinders alternative) <> exprReps (grinAltRhs alternative)) alternatives
        _ -> []

-- | The info tables one node needs. An unsaturated constructor names the
-- saturated table too: its own table points at it, and the runtime reads the
-- full width and the pointer map from there.
requiredNodeConstructorInfos :: GrinNode -> [RuntimeInfoKey]
requiredNodeConstructorInfos node =
  case grinNodeTag node of
    GrinConstructor name remaining
      | constructorStage remaining == SaturatedConstructor -> [ConstructorRuntimeInfo name SaturatedConstructor]
      | otherwise ->
          [ ConstructorRuntimeInfo name PartialConstructor,
            ConstructorRuntimeInfo name SaturatedConstructor
          ]
    _ -> []

runtimeInfoKeyStages :: GrinNode -> [RuntimeInfoKey]
runtimeInfoKeyStages node =
  case grinNodeTag node of
    GrinConstructor name remaining -> [ConstructorRuntimeInfo name (constructorStage remaining)]
    GrinClosure name layouts -> stages fields layouts
      where
        stages current remaining =
          ClosureRuntimeInfo name current remaining : case remaining of
            [] -> []
            layout : rest -> stages (current <> layout) rest
    GrinThunk name -> [ThunkRuntimeInfo name fields]
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)

runtimeInfoFunctionName :: RuntimeInfoKey -> Maybe FunctionName
runtimeInfoFunctionName key =
  case key of
    ConstructorRuntimeInfo {} -> Nothing
    ClosureRuntimeInfo name _ _ -> Just name
    ThunkRuntimeInfo name _ -> Just name

runtimeInfoKeyFields :: RuntimeInfoKey -> [GrinRep]
runtimeInfoKeyFields key =
  case key of
    ConstructorRuntimeInfo {} -> []
    ClosureRuntimeInfo _ fields _ -> fields
    ThunkRuntimeInfo _ fields -> fields

runtimeInfoKeyRemainingArity :: RuntimeInfoKey -> Int
runtimeInfoKeyRemainingArity key =
  case key of
    ConstructorRuntimeInfo {} -> 0
    ClosureRuntimeInfo _ _ layouts -> length layouts
    ThunkRuntimeInfo {} -> 0

runtimeInfoKeyObjectKind :: RuntimeInfoKey -> Int
runtimeInfoKeyObjectKind key =
  case key of
    ConstructorRuntimeInfo _ SaturatedConstructor -> runtimeObjectNode
    ConstructorRuntimeInfo _ PartialConstructor -> runtimeObjectPartialConstructor
    ClosureRuntimeInfo {} -> runtimeObjectClosure
    ThunkRuntimeInfo {} -> runtimeObjectThunk

runtimeInfoKeyNext :: RuntimeInfoKey -> Maybe RuntimeInfoKey
runtimeInfoKeyNext key =
  case key of
    ConstructorRuntimeInfo name PartialConstructor -> Just (ConstructorRuntimeInfo name SaturatedConstructor)
    ConstructorRuntimeInfo {} -> Nothing
    ClosureRuntimeInfo name fields (layout : rest) -> Just (ClosureRuntimeInfo name (fields <> layout) rest)
    ClosureRuntimeInfo {} -> Nothing
    ThunkRuntimeInfo {} -> Nothing

normalizedLiteralInteger :: GrinLiteral -> Maybe Integer
normalizedLiteralInteger literal =
  case literal of
    GrinLitInt runtimeRep value -> Just (normalizeScalar runtimeRep value)
    GrinLitChar _ value -> Just (toInteger (ord value))
    _ -> Nothing

normalizeScalar :: GrinRep -> Integer -> Integer
normalizeScalar runtimeRep integer =
  case runtimeRep of
    IntRep -> signed 64
    Int8Rep -> signed 8
    Int16Rep -> signed 16
    Int32Rep -> signed 32
    Int64Rep -> signed 64
    WordRep -> unsigned 64
    Word8Rep -> unsigned 8
    Word16Rep -> unsigned 16
    Word32Rep -> unsigned 32
    Word64Rep -> unsigned 64
    _ -> integer
  where
    modulus width = 2 ^ (width :: Int)
    unsigned width = integer `mod` modulus width
    signed width =
      let value = unsigned width
          sign = 2 ^ (width - 1)
       in if value >= sign then value - modulus width else value
