{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to WebAssembly in the assembly syntax of LLVM MC.
--
-- Every Lir value is a WebAssembly local. The narrow integer types, @ptr@,
-- and @code@ are @i32@; a narrow integer is canonical, that is zero-extended.
-- A @code@ value is an index into the function table, so a data field or a
-- literal that names a function is a table-index relocation.
--
-- WebAssembly has structured control flow, so a function is one loop with
-- one nested block per Lir block and a @br_table@ on the current block
-- index. A jump assigns the parameters of the target and continues the loop.
-- Both calling conventions are the WebAssembly convention: the @c@
-- convention of wasm32 maps every Lir type to the same value type. Tail
-- calls are @return_call@.
--
-- @stack.alloc@ reserves memory on the shadow stack below
-- @__stack_pointer@. A trap calls @aihc_lir_trap@ with the message and its
-- length and then executes @unreachable@; the host runtime provides that
-- function.
module Aihc.Wasm.Lir
  ( WasmLirError (..),
    compileLirModule,
  )
where

import Aihc.Lir.Lint (LintError, lintModule)
import Aihc.Lir.Resolve (resolveConstants)
import Aihc.Lir.Syntax
import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', put, runStateT)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float)

data WasmLirError
  = WasmLirLintErrors ![LintError]
  | WasmLirUnsupported !Text
  deriving (Eq, Show)

-- | Lint the module, then render it. The helpers for wide 64-bit
-- multiplication are Lir functions that the backend adds to the module when
-- it uses them.
compileLirModule :: Module -> Either WasmLirError Text
compileLirModule lirModule =
  case lintModule lirModule of
    [] -> do
      let items = userItems <> [ItemFunction helper | usesWideMultiply, helper <- wideHelpers]
          ctx = moduleContext items
      (functions, final) <- runStateT (mapM (compileFunction ctx) [function | ItemFunction function <- items]) initialState
      dataLines <- concat <$> traverse (renderData ctx) [dataItem | ItemData dataItem <- items]
      let traps = Map.toAscList (moduleTraps final)
      pure
        ( T.unlines
            ( header ctx items (moduleUsesStack final)
                <> concat functions
                <> dataLines
                <> concatMap (renderGlobal ctx) [global | ItemGlobal global <- items]
                <> concat
                  [ renderBytes (trapMessageSymbol index) (Text.encodeUtf8 message)
                  | (message, index) <- traps
                  ]
                <> ["\t.no_dead_strip\t__indirect_function_table", ""]
            )
        )
    errors -> Left (WasmLirLintErrors errors)
  where
    Module userItems = resolveConstants lirModule
    usesWideMultiply =
      or
        [ True
        | ItemFunction function <- userItems,
          block <- functionBlocks function,
          Instruction _ (Wide op I64 _ _) <- blockInstructions block,
          op `elem` [MulWideU, MulWideS]
        ]
    initialState = ModuleState {moduleTraps = Map.empty, moduleUsesStack = False}

-- | The module-level facts the functions need.
data Ctx = Ctx
  { ctxSymbols :: !(Map Symbol Text),
    ctxSignatures :: !(Map Symbol Signature),
    ctxGlobals :: !(Map Symbol Type)
  }

moduleContext :: [Item] -> Ctx
moduleContext items =
  Ctx
    { ctxSymbols = Map.fromList (mapMaybe symbolName items),
      ctxSignatures =
        Map.fromList
          ( [(functionName function, functionSignature function) | ItemFunction function <- items]
              <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
          ),
      ctxGlobals = Map.fromList [(globalName global, globalType global) | ItemGlobal global <- items]
    }
  where
    symbolName item =
      case item of
        ItemFunction function -> Just (functionName function, linkedName (functionLinkage function) (functionName function))
        ItemExternFunction external -> Just (externFunctionName external, unSymbol (externFunctionName external))
        ItemGlobal global -> Just (globalName global, linkedName Internal (globalName global))
        ItemData dataItem -> Just (dataName dataItem, linkedName (dataLinkage dataItem) (dataName dataItem))
        ItemExternData symbol -> Just (symbol, unSymbol symbol)
        ItemConstant _ -> Nothing
        ItemInclude _ -> Nothing

-- | An internal symbol is local to its object.
linkedName :: Linkage -> Symbol -> Text
linkedName linkage symbol =
  case linkage of
    Export -> unSymbol symbol
    Internal -> ".L" <> unSymbol symbol

symbolText :: Ctx -> Symbol -> Text
symbolText ctx symbol = fromMaybe (unSymbol symbol) (Map.lookup symbol (ctxSymbols ctx))

data ModuleState = ModuleState
  { moduleTraps :: !(Map Text Int),
    moduleUsesStack :: !Bool
  }

trapMessageSymbol :: Int -> Text
trapMessageSymbol index = ".Llir_trap_" <> tshow index

trapSymbol :: Text
trapSymbol = "aihc_lir_trap"

stackPointer :: Text
stackPointer = "__stack_pointer"

-- Types

wasmType :: Type -> Text
wasmType ty =
  case ty of
    I64 -> "i64"
    F32 -> "f32"
    F64 -> "f64"
    _ -> "i32"

-- | Whether a value type is @i32@ or @i64@.
is64 :: Type -> Bool
is64 ty = ty == I64

-- | The instruction prefix of the integer type.
prefix :: Type -> Text
prefix ty = if is64 ty then "i64" else "i32"

renderSignature :: Signature -> Text
renderSignature signature =
  "(" <> T.intercalate ", " (map wasmType (signatureParameters signature)) <> ") -> (" <> T.intercalate ", " (map wasmType (signatureResults signature)) <> ")"

-- Module header

header :: Ctx -> [Item] -> Bool -> [Text]
header ctx items usesStack =
  [ "# Lir module compiled by Aihc.Wasm.Lir.",
    "\t.text",
    "\t.functype\t" <> trapSymbol <> " (i32, i64) -> ()"
  ]
    <> ["\t.functype\t" <> symbolText ctx (externFunctionName external) <> " " <> renderSignature (externFunctionSignature external) | ItemExternFunction external <- items]
    <> ["\t.functype\t" <> symbolText ctx (functionName function) <> " " <> renderSignature (functionSignature function) | ItemFunction function <- items]
    <> ["\t.globaltype\t" <> stackPointer <> ", i32" | usesStack]
    <> ["\t.globaltype\t" <> symbolText ctx (globalName global) <> ", " <> wasmType (globalType global) | ItemGlobal global <- items]
    <> ["\t.tabletype\t__indirect_function_table, funcref", ""]

-- Data

-- | The linter and 'resolveConstants' leave no constant reference behind, so
-- one is an error here rather than a value.
renderData :: Ctx -> DataItem -> Either WasmLirError [Text]
renderData ctx dataItem = do
  fields <- traverse field (dataFields dataItem)
  pure
    ( [ "\t.type\t" <> name <> ",@object",
        "\t.section\t" <> (if dataMutable dataItem then ".data." else ".rodata.") <> name <> ",\"\",@"
      ]
        <> ["\t.hidden\t" <> name | dataLinkage dataItem == Export]
        <> ["\t.globl\t" <> name | dataLinkage dataItem == Export]
        <> ["\t.p2align\t" <> tshow (log2 (dataAlignment dataItem)) <> ", 0x0" | dataAlignment dataItem > 1]
        <> [name <> ":"]
        <> concatMap fst fields
        <> ["\t.size\t" <> name <> ", " <> tshow (sum (map snd fields)), ""]
    )
  where
    name = symbolText ctx (dataName dataItem)
    field :: DataField -> Either WasmLirError ([Text], Int)
    field dataField =
      case dataField of
        DataInt ty value -> pure (["\t.int" <> tshow (8 * typeBytes ty) <> "\t" <> renderInteger (typeBytes ty) value], typeBytes ty)
        DataIntConstant _ constant -> unresolved constant
        DataFloat F32 value -> pure (["\t.int32\t" <> tshow (castFloatToWord32 (double2Float value))], 4)
        DataFloat _ value -> pure (["\t.int64\t" <> tshow (castDoubleToWord64 value)], 8)
        DataSymbol target 0 -> pure (["\t.int32\t" <> symbolText ctx target], 4)
        DataSymbol target addend -> pure (["\t.int32\t" <> symbolText ctx target <> (if addend < 0 then "-" <> tshow (negate addend) else "+" <> tshow addend)], 4)
        DataNull -> pure (["\t.int32\t0"], 4)
        DataWord value -> pure (["\t.int32\t" <> renderInteger 4 value], 4)
        DataWordConstant constant -> unresolved constant
        DataCode Nothing -> pure (["\t.int32\t0"], 4)
        DataCode (Just target) -> pure (["\t.int32\t" <> symbolText ctx target], 4)
        DataBytes bytes -> pure (["\t.ascii\t\"" <> escapeBytes bytes <> "\"" | not (BS.null bytes)], BS.length bytes)
        DataZero count -> pure (["\t.skip\t" <> tshow count | count > 0], fromInteger count)
    unresolved constant = Left (WasmLirUnsupported ("unknown constant " <> unSymbol constant))

-- | A read-only byte object with a local symbol.
renderBytes :: Text -> BS.ByteString -> [Text]
renderBytes name bytes =
  [ "\t.type\t" <> name <> ",@object",
    "\t.section\t.rodata." <> name <> ",\"\",@",
    name <> ":",
    "\t.ascii\t\"" <> escapeBytes bytes <> "\"",
    "\t.size\t" <> name <> ", " <> tshow (BS.length bytes),
    ""
  ]

renderGlobal :: Ctx -> Global -> [Text]
renderGlobal ctx global =
  [ "\t.globaltype\t" <> name <> ", " <> wasmType (globalType global),
    name <> ":",
    ""
  ]
  where
    name = symbolText ctx (globalName global)

escapeBytes :: BS.ByteString -> Text
escapeBytes = T.concat . map escapeByte . BS.unpack
  where
    escapeByte :: Word8 -> Text
    escapeByte byte
      | byte >= 0x20 && byte < 0x7f && byte /= 0x22 && byte /= 0x5c = T.singleton (toEnum (fromIntegral byte))
      | otherwise = "\\" <> T.pack (octal byte)
    octal byte = [digit (byte `div` 64), digit ((byte `div` 8) `mod` 8), digit (byte `mod` 8)]
    digit value = toEnum (fromIntegral value + 48)

log2 :: Integer -> Int
log2 value = length (takeWhile (< value) (iterate (* 2) 1))

typeBytes :: Type -> Int
typeBytes ty = max 1 (typeBits ty `div` 8)

-- | An integer in the signed range of its width, as the assembler expects.
renderInteger :: Int -> Integer -> Text
renderInteger bytes value =
  let bits = 8 * bytes
      wrapped = value `mod` (2 ^ bits)
   in tshow (if wrapped >= 2 ^ (bits - 1) then wrapped - 2 ^ bits else wrapped)

-- Functions

data FunctionState = FunctionState
  { functionModule :: !ModuleState,
    functionLinesRev :: ![Text],
    functionNext :: !Int
  }

type M = StateT FunctionState (Either WasmLirError)

data Fn = Fn
  { fnCtx :: !Ctx,
    fnFunction :: !Function,
    fnLocals :: !(Map Var Int),
    fnBlockIndex :: !(Map Label Int),
    fnBlockParameters :: !(Map Label [(Var, Type)]),
    fnState :: !Int,
    -- | The frame pointer local and the frame size when the function
    -- allocates stack memory.
    fnFrame :: !(Maybe (Int, Int)),
    fnAllocs :: !(Map Var Int),
    -- | Scratch locals: @i32@, @i64@, @f32@, and @f64@.
    fnScratch :: !(Map Text Int)
  }

unsupported :: Text -> M value
unsupported = lift . Left . WasmLirUnsupported

emit :: Text -> M ()
emit line = modify' $ \state -> state {functionLinesRev = ("\t" <> line) : functionLinesRev state}

emitLabel :: Text -> M ()
emitLabel line = modify' $ \state -> state {functionLinesRev = line : functionLinesRev state}

trapIndex :: Text -> M Int
trapIndex message = do
  state <- get
  let traps = moduleTraps (functionModule state)
  case Map.lookup message traps of
    Just index -> pure index
    Nothing -> do
      let index = Map.size traps
      put state {functionModule = (functionModule state) {moduleTraps = Map.insert message index traps}}
      pure index

-- | Report a trap message and stop.
trap :: Text -> M ()
trap message = do
  index <- trapIndex message
  emit ("i32.const\t" <> trapMessageSymbol index)
  emit ("i64.const\t" <> tshow (BS.length (Text.encodeUtf8 message)))
  emit ("call\t" <> trapSymbol)
  emit "unreachable"

-- | Trap when the value on the stack is nonzero.
trapIf :: Text -> M ()
trapIf message = do
  emit "if"
  trap message
  emit "end_if"

compileFunction :: Ctx -> Function -> StateT ModuleState (Either WasmLirError) [Text]
compileFunction ctx function = do
  moduleState <- get
  let parameters = functionParameters function
      blocks = functionBlocks function
      definitions = concat [map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block) | block <- blocks]
      definitionTypes = Map.fromList (concat [blockParameters block | block <- blocks] <> concat [resultTypes ctx instruction | block <- blocks, instruction <- blockInstructions block])
      allocations = [(var, size, alignment) | block <- take 1 blocks, Instruction [var] (StackAlloc size alignment) <- blockInstructions block]
      (allocs, frameEnd) = placeAllocations allocations
      frameSize = ((frameEnd + 15) `div` 16) * 16
      hasFrame = not (null allocations)
      parameterCount = length parameters
      stateLocal = parameterCount
      frameLocal = parameterCount + 1
      scratchBase = parameterCount + 2
      scratch = Map.fromList (zip ["i32", "i64", "f32", "f64"] [scratchBase ..])
      valueBase = scratchBase + 4
      locals = Map.fromList (zip (map fst parameters) [0 ..] <> zip definitions [valueBase ..])
      localTypes = ["i32", "i32", "i32", "i64", "f32", "f64"] <> [wasmType (Map.findWithDefault I64 var definitionTypes) | var <- definitions]
      fn =
        Fn
          { fnCtx = ctx,
            fnFunction = function,
            fnLocals = locals,
            fnBlockIndex = Map.fromList (zip (map blockLabel blocks) [0 ..]),
            fnBlockParameters = Map.fromList [(blockLabel block, blockParameters block) | block <- blocks],
            fnState = stateLocal,
            fnFrame = if hasFrame then Just (frameLocal, frameSize) else Nothing,
            fnAllocs = allocs,
            fnScratch = scratch
          }
      name = symbolText ctx (functionName function)
      initial = FunctionState {functionModule = moduleState, functionLinesRev = [], functionNext = 0}
  ((), final) <- lift (runStateT (functionBody fn) initial)
  put (functionModule final) {moduleUsesStack = moduleUsesStack (functionModule final) || hasFrame}
  pure
    ( [ "\t.section\t.text." <> name <> ",\"\",@",
        "\t.type\t" <> name <> ",@function"
      ]
        <> ["\t.hidden\t" <> name | functionLinkage function == Export]
        <> ["\t.globl\t" <> name | functionLinkage function == Export]
        <> [ name <> ":",
             "\t.functype\t" <> name <> " " <> renderSignature (functionSignature function),
             "\t.local\t" <> T.intercalate ", " localTypes
           ]
        <> reverse (functionLinesRev final)
        <> ["\tend_function", ""]
    )

placeAllocations :: [(Var, Integer, Integer)] -> (Map Var Int, Int)
placeAllocations = go Map.empty 0
  where
    go placed next [] = (placed, next)
    go placed next ((var, size, alignment) : rest) =
      let start = roundUp (fromInteger alignment) next
       in go (Map.insert var start placed) (start + fromInteger size) rest
    roundUp alignment value = ((value + alignment - 1) `div` alignment) * alignment

-- | The types of the results of an instruction, for the local declarations.
resultTypes :: Ctx -> Instruction -> [(Var, Type)]
resultTypes ctx (Instruction results operation) =
  zip results $ case operation of
    Binary _ ty _ _ -> [ty]
    Unary _ ty _ -> [ty]
    Wide op ty _ _ -> if op `elem` [AddCarry, SubBorrow] then [ty, I1] else [ty, ty]
    Compare {} -> [I1]
    FloatBinary _ ty _ _ -> [ty]
    FloatUnary _ ty _ -> [ty]
    Convert _ _ _ to -> [to]
    PtrToInt _ -> [I64]
    PtrFromInt _ -> [Ptr]
    Select ty _ _ _ -> [ty]
    Load ty _ _ -> [ty]
    Store {} -> []
    PtrAdd _ _ -> [Ptr]
    StackAlloc _ _ -> [Ptr]
    GlobalGet symbol -> [Map.findWithDefault I64 symbol (ctxGlobals ctx)]
    GlobalSet _ _ -> []
    Call symbol _ -> maybe [] signatureResults (Map.lookup symbol (ctxSignatures ctx))
    CallIndirect _ _ signature -> signatureResults signature

-- | The prologue, the dispatch loop, and the bodies of the blocks.
functionBody :: Fn -> M ()
functionBody fn = do
  forM_ (fnFrame fn) $ \(frameLocal, frameSize) -> do
    emit ("global.get\t" <> stackPointer)
    emit ("i32.const\t" <> tshow frameSize)
    emit "i32.sub"
    emit ("local.tee\t" <> tshow frameLocal)
    emit ("global.set\t" <> stackPointer)
    -- The allocations are zero.
    forM_ [(offset, size) | block <- take 1 (functionBlocks (fnFunction fn)), Instruction [var] (StackAlloc size _) <- blockInstructions block, Just offset <- [Map.lookup var (fnAllocs fn)]] $ \(offset, size) -> do
      forM_ [offset, offset + 8 .. offset + fromInteger size - 8] $ \position -> do
        emit ("local.get\t" <> tshow frameLocal)
        emit "i64.const\t0"
        emit ("i64.store\t" <> tshow position)
      forM_ [offset + (fromInteger size `div` 8) * 8 .. offset + fromInteger size - 1] $ \position -> do
        emit ("local.get\t" <> tshow frameLocal)
        emit "i32.const\t0"
        emit ("i32.store8\t" <> tshow position)
  let blocks = functionBlocks (fnFunction fn)
      count = length blocks
  emit "loop"
  forM_ [1 .. count] $ \_ -> emit "block"
  emit ("local.get\t" <> tshow (fnState fn))
  emit ("br_table\t{" <> T.intercalate ", " (map tshow ([0 .. count - 1] <> [count - 1])) <> "}")
  forM_ (zip [0 ..] blocks) $ \(index, block) -> do
    emit "end_block"
    emitLabel ("# " <> unLabel (blockLabel block))
    mapM_ (compileInstruction fn) (blockInstructions block)
    compileTerminator fn (count - 1 - index) (blockTerminator block)
  emit "end_loop"
  emit "unreachable"

local :: Fn -> Var -> M Int
local fn var = maybe (unsupported ("unknown value " <> unVar var)) pure (Map.lookup var (fnLocals fn))

getVar :: Fn -> Var -> M ()
getVar fn var = do
  index <- local fn var
  emit ("local.get\t" <> tshow index)

setVar :: Fn -> Var -> M ()
setVar fn var = do
  index <- local fn var
  emit ("local.set\t" <> tshow index)

scratchLocal :: Fn -> Type -> Int
scratchLocal fn ty = fnScratch fn Map.! wasmType ty

-- | Push an operand with the encoding of its type.
push :: Fn -> Type -> Operand -> M ()
push fn ty operand =
  case operand of
    OperandVar var -> getVar fn var
    OperandLiteral literal ->
      case literal of
        LitInt value
          | ty == F64 -> pushDouble (fromInteger value)
          | ty == F32 -> pushFloat (fromInteger value)
          | is64 ty -> emit ("i64.const\t" <> renderInteger 8 value)
          | otherwise -> emit ("i32.const\t" <> renderInteger 4 (canonicalInteger ty value))
        LitFloat value
          | ty == F32 -> pushFloat value
          | otherwise -> pushDouble value
        LitNull -> emit "i32.const\t0"
        LitSymbol symbol -> emit ("i32.const\t" <> symbolText (fnCtx fn) symbol)
  where
    pushDouble value = do
      emit ("i64.const\t" <> renderInteger 8 (toInteger (castDoubleToWord64 value)))
      emit "f64.reinterpret_i64"
    pushFloat value = do
      emit ("i32.const\t" <> renderInteger 4 (toInteger (castFloatToWord32 (double2Float value))))
      emit "f32.reinterpret_i32"

canonicalInteger :: Type -> Integer -> Integer
canonicalInteger ty value = value `mod` (2 ^ min 64 (typeBits ty))

-- | Mask the value on the stack to the width of a narrow type.
narrow :: Type -> M ()
narrow ty =
  case ty of
    I1 -> emit "i32.const\t1" >> emit "i32.and"
    I8 -> emit "i32.const\t255" >> emit "i32.and"
    I16 -> emit "i32.const\t65535" >> emit "i32.and"
    _ -> pure ()

-- | Sign-extend the canonical value on the stack within its value type.
signExtend :: Type -> M ()
signExtend ty =
  case ty of
    I1 -> emit "i32.const\t31" >> emit "i32.shl" >> emit "i32.const\t31" >> emit "i32.shr_s"
    I8 -> emit "i32.extend8_s"
    I16 -> emit "i32.extend16_s"
    _ -> pure ()

-- | Restore the shadow stack pointer before the function leaves.
leaveFrame :: Fn -> M ()
leaveFrame fn =
  forM_ (fnFrame fn) $ \(frameLocal, frameSize) -> do
    emit ("local.get\t" <> tshow frameLocal)
    emit ("i32.const\t" <> tshow frameSize)
    emit "i32.add"
    emit ("global.set\t" <> stackPointer)

-- | Assign the parameters of the target and continue the dispatch loop.
-- The depth is the number of labels between the current point and the loop.
jumpTo :: Fn -> Int -> Target -> M ()
jumpTo fn depth (Target label arguments) = do
  let parameters = Map.findWithDefault [] label (fnBlockParameters fn)
  forM_ (zip parameters arguments) $ \((_, ty), argument) -> push fn ty argument
  forM_ (reverse parameters) $ \(var, _) -> setVar fn var
  emit ("i32.const\t" <> tshow (Map.findWithDefault 0 label (fnBlockIndex fn)))
  emit ("local.set\t" <> tshow (fnState fn))
  emit ("br\t" <> tshow depth)

compileTerminator :: Fn -> Int -> Terminator -> M ()
compileTerminator fn depth terminator =
  case terminator of
    Jump target -> jumpTo fn depth target
    Branch condition whenTrue whenFalse -> do
      push fn I1 condition
      emit "if"
      jumpTo fn (depth + 1) whenTrue
      emit "else"
      jumpTo fn (depth + 1) whenFalse
      emit "end_if"
    Switch ty scrutinee cases fallback -> do
      forM_ cases $ \switchCase -> do
        push fn ty scrutinee
        push fn ty (OperandLiteral (LitInt (switchCaseValue switchCase)))
        emit (prefix ty <> ".eq")
        emit "if"
        jumpTo fn (depth + 1) (switchCaseTarget switchCase)
        emit "end_if"
      case fallback of
        Just target -> jumpTo fn depth target
        Nothing -> trap "switch without a matching case"
    Return values -> do
      leaveFrame fn
      forM_ (zip (functionResults (fnFunction fn)) values) (uncurry (push fn))
      emit "return"
    TailCall symbol arguments -> do
      let signature = Map.findWithDefault (Signature [] [] AihcConvention) symbol (ctxSignatures (fnCtx fn))
      leaveFrame fn
      forM_ (zip (signatureParameters signature) arguments) (uncurry (push fn))
      emit ("return_call\t" <> symbolText (fnCtx fn) symbol)
    TailCallIndirect target arguments signature -> do
      guardCallee fn target
      leaveFrame fn
      forM_ (zip (signatureParameters signature) arguments) (uncurry (push fn))
      push fn Code target
      emit ("return_call_indirect\t__indirect_function_table, " <> renderSignature signature)
    Trap message -> trap message

-- | Trap when a callee is null.
guardCallee :: Fn -> Operand -> M ()
guardCallee fn target = do
  push fn Code target
  emit "i32.eqz"
  trapIf "indirect call to a non-function"

compileInstruction :: Fn -> Instruction -> M ()
compileInstruction fn (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      let p = prefix ty
      case op of
        Add -> operands ty left right >> emit (p <> ".add") >> narrow ty >> single
        Sub -> operands ty left right >> emit (p <> ".sub") >> narrow ty >> single
        Mul -> operands ty left right >> emit (p <> ".mul") >> narrow ty >> single
        And -> operands ty left right >> emit (p <> ".and") >> single
        Or -> operands ty left right >> emit (p <> ".or") >> single
        Xor -> operands ty left right >> emit (p <> ".xor") >> single
        Shl -> push fn ty left >> shiftCount ty right >> emit (p <> ".shl") >> narrow ty >> single
        ShrU -> push fn ty left >> shiftCount ty right >> emit (p <> ".shr_u") >> single
        ShrS -> push fn ty left >> signExtend ty >> shiftCount ty right >> emit (p <> ".shr_s") >> narrow ty >> single
        DivS -> do
          zeroCheck ty right
          overflowCheck ty left right
          signedOperands ty left right
          emit (p <> ".div_s") >> narrow ty >> single
        DivU -> zeroCheck ty right >> operands ty left right >> emit (p <> ".div_u") >> single
        RemS -> do
          zeroCheck ty right
          signedOperands ty left right
          emit (p <> ".rem_s") >> narrow ty >> single
        RemU -> zeroCheck ty right >> operands ty left right >> emit (p <> ".rem_u") >> single
    Wide op ty left right ->
      case results of
        [first, second] ->
          case op of
            MulWideU
              | is64 ty -> wideCall "aihc_lir_wasm_mul_wide_u64" first second
              | otherwise -> narrowWide "extend_i32_u" "shr_u" first second
            MulWideS
              | is64 ty -> wideCall "aihc_lir_wasm_mul_wide_s64" first second
              | otherwise -> narrowWide "extend_i32_s" "shr_s" first second
            AddCarry -> do
              operands ty left right
              emit (prefix ty <> ".add")
              narrow ty
              setVar fn first
              -- The sum is below the left operand exactly when it wrapped.
              getVar fn first
              push fn ty left
              emit (prefix ty <> ".lt_u")
              setVar fn second
            SubBorrow -> do
              push fn ty left
              push fn ty right
              emit (prefix ty <> ".lt_u")
              setVar fn second
              operands ty left right
              emit (prefix ty <> ".sub")
              narrow ty
              setVar fn first
          where
            wideCall helper first' second' = do
              operands ty left right
              emit ("call\t" <> linkedName Internal (Symbol helper))
              setVar fn second'
              setVar fn first'
            -- A narrow product fits an i64.
            narrowWide extend shift first' second' = do
              let bits = typeBits ty
                  scratch = scratchLocal fn I64
              push fn ty left >> signOrZero extend
              emit ("i64." <> extend)
              push fn ty right >> signOrZero extend
              emit ("i64." <> extend)
              emit "i64.mul"
              emit ("local.tee\t" <> tshow scratch)
              emit "i32.wrap_i64"
              narrow ty
              setVar fn first'
              emit ("local.get\t" <> tshow scratch)
              emit ("i64.const\t" <> tshow bits)
              emit ("i64." <> shift)
              emit "i32.wrap_i64"
              narrow ty
              setVar fn second'
            signOrZero extend = when (extend == "extend_i32_s") (signExtend ty)
        _ -> unsupported "wide operation result count"
    -- WebAssembly counts the bits of the whole container. A narrow type
    -- lives zero-extended in an i32, so a leading-zero count subtracts the
    -- bits above the type, and a trailing-zero count sets the first bit above
    -- it so that a zero operand counts the width of the type instead of 32.
    Unary op ty value -> do
      let bits = typeBits ty
          narrowContainer = bits < 32
      push fn ty value
      case op of
        Clz -> do
          emit (prefix ty <> ".clz")
          when narrowContainer $ do
            emit ("i32.const\t" <> tshow (32 - bits))
            emit "i32.sub"
        Ctz -> do
          when narrowContainer $ do
            emit ("i32.const\t" <> tshow ((2 :: Integer) ^ bits))
            emit "i32.or"
          emit (prefix ty <> ".ctz")
        Popcount -> emit (prefix ty <> ".popcnt")
      single
    Compare op ty left right -> do
      let p = if isFloatType ty then wasmType ty else prefix ty
      if op `elem` [LtS, LeS, GtS, GeS] && not (isFloatType ty)
        then signedOperands ty left right
        else operands ty left right
      emit (p <> "." <> comparison op ty)
      single
    FloatBinary op ty left right -> do
      operands ty left right
      emit (wasmType ty <> "." <> floatBinary op)
      single
    FloatUnary op ty value -> do
      push fn ty value
      emit (wasmType ty <> "." <> floatUnary op)
      single
    Convert op from value to ->
      case op of
        SExt -> do
          push fn from value
          signExtend from
          when (is64 to) (emit "i64.extend_i32_s")
          narrow to
          single
        ZExt -> do
          push fn from value
          when (is64 to) (emit "i64.extend_i32_u")
          single
        Trunc -> do
          push fn from value
          when (is64 from) (emit "i32.wrap_i64")
          narrow to
          single
        IToFS -> do
          push fn from value
          signExtend from
          emit (wasmType to <> ".convert_" <> prefix from <> "_s")
          single
        IToFU -> do
          push fn from value
          emit (wasmType to <> ".convert_" <> prefix from <> "_u")
          single
        FToIS -> floatToInteger True from value to
        FToIU -> floatToInteger False from value to
        FpExt -> push fn from value >> emit "f64.promote_f32" >> single
        FpTrunc -> push fn from value >> emit "f32.demote_f64" >> single
        Bitcast -> do
          push fn from value
          emit (wasmType to <> ".reinterpret_" <> wasmType from)
          single
    PtrToInt value -> push fn Ptr value >> emit "i64.extend_i32_u" >> single
    PtrFromInt value -> push fn I64 value >> emit "i32.wrap_i64" >> single
    Select ty condition left right -> do
      push fn ty left
      push fn ty right
      push fn I1 condition
      emit (wasmType ty <> ".select")
      single
    Load ty address _ -> do
      offset <- effectiveAddress address
      emit (loadInstruction ty <> "\t" <> tshow offset)
      single
    Store ty value address _ -> do
      offset <- effectiveAddress address
      push fn ty value
      emit (storeInstruction ty <> "\t" <> tshow offset)
    PtrAdd base offset -> do
      push fn Ptr base
      push fn I64 offset
      emit "i32.wrap_i64"
      emit "i32.add"
      single
    StackAlloc _ _ ->
      case (results, fnFrame fn) of
        ([var], Just (frameLocal, _)) | Just offset <- Map.lookup var (fnAllocs fn) -> do
          emit ("local.get\t" <> tshow frameLocal)
          emit ("i32.const\t" <> tshow offset)
          emit "i32.add"
          single
        _ -> unsupported "stack.alloc without a placed result"
    GlobalGet symbol -> emit ("global.get\t" <> symbolText (fnCtx fn) symbol) >> single
    GlobalSet symbol value -> do
      push fn (Map.findWithDefault I64 symbol (ctxGlobals (fnCtx fn))) value
      emit ("global.set\t" <> symbolText (fnCtx fn) symbol)
    Call symbol arguments -> do
      let original = Map.findWithDefault (Signature [] [] AihcConvention) symbol (ctxSignatures (fnCtx fn))
      forM_ (zip (signatureParameters original) arguments) (uncurry (push fn))
      emit ("call\t" <> symbolText (fnCtx fn) symbol)
      storeResults
    CallIndirect target arguments signature -> do
      guardCallee fn target
      forM_ (zip (signatureParameters signature) arguments) (uncurry (push fn))
      push fn Code target
      emit ("call_indirect\t__indirect_function_table, " <> renderSignature signature)
      storeResults
  where
    single =
      case results of
        [var] -> setVar fn var
        _ -> unsupported "instruction result count"
    storeResults = forM_ (reverse results) (setVar fn)
    operands ty left right = push fn ty left >> push fn ty right
    signedOperands ty left right = do
      push fn ty left
      signExtend ty
      push fn ty right
      signExtend ty

    shiftCount ty count = do
      push fn ty count
      unless (typeBits ty `elem` [32, 64]) $ do
        emit ("i32.const\t" <> tshow (typeBits ty - 1))
        emit "i32.and"

    zeroCheck ty divisor = do
      push fn ty divisor
      emit (prefix ty <> ".eqz")
      trapIf "integer division by zero"

    overflowCheck ty left right = do
      push fn ty left
      push fn ty (OperandLiteral (LitInt (negate (2 ^ (typeBits ty - 1)))))
      emit (prefix ty <> ".eq")
      push fn ty right
      push fn ty (OperandLiteral (LitInt (-1)))
      emit (prefix ty <> ".eq")
      emit "i32.and"
      trapIf "integer overflow"

    -- NaN and values outside the range of the target trap. The bounds are
    -- powers of two, so they are exact in both float widths.
    floatToInteger signed from value to = do
      let bits = typeBits to
          lower = if signed then negate (2 ^^ (bits - 1)) else -1 :: Double
          upper = if signed then 2 ^^ (bits - 1) else 2 ^^ bits :: Double
          f = wasmType from
          target = if bits > 32 then I64 else I32
          suffix = if signed then "_s" else "_u"
      push fn from value
      push fn from value
      emit (f <> ".ne")
      trapIf "invalid float to integer conversion"
      push fn from value
      push fn from (OperandLiteral (LitFloat lower))
      emit (f <> (if signed then ".lt" else ".le"))
      trapIf "invalid float to integer conversion"
      push fn from value
      push fn from (OperandLiteral (LitFloat upper))
      emit (f <> ".ge")
      trapIf "invalid float to integer conversion"
      push fn from value
      emit (prefix target <> ".trunc_" <> f <> suffix)
      narrow to
      single

    effectiveAddress (Address base offset) = do
      push fn Ptr base
      if offset >= 0
        then pure offset
        else do
          emit ("i32.const\t" <> tshow offset)
          emit "i32.add"
          pure 0

loadInstruction :: Type -> Text
loadInstruction ty =
  case ty of
    I1 -> "i32.load8_u"
    I8 -> "i32.load8_u"
    I16 -> "i32.load16_u"
    I64 -> "i64.load"
    F32 -> "f32.load"
    F64 -> "f64.load"
    _ -> "i32.load"

storeInstruction :: Type -> Text
storeInstruction ty =
  case ty of
    I1 -> "i32.store8"
    I8 -> "i32.store8"
    I16 -> "i32.store16"
    I64 -> "i64.store"
    F32 -> "f32.store"
    F64 -> "f64.store"
    _ -> "i32.store"

comparison :: CompareOp -> Type -> Text
comparison op ty
  | isFloatType ty =
      case op of
        Eq -> "eq"
        Ne -> "ne"
        FLt -> "lt"
        FLe -> "le"
        FGt -> "gt"
        FGe -> "ge"
        _ -> "eq"
  | otherwise =
      case op of
        Eq -> "eq"
        Ne -> "ne"
        LtS -> "lt_s"
        LtU -> "lt_u"
        LeS -> "le_s"
        LeU -> "le_u"
        GtS -> "gt_s"
        GtU -> "gt_u"
        GeS -> "ge_s"
        GeU -> "ge_u"
        FLt -> "lt_u"
        FLe -> "le_u"
        FGt -> "gt_u"
        FGe -> "ge_u"

floatBinary :: FloatBinaryOp -> Text
floatBinary op =
  case op of
    FAdd -> "add"
    FSub -> "sub"
    FMul -> "mul"
    FDiv -> "div"

floatUnary :: FloatUnaryOp -> Text
floatUnary op =
  case op of
    FNeg -> "neg"
    FAbs -> "abs"
    FSqrt -> "sqrt"

-- Wide multiplication helpers

-- | The full 64-bit multiplication in 32-bit limbs, as Lir. The signed form
-- corrects the unsigned high half by the operands that are negative.
wideHelpers :: [Function]
wideHelpers = [unsignedHelper, signedHelper]
  where
    a = Var "a"
    b = Var "b"
    var = OperandVar . Var
    int = OperandLiteral . LitInt
    op name = Instruction [Var name]
    limbs =
      [ op "al" (Binary And I64 (var "a") (int 0xffffffff)),
        op "ah" (Binary ShrU I64 (var "a") (int 32)),
        op "bl" (Binary And I64 (var "b") (int 0xffffffff)),
        op "bh" (Binary ShrU I64 (var "b") (int 32)),
        op "p00" (Binary Mul I64 (var "al") (var "bl")),
        op "p01" (Binary Mul I64 (var "al") (var "bh")),
        op "p10" (Binary Mul I64 (var "ah") (var "bl")),
        op "p11" (Binary Mul I64 (var "ah") (var "bh")),
        op "p00h" (Binary ShrU I64 (var "p00") (int 32)),
        op "p01l" (Binary And I64 (var "p01") (int 0xffffffff)),
        op "p10l" (Binary And I64 (var "p10") (int 0xffffffff)),
        op "t0" (Binary Add I64 (var "p00h") (var "p01l")),
        op "t" (Binary Add I64 (var "t0") (var "p10l")),
        op "p00l" (Binary And I64 (var "p00") (int 0xffffffff)),
        op "tl" (Binary Shl I64 (var "t") (int 32)),
        op "low" (Binary Or I64 (var "p00l") (var "tl")),
        op "p01h" (Binary ShrU I64 (var "p01") (int 32)),
        op "p10h" (Binary ShrU I64 (var "p10") (int 32)),
        op "th" (Binary ShrU I64 (var "t") (int 32)),
        op "h0" (Binary Add I64 (var "p11") (var "p01h")),
        op "h1" (Binary Add I64 (var "h0") (var "p10h")),
        op "high" (Binary Add I64 (var "h1") (var "th"))
      ]
    helper name instructions =
      Function
        { functionName = Symbol name,
          functionLinkage = Internal,
          functionParameters = [(a, I64), (b, I64)],
          functionResults = [I64, I64],
          functionConvention = AihcConvention,
          functionBlocks = [Block (Label "entry") [] instructions (Return [var "low", var "result_high"])]
        }
    unsignedHelper = helper "aihc_lir_wasm_mul_wide_u64" (limbs <> [op "result_high" (Binary Add I64 (var "high") (int 0))])
    signedHelper =
      helper
        "aihc_lir_wasm_mul_wide_s64"
        ( limbs
            <> [ op "a_sign" (Binary ShrS I64 (var "a") (int 63)),
                 op "b_sign" (Binary ShrS I64 (var "b") (int 63)),
                 op "a_correction" (Binary And I64 (var "a_sign") (var "b")),
                 op "b_correction" (Binary And I64 (var "b_sign") (var "a")),
                 op "high_a" (Binary Sub I64 (var "high") (var "a_correction")),
                 op "result_high" (Binary Sub I64 (var "high_a") (var "b_correction"))
               ]
        )

tshow :: (Show value) => value -> Text
tshow = T.pack . show
