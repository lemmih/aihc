{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to AArch64 Mach-O objects for Darwin.
--
-- This backend is a proof of concept. Every value lives in a frame slot.
-- Instruction selection loads the operands into scratch registers, computes
-- the result, and stores it back. The @aihc@ calling convention passes the
-- first eight arguments in @x0@ to @x7@ and the rest in a 16-byte aligned
-- block on the stack. The callee pops that block, so a tail call restores the
-- stack of the caller before it pushes its own block and the stack does not
-- grow. Results come back in @x0@ to @x7@.
--
-- Narrow integers are canonical: an @iN@ value is zero-extended to 64 bits in
-- its slot. A float is its IEEE bit pattern.
module Aihc.Arm64.Lir
  ( Arm64LirError (..),
    compileLirObject,
    compileLirStatements,
    elideSlotReloads,
    lirSymbol,
  )
where

import Aihc.Arm64.Assemble
import Aihc.Lir.Lint (LintError, lintModule)
import Aihc.Lir.RegAlloc (Allocation (..), allocateRegisters)
import Aihc.Lir.Syntax
import Aihc.Native.Object (SectionRole (..))
import Control.Monad (forM, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float)

data Arm64LirError
  = Arm64LirLintErrors ![LintError]
  | Arm64LirUnsupported !Text
  | Arm64LirObjectError !Text
  deriving (Eq, Show)

-- | The object symbol of a Lir symbol. Darwin prefixes C symbols with an
-- underscore, and the module boundary uses the C symbol names.
lirSymbol :: Symbol -> Text
lirSymbol (Symbol name) = "_" <> name

-- | Lint the module, then assemble it.
compileLirObject :: Module -> Either Arm64LirError BL.ByteString
compileLirObject lirModule = do
  statements <- compileLirStatements lirModule
  either (Left . Arm64LirObjectError . T.pack . show) pure (assembleMachO statements)

compileLirStatements :: Module -> Either Arm64LirError [Arm64Statement]
compileLirStatements lirModule@(Module items) =
  case lintModule lirModule of
    [] -> evalStateT compileItems initialState
    errors -> Left (Arm64LirLintErrors errors)
  where
    initialState = ObjectState {objectTraps = Map.empty, objectNextLabel = 0}
    signatures =
      Map.fromList
        ( [(functionName function, functionSignature function) | ItemFunction function <- items]
            <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
        )
    compileItems = do
      functionStatements <- concat <$> zipWithM (compileFunction signatures) [0 ..] [function | ItemFunction function <- items]
      let dataStatements = concatMap compileData [dataItem | ItemData dataItem <- items]
          globalStatements = concatMap compileGlobal [global | ItemGlobal global <- items]
      trapStatements <- renderTraps
      pure (functionStatements <> trapStatements <> dataStatements <> globalStatements)

-- Object state

data ObjectState = ObjectState
  { objectTraps :: !(Map Text Int),
    objectNextLabel :: !Int
  }

type M = StateT ObjectState (Either Arm64LirError)

unsupported :: Text -> M value
unsupported = lift . Left . Arm64LirUnsupported

freshLabel :: Text -> M Text
freshLabel kind = do
  state <- get
  let index = objectNextLabel state
  put state {objectNextLabel = index + 1}
  pure (".Llir_" <> kind <> "_" <> tshow index)

-- | The label of the stub that reports one trap message.
trapLabel :: Text -> M Text
trapLabel message = do
  state <- get
  index <-
    case Map.lookup message (objectTraps state) of
      Just index -> pure index
      Nothing -> do
        let index = Map.size (objectTraps state)
        put state {objectTraps = Map.insert message index (objectTraps state)}
        pure index
  pure (trapStubLabel index)

trapStubLabel :: Int -> Text
trapStubLabel index = ".Llir_trap_" <> tshow index

-- | One stub per message loads the message and enters the shared reporter.
-- The reporter writes the message to the standard error stream and exits
-- with status one.
renderTraps :: M [Arm64Statement]
renderTraps = do
  traps <- Map.toAscList . objectTraps <$> get
  let messageLabel index = ".Llir_trap_message_" <> tshow index
      stubs =
        concat
          [ [arm64Align 2, arm64Label (trapStubLabel index)]
              <> address X0 (messageLabel index)
              <> [immediate X1 (BS.length bytes), arm64Instruction (ArmB ".Llir_trap")]
          | (message, index) <- traps,
            let bytes = Text.encodeUtf8 (message <> "\n")
          ]
      reporter =
        [ arm64Align 2,
          arm64Label ".Llir_trap",
          arm64Instruction (ArmMov X2 (Arm64RegisterValue X1)),
          arm64Instruction (ArmMov X1 (Arm64RegisterValue X0)),
          arm64Instruction (ArmMov X0 (Arm64ImmediateValue 2)),
          arm64Instruction (ArmBl "_write"),
          arm64Instruction (ArmMov X0 (Arm64ImmediateValue 1)),
          arm64Instruction (ArmBl "__exit"),
          arm64Instruction (ArmBrk 0)
        ]
      messages =
        concat
          [ [arm64Label (messageLabel index), arm64Bytes (Text.encodeUtf8 (message <> "\n"))]
          | (message, index) <- traps
          ]
  pure
    ( [arm64Section TextSection]
        <> stubs
        <> reporter
        <> (if null traps then [] else arm64Section ReadOnlySection : messages)
    )

-- Data

compileData :: DataItem -> [Arm64Statement]
compileData dataItem =
  [ arm64Section (if dataMutable dataItem then DataSection else ReadOnlySection),
    arm64Align (log2 (dataAlignment dataItem))
  ]
    <> [arm64Global symbol | dataLinkage dataItem == Export]
    <> [arm64Label symbol]
    <> concatMap field (dataFields dataItem)
  where
    symbol = lirSymbol (dataName dataItem)
    field dataField =
      case dataField of
        DataInt ty value -> [arm64Word (typeBytes ty) (fromInteger value)]
        DataFloat F32 value -> [arm64Word 4 (fromIntegral (castFloatToWord32 (double2Float value)))]
        DataFloat _ value -> [arm64Word 8 (castDoubleToWord64 value)]
        DataSymbol target 0 -> [arm64QuadSymbol (lirSymbol target)]
        DataSymbol target addend -> [arm64QuadSymbolAddend (lirSymbol target) (fromInteger addend)]
        DataNull -> [arm64Quad 0]
        DataWord value -> [arm64Word 8 (fromInteger value)]
        DataCode Nothing -> [arm64Quad 0]
        DataCode (Just target) -> [arm64QuadSymbol (lirSymbol target)]
        DataBytes bytes -> [arm64Bytes bytes]
        DataZero count -> [arm64Bytes (BS.replicate (fromInteger count) 0)]

-- | A global is one word in the data section of its module.
compileGlobal :: Global -> [Arm64Statement]
compileGlobal global =
  [ arm64Section DataSection,
    arm64Align 3,
    arm64Label (lirSymbol (globalName global)),
    arm64Quad 0
  ]

log2 :: Integer -> Int
log2 value = length (takeWhile (< value) (iterate (* 2) 1))

typeBytes :: Type -> Int
typeBytes ty = max 1 (typeBits ty `div` 8)

-- Functions

-- | The frame of one function. Offsets are bytes above the stack pointer
-- after the prologue.
data Layout = Layout
  { -- | The values the allocator gave a register.
    layoutRegisters :: !(Map Var Arm64Register),
    -- | The frame slot of every value the allocator spilled.
    layoutSlots :: !(Map Var Int),
    -- | The callee-saved registers the allocator handed out, with the frame
    -- slot the prologue saves each one in.
    layoutSaved :: ![(Arm64Register, Int)],
    layoutTemps :: ![Int],
    layoutAllocs :: !(Map Var (Int, Int)),
    layoutSize :: !Int
  }

data Ctx = Ctx
  { ctxFunction :: !Function,
    ctxLayout :: !Layout,
    ctxLabels :: !(Map Label Text),
    ctxBlockParameters :: !(Map Label [(Var, Type)]),
    -- | The signatures of the functions the module defines or declares.
    ctxSignatures :: !(Map Symbol Signature),
    -- | The size of the incoming overflow block that the epilogue pops.
    ctxIncomingOverflow :: !Int
  }

argumentRegisters :: [Arm64Register]
argumentRegisters = [X0, X1, X2, X3, X4, X5, X6, X7]

-- | The registers that hold block arguments during a jump.
moveRegisters :: [Arm64Register]
moveRegisters = [X9, X10, X11, X12, X13, X14, X15]

-- | The registers the allocator hands out.
--
-- They are the callee-saved general registers, and nothing else. The callee
-- owns them across a call, so a value that lives in one survives every call
-- of the function and the allocator never has to split an interval. They are
-- also disjoint from the argument registers, from 'moveRegisters', and from
-- the scratch registers of instruction selection, so an allocated value is
-- never in the way of a convention.
allocatableRegisters :: [Arm64Register]
allocatableRegisters = [X19, X20, X21, X22, X23, X24, X25, X26, X27, X28]

overflowBytes :: Int -> Int
overflowBytes count = ((max 0 (count - length argumentRegisters) * 8 + 15) `div` 16) * 16

compileFunction :: Map Symbol Signature -> Int -> Function -> M [Arm64Statement]
compileFunction signatures index function = do
  layout <- functionLayout function
  let blocks = functionBlocks function
      labels = Map.fromList [(blockLabel block, ".Llir_" <> tshow index <> "_" <> tshow position) | (position, block) <- zip [0 :: Int ..] blocks]
      ctx =
        Ctx
          { ctxFunction = function,
            ctxLayout = layout,
            ctxLabels = labels,
            ctxBlockParameters = Map.fromList [(blockLabel block, blockParameters block) | block <- blocks],
            ctxSignatures = signatures,
            ctxIncomingOverflow = case functionConvention function of
              AihcConvention -> overflowBytes (length (functionParameters function))
              CConvention -> 0
          }
  when (functionConvention function == CConvention && length (functionParameters function) > length argumentRegisters) $
    unsupported ("function " <> unSymbol (functionName function) <> " has more than eight C parameters")
  prologue <- functionPrologue ctx
  body <- concat <$> mapM (compileBlock ctx) (zip blocks (map Just (drop 1 blocks) <> [Nothing]))
  pure
    ( [arm64Section TextSection, arm64Align 2]
        <> [arm64Global symbol | functionLinkage function == Export]
        <> [arm64Label symbol]
        <> elideSlotReloads (prologue <> body)
    )
  where
    symbol = lirSymbol (functionName function)

-- | Drop a read that the destination register already holds. Instruction
-- selection loads its operands into scratch registers, so the same value
-- reaches the same scratch register several times in a row while nothing has
-- touched it in between.
--
-- The pass tracks, for each general register, where its contents last came
-- from: a stack pointer offset, or another register. A read whose destination
-- already holds that source is dropped. A write to a register invalidates
-- both what that register held and every register that copied it. Anything
-- that moves the stack pointer, calls, or reaches a label forgets
-- everything. Tracking a slot is sound because the only frame addresses that
-- escape into a register come from @stack.alloc@, which 'functionLayout'
-- places above every value slot, so a store through a register base never
-- writes a tracked slot.
elideSlotReloads :: [Arm64Statement] -> [Arm64Statement]
elideSlotReloads = go IntMap.empty
  where
    go :: IntMap Source -> [Arm64Statement] -> [Arm64Statement]
    go held statements =
      case statements of
        [] -> []
        statement : rest ->
          case statement of
            Arm64Code instruction ->
              case instructionEffect instruction of
                LoadsSlot register offset -> reads' statement rest held register (FromSlot offset)
                -- A copy back the way one already went is redundant: the two
                -- registers still hold the same value.
                MovesRegister destination source
                  | IntMap.lookup source held == Just (FromRegister destination) -> go held rest
                  | otherwise -> reads' statement rest held destination (FromRegister source)
                -- The stored register holds the slot, and any register that
                -- held the previous contents of the slot is stale.
                StoresSlot register offset ->
                  statement : go (IntMap.insert register (FromSlot offset) (IntMap.filter (/= FromSlot offset) held)) rest
                Writes registers -> statement : go (foldr invalidate held registers) rest
                Forgets -> statement : go IntMap.empty rest
            _ -> statement : go IntMap.empty rest
    reads' statement rest held register source
      | IntMap.lookup register held == Just source = go held rest
      | otherwise = statement : go (IntMap.insert register source (invalidate register held)) rest
    -- A write to a register ends both what it held and every copy of it.
    invalidate register held = IntMap.filter (/= FromRegister register) (IntMap.delete register held)

-- | Where the contents of a register last came from.
data Source
  = FromSlot !Int64
  | FromRegister !Int
  deriving (Eq)

-- | What one instruction does to the registers and frame slots the reload
-- pass tracks.
data SlotEffect
  = -- | Overwrites these general registers and nothing else the pass tracks.
    Writes ![Int]
  | -- | Reads a general register from a literal stack pointer offset.
    LoadsSlot !Int !Int64
  | -- | Writes a general register to a literal stack pointer offset.
    StoresSlot !Int !Int64
  | -- | Copies one general register into another.
    MovesRegister !Int !Int
  | -- | Everything the pass knows becomes stale.
    Forgets

instructionEffect :: Arm64Instruction -> SlotEffect
instructionEffect instruction =
  case instruction of
    ArmRet -> Forgets
    ArmBrk _ -> Forgets
    ArmBr _ -> Forgets
    ArmB _ -> Forgets
    ArmBl _ -> Forgets
    ArmBlr _ -> Forgets
    -- A conditional branch changes nothing, and its target begins with a
    -- label, which forgets every register in its own right.
    ArmBCond _ _ -> Writes []
    ArmCbz _ _ -> Writes []
    ArmCbnz _ _ -> Writes []
    ArmCmp _ _ -> Writes []
    ArmFcmp {} -> Writes []
    ArmAdr destination _ -> writes [destination]
    ArmAdrp destination _ -> writes [destination]
    -- A copy between 64-bit names is a move the pass can follow. A narrow
    -- name clears the top half of its register, so it is only a write.
    ArmMov destination value ->
      case value of
        Arm64RegisterValue source
          | doubleWord destination && doubleWord source -> MovesRegister (generalRegister destination) (generalRegister source)
        _ -> writes [destination]
    ArmLdrImmediate destination _ -> writes [destination]
    ArmAddPageOffset destination _ _ -> writes [destination]
    ArmAdd destination _ _ -> writes [destination]
    ArmAdds destination _ _ -> writes [destination]
    ArmSub destination _ _ -> writes [destination]
    ArmSubs destination _ _ -> writes [destination]
    ArmAnd destination _ _ -> writes [destination]
    ArmOrr destination _ _ -> writes [destination]
    ArmEor destination _ _ -> writes [destination]
    ArmMvn destination _ -> writes [destination]
    ArmMul destination _ _ -> writes [destination]
    ArmUmulh destination _ _ -> writes [destination]
    ArmSmulh destination _ _ -> writes [destination]
    ArmUdiv destination _ _ -> writes [destination]
    ArmSdiv destination _ _ -> writes [destination]
    ArmMsub destination _ _ _ -> writes [destination]
    ArmLsl destination _ _ -> writes [destination]
    ArmLsr destination _ _ -> writes [destination]
    ArmAsr destination _ _ -> writes [destination]
    ArmCset destination _ -> writes [destination]
    ArmCsinv destination _ _ _ -> writes [destination]
    ArmCsel destination _ _ _ -> writes [destination]
    ArmSxtw destination _ -> writes [destination]
    ArmSxtb destination _ -> writes [destination]
    ArmSxth destination _ -> writes [destination]
    ArmClz destination _ -> writes [destination]
    ArmRbit destination _ -> writes [destination]
    -- The vector registers are outside the model, and no general register
    -- changes.
    ArmCnt {} -> Writes []
    ArmAddv {} -> Writes []
    ArmAndMask destination _ _ -> writes [destination]
    ArmFmovFromFloat _ destination _ -> writes [destination]
    ArmFcvtzs _ destination _ -> writes [destination]
    ArmFcvtzu _ destination _ -> writes [destination]
    -- The float registers are outside the model, and no general register
    -- changes.
    ArmFmovToFloat {} -> Writes []
    ArmFloat {} -> Writes []
    ArmFcvt {} -> Writes []
    ArmScvtf {} -> Writes []
    ArmUcvtf {} -> Writes []
    ArmLdr destination target ->
      case target of
        Arm64Offset SP offset -> LoadsSlot (generalRegister destination) offset
        Arm64Offset _ _ -> writes [destination]
        _ -> Forgets
    ArmStr source target ->
      case target of
        Arm64Offset SP offset -> StoresSlot (generalRegister source) offset
        Arm64Offset _ _ -> Writes []
        _ -> Forgets
    ArmLdrb destination base _ -> narrowAccess base (writes [destination])
    ArmLdrh destination base _ -> narrowAccess base (writes [destination])
    ArmStrb _ base _ -> narrowAccess base (Writes [])
    ArmStrh _ base _ -> narrowAccess base (Writes [])
    -- The pair instructions only ever address the stack pointer, and both
    -- forms in this backend move it.
    ArmLdp {} -> Forgets
    ArmStp {} -> Forgets
  where
    writes registers
      | SP `elem` registers = Forgets
      | otherwise = Writes (map generalRegister registers)
    narrowAccess base effect
      | base == SP = Forgets
      | otherwise = effect

-- | Whether a register name reads and writes all 64 bits. The stack pointer
-- is excluded: moving it is not a copy the pass may follow.
doubleWord :: Arm64Register -> Bool
doubleWord register = (register >= X0 && register <= X30) || register == XZR

-- | The key of the 64-bit register a name refers to. Writing @wN@ clears the
-- top half of @xN@, so both names have to invalidate the same entry.
generalRegister :: Arm64Register -> Int
generalRegister register
  | register >= W0 && register <= W30 = fromEnum register - fromEnum W0
  | register == WZR = fromEnum XZR
  | otherwise = fromEnum register

-- | Place the frame of one function. The allocator decides which values need
-- a slot at all; the rest of the frame holds the saved callee-saved
-- registers, the block-argument temporaries, and the stack allocations, in
-- that order. The stack allocations stay last so that no address that escapes
-- into a register can reach a slot, which is what 'elideSlotReloads' relies
-- on.
functionLayout :: Function -> M Layout
functionLayout function = do
  let blocks = functionBlocks function
      allocation = allocateRegisters allocatableRegisters function
      slots = Map.fromList (zip (allocationSpills allocation) [0, 8 ..])
      slotsEnd = 8 * Map.size slots
      saved = zip (allocationUsed allocation) [slotsEnd, slotsEnd + 8 ..]
      savedEnd = slotsEnd + 8 * length saved
      maxJumpArguments = maximum (0 : [length (targetArguments target) | block <- blocks, target <- terminatorTargets (blockTerminator block)])
      tempCount = if maxJumpArguments > length moveRegisters then maxJumpArguments else 0
      temps = take tempCount [savedEnd, savedEnd + 8 ..]
      allocsStart = savedEnd + 8 * tempCount
      allocations = [(var, size, alignment) | block <- take 1 blocks, Instruction [var] (StackAlloc size alignment) <- blockInstructions block]
  allocs <- placeAllocations allocsStart allocations
  let end = case Map.elems allocs of
        [] -> allocsStart
        placed -> maximum [offset + allocated | (offset, allocated) <- placed]
      size = ((end + 15) `div` 16) * 16
  when (size > 32000) $ unsupported ("function " <> unSymbol (functionName function) <> " needs a frame larger than 32000 bytes")
  pure
    Layout
      { layoutRegisters = allocationRegisters allocation,
        layoutSlots = slots,
        layoutSaved = saved,
        layoutTemps = temps,
        layoutAllocs = allocs,
        layoutSize = size
      }

placeAllocations :: Int -> [(Var, Integer, Integer)] -> M (Map Var (Int, Int))
placeAllocations = go Map.empty
  where
    go placed _ [] = pure placed
    go placed next ((var, size, alignment) : rest) = do
      when (alignment > 16) $ unsupported "stack.alloc alignment above 16"
      let start = roundUp (fromInteger alignment) next
      go (Map.insert var (start, fromInteger size) placed) (start + fromInteger size) rest
    roundUp alignment value = ((value + alignment - 1) `div` alignment) * alignment

-- | Save the frame pointer and the link register, reserve the frame, zero
-- the stack allocations, and copy the parameters into their slots.
functionPrologue :: Ctx -> M [Arm64Statement]
functionPrologue ctx = do
  parameterStores <- concat <$> mapM storeParameter (zip [0 ..] (functionParameters function))
  pure
    ( [ arm64Instruction (ArmStp X29 X30 (Arm64PreIndex SP (-16))),
        arm64Instruction (ArmMov X29 (Arm64RegisterValue SP))
      ]
        <> adjustStack ArmSub (layoutSize layout)
        <> saveRegisters ctx
        <> concatMap zeroAllocation (Map.elems (layoutAllocs layout))
        <> parameterStores
    )
  where
    function = ctxFunction ctx
    layout = ctxLayout ctx
    storeParameter (index, (var, ty))
      | index < length argumentRegisters =
          let register = argumentRegisters !! index
           in pure (floatParameter ty register <> canonicalize ty register <> [writeValue ctx 0 register var])
      | otherwise =
          -- The overflow block sits above the saved frame pointer pair.
          pure
            [ arm64Instruction (ArmLdr X9 (Arm64Offset X29 (fromIntegral (16 + 8 * (index - length argumentRegisters))))),
              writeValue ctx 0 X9 var
            ]
    canonicalize ty register =
      case functionConvention function of
        CConvention -> canonicalizeRegister ty register
        AihcConvention -> []
    -- C passes floats in the float registers of the same index.
    floatParameter ty register =
      case functionConvention function of
        CConvention | isFloatType ty -> [arm64Instruction (ArmFmovFromFloat (ty == F64) register (registerIndex register))]
        _ -> []
    zeroAllocation (offset, size) =
      [ arm64Instruction (ArmStr XZR (Arm64Offset SP (fromIntegral (offset + position))))
      | position <- [0, 8 .. size - 1]
      ]

-- | Narrow a value that arrives from C code in the low bits of a register.
canonicalizeRegister :: Type -> Arm64Register -> [Arm64Statement]
canonicalizeRegister ty register =
  case ty of
    I1 -> [arm64Instruction (ArmAndMask register register 1)]
    I8 -> [arm64Instruction (ArmAndMask register register 8)]
    I16 -> [arm64Instruction (ArmAndMask register register 16)]
    I32 -> [arm64Instruction (ArmAndMask register register 32)]
    F32 -> [arm64Instruction (ArmAndMask register register 32)]
    _ -> []

adjustStack :: (Arm64Register -> Arm64Register -> Arm64Value -> Arm64Instruction) -> Int -> [Arm64Statement]
adjustStack operation bytes
  | bytes == 0 = []
  | bytes < 4096 = [arm64Instruction (operation SP SP (Arm64ImmediateValue (fromIntegral bytes)))]
  | otherwise = [immediate X9 bytes, arm64Instruction (operation SP SP (Arm64RegisterValue X9))]

-- | Restore the stack of the caller: the frame, the saved pair, and the
-- incoming overflow block.
functionEpilogue :: Ctx -> [Arm64Statement]
functionEpilogue ctx =
  [ arm64Instruction (ArmMov SP (Arm64RegisterValue X29)),
    arm64Instruction (ArmLdp X29 X30 (Arm64PostIndex SP 16))
  ]
    <> adjustStack ArmAdd (ctxIncomingOverflow ctx)

slotOffset :: Ctx -> Int -> Var -> Int64
slotOffset ctx displacement var =
  case Map.lookup var (layoutSlots (ctxLayout ctx)) of
    Just offset -> fromIntegral (offset + displacement)
    Nothing -> error ("Aihc.Arm64.Lir: unknown value " <> T.unpack (unVar var))

-- | The register the allocator gave a value, if it gave it one.
valueRegister :: Ctx -> Var -> Maybe Arm64Register
valueRegister ctx var = Map.lookup var (layoutRegisters (ctxLayout ctx))

-- | Read a value into a register. A value the allocator placed is already in
-- a register, so the read is a move; a spilled value comes from its frame
-- slot. The displacement is the number of bytes the stack pointer currently
-- sits below the frame base, and it reaches only the slot form.
readValue :: Ctx -> Int -> Arm64Register -> Var -> Arm64Statement
readValue ctx displacement register var =
  case valueRegister ctx var of
    Just held -> arm64Instruction (ArmMov register (Arm64RegisterValue held))
    Nothing -> arm64Instruction (ArmLdr register (Arm64Offset SP (slotOffset ctx displacement var)))

-- | Write a register into a value: the mirror of 'readValue'.
writeValue :: Ctx -> Int -> Arm64Register -> Var -> Arm64Statement
writeValue ctx displacement register var =
  case valueRegister ctx var of
    Just held -> arm64Instruction (ArmMov held (Arm64RegisterValue register))
    Nothing -> arm64Instruction (ArmStr register (Arm64Offset SP (slotOffset ctx displacement var)))

-- | Save the allocated callee-saved registers into the frame. The prologue
-- runs this before it moves any parameter into a register.
saveRegisters :: Ctx -> [Arm64Statement]
saveRegisters ctx =
  [ arm64Instruction (ArmStr register (Arm64Offset SP (fromIntegral offset)))
  | (register, offset) <- layoutSaved (ctxLayout ctx)
  ]

-- | Restore what 'saveRegisters' saved. Every exit runs this after it has
-- read the last allocated value and before it moves the stack pointer.
restoreRegisters :: Ctx -> Int -> [Arm64Statement]
restoreRegisters ctx displacement =
  [ arm64Instruction (ArmLdr register (Arm64Offset SP (fromIntegral (offset + displacement))))
  | (register, offset) <- layoutSaved (ctxLayout ctx)
  ]

-- | Load an operand into a register. The displacement is the number of bytes
-- the stack pointer currently sits below the frame base.
loadOperand :: Ctx -> Int -> Arm64Register -> Operand -> [Arm64Statement]
loadOperand ctx displacement register operand =
  case operand of
    OperandVar var -> [readValue ctx displacement register var]
    OperandLiteral literal ->
      case literal of
        LitInt value -> [immediate register value]
        LitFloat value -> [immediate register (toInteger (castDoubleToWord64 value))]
        LitNull -> [arm64Instruction (ArmMov register (Arm64RegisterValue XZR))]
        LitSymbol symbol -> address register (lirSymbol symbol)

-- | Load a literal with the encoding of its type. Float literals need the
-- width of the type, and integer literals are canonical for the type.
loadTyped :: Ctx -> Int -> Type -> Arm64Register -> Operand -> [Arm64Statement]
loadTyped ctx displacement ty register operand =
  case (ty, operand) of
    (F32, OperandLiteral (LitFloat value)) -> [immediate register (toInteger (castFloatToWord32 (double2Float value)))]
    (F32, OperandLiteral (LitInt value)) -> [immediate register (toInteger (castFloatToWord32 (fromInteger value)))]
    (F64, OperandLiteral (LitInt value)) -> [immediate register (toInteger (castDoubleToWord64 (fromInteger value)))]
    (_, OperandLiteral (LitInt value)) -> [immediate register (canonicalInteger ty value)]
    _ -> loadOperand ctx displacement register operand

canonicalInteger :: Type -> Integer -> Integer
canonicalInteger ty value
  | typeBits ty >= 64 = value `mod` (2 ^ (64 :: Int))
  | otherwise = value `mod` (2 ^ typeBits ty)

address :: Arm64Register -> Text -> [Arm64Statement]
address register label =
  [ arm64Instruction (ArmAdrp register label),
    arm64Instruction (ArmAddPageOffset register register label)
  ]

immediate :: (Integral value) => Arm64Register -> value -> Arm64Statement
immediate register value
  | integer >= -65536 && integer <= 65535 = arm64Instruction (ArmMov register (Arm64ImmediateValue integer))
  | otherwise = arm64Instruction (ArmLdrImmediate register integer)
  where
    integer = toInteger value

tshow :: (Show value) => value -> Text
tshow = T.pack . show

-- Blocks

compileBlock :: Ctx -> (Block, Maybe Block) -> M [Arm64Statement]
compileBlock ctx (block, next) = do
  instructions <- concat <$> mapM (compileInstruction ctx) (blockInstructions block)
  terminator <- compileTerminator ctx (blockLabel <$> next) (blockTerminator block)
  pure (arm64Label (ctxLabels ctx Map.! blockLabel block) : instructions <> terminator)

compileTerminator :: Ctx -> Maybe Label -> Terminator -> M [Arm64Statement]
compileTerminator ctx next terminator =
  case terminator of
    Jump target -> jumpTo target
    Branch condition whenTrue whenFalse -> do
      falseLabel <- freshLabel "else"
      trueLines <- jumpTo whenTrue
      falseLines <- jumpTo whenFalse
      pure
        ( loadOperand ctx 0 X9 condition
            <> [arm64Instruction (ArmCbz X9 falseLabel)]
            <> trueLines
            <> [arm64Label falseLabel]
            <> falseLines
        )
    Switch ty scrutinee cases fallback -> do
      edges <- forM cases $ \switchCase -> do
        label <- freshLabel "case"
        lines' <- jumpTo (switchCaseTarget switchCase)
        pure (switchCase, label, lines')
      fallbackLines <-
        case fallback of
          Just target -> jumpTo target
          Nothing -> do
            stub <- trapLabel "switch without a matching case"
            pure [arm64Instruction (ArmB stub)]
      let checks =
            concat
              [ [immediate X10 (canonicalInteger ty (switchCaseValue switchCase)), arm64Instruction (ArmCmp X9 (Arm64RegisterValue X10)), arm64Instruction (ArmBCond ArmEq label)]
              | (switchCase, label, _) <- edges
              ]
          bodies = concat [arm64Label label : lines' | (_, label, lines') <- edges]
      pure (loadOperand ctx 0 X9 scrutinee <> checks <> fallbackLines <> bodies)
    Return values -> do
      when (length values > length argumentRegisters) $ unsupported "return of more than eight values"
      let loads = concat [loadTyped ctx 0 ty register value | (ty, register, value) <- zip3 (functionResults function) argumentRegisters values]
          floatMoves =
            case (functionConvention function, functionResults function) of
              (CConvention, [ty]) | isFloatType ty -> [arm64Instruction (ArmFmovToFloat (ty == F64) 0 X0)]
              _ -> []
      pure (loads <> floatMoves <> restoreRegisters ctx 0 <> functionEpilogue ctx <> [arm64Instruction ArmRet])
    TailCall symbol arguments -> tailCall (Left (lirSymbol symbol)) arguments
    TailCallIndirect target arguments _ -> tailCall (Right target) arguments
    Trap message -> do
      stub <- trapLabel message
      pure [arm64Instruction (ArmB stub)]
  where
    function = ctxFunction ctx
    -- Only an unconditional jump falls through to the next block.
    jumpTo target = do
      moves <- blockArgumentMoves ctx target
      let label = ctxLabels ctx Map.! targetLabel target
          fallsThrough = case terminator of
            Jump _ -> Just (targetLabel target) == next
            _ -> False
      pure (moves <> [arm64Instruction (ArmB label) | not fallsThrough])
    tailCall callee arguments = do
      let outgoing = overflowBytes (length arguments)
          incoming = ctxIncomingOverflow ctx
          overflow = drop (length argumentRegisters) arguments
          targetLoad = case callee of
            Left _ -> []
            Right operand -> loadOperand ctx outgoing X8 operand
          overflowStores =
            concat
              [ loadOperand ctx outgoing X9 argument <> [arm64Instruction (ArmStr X9 (Arm64Offset SP (fromIntegral (8 * position))))]
              | (position, argument) <- zip [0 :: Int ..] overflow
              ]
          registerLoads = concat [loadOperand ctx outgoing register argument | (register, argument) <- zip argumentRegisters arguments]
          -- Restore the saved pair first, then copy the outgoing block to its
          -- final place just below the stack of the caller, highest word first
          -- because the destination lies above the source.
          relocation
            | outgoing == 0 = functionEpilogue ctx
            | otherwise =
                [ arm64Instruction (ArmMov X10 (Arm64RegisterValue X29)),
                  arm64Instruction (ArmLdp X29 X30 (Arm64Offset X10 0))
                ]
                  <> destination (16 + incoming - outgoing)
                  <> concat
                    [ [ arm64Instruction (ArmLdr X9 (Arm64Offset SP (fromIntegral (8 * position)))),
                        arm64Instruction (ArmStr X9 (Arm64Offset X11 (fromIntegral (8 * position))))
                      ]
                    | position <- reverse [0 .. length overflow - 1]
                    ]
                  <> [arm64Instruction (ArmMov SP (Arm64RegisterValue X11))]
          destination delta
            | delta >= 0 = [arm64Instruction (ArmAdd X11 X10 (Arm64ImmediateValue (fromIntegral delta)))]
            | otherwise = [arm64Instruction (ArmSub X11 X10 (Arm64ImmediateValue (fromIntegral (negate delta))))]
          branch = case callee of
            Left label -> arm64Instruction (ArmB label)
            Right _ -> arm64Instruction (ArmBr X8)
      nullCheck <-
        case callee of
          Left _ -> pure []
          Right _ -> do
            stub <- trapLabel "indirect call to a non-function"
            pure [arm64Instruction (ArmCbz X8 stub)]
      pure
        ( adjustStack ArmSub outgoing
            <> targetLoad
            <> nullCheck
            <> overflowStores
            <> registerLoads
            -- Every allocated value has been read by now, so the saved
            -- registers go back before the frame does.
            <> restoreRegisters ctx outgoing
            <> relocation
            <> [branch]
        )

-- | Move the arguments of a jump into the parameter slots of the target. All
-- arguments are read before any parameter is written.
blockArgumentMoves :: Ctx -> Target -> M [Arm64Statement]
blockArgumentMoves ctx (Target label arguments) = do
  let parameters = Map.findWithDefault [] label (ctxBlockParameters ctx)
  if length arguments <= length moveRegisters
    then
      pure
        ( concat [loadTyped ctx 0 ty register argument | ((_, ty), register, argument) <- zip3 parameters moveRegisters arguments]
            <> [writeValue ctx 0 register var | ((var, _), register) <- zip parameters moveRegisters]
        )
    else do
      let temps = layoutTemps (ctxLayout ctx)
      pure
        ( concat
            [ loadTyped ctx 0 ty X9 argument <> [arm64Instruction (ArmStr X9 (Arm64Offset SP (fromIntegral temp)))]
            | ((_, ty), temp, argument) <- zip3 parameters temps arguments
            ]
            <> concat
              [ [arm64Instruction (ArmLdr X9 (Arm64Offset SP (fromIntegral temp))), writeValue ctx 0 X9 var]
              | ((var, _), temp) <- zip parameters temps
              ]
        )

-- Instructions

compileInstruction :: Ctx -> Instruction -> M [Arm64Statement]
compileInstruction ctx (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      body <- binary op ty
      single (loadTyped ctx 0 ty X9 left <> loadTyped ctx 0 ty X10 right <> body)
    Unary op ty value -> single (loadTyped ctx 0 ty X9 value <> bitCount op ty)
    Wide op ty left right -> do
      body <- wide op ty
      pair (loadTyped ctx 0 ty X9 left <> loadTyped ctx 0 ty X10 right <> body)
    Compare op ty left right ->
      single (loadTyped ctx 0 ty X9 left <> loadTyped ctx 0 ty X10 right <> compare' op ty)
    FloatBinary op ty left right ->
      single
        ( loadTyped ctx 0 ty X9 left
            <> loadTyped ctx 0 ty X10 right
            <> [toFloat ty 16 X9, toFloat ty 17 X10, arm64Instruction (ArmFloat (floatOp op) (ty == F64) 16 16 17), fromFloat ty X9 16]
        )
    FloatUnary op ty value ->
      single (loadTyped ctx 0 ty X9 value <> [toFloat ty 16 X9, arm64Instruction (ArmFloat (floatUnaryOp op) (ty == F64) 16 16 16), fromFloat ty X9 16])
    Convert op from value to -> do
      body <- convert op from to
      single (loadTyped ctx 0 from X9 value <> body)
    PtrToInt value -> single (loadOperand ctx 0 X9 value)
    PtrFromInt value -> single (loadOperand ctx 0 X9 value)
    Select ty condition left right ->
      single
        ( loadOperand ctx 0 X11 condition
            <> loadTyped ctx 0 ty X9 left
            <> loadTyped ctx 0 ty X10 right
            <> [arm64Instruction (ArmCmp X11 (Arm64ImmediateValue 0)), arm64Instruction (ArmCsel X9 X9 X10 ArmNe)]
        )
    Load ty (Address base offset) _ -> do
      addressLines <- effectiveAddress base offset ty
      single (addressLines <> [loadMemory ty X9 X10 (memoryOffset offset ty)])
    Store ty value (Address base offset) _ -> do
      addressLines <- effectiveAddress base offset ty
      pure (loadTyped ctx 0 ty X9 value <> addressLines <> [storeMemory ty X9 X10 (memoryOffset offset ty)])
    PtrAdd base offset ->
      single (loadOperand ctx 0 X9 base <> loadOperand ctx 0 X10 offset <> [arm64Instruction (ArmAdd X9 X9 (Arm64RegisterValue X10))])
    StackAlloc _ _ ->
      case results of
        [var]
          | Just (offset, _) <- Map.lookup var (layoutAllocs (ctxLayout ctx)) ->
              single
                ( if offset < 4096
                    then [arm64Instruction (ArmAdd X9 SP (Arm64ImmediateValue (fromIntegral offset)))]
                    else [immediate X9 offset, arm64Instruction (ArmAdd X9 SP (Arm64RegisterValue X9))]
                )
        _ -> unsupported "stack.alloc without a placed result"
    GlobalGet symbol -> single (address X10 (lirSymbol symbol) <> [arm64Instruction (ArmLdr X9 (Arm64Offset X10 0))])
    GlobalSet symbol value ->
      pure (loadOperand ctx 0 X9 value <> address X10 (lirSymbol symbol) <> [arm64Instruction (ArmStr X9 (Arm64Offset X10 0))])
    Call symbol arguments -> call (Left symbol) arguments
    CallIndirect target arguments signature -> callIndirect target arguments signature
  where
    single body =
      case results of
        [var] -> pure (body <> [writeValue ctx 0 X9 var])
        _ -> unsupported "instruction result count"
    pair body =
      case results of
        [first, second] -> pure (body <> [writeValue ctx 0 X9 first, writeValue ctx 0 X10 second])
        _ -> unsupported "instruction result count"

    -- The offset of a load or a store: folded into the instruction when the
    -- scaled immediate form permits it, otherwise added to the base.
    memoryOffset offset ty
      | fitsScaled offset ty = fromInteger offset
      | otherwise = 0
    fitsScaled offset ty =
      let size = toInteger (typeBytes ty)
       in offset >= 0 && offset `mod` size == 0 && offset `div` size < 4096
    effectiveAddress base offset ty
      | fitsScaled offset ty = pure (loadOperand ctx 0 X10 base)
      | otherwise = pure (loadOperand ctx 0 X10 base <> [immediate X11 offset, arm64Instruction (ArmAdd X10 X10 (Arm64RegisterValue X11))])

    -- A narrow value is zero-extended in its slot, so a leading-zero count
    -- includes the bits above the type and a trailing-zero count of zero
    -- would reach the top of the register. Setting the first bit above the
    -- type keeps the trailing count at the width of the type.
    bitCount op ty =
      let bits = typeBits ty
       in case op of
            Popcount ->
              [ toFloat F64 16 X9,
                arm64Instruction (ArmCnt 16 16),
                arm64Instruction (ArmAddv 16 16),
                fromFloat F64 X9 16
              ]
            Clz ->
              arm64Instruction (ArmClz X9 X9)
                : [arm64Instruction (ArmSub X9 X9 (Arm64ImmediateValue (toInteger (64 - bits)))) | bits < 64]
            Ctz ->
              concat [[immediate X10 (2 ^ bits :: Integer), arm64Instruction (ArmOrr X9 X9 (Arm64RegisterValue X10))] | bits < 64]
                <> [arm64Instruction (ArmRbit X9 X9), arm64Instruction (ArmClz X9 X9)]

    binary op ty =
      case op of
        Add -> pure (narrow ty [arm64Instruction (ArmAdd X9 X9 (Arm64RegisterValue X10))])
        Sub -> pure (narrow ty [arm64Instruction (ArmSub X9 X9 (Arm64RegisterValue X10))])
        Mul -> pure (narrow ty [arm64Instruction (ArmMul X9 X9 X10)])
        DivS -> do
          zero <- trapLabel "integer division by zero"
          overflow <- trapLabel "integer overflow"
          skip <- freshLabel "div"
          pure
            ( signExtend ty X9
                <> signExtend ty X10
                <> [ arm64Instruction (ArmCbz X10 zero),
                     immediate X11 (-1 :: Integer),
                     arm64Instruction (ArmCmp X10 (Arm64RegisterValue X11)),
                     arm64Instruction (ArmBCond ArmNe skip),
                     immediate X11 (minimumSigned ty),
                     arm64Instruction (ArmCmp X9 (Arm64RegisterValue X11)),
                     arm64Instruction (ArmBCond ArmEq overflow),
                     arm64Label skip
                   ]
                <> narrow ty [arm64Instruction (ArmSdiv X9 X9 X10)]
            )
        DivU -> do
          zero <- trapLabel "integer division by zero"
          pure [arm64Instruction (ArmCbz X10 zero), arm64Instruction (ArmUdiv X9 X9 X10)]
        RemS -> do
          zero <- trapLabel "integer division by zero"
          pure
            ( signExtend ty X9
                <> signExtend ty X10
                <> [arm64Instruction (ArmCbz X10 zero), arm64Instruction (ArmSdiv X11 X9 X10)]
                <> narrow ty [arm64Instruction (ArmMsub X9 X11 X10 X9)]
            )
        RemU -> do
          zero <- trapLabel "integer division by zero"
          pure [arm64Instruction (ArmCbz X10 zero), arm64Instruction (ArmUdiv X11 X9 X10), arm64Instruction (ArmMsub X9 X11 X10 X9)]
        And -> pure [arm64Instruction (ArmAnd X9 X9 X10)]
        Or -> pure [arm64Instruction (ArmOrr X9 X9 (Arm64RegisterValue X10))]
        Xor -> pure [arm64Instruction (ArmEor X9 X9 X10)]
        Shl -> pure (shiftCount ty <> narrow ty [arm64Instruction (ArmLsl X9 X9 (Arm64RegisterShift X10))])
        ShrS -> pure (signExtend ty X9 <> shiftCount ty <> narrow ty [arm64Instruction (ArmAsr X9 X9 (Arm64RegisterShift X10))])
        ShrU -> pure (shiftCount ty <> [arm64Instruction (ArmLsr X9 X9 (Arm64RegisterShift X10))])

    wide op ty =
      case op of
        MulWideU
          | typeBits ty == 64 -> pure [arm64Instruction (ArmUmulh X11 X9 X10), arm64Instruction (ArmMul X9 X9 X10), arm64Instruction (ArmMov X10 (Arm64RegisterValue X11))]
          | otherwise ->
              pure
                ( [arm64Instruction (ArmMul X11 X9 X10)]
                    <> [arm64Instruction (ArmLsr X10 X11 (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
                    <> narrowRegister ty X10
                    <> [arm64Instruction (ArmMov X9 (Arm64RegisterValue X11))]
                    <> narrowRegister ty X9
                )
        MulWideS
          | typeBits ty == 64 -> pure [arm64Instruction (ArmSmulh X11 X9 X10), arm64Instruction (ArmMul X9 X9 X10), arm64Instruction (ArmMov X10 (Arm64RegisterValue X11))]
          | otherwise ->
              pure
                ( signExtend ty X9
                    <> signExtend ty X10
                    <> [arm64Instruction (ArmMul X11 X9 X10)]
                    <> [arm64Instruction (ArmAsr X10 X11 (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
                    <> narrowRegister ty X10
                    <> [arm64Instruction (ArmMov X9 (Arm64RegisterValue X11))]
                    <> narrowRegister ty X9
                )
        AddCarry
          | typeBits ty == 64 -> pure [arm64Instruction (ArmAdds X9 X9 (Arm64RegisterValue X10)), arm64Instruction (ArmCset X10 ArmCs)]
          | otherwise ->
              pure
                ( [arm64Instruction (ArmAdd X9 X9 (Arm64RegisterValue X10)), arm64Instruction (ArmLsr X10 X9 (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
                    <> narrowRegister ty X9
                )
        SubBorrow
          | typeBits ty == 64 -> pure [arm64Instruction (ArmSubs X9 X9 (Arm64RegisterValue X10)), arm64Instruction (ArmCset X10 ArmCc)]
          | otherwise ->
              pure
                ( [ arm64Instruction (ArmCmp X9 (Arm64RegisterValue X10)),
                    arm64Instruction (ArmCset X11 ArmCc),
                    arm64Instruction (ArmSub X9 X9 (Arm64RegisterValue X10))
                  ]
                    <> narrowRegister ty X9
                    <> [arm64Instruction (ArmMov X10 (Arm64RegisterValue X11))]
                )

    compare' op ty
      | isFloatType ty =
          [toFloat ty 16 X9, toFloat ty 17 X10, arm64Instruction (ArmFcmp (ty == F64) 16 17), arm64Instruction (ArmCset X9 (floatCondition op))]
      | op `elem` [LtS, LeS, GtS, GeS] =
          signExtend ty X9 <> signExtend ty X10 <> [arm64Instruction (ArmCmp X9 (Arm64RegisterValue X10)), arm64Instruction (ArmCset X9 (integerCondition op))]
      | otherwise = [arm64Instruction (ArmCmp X9 (Arm64RegisterValue X10)), arm64Instruction (ArmCset X9 (integerCondition op))]

    convert op from to =
      case op of
        SExt -> pure (signExtend from X9 <> narrowRegister to X9)
        ZExt -> pure []
        Trunc -> pure (narrowRegister to X9)
        IToFS -> pure (signExtend from X9 <> [arm64Instruction (ArmScvtf (to == F64) 16 X9), fromFloat to X9 16])
        IToFU -> pure [arm64Instruction (ArmUcvtf (to == F64) 16 X9), fromFloat to X9 16]
        FToIS -> floatToInteger True from to
        FToIU -> floatToInteger False from to
        FpExt -> pure [arm64Instruction (ArmFmovToFloat False 16 W9), arm64Instruction (ArmFcvt True 16 16), arm64Instruction (ArmFmovFromFloat True X9 16)]
        FpTrunc -> pure [arm64Instruction (ArmFmovToFloat True 16 X9), arm64Instruction (ArmFcvt False 16 16), arm64Instruction (ArmFmovFromFloat False W9 16)]
        Bitcast -> pure []

    -- Widen the source to double, reject NaN and values outside the target
    -- range, then convert with rounding toward zero.
    floatToInteger signed from to = do
      invalid <- trapLabel "invalid float to integer conversion"
      let widen = if from == F64 then [arm64Instruction (ArmFmovToFloat True 16 X9)] else [arm64Instruction (ArmFmovToFloat False 16 W9), arm64Instruction (ArmFcvt True 16 16)]
          bits = typeBits to
          lower = if signed then negate (2 ^^ (bits - 1)) else -1 :: Double
          upper = if signed then 2 ^^ (bits - 1) else 2 ^^ bits :: Double
          lowerCondition = if signed then ArmMi else ArmLe
          convertOp = if signed then ArmFcvtzs True X9 16 else ArmFcvtzu True X9 16
      pure
        ( widen
            <> [ arm64Instruction (ArmFcmp True 16 16),
                 arm64Instruction (ArmBCond ArmVs invalid),
                 immediate X10 (toInteger (castDoubleToWord64 lower)),
                 arm64Instruction (ArmFmovToFloat True 17 X10),
                 arm64Instruction (ArmFcmp True 16 17),
                 arm64Instruction (ArmBCond lowerCondition invalid),
                 immediate X10 (toInteger (castDoubleToWord64 upper)),
                 arm64Instruction (ArmFmovToFloat True 17 X10),
                 arm64Instruction (ArmFcmp True 16 17),
                 arm64Instruction (ArmBCond ArmGe invalid),
                 arm64Instruction convertOp
               ]
            <> narrowRegister to X9
        )

    shiftCount ty
      | typeBits ty == 64 = []
      | otherwise = [arm64Instruction (ArmAndMask X10 X10 (log2 (toInteger (typeBits ty))))]
    narrow ty body = body <> narrowRegister ty X9

    call callee arguments = do
      let (convention, resultTypes, parameterTypes) = calleeSignature callee
      when (convention == CConvention && length arguments > length argumentRegisters) $ unsupported "C call with more than eight arguments"
      let outgoing = case convention of
            CConvention -> 0
            AihcConvention -> overflowBytes (length arguments)
          overflowStores =
            concat
              [ loadTyped ctx outgoing ty X9 argument <> [arm64Instruction (ArmStr X9 (Arm64Offset SP (fromIntegral (8 * position))))]
              | (position, (ty, argument)) <- zip [0 :: Int ..] (drop (length argumentRegisters) (zip parameterTypes arguments))
              ]
          registerLoads =
            concat
              [ loadTyped ctx outgoing ty register argument <> floatArgument convention ty register
              | (register, (ty, argument)) <- zip argumentRegisters (zip parameterTypes arguments)
              ]
          branch = case callee of
            Left symbol -> [arm64Instruction (ArmBl (lirSymbol symbol))]
            Right _ -> [arm64Instruction (ArmBlr X8)]
          resultStores =
            concat
              [ floatResult convention ty register <> canonicalResult convention ty register <> [writeValue ctx 0 register var]
              | (var, ty, register) <- zip3 results resultTypes argumentRegisters
              ]
      pure (adjustStack ArmSub outgoing <> overflowStores <> registerLoads <> branch <> resultStores)
    callIndirect target arguments signature = do
      stub <- trapLabel "indirect call to a non-function"
      body <- call (Right signature) arguments
      pure (loadOperand ctx 0 X8 target <> [arm64Instruction (ArmCbz X8 stub)] <> body)
    calleeSignature callee =
      case callee of
        Left symbol ->
          case Map.lookup symbol (ctxSignatures ctx) of
            Just signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)
            Nothing -> (AihcConvention, [], [])
        Right signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)
    floatArgument convention ty register =
      case convention of
        CConvention | isFloatType ty -> [arm64Instruction (ArmFmovToFloat (ty == F64) (registerIndex register) register)]
        _ -> []
    floatResult convention ty register =
      case convention of
        CConvention | isFloatType ty -> [arm64Instruction (ArmFmovFromFloat (ty == F64) register 0)]
        _ -> []
    canonicalResult convention ty register =
      case convention of
        CConvention -> canonicalizeRegister ty register
        AihcConvention -> []

registerIndex :: Arm64Register -> Int
registerIndex register = fromEnum register - fromEnum X0

minimumSigned :: Type -> Integer
minimumSigned ty = negate (2 ^ (typeBits ty - 1))

-- | Mask a register to the width of a narrow type.
narrowRegister :: Type -> Arm64Register -> [Arm64Statement]
narrowRegister ty register
  | typeBits ty >= 64 = []
  | otherwise = [arm64Instruction (ArmAndMask register register (typeBits ty))]

-- | Sign-extend a canonical narrow value to 64 bits.
signExtend :: Type -> Arm64Register -> [Arm64Statement]
signExtend ty register =
  case ty of
    I1 -> [arm64Instruction (ArmSub register XZR (Arm64RegisterValue register))]
    I8 -> [arm64Instruction (ArmSxtb register register)]
    I16 -> [arm64Instruction (ArmSxth register register)]
    I32 -> [arm64Instruction (ArmSxtw register register)]
    _ -> []

toFloat :: Type -> Int -> Arm64Register -> Arm64Statement
toFloat ty float general = arm64Instruction (ArmFmovToFloat (ty == F64) float (if ty == F64 then general else wordRegister general))

fromFloat :: Type -> Arm64Register -> Int -> Arm64Statement
fromFloat ty general float = arm64Instruction (ArmFmovFromFloat (ty == F64) (if ty == F64 then general else wordRegister general) float)

wordRegister :: Arm64Register -> Arm64Register
wordRegister register = toEnum (fromEnum register - fromEnum X0 + fromEnum W0)

floatOp :: FloatBinaryOp -> Arm64FloatOp
floatOp op =
  case op of
    FAdd -> ArmFAdd
    FSub -> ArmFSub
    FMul -> ArmFMul
    FDiv -> ArmFDiv

floatUnaryOp :: FloatUnaryOp -> Arm64FloatOp
floatUnaryOp op =
  case op of
    FNeg -> ArmFNeg
    FAbs -> ArmFAbs
    FSqrt -> ArmFSqrt

integerCondition :: CompareOp -> Arm64Condition
integerCondition op =
  case op of
    Eq -> ArmEq
    Ne -> ArmNe
    LtS -> ArmLt
    LtU -> ArmCc
    LeS -> ArmLe
    LeU -> ArmLs
    GtS -> ArmGt
    GtU -> ArmHi
    GeS -> ArmGe
    GeU -> ArmCs
    FLt -> ArmMi
    FLe -> ArmLs
    FGt -> ArmGt
    FGe -> ArmGe

-- | Ordered float conditions: every unordered comparison is false except
-- @ne@.
floatCondition :: CompareOp -> Arm64Condition
floatCondition op =
  case op of
    Eq -> ArmEq
    Ne -> ArmNe
    FLt -> ArmMi
    FLe -> ArmLs
    FGt -> ArmGt
    FGe -> ArmGe
    _ -> ArmEq

loadMemory :: Type -> Arm64Register -> Arm64Register -> Int64 -> Arm64Statement
loadMemory ty value base offset =
  case typeBytes ty of
    1 -> arm64Instruction (ArmLdrb (wordRegister value) base offset)
    2 -> arm64Instruction (ArmLdrh (wordRegister value) base offset)
    4 -> arm64Instruction (ArmLdr (wordRegister value) (Arm64Offset base offset))
    _ -> arm64Instruction (ArmLdr value (Arm64Offset base offset))

storeMemory :: Type -> Arm64Register -> Arm64Register -> Int64 -> Arm64Statement
storeMemory ty value base offset =
  case typeBytes ty of
    1 -> arm64Instruction (ArmStrb (wordRegister value) base offset)
    2 -> arm64Instruction (ArmStrh (wordRegister value) base offset)
    4 -> arm64Instruction (ArmStr (wordRegister value) (Arm64Offset base offset))
    _ -> arm64Instruction (ArmStr value (Arm64Offset base offset))
