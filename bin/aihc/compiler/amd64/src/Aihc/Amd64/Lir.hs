{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to AMD64 ELF objects for Linux.
--
-- Every value lives in one 8-byte frame slot. Instruction selection loads
-- the operands into scratch registers, computes the result, and stores it
-- back. The @aihc@ calling convention passes the first six arguments in
-- @rdi@, @rsi@, @rdx@, @rcx@, @r8@, and @r9@ and the rest in a 16-byte
-- aligned block above the return address. The callee pops that block with
-- @ret imm16@, so a tail call moves the return address and the outgoing
-- block to the place of the incoming block and the stack does not grow.
-- Results come back in @rax@, @rdx@, @rcx@, @rsi@, @rdi@, @r8@, @r9@, and
-- @r10@. The @c@ convention is the System V convention with at most six
-- integer and eight float arguments and one result.
--
-- Narrow integers are canonical: an @iN@ value is zero-extended to 64 bits in
-- its slot. A float is its IEEE bit pattern.
module Aihc.Amd64.Lir
  ( Amd64LirError (..),
    compileLirObject,
    compileLirStatements,
    lirSymbol,
  )
where

import Aihc.Amd64.Assemble
import Aihc.Lir.Lint (LintError, lintModule)
import Aihc.Lir.RegAlloc (Allocation (..), allocateRegisters)
import Aihc.Lir.Syntax
import Aihc.Native.Object (SectionRole (..))
import Control.Monad (forM, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float)

data Amd64LirError
  = Amd64LirLintErrors ![LintError]
  | Amd64LirUnsupported !Text
  | Amd64LirObjectError !Text
  deriving (Eq, Show)

-- | The object symbol of a Lir symbol. Linux uses the C symbol names as they
-- are.
lirSymbol :: Symbol -> Text
lirSymbol = unSymbol

-- | Lint the module, then assemble it.
compileLirObject :: Module -> Either Amd64LirError BL.ByteString
compileLirObject lirModule = do
  statements <- compileLirStatements lirModule
  either (Left . Amd64LirObjectError . T.pack . show) pure (assembleElf statements)

compileLirStatements :: Module -> Either Amd64LirError [Amd64Statement]
compileLirStatements lirModule@(Module items) =
  case lintModule lirModule of
    [] -> evalStateT compileItems initialState
    errors -> Left (Amd64LirLintErrors errors)
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
      pure (functionStatements <> trapStatements <> dataStatements <> globalStatements <> [amd64Section NoExecuteStackSection])

-- Object state

data ObjectState = ObjectState
  { objectTraps :: !(Map Text Int),
    objectNextLabel :: !Int
  }

type M = StateT ObjectState (Either Amd64LirError)

unsupported :: Text -> M value
unsupported = lift . Left . Amd64LirUnsupported

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
renderTraps :: M [Amd64Statement]
renderTraps = do
  traps <- Map.toAscList . objectTraps <$> get
  let messageLabel index = ".Llir_trap_message_" <> tshow index
      stubs =
        concat
          [ [ amd64Align 4,
              amd64Label (trapStubLabel index),
              amd64Instruction (AmdLea RSI (Amd64RipAddress (messageLabel index))),
              amd64Instruction (AmdMov EDX (Amd64MoveImmediate (toInteger (BS.length bytes)))),
              amd64Instruction (AmdJmp (Amd64JumpLabel ".Llir_trap"))
            ]
          | (message, index) <- traps,
            let bytes = Text.encodeUtf8 (message <> "\n")
          ]
      reporter =
        [ amd64Align 4,
          amd64Label ".Llir_trap",
          amd64Instruction (AmdMov EDI (Amd64MoveImmediate 2)),
          amd64Instruction (AmdAnd (Amd64RmRegister RSP) (Amd64BinaryImmediate (-16))),
          amd64Instruction (AmdCall "write"),
          amd64Instruction (AmdMov EDI (Amd64MoveImmediate 1)),
          amd64Instruction (AmdCall "_exit"),
          amd64Instruction AmdUd2
        ]
      messages =
        concat
          [ [amd64Label (messageLabel index), amd64Bytes (Text.encodeUtf8 (message <> "\n"))]
          | (message, index) <- traps
          ]
  pure
    ( if null traps
        then []
        else [amd64Section TextSection] <> stubs <> reporter <> (amd64Section ReadOnlySection : messages)
    )

-- Data

compileData :: DataItem -> [Amd64Statement]
compileData dataItem =
  [ amd64Section (if dataMutable dataItem then DataSection else ReadOnlySection),
    amd64Align (log2 (dataAlignment dataItem))
  ]
    <> [amd64Global symbol | dataLinkage dataItem == Export]
    <> [amd64Label symbol]
    <> concatMap field (dataFields dataItem)
  where
    symbol = lirSymbol (dataName dataItem)
    field dataField =
      case dataField of
        DataInt ty value -> [amd64Bytes (littleEndian (typeBytes ty) (fromInteger value))]
        DataFloat F32 value -> [amd64Bytes (littleEndian 4 (fromIntegral (castFloatToWord32 (double2Float value))))]
        DataFloat _ value -> [amd64Bytes (littleEndian 8 (castDoubleToWord64 value))]
        DataSymbol target 0 -> [amd64QuadSymbol (lirSymbol target)]
        DataSymbol target addend -> [amd64QuadSymbolAddend (lirSymbol target) (fromInteger addend)]
        DataNull -> [amd64Quad 0]
        DataWord value -> [amd64Bytes (littleEndian 8 (fromInteger value))]
        DataCode Nothing -> [amd64Quad 0]
        DataCode (Just target) -> [amd64QuadSymbol (lirSymbol target)]
        DataBytes bytes -> [amd64Bytes bytes]
        DataZero count -> [amd64Bytes (BS.replicate (fromInteger count) 0)]

-- | A global is one word in the data section of its module.
compileGlobal :: Global -> [Amd64Statement]
compileGlobal global =
  [ amd64Section DataSection,
    amd64Align 3,
    amd64Label (lirSymbol (globalName global)),
    amd64Quad 0
  ]

log2 :: Integer -> Int
log2 value = length (takeWhile (< value) (iterate (* 2) 1))

typeBytes :: Type -> Int
typeBytes ty = max 1 (typeBits ty `div` 8)

littleEndian :: Int -> Word64 -> BS.ByteString
littleEndian count value = BS.pack [fromIntegral (value `shiftR` (8 * index)) | index <- [0 .. count - 1]]

-- Functions

-- | The frame of one function. Offsets are bytes above the stack pointer
-- after the prologue.
data Layout = Layout
  { -- | The values the allocator gave a register.
    layoutRegisters :: !(Map Var Amd64Register),
    -- | The frame slot of every value the allocator spilled.
    layoutSlots :: !(Map Var Int),
    -- | The callee-saved registers the allocator handed out, with the frame
    -- slot the prologue saves each one in.
    layoutSaved :: ![(Amd64Register, Int)],
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

-- | The integer argument registers of both conventions.
argumentRegisters :: [Amd64Register]
argumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

-- | The result registers of the @aihc@ convention.
resultRegisters :: [Amd64Register]
resultRegisters = [RAX, RDX, RCX, RSI, RDI, R8, R9, R10]

-- | The registers that hold block arguments during a jump.
moveRegisters :: [Amd64Register]
moveRegisters = [RAX, R10, R11, RCX, RDX, RSI, RDI, R8, R9]

-- | The registers the allocator hands out.
--
-- They are the callee-saved general registers other than @rbp@, which is the
-- frame pointer. The callee owns them across a call, so a value that lives in
-- one survives every call of the function and the allocator never has to
-- split an interval. They are also disjoint from the argument registers, from
-- the result registers, from 'moveRegisters', and from the @rax@, @r10@, and
-- @r11@ scratch registers of instruction selection, so an allocated value is
-- never in the way of a convention.
allocatableRegisters :: [Amd64Register]
allocatableRegisters = [RBX, R12, R13, R14, R15]

floatArgumentCount :: Int
floatArgumentCount = 8

overflowBytes :: Int -> Int
overflowBytes count = ((max 0 (count - length argumentRegisters) * 8 + 15) `div` 16) * 16

compileFunction :: Map Symbol Signature -> Int -> Function -> M [Amd64Statement]
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
  when (functionConvention function == CConvention) $ do
    let (integers, floats) = classify (map snd (functionParameters function))
    when (length integers > length argumentRegisters) $
      unsupported ("function " <> unSymbol (functionName function) <> " has more than six integer C parameters")
    when (length floats > floatArgumentCount) $
      unsupported ("function " <> unSymbol (functionName function) <> " has more than eight float C parameters")
  prologue <- functionPrologue ctx
  body <- concat <$> mapM (compileBlock ctx) (zip blocks (map Just (drop 1 blocks) <> [Nothing]))
  pure
    ( [amd64Section TextSection, amd64Align 4]
        <> [amd64Global symbol | functionLinkage function == Export]
        <> [amd64Label symbol]
        <> prologue
        <> body
    )
  where
    symbol = lirSymbol (functionName function)

-- | Split the parameters of a C function into the integer class and the
-- float class. Each list pairs the parameter index with its type.
classify :: [Type] -> ([(Int, Type)], [(Int, Type)])
classify types =
  ( [(index, ty) | (index, ty) <- zip [0 ..] types, not (isFloatType ty)],
    [(index, ty) | (index, ty) <- zip [0 ..] types, isFloatType ty]
  )

-- | Place the frame of one function. The allocator decides which values need
-- a slot at all; the rest of the frame holds the saved callee-saved
-- registers, the block-argument temporaries, and the stack allocations, in
-- that order.
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

-- | Save the frame pointer, reserve the frame, zero the stack allocations,
-- and copy the parameters into their slots.
functionPrologue :: Ctx -> M [Amd64Statement]
functionPrologue ctx = do
  parameterStores <-
    case functionConvention function of
      AihcConvention -> pure (concat (zipWith aihcParameter [0 ..] (functionParameters function)))
      CConvention -> do
        let (integers, floats) = classify (map snd (functionParameters function))
            names = map fst (functionParameters function)
        pure
          ( concat [canonicalizeRegister ty register <> [writeValue ctx 0 register (names !! index)] | ((index, ty), register) <- zip integers argumentRegisters]
              <> concat [[amd64Instruction (AmdMovqFromXmm RAX xmm)] <> canonicalizeRegister ty RAX <> [writeValue ctx 0 RAX (names !! index)] | ((index, ty), xmm) <- zip floats [0 ..]]
          )
  pure
    ( [ amd64Instruction (AmdPush RBP),
        amd64Instruction (AmdMov RBP (Amd64MoveRegister RSP))
      ]
        <> adjustStack AmdSub (layoutSize layout)
        <> saveRegisters ctx
        <> concatMap zeroAllocation (Map.elems (layoutAllocs layout))
        <> parameterStores
    )
  where
    function = ctxFunction ctx
    layout = ctxLayout ctx
    aihcParameter index (var, _)
      | index < length argumentRegisters = [writeValue ctx 0 (argumentRegisters !! index) var]
      | otherwise =
          -- The overflow block sits above the saved frame pointer and the
          -- return address.
          [ amd64Instruction (AmdMov RAX (Amd64MoveMemory (Amd64Memory RBP (fromIntegral (16 + 8 * (index - length argumentRegisters)))))),
            writeValue ctx 0 RAX var
          ]
    zeroAllocation (offset, size) =
      [ amd64Instruction (AmdStore (Amd64Memory RSP (fromIntegral (offset + position))) (Amd64StoreImmediate 0))
      | position <- [0, 8 .. size - 1]
      ]

-- | Narrow a value that arrives from C code in the low bits of a register.
canonicalizeRegister :: Type -> Amd64Register -> [Amd64Statement]
canonicalizeRegister ty register =
  case ty of
    I1 -> [amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))]
    I8 -> [amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))]
    I16 -> [amd64Instruction (AmdMovzxWord register (Amd64RmRegister register))]
    I32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    F32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    _ -> []

adjustStack :: (Amd64Rm -> Amd64BinarySource -> Amd64Instruction) -> Int -> [Amd64Statement]
adjustStack operation bytes
  | bytes == 0 = []
  | otherwise = [amd64Instruction (operation (Amd64RmRegister RSP) (Amd64BinaryImmediate (toInteger bytes)))]

-- | Restore the stack of the caller and return, popping the incoming
-- overflow block.
functionEpilogue :: Ctx -> [Amd64Statement]
functionEpilogue ctx =
  [ amd64Instruction (AmdMov RSP (Amd64MoveRegister RBP)),
    amd64Instruction (AmdPop RBP),
    amd64Instruction (if ctxIncomingOverflow ctx == 0 then AmdRet else AmdRetImm (ctxIncomingOverflow ctx))
  ]

slotOffset :: Ctx -> Int -> Var -> Int64
slotOffset ctx displacement var =
  case Map.lookup var (layoutSlots (ctxLayout ctx)) of
    Just offset -> fromIntegral (offset + displacement)
    Nothing -> error ("Aihc.Amd64.Lir: unknown value " <> T.unpack (unVar var))

-- | The register the allocator gave a value, if it gave it one.
valueRegister :: Ctx -> Var -> Maybe Amd64Register
valueRegister ctx var = Map.lookup var (layoutRegisters (ctxLayout ctx))

-- | Read a value into a register. A value the allocator placed is already in
-- a register, so the read is a move; a spilled value comes from its frame
-- slot. The displacement is the number of bytes the stack pointer currently
-- sits below the frame base, and it reaches only the slot form.
readValue :: Ctx -> Int -> Amd64Register -> Var -> Amd64Statement
readValue ctx displacement register var =
  case valueRegister ctx var of
    Just held -> amd64Instruction (AmdMov register (Amd64MoveRegister held))
    Nothing -> amd64Instruction (AmdMov register (Amd64MoveMemory (Amd64Memory RSP (slotOffset ctx displacement var))))

-- | Write a register into a value: the mirror of 'readValue'.
writeValue :: Ctx -> Int -> Amd64Register -> Var -> Amd64Statement
writeValue ctx displacement register var =
  case valueRegister ctx var of
    Just held -> amd64Instruction (AmdMov held (Amd64MoveRegister register))
    Nothing -> amd64Instruction (AmdStore (Amd64Memory RSP (slotOffset ctx displacement var)) (Amd64StoreRegister register))

-- | Save the allocated callee-saved registers into the frame. The prologue
-- runs this before it moves any parameter into a register.
saveRegisters :: Ctx -> [Amd64Statement]
saveRegisters ctx =
  [ amd64Instruction (AmdStore (Amd64Memory RSP (fromIntegral offset)) (Amd64StoreRegister register))
  | (register, offset) <- layoutSaved (ctxLayout ctx)
  ]

-- | Restore what 'saveRegisters' saved. Every exit runs this after it has
-- read the last allocated value and before it moves the stack pointer.
restoreRegisters :: Ctx -> Int -> [Amd64Statement]
restoreRegisters ctx displacement =
  [ amd64Instruction (AmdMov register (Amd64MoveMemory (Amd64Memory RSP (fromIntegral (offset + displacement)))))
  | (register, offset) <- layoutSaved (ctxLayout ctx)
  ]

-- | Load an operand into a register. The displacement is the number of bytes
-- the stack pointer currently sits below the frame base.
loadOperand :: Ctx -> Int -> Amd64Register -> Operand -> [Amd64Statement]
loadOperand ctx displacement register operand =
  case operand of
    OperandVar var -> [readValue ctx displacement register var]
    OperandLiteral literal ->
      case literal of
        LitInt value -> [immediate register value]
        LitFloat value -> [immediate register (toInteger (castDoubleToWord64 value))]
        LitNull -> [immediate register (0 :: Integer)]
        LitSymbol symbol -> [address register (lirSymbol symbol)]

-- | Load a literal with the encoding of its type. Float literals need the
-- width of the type, and integer literals are canonical for the type.
loadTyped :: Ctx -> Int -> Type -> Amd64Register -> Operand -> [Amd64Statement]
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

address :: Amd64Register -> Text -> Amd64Statement
address register label = amd64Instruction (AmdLea register (Amd64RipAddress label))

-- | Load an integer. A value that fits 32 bits uses the short form, which
-- zero-extends through the 32-bit register.
immediate :: (Integral value) => Amd64Register -> value -> Amd64Statement
immediate register value
  | integer >= 0 && integer <= 0xffffffff = amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveImmediate integer))
  | otherwise = amd64Instruction (AmdMov register (Amd64MoveImmediate (integer `mod` (2 ^ (64 :: Int)))))
  where
    integer = toInteger value

tshow :: (Show value) => value -> Text
tshow = T.pack . show

-- Blocks

compileBlock :: Ctx -> (Block, Maybe Block) -> M [Amd64Statement]
compileBlock ctx (block, next) = do
  instructions <- concat <$> mapM (compileInstruction ctx) (blockInstructions block)
  terminator <- compileTerminator ctx (blockLabel <$> next) (blockTerminator block)
  pure (amd64Label (ctxLabels ctx Map.! blockLabel block) : instructions <> terminator)

compileTerminator :: Ctx -> Maybe Label -> Terminator -> M [Amd64Statement]
compileTerminator ctx next terminator =
  case terminator of
    Jump target -> jumpTo target
    Branch condition whenTrue whenFalse -> do
      falseLabel <- freshLabel "else"
      trueLines <- jumpTo whenTrue
      falseLines <- jumpTo whenFalse
      pure
        ( loadOperand ctx 0 RAX condition
            <> [amd64Instruction (AmdTest (Amd64RmRegister RAX) RAX), amd64Instruction (AmdJe falseLabel)]
            <> trueLines
            <> [amd64Label falseLabel]
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
            pure [amd64Instruction (AmdJmp (Amd64JumpLabel stub))]
      let checks =
            concat
              [ [immediate R10 (canonicalInteger ty (switchCaseValue switchCase)), amd64Instruction (AmdCmp (Amd64RmRegister RAX) (Amd64BinaryRegister R10)), amd64Instruction (AmdJe label)]
              | (switchCase, label, _) <- edges
              ]
          bodies = concat [amd64Label label : lines' | (_, label, lines') <- edges]
      pure (loadOperand ctx 0 RAX scrutinee <> checks <> fallbackLines <> bodies)
    Return values -> do
      when (length values > length resultRegisters) $ unsupported "return of more than eight values"
      let loads = concat [loadTyped ctx 0 ty register value | (ty, register, value) <- zip3 (functionResults function) resultRegisters values]
          floatMoves =
            case (functionConvention function, functionResults function) of
              (CConvention, [F64]) -> [amd64Instruction (AmdMovqToXmm 0 RAX)]
              (CConvention, [F32]) -> [amd64Instruction (AmdMovdToXmm 0 EAX)]
              _ -> []
      pure (loads <> floatMoves <> restoreRegisters ctx 0 <> functionEpilogue ctx)
    TailCall symbol arguments -> tailCall (Left (lirSymbol symbol)) arguments
    TailCallIndirect target arguments _ -> tailCall (Right target) arguments
    Trap message -> do
      stub <- trapLabel message
      pure [amd64Instruction (AmdJmp (Amd64JumpLabel stub))]
  where
    function = ctxFunction ctx
    -- Only an unconditional jump falls through to the next block.
    jumpTo target = do
      moves <- blockArgumentMoves ctx target
      let label = ctxLabels ctx Map.! targetLabel target
          fallsThrough = case terminator of
            Jump _ -> Just (targetLabel target) == next
            _ -> False
      pure (moves <> [amd64Instruction (AmdJmp (Amd64JumpLabel label)) | not fallsThrough])
    tailCall callee arguments = do
      let outgoing = overflowBytes (length arguments)
          incoming = ctxIncomingOverflow ctx
          overflow = drop (length argumentRegisters) arguments
          -- The outgoing block is built in a temporary area below the
          -- frame: the return address, then the overflow arguments.
          temporary = if null overflow && incoming == 0 then 0 else 8 + outgoing
          targetLoad = case callee of
            Left _ -> []
            Right operand -> loadOperand ctx temporary R11 operand
          overflowStores =
            concat
              [ loadOperand ctx temporary RAX argument <> [amd64Instruction (AmdStore (Amd64Memory RSP (fromIntegral (8 + 8 * position))) (Amd64StoreRegister RAX))]
              | (position, argument) <- zip [0 :: Int ..] overflow
              ]
          registerLoads = concat [loadOperand ctx temporary register argument | (register, argument) <- zip argumentRegisters arguments]
          -- Move the return address and the outgoing block to the place of
          -- the incoming block. The destination lies above the source, so
          -- the copy runs highest word first. The saved frame pointer and
          -- the return address are read before the copy overwrites them.
          delta = fromIntegral (8 + incoming - outgoing) :: Int64
          relocation
            | temporary == 0 = [amd64Instruction (AmdMov RSP (Amd64MoveRegister RBP)), amd64Instruction (AmdPop RBP)]
            | otherwise =
                [ amd64Instruction (AmdMov R10 (Amd64MoveRegister RBP)),
                  amd64Instruction (AmdMov RAX (Amd64MoveMemory (Amd64Memory R10 8))),
                  amd64Instruction (AmdStore (Amd64Memory RSP 0) (Amd64StoreRegister RAX)),
                  amd64Instruction (AmdMov RBP (Amd64MoveMemory (Amd64Memory R10 0)))
                ]
                  <> concat
                    [ [ amd64Instruction (AmdMov RAX (Amd64MoveMemory (Amd64Memory RSP (fromIntegral (8 * position))))),
                        amd64Instruction (AmdStore (Amd64Memory R10 (delta + fromIntegral (8 * position))) (Amd64StoreRegister RAX))
                      ]
                    | position <- reverse [0 .. length overflow]
                    ]
                  <> [amd64Instruction (AmdLea RSP (Amd64MemoryAddress (Amd64Memory R10 delta)))]
          branch = case callee of
            Left label -> amd64Instruction (AmdJmp (Amd64JumpLabel label))
            Right _ -> amd64Instruction (AmdJmp (Amd64JumpRegister R11))
      nullCheck <-
        case callee of
          Left _ -> pure []
          Right _ -> do
            stub <- trapLabel "indirect call to a non-function"
            pure [amd64Instruction (AmdTest (Amd64RmRegister R11) R11), amd64Instruction (AmdJe stub)]
      pure
        ( adjustStack AmdSub temporary
            <> targetLoad
            <> nullCheck
            <> overflowStores
            <> registerLoads
            -- Every allocated value has been read by now, so the saved
            -- registers go back before the frame does.
            <> restoreRegisters ctx temporary
            <> relocation
            <> [branch]
        )

-- | Move the arguments of a jump into the parameter slots of the target. All
-- arguments are read before any parameter is written.
blockArgumentMoves :: Ctx -> Target -> M [Amd64Statement]
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
            [ loadTyped ctx 0 ty RAX argument <> [amd64Instruction (AmdStore (Amd64Memory RSP (fromIntegral temp)) (Amd64StoreRegister RAX))]
            | ((_, ty), temp, argument) <- zip3 parameters temps arguments
            ]
            <> concat
              [ [amd64Instruction (AmdMov RAX (Amd64MoveMemory (Amd64Memory RSP (fromIntegral temp)))), writeValue ctx 0 RAX var]
              | ((var, _), temp) <- zip parameters temps
              ]
        )

-- Instructions

-- | The operands of a binary operation are loaded into @rax@ and @r10@, and
-- the result is left in @rax@. @r11@, @rcx@, and @rdx@ are scratch.
compileInstruction :: Ctx -> Instruction -> M [Amd64Statement]
compileInstruction ctx (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      body <- binary op ty
      single (loadTyped ctx 0 ty RAX left <> loadTyped ctx 0 ty R10 right <> body)
    Unary op ty value -> single (loadTyped ctx 0 ty RAX value <> bitCount op ty)
    Wide op ty left right -> do
      body <- wide op ty
      pair (loadTyped ctx 0 ty RAX left <> loadTyped ctx 0 ty R10 right <> body)
    Compare op ty left right ->
      single (loadTyped ctx 0 ty RAX left <> loadTyped ctx 0 ty R10 right <> compare' op ty)
    FloatBinary op ty left right ->
      single
        ( loadTyped ctx 0 ty RAX left
            <> loadTyped ctx 0 ty R10 right
            <> [toFloat ty 0 RAX, toFloat ty 1 R10, amd64Instruction (AmdSse (floatOp op) (ty == F64) 0 1), fromFloat ty RAX 0]
        )
    FloatUnary op ty value -> do
      body <- floatUnary op ty
      single (loadTyped ctx 0 ty RAX value <> body)
    Convert op from value to -> do
      body <- convert op from to
      single (loadTyped ctx 0 from RAX value <> body)
    PtrToInt value -> single (loadOperand ctx 0 RAX value)
    PtrFromInt value -> single (loadOperand ctx 0 RAX value)
    Select ty condition left right ->
      single
        ( loadOperand ctx 0 R11 condition
            <> loadTyped ctx 0 ty RAX left
            <> loadTyped ctx 0 ty R10 right
            <> [amd64Instruction (AmdTest (Amd64RmRegister R11) R11), amd64Instruction (AmdCmov AmdEqual RAX (Amd64RmRegister R10))]
        )
    Load ty (Address base offset) _ ->
      single (loadOperand ctx 0 R10 base <> [loadMemory ty RAX (Amd64Memory R10 (fromInteger offset))])
    Store ty value (Address base offset) _ ->
      pure (loadTyped ctx 0 ty RAX value <> loadOperand ctx 0 R10 base <> [storeMemory ty (Amd64Memory R10 (fromInteger offset)) RAX])
    PtrAdd base offset ->
      single (loadOperand ctx 0 RAX base <> loadOperand ctx 0 R10 offset <> [amd64Instruction (AmdAdd (Amd64RmRegister RAX) (Amd64BinaryRegister R10))])
    StackAlloc _ _ ->
      case results of
        [var]
          | Just (offset, _) <- Map.lookup var (layoutAllocs (ctxLayout ctx)) ->
              single [amd64Instruction (AmdLea RAX (Amd64MemoryAddress (Amd64Memory RSP (fromIntegral offset))))]
        _ -> unsupported "stack.alloc without a placed result"
    GlobalGet symbol -> single [address R10 (lirSymbol symbol), amd64Instruction (AmdMov RAX (Amd64MoveMemory (Amd64Memory R10 0)))]
    GlobalSet symbol value ->
      pure (loadOperand ctx 0 RAX value <> [address R10 (lirSymbol symbol), amd64Instruction (AmdStore (Amd64Memory R10 0) (Amd64StoreRegister RAX))])
    Call symbol arguments -> call (Left symbol) arguments
    CallIndirect target arguments signature -> callIndirect target arguments signature
  where
    single body =
      case results of
        [var] -> pure (body <> [writeValue ctx 0 RAX var])
        _ -> unsupported "instruction result count"
    pair body =
      case results of
        [first, second] -> pure (body <> [writeValue ctx 0 RAX first, writeValue ctx 0 R10 second])
        _ -> unsupported "instruction result count"

    -- A narrow value is zero-extended in its slot, so a leading-zero count
    -- includes the bits above the type and a trailing-zero count of zero
    -- would reach the top of the register. Setting the first bit above the
    -- type keeps the trailing count at the width of the type.
    bitCount op ty =
      let bits = typeBits ty
       in case op of
            Popcount -> [amd64Instruction (AmdBitCount AmdPopcnt RAX (Amd64RmRegister RAX))]
            Clz ->
              [amd64Instruction (AmdBitCount AmdLzcnt RAX (Amd64RmRegister RAX))]
                <> [amd64Instruction (AmdSub (Amd64RmRegister RAX) (Amd64BinaryImmediate (toInteger (64 - bits)))) | bits < 64]
            Ctz ->
              concat
                [ [ amd64Instruction (AmdMov R10 (Amd64MoveImmediate (2 ^ bits))),
                    amd64Instruction (AmdOr (Amd64RmRegister RAX) (Amd64BinaryRegister R10))
                  ]
                | bits < 64
                ]
                <> [amd64Instruction (AmdBitCount AmdTzcnt RAX (Amd64RmRegister RAX))]

    binary op ty =
      case op of
        Add -> pure (narrow ty [amd64Instruction (AmdAdd (Amd64RmRegister RAX) (Amd64BinaryRegister R10))])
        Sub -> pure (narrow ty [amd64Instruction (AmdSub (Amd64RmRegister RAX) (Amd64BinaryRegister R10))])
        Mul -> pure (narrow ty [amd64Instruction (AmdImul RAX (Amd64RmRegister R10))])
        DivS -> do
          checks <- signedDivisionChecks ty
          pure (checks <> narrow ty [amd64Instruction AmdCqo, amd64Instruction (AmdIdiv (Amd64RmRegister R10))])
        DivU -> do
          zero <- trapLabel "integer division by zero"
          pure [testZero R10, amd64Instruction (AmdJe zero), clearRdx, amd64Instruction (AmdDiv (Amd64RmRegister R10))]
        RemS -> do
          checks <- signedDivisionChecks ty
          pure (checks <> [amd64Instruction AmdCqo, amd64Instruction (AmdIdiv (Amd64RmRegister R10))] <> narrow ty [amd64Instruction (AmdMov RAX (Amd64MoveRegister RDX))])
        RemU -> do
          zero <- trapLabel "integer division by zero"
          pure [testZero R10, amd64Instruction (AmdJe zero), clearRdx, amd64Instruction (AmdDiv (Amd64RmRegister R10)), amd64Instruction (AmdMov RAX (Amd64MoveRegister RDX))]
        And -> pure [amd64Instruction (AmdAnd (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
        Or -> pure [amd64Instruction (AmdOr (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
        Xor -> pure [amd64Instruction (AmdXor (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
        Shl -> pure (shiftCount ty <> narrow ty [amd64Instruction (AmdShl (Amd64RmRegister RAX))])
        ShrS -> pure (signExtend ty RAX <> shiftCount ty <> narrow ty [amd64Instruction (AmdSar (Amd64RmRegister RAX))])
        ShrU -> pure (shiftCount ty <> [amd64Instruction (AmdShr (Amd64RmRegister RAX))])

    -- The divisor must not be zero, and the minimum value divided by minus
    -- one does not fit. The operands are sign-extended to 64 bits, so the
    -- machine division itself cannot fault.
    signedDivisionChecks ty = do
      zero <- trapLabel "integer division by zero"
      overflow <- trapLabel "integer overflow"
      skip <- freshLabel "div"
      pure
        ( signExtend ty RAX
            <> signExtend ty R10
            <> [ testZero R10,
                 amd64Instruction (AmdJe zero),
                 amd64Instruction (AmdCmp (Amd64RmRegister R10) (Amd64BinaryImmediate (-1))),
                 amd64Instruction (AmdJne skip),
                 immediate R11 (minimumSigned ty),
                 amd64Instruction (AmdCmp (Amd64RmRegister RAX) (Amd64BinaryRegister R11)),
                 amd64Instruction (AmdJe overflow),
                 amd64Label skip
               ]
        )

    wide op ty =
      case op of
        MulWideU
          | typeBits ty == 64 -> pure [amd64Instruction (AmdMul (Amd64RmRegister R10)), amd64Instruction (AmdMov R10 (Amd64MoveRegister RDX))]
          | otherwise ->
              pure
                ( [amd64Instruction (AmdImul RAX (Amd64RmRegister R10)), amd64Instruction (AmdMov R10 (Amd64MoveRegister RAX)), immediate RCX (typeBits ty), amd64Instruction (AmdShr (Amd64RmRegister R10))]
                    <> narrowRegister ty RAX
                    <> narrowRegister ty R10
                )
        MulWideS
          | typeBits ty == 64 -> pure [amd64Instruction (AmdImulWide (Amd64RmRegister R10)), amd64Instruction (AmdMov R10 (Amd64MoveRegister RDX))]
          | otherwise ->
              pure
                ( signExtend ty RAX
                    <> signExtend ty R10
                    <> [amd64Instruction (AmdImul RAX (Amd64RmRegister R10)), amd64Instruction (AmdMov R10 (Amd64MoveRegister RAX)), immediate RCX (typeBits ty), amd64Instruction (AmdSar (Amd64RmRegister R10))]
                    <> narrowRegister ty RAX
                    <> narrowRegister ty R10
                )
        AddCarry
          | typeBits ty == 64 -> pure (amd64Instruction (AmdAdd (Amd64RmRegister RAX) (Amd64BinaryRegister R10)) : setFlag AmdCarry R10)
          | otherwise ->
              pure
                ( [amd64Instruction (AmdAdd (Amd64RmRegister RAX) (Amd64BinaryRegister R10)), amd64Instruction (AmdMov R10 (Amd64MoveRegister RAX)), immediate RCX (typeBits ty), amd64Instruction (AmdShr (Amd64RmRegister R10))]
                    <> narrowRegister ty RAX
                )
        SubBorrow
          | typeBits ty == 64 -> pure (amd64Instruction (AmdSub (Amd64RmRegister RAX) (Amd64BinaryRegister R10)) : setFlag AmdCarry R10)
          | otherwise ->
              pure
                ( [amd64Instruction (AmdCmp (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
                    <> setFlag AmdBelow R11
                    <> [amd64Instruction (AmdSub (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
                    <> narrowRegister ty RAX
                    <> [amd64Instruction (AmdMov R10 (Amd64MoveRegister R11))]
                )

    compare' op ty
      | isFloatType ty = floatCompare op ty
      | op `elem` [LtS, LeS, GtS, GeS] =
          signExtend ty RAX <> signExtend ty R10 <> [amd64Instruction (AmdCmp (Amd64RmRegister RAX) (Amd64BinaryRegister R10))] <> setFlag (integerCondition op) RAX
      | otherwise = amd64Instruction (AmdCmp (Amd64RmRegister RAX) (Amd64BinaryRegister R10)) : setFlag (integerCondition op) RAX

    -- @ucomis@ sets the carry flag for less than and every flag for an
    -- unordered comparison, so the ordered conditions are the unsigned
    -- above conditions with the operands in the right order.
    floatCompare op ty =
      case op of
        Eq -> [toFloat ty 0 RAX, toFloat ty 1 R10, ucomis] <> setFlag AmdEqual RAX <> setFlag AmdNotParity R10 <> [amd64Instruction (AmdAnd (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
        Ne -> [toFloat ty 0 RAX, toFloat ty 1 R10, ucomis] <> setFlag AmdNotEqual RAX <> setFlag AmdParity R10 <> [amd64Instruction (AmdOr (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
        FLt -> [toFloat ty 0 R10, toFloat ty 1 RAX, ucomis] <> setFlag AmdAbove RAX
        FLe -> [toFloat ty 0 R10, toFloat ty 1 RAX, ucomis] <> setFlag AmdAboveOrEqual RAX
        FGt -> [toFloat ty 0 RAX, toFloat ty 1 R10, ucomis] <> setFlag AmdAbove RAX
        FGe -> [toFloat ty 0 RAX, toFloat ty 1 R10, ucomis] <> setFlag AmdAboveOrEqual RAX
        _ -> [immediate RAX (0 :: Integer)]
      where
        ucomis = amd64Instruction (AmdUcomis (ty == F64) 0 1)

    floatUnary op ty =
      case op of
        FNeg -> pure [immediate R10 (signBit ty), amd64Instruction (AmdXor (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
        FAbs -> pure [immediate R10 (signBit ty - 1), amd64Instruction (AmdAnd (Amd64RmRegister RAX) (Amd64BinaryRegister R10))]
        FSqrt -> pure [toFloat ty 0 RAX, amd64Instruction (AmdSse SseSqrt (ty == F64) 0 0), fromFloat ty RAX 0]

    convert op from to =
      case op of
        SExt -> pure (signExtend from RAX <> narrowRegister to RAX)
        ZExt -> pure []
        Trunc -> pure (narrowRegister to RAX)
        IToFS -> pure (signExtend from RAX <> [amd64Instruction (AmdCvtsi2s (to == F64) 0 RAX), fromFloat to RAX 0])
        IToFU
          | typeBits from == 64 -> unsignedToFloat to
          | otherwise -> pure [amd64Instruction (AmdCvtsi2s (to == F64) 0 RAX), fromFloat to RAX 0]
        FToIS -> floatToInteger True from to
        FToIU -> floatToInteger False from to
        FpExt -> pure [amd64Instruction (AmdMovdToXmm 0 EAX), amd64Instruction (AmdSse SseConvertWidth False 0 0), amd64Instruction (AmdMovqFromXmm RAX 0)]
        FpTrunc -> pure [amd64Instruction (AmdMovqToXmm 0 RAX), amd64Instruction (AmdSse SseConvertWidth True 0 0), amd64Instruction (AmdMovdFromXmm EAX 0)]
        Bitcast -> pure []

    -- A 64-bit value with the top bit set is halved with its low bit kept
    -- as a sticky bit, converted, and doubled, which rounds correctly.
    unsignedToFloat to = do
      large <- freshLabel "utof_large"
      done <- freshLabel "utof_done"
      pure
        [ amd64Instruction (AmdTest (Amd64RmRegister RAX) RAX),
          amd64Instruction (AmdJcc AmdSign large),
          amd64Instruction (AmdCvtsi2s (to == F64) 0 RAX),
          amd64Instruction (AmdJmp (Amd64JumpLabel done)),
          amd64Label large,
          amd64Instruction (AmdMov R10 (Amd64MoveRegister RAX)),
          immediate RCX (1 :: Integer),
          amd64Instruction (AmdShr (Amd64RmRegister R10)),
          amd64Instruction (AmdAnd (Amd64RmRegister RAX) (Amd64BinaryImmediate 1)),
          amd64Instruction (AmdOr (Amd64RmRegister R10) (Amd64BinaryRegister RAX)),
          amd64Instruction (AmdCvtsi2s (to == F64) 0 R10),
          amd64Instruction (AmdSse SseAdd (to == F64) 0 0),
          amd64Label done,
          fromFloat to RAX 0
        ]

    -- Widen the source to double, reject NaN and values outside the target
    -- range, then convert with rounding toward zero. An unsigned 64-bit
    -- result above the signed range is converted from the value minus
    -- two to the sixty-third.
    floatToInteger signed from to = do
      invalid <- trapLabel "invalid float to integer conversion"
      let widen = if from == F64 then [amd64Instruction (AmdMovqToXmm 0 RAX)] else [amd64Instruction (AmdMovdToXmm 0 EAX), amd64Instruction (AmdSse SseConvertWidth False 0 0)]
          bits = typeBits to
          lower = if signed then negate (2 ^^ (bits - 1)) else -1 :: Double
          upper = if signed then 2 ^^ (bits - 1) else 2 ^^ bits :: Double
          bound value = [immediate R10 (toInteger (castDoubleToWord64 value)), amd64Instruction (AmdMovqToXmm 1 R10), amd64Instruction (AmdUcomis True 0 1)]
          -- Below the lower bound: for the signed range the bound itself is
          -- valid, so the check is strict.
          lowerCheck = bound lower <> [amd64Instruction (AmdJcc (if signed then AmdBelow else AmdBelowOrEqual) invalid)]
          upperCheck = bound upper <> [amd64Instruction (AmdJcc AmdAboveOrEqual invalid)]
      body <-
        if signed || bits < 64
          then pure [amd64Instruction (AmdCvtts2si True RAX 0)]
          else do
            large <- freshLabel "ftou_large"
            done <- freshLabel "ftou_done"
            pure
              [ immediate R10 (toInteger (castDoubleToWord64 (2 ^^ (63 :: Int)))),
                amd64Instruction (AmdMovqToXmm 1 R10),
                amd64Instruction (AmdUcomis True 0 1),
                amd64Instruction (AmdJcc AmdAboveOrEqual large),
                amd64Instruction (AmdCvtts2si True RAX 0),
                amd64Instruction (AmdJmp (Amd64JumpLabel done)),
                amd64Label large,
                amd64Instruction (AmdSse SseSub True 0 1),
                amd64Instruction (AmdCvtts2si True RAX 0),
                immediate R10 (signBit F64),
                amd64Instruction (AmdXor (Amd64RmRegister RAX) (Amd64BinaryRegister R10)),
                amd64Label done
              ]
      pure
        ( widen
            <> [amd64Instruction (AmdUcomis True 0 0), amd64Instruction (AmdJcc AmdParity invalid)]
            <> lowerCheck
            <> upperCheck
            <> body
            <> narrowRegister to RAX
        )

    shiftCount ty =
      [amd64Instruction (AmdMov RCX (Amd64MoveRegister R10))]
        <> [amd64Instruction (AmdAnd (Amd64RmRegister RCX) (Amd64BinaryImmediate (toInteger (typeBits ty - 1)))) | typeBits ty /= 64]
    narrow ty body = body <> narrowRegister ty RAX

    call callee arguments = do
      let (convention, resultTypes, parameterTypes) = calleeSignature callee
      let outgoing = case convention of
            CConvention -> 0
            AihcConvention -> overflowBytes (length arguments)
      argumentLoads <-
        case convention of
          AihcConvention ->
            pure
              ( concat
                  [ loadTyped ctx outgoing ty RAX argument <> [amd64Instruction (AmdStore (Amd64Memory RSP (fromIntegral (8 * position))) (Amd64StoreRegister RAX))]
                  | (position, (ty, argument)) <- zip [0 :: Int ..] (drop (length argumentRegisters) (zip parameterTypes arguments))
                  ]
                  <> concat [loadTyped ctx outgoing ty register argument | (register, (ty, argument)) <- zip argumentRegisters (zip parameterTypes arguments)]
              )
          CConvention -> do
            let (integers, floats) = classify parameterTypes
            when (length integers > length argumentRegisters) $ unsupported "C call with more than six integer arguments"
            when (length floats > floatArgumentCount) $ unsupported "C call with more than eight float arguments"
            pure
              ( concat [loadTyped ctx 0 ty RAX (arguments !! index) <> [amd64Instruction (AmdMovqToXmm xmm RAX)] | ((index, ty), xmm) <- zip floats [0 ..]]
                  <> concat [loadTyped ctx 0 ty register (arguments !! index) | ((index, ty), register) <- zip integers argumentRegisters]
                  -- A variadic callee reads the number of vector registers
                  -- from @al@.
                  <> [immediate RAX (length floats)]
              )
      let branch = case callee of
            Left symbol -> [amd64Instruction (AmdCall (lirSymbol symbol))]
            Right _ -> [amd64Instruction (AmdCallRegister R11)]
          resultStores =
            case convention of
              AihcConvention -> [writeValue ctx 0 register var | (var, _, register) <- zip3 results resultTypes resultRegisters]
              CConvention ->
                concat
                  [ floatResult ty <> canonicalizeRegister ty RAX <> [writeValue ctx 0 RAX var]
                  | (var, ty) <- zip results resultTypes
                  ]
      pure (adjustStack AmdSub outgoing <> argumentLoads <> branch <> resultStores)
    callIndirect target arguments signature = do
      stub <- trapLabel "indirect call to a non-function"
      body <- call (Right signature) arguments
      pure (loadOperand ctx 0 R11 target <> [amd64Instruction (AmdTest (Amd64RmRegister R11) R11), amd64Instruction (AmdJe stub)] <> body)
    calleeSignature callee =
      case callee of
        Left symbol ->
          case Map.lookup symbol (ctxSignatures ctx) of
            Just signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)
            Nothing -> (AihcConvention, [], [])
        Right signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)
    floatResult ty =
      case ty of
        F64 -> [amd64Instruction (AmdMovqFromXmm RAX 0)]
        F32 -> [amd64Instruction (AmdMovdFromXmm EAX 0)]
        _ -> []

testZero :: Amd64Register -> Amd64Statement
testZero register = amd64Instruction (AmdTest (Amd64RmRegister register) register)

clearRdx :: Amd64Statement
clearRdx = amd64Instruction (AmdXor (Amd64RmRegister EDX) (Amd64BinaryRegister EDX))

-- | Materialize a condition as @0@ or @1@ in a register. @setcc@ writes one
-- byte, so the register is zero-extended afterwards.
setFlag :: Amd64Condition -> Amd64Register -> [Amd64Statement]
setFlag condition register =
  [ amd64Instruction (AmdSet condition (Amd64RmRegister (byteRegister register))),
    amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))
  ]

minimumSigned :: Type -> Integer
minimumSigned ty = negate (2 ^ (typeBits ty - 1))

signBit :: Type -> Integer
signBit ty = 2 ^ (typeBits ty - 1)

-- | Mask a register to the width of a narrow type.
narrowRegister :: Type -> Amd64Register -> [Amd64Statement]
narrowRegister ty register =
  case ty of
    I1 -> [amd64Instruction (AmdAnd (Amd64RmRegister register) (Amd64BinaryImmediate 1))]
    I8 -> [amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))]
    I16 -> [amd64Instruction (AmdMovzxWord register (Amd64RmRegister register))]
    I32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    F32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    _ -> []

-- | Sign-extend a canonical narrow value to 64 bits.
signExtend :: Type -> Amd64Register -> [Amd64Statement]
signExtend ty register =
  case ty of
    I1 -> [amd64Instruction (AmdNeg (Amd64RmRegister register))]
    I8 -> [amd64Instruction (AmdMovsxByte register (Amd64RmRegister (byteRegister register)))]
    I16 -> [amd64Instruction (AmdMovsxWord register (Amd64RmRegister register))]
    I32 -> [amd64Instruction (AmdMovsxd register (Amd64RmRegister (dwordRegister register)))]
    _ -> []

toFloat :: Type -> Int -> Amd64Register -> Amd64Statement
toFloat ty xmm general
  | ty == F64 = amd64Instruction (AmdMovqToXmm xmm general)
  | otherwise = amd64Instruction (AmdMovdToXmm xmm (dwordRegister general))

fromFloat :: Type -> Amd64Register -> Int -> Amd64Statement
fromFloat ty general xmm
  | ty == F64 = amd64Instruction (AmdMovqFromXmm general xmm)
  | otherwise = amd64Instruction (AmdMovdFromXmm (dwordRegister general) xmm)

byteRegister :: Amd64Register -> Amd64Register
byteRegister register =
  case register of
    RAX -> AL
    RCX -> CL
    RDX -> DL
    RBX -> BL
    RSP -> SPL
    RBP -> BPL
    RSI -> SIL
    RDI -> DIL
    R8 -> R8B
    R9 -> R9B
    R10 -> R10B
    R11 -> R11B
    R12 -> R12B
    R13 -> R13B
    R14 -> R14B
    R15 -> R15B
    other -> other

dwordRegister :: Amd64Register -> Amd64Register
dwordRegister register =
  case register of
    RAX -> EAX
    RCX -> ECX
    RDX -> EDX
    RBX -> EBX
    RSP -> ESP
    RBP -> EBP
    RSI -> ESI
    RDI -> EDI
    R8 -> R8D
    R9 -> R9D
    R10 -> R10D
    R11 -> R11D
    R12 -> R12D
    R13 -> R13D
    R14 -> R14D
    R15 -> R15D
    other -> other

floatOp :: FloatBinaryOp -> Amd64SseOp
floatOp op =
  case op of
    FAdd -> SseAdd
    FSub -> SseSub
    FMul -> SseMul
    FDiv -> SseDiv

integerCondition :: CompareOp -> Amd64Condition
integerCondition op =
  case op of
    Eq -> AmdEqual
    Ne -> AmdNotEqual
    LtS -> AmdLess
    LtU -> AmdBelow
    LeS -> AmdLessOrEqual
    LeU -> AmdBelowOrEqual
    GtS -> AmdGreater
    GtU -> AmdAbove
    GeS -> AmdGreaterOrEqual
    GeU -> AmdAboveOrEqual
    FLt -> AmdBelow
    FLe -> AmdBelowOrEqual
    FGt -> AmdAbove
    FGe -> AmdAboveOrEqual

-- | A load zero-extends a narrow value into the whole register.
loadMemory :: Type -> Amd64Register -> Amd64Memory -> Amd64Statement
loadMemory ty value memory =
  case typeBytes ty of
    1 -> amd64Instruction (AmdMovzx value (Amd64RmMemory memory))
    2 -> amd64Instruction (AmdMovzxWord value (Amd64RmMemory memory))
    4 -> amd64Instruction (AmdMov (dwordRegister value) (Amd64MoveMemory memory))
    _ -> amd64Instruction (AmdMov value (Amd64MoveMemory memory))

storeMemory :: Type -> Amd64Memory -> Amd64Register -> Amd64Statement
storeMemory ty memory value =
  case typeBytes ty of
    1 -> amd64Instruction (AmdStoreByte memory (byteRegister value))
    2 -> amd64Instruction (AmdStoreWord memory value)
    4 -> amd64Instruction (AmdStore memory (Amd64StoreRegister (dwordRegister value)))
    _ -> amd64Instruction (AmdStore memory (Amd64StoreRegister value))
