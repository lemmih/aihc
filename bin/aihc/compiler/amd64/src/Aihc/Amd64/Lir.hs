{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to AMD64 ELF objects for Linux.
--
-- Every value lives where the register allocator put it: in a register, or
-- in a frame slot when the registers ran out or the value lives across a
-- call that clobbers them all. Instruction selection reads a register
-- operand in place, loads a slot or a literal into a scratch register, and
-- writes the result straight into its home. A convention boundary is a
-- parallel move: the arguments of a call, the values of a return, and the
-- arguments of a jump are moved to their destinations at once, so a value
-- the allocator already placed where the convention wants it costs nothing.
--
-- The @aihc@ calling convention passes the first six arguments in @rdi@,
-- @rsi@, @rdx@, @rcx@, @r8@, and @r9@ and the rest in a 16-byte aligned
-- block above the return address. The callee pops that block with
-- @ret imm16@, so a tail call moves the return address and the outgoing
-- block to the place of the incoming block and the stack does not grow.
-- Results come back in @rax@, @rdx@, @rcx@, @rsi@, @rdi@, @r8@, @r9@, and
-- @r10@. An aihc function preserves no register: every call clobbers them
-- all, so an aihc function that makes no call and spills nothing needs no
-- frame at all. The @c@ convention is the System V convention with at most
-- six integer and eight float arguments and one result. A C function
-- preserves @rbx@ and @r12@ to @r15@ and saves the ones it touches, and it
-- saves all of them when it calls into aihc code.
--
-- Narrow integers are canonical: an @iN@ value is zero-extended to 64 bits
-- wherever it lives. A float is its IEEE bit pattern.
module Aihc.Amd64.Lir
  ( Amd64LirError (..),
    compileLirObject,
    compileLirStatements,
    elideSlotReloads,
    lirSymbol,
  )
where

import Aihc.Amd64.Assemble
import Aihc.Lir.Convert (integerConversionBounds)
import Aihc.Lir.Lint (LintError, lintModule)
import Aihc.Lir.RegAlloc (Allocation (..), Registers (..), allocateRegistersFor, readCounts)
import Aihc.Lir.Resolve (resolveConstants, resolvedSwitchCaseValue, unresolvedConstant)
import Aihc.Lir.Syntax
import Aihc.Native.Move (orderMoves)
import Control.Monad (forM, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
compileLirStatements lirModule =
  case lintModule lirModule of
    [] -> evalStateT compileItems initialState
    errors -> Left (Amd64LirLintErrors errors)
  where
    Module items = resolveConstants lirModule
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
        DataIntConstant _ constant -> unresolvedConstant constant
        DataInt ty value -> [amd64Bytes (littleEndian (typeBytes ty) (fromInteger value))]
        DataFloat F32 value -> [amd64Bytes (littleEndian 4 (fromIntegral (castFloatToWord32 (double2Float value))))]
        DataFloat _ value -> [amd64Bytes (littleEndian 8 (castDoubleToWord64 value))]
        DataSymbol target 0 -> [amd64QuadSymbol (lirSymbol target)]
        DataSymbol target addend -> [amd64QuadSymbolAddend (lirSymbol target) (fromInteger addend)]
        DataNull -> [amd64Quad 0]
        DataWordConstant constant -> unresolvedConstant constant
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
    -- | The preserved registers the function saves, with the frame slot the
    -- prologue saves each one in.
    layoutSaved :: ![(Amd64Register, Int)],
    layoutAllocs :: !(Map Var (Int, Int)),
    -- | The bytes below the saved frame pointer.
    layoutSize :: !Int,
    -- | Whether the function saves the frame pointer and reserves the frame
    -- at all. A function without a frame leaves the stack pointer on the
    -- return address.
    layoutFramed :: !Bool
  }

-- | The bytes between the stack pointer after the prologue and the return
-- address.
frameBytes :: Layout -> Int
frameBytes layout
  | layoutFramed layout = layoutSize layout + 8
  | otherwise = 0

data Ctx = Ctx
  { ctxFunction :: !Function,
    ctxLayout :: !Layout,
    ctxLabels :: !(Map Label Text),
    ctxBlockParameters :: !(Map Label [(Var, Type)]),
    -- | The signatures of the functions the module defines or declares.
    ctxSignatures :: !(Map Symbol Signature),
    -- | The size of the incoming overflow block that the epilogue pops.
    ctxIncomingOverflow :: !Int,
    -- | How many times the function reads each value.
    ctxReads :: !(Map Var Int)
  }

-- | The integer argument registers of both conventions.
argumentRegisters :: [Amd64Register]
argumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

-- | The result registers of the @aihc@ convention.
resultRegisters :: [Amd64Register]
resultRegisters = [RAX, RDX, RCX, RSI, RDI, R8, R9, R10]

-- | The registers a C call preserves. An aihc call clobbers them too.
preservedRegisters :: [Amd64Register]
preservedRegisters = [RBX, R12, R13, R14, R15]

-- | The scratch registers of instruction selection. A left operand that is
-- not in a register lands in the first and a right operand in the second; a
-- result without a register is computed in the first, and the second one of
-- a pair in the second. The second also carries the target of an indirect
-- call and a slot-to-slot move, and the parallel moves park a value in the
-- first to break a cycle. Beyond these, a function that divides or
-- multiplies wide owns @rax@ and @rdx@ as well, and a function that shifts by
-- a variable count owns @rcx@; every other function hands those to the
-- allocator.
scratchLeft, scratchRight :: Amd64Register
scratchLeft = R11
scratchRight = R10

-- | The registers instruction selection needs beyond the two scratch
-- registers for the instructions of one function.
data Scratch = Scratch
  { -- | Division and wide multiplication go through @rdx:rax@.
    scratchDivides :: !Bool,
    -- | A shift by a variable count reads @cl@.
    scratchShifts :: !Bool
  }

functionScratch :: Function -> Scratch
functionScratch function =
  Scratch
    { scratchDivides = any divides operations,
      scratchShifts = any shifts operations
    }
  where
    operations = [operation | block <- functionBlocks function, Instruction _ operation <- blockInstructions block]
    divides operation =
      case operation of
        Binary op _ _ _ -> op `elem` [DivS, DivU, RemS, RemU]
        Wide op ty _ _ -> op `elem` [MulWideU, MulWideS] && typeBits ty == 64
        _ -> False
    shifts operation =
      case operation of
        Binary op _ _ right -> op `elem` [Shl, ShrS, ShrU] && not (isLiteral right)
        _ -> False
    isLiteral operand = case operand of
      OperandLiteral _ -> True
      OperandVar _ -> False

-- | The volatile registers the allocator may hand out in one function, in
-- the order it prefers them. The registers most conventions want come last,
-- so that a value without a hint leaves them for the values with one.
volatileRegisters :: Scratch -> [Amd64Register]
volatileRegisters scratch =
  [R9, R8]
    <> [RCX | not (scratchShifts scratch)]
    <> [RDX | not (scratchDivides scratch)]
    <> [RAX | not (scratchDivides scratch)]
    <> [RSI, RDI]

-- | What the allocator may hand out under one convention. A preserved
-- register costs a C function a save and a restore; an aihc function
-- preserves nothing and pays nothing.
registersFor :: CallingConvention -> Scratch -> Registers Amd64Register
registersFor convention scratch =
  Registers
    { registersVolatile = volatileRegisters scratch,
      registersPreserved = preservedRegisters,
      registersPreservedCost = convention == CConvention,
      registersArgument = carrier argumentRegisters,
      registersResult = carrier resultRegisters
    }
  where
    carrier registers index
      | index < length registers = Just (registers !! index)
      | otherwise = Nothing

floatArgumentCount :: Int
floatArgumentCount = 8

overflowBytes :: Int -> Int
overflowBytes count = ((max 0 (count - length argumentRegisters) * 8 + 15) `div` 16) * 16

compileFunction :: Map Symbol Signature -> Int -> Function -> M [Amd64Statement]
compileFunction signatures index function = do
  layout <- functionLayout signatures function
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
              CConvention -> 0,
            ctxReads = readCounts function
          }
  when (functionConvention function == CConvention) $ do
    let (integers, floats) = classify (map snd (functionParameters function))
    when (length integers > length argumentRegisters) $
      unsupported ("function " <> unSymbol (functionName function) <> " has more than six integer C parameters")
    when (length floats > floatArgumentCount) $
      unsupported ("function " <> unSymbol (functionName function) <> " has more than eight float C parameters")
  prologue <- functionPrologue ctx
  body <- concat <$> mapM (compileBlock ctx) (zip3 (True : repeat False) blocks (map Just (drop 1 blocks) <> [Nothing]))
  pure
    ( [amd64Section TextSection, amd64Align 4]
        <> [amd64Global symbol | functionLinkage function == Export]
        <> [amd64Label symbol]
        <> elideSlotReloads (prologue <> body)
    )
  where
    symbol = lirSymbol (functionName function)

-- | Drop a read that the destination register already holds, and a store
-- of what the slot already holds. Instruction selection loads slots and
-- literals into scratch registers, so the same value reaches the same
-- scratch register several times in a row while nothing has touched it in
-- between, and a tail call that keeps its overflow block in place stores
-- back words it loaded from the same slots.
--
-- The pass tracks, for each general register, where its contents last came
-- from: a stack pointer offset, or another register. A read whose destination
-- already holds that source is dropped, and so is a store whose register
-- came from the slot. A write to a register invalidates both what that
-- register held and every register that copied it. Anything that moves the
-- stack pointer, calls, or reaches a label forgets everything. Tracking a
-- slot is sound because the only frame addresses that escape into a register
-- come from @stack.alloc@, which 'functionLayout' places above every value
-- slot, so a store through a register base never writes a tracked slot.
elideSlotReloads :: [Amd64Statement] -> [Amd64Statement]
elideSlotReloads = go IntMap.empty
  where
    go :: IntMap Source -> [Amd64Statement] -> [Amd64Statement]
    go held statements =
      case statements of
        [] -> []
        statement : rest ->
          case statement of
            Amd64Code instruction ->
              case instructionEffect instruction of
                LoadsSlot register offset -> reads' statement rest held register (FromSlot offset)
                -- A copy back the way one already went is redundant: the two
                -- registers still hold the same value.
                MovesRegister destination source
                  | IntMap.lookup source held == Just (FromRegister destination) -> go held rest
                  | otherwise -> reads' statement rest held destination (FromRegister source)
                -- The stored register holds the slot, and any register that
                -- held the previous contents of the slot is stale. A store
                -- of what the slot already holds changes nothing.
                StoresSlot register offset
                  | IntMap.lookup register held == Just (FromSlot offset) -> go held rest
                  | otherwise ->
                      statement : go (IntMap.insert register (FromSlot offset) (IntMap.filter (/= FromSlot offset) held)) rest
                WritesSlot offset -> statement : go (IntMap.filter (/= FromSlot offset) held) rest
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
  | -- | Writes something else to a literal stack pointer offset.
    WritesSlot !Int64
  | -- | Copies one general register into another.
    MovesRegister !Int !Int
  | -- | Everything the pass knows becomes stale.
    Forgets

instructionEffect :: Amd64Instruction -> SlotEffect
instructionEffect instruction =
  case instruction of
    AmdRet -> Forgets
    AmdRetImm _ -> Forgets
    AmdUd2 -> Forgets
    AmdPush _ -> Forgets
    AmdPop _ -> Forgets
    AmdCall _ -> Forgets
    AmdCallRegister _ -> Forgets
    AmdJmp _ -> Forgets
    -- A conditional branch changes nothing, and its target begins with a
    -- label, which forgets every register in its own right.
    AmdJe _ -> Writes []
    AmdJne _ -> Writes []
    AmdJcc _ _ -> Writes []
    AmdMov destination source ->
      case source of
        -- A copy between 64-bit names is a move the pass can follow. A
        -- narrow name clears the top half of its register, so it is only a
        -- write.
        Amd64MoveRegister register
          | quadWord destination && quadWord register -> MovesRegister (registerKey destination) (registerKey register)
        Amd64MoveMemory (Amd64Memory RSP offset)
          | quadWord destination -> LoadsSlot (registerKey destination) offset
        _ -> writes [destination]
    AmdStore (Amd64Memory base offset) source ->
      case source of
        Amd64StoreRegister register
          | base == RSP && quadWord register -> StoresSlot (registerKey register) offset
        _
          | base == RSP -> WritesSlot offset
          | otherwise -> Writes []
    AmdStoreByte (Amd64Memory base _) _ -> narrowAccess base
    AmdStoreWord (Amd64Memory base _) _ -> narrowAccess base
    AmdMovsxd destination _ -> writes [destination]
    AmdMovsxByte destination _ -> writes [destination]
    AmdMovsxWord destination _ -> writes [destination]
    AmdMovzx destination _ -> writes [destination]
    AmdMovzxWord destination _ -> writes [destination]
    AmdLea destination _ -> writes [destination]
    AmdAdd destination _ -> modifies destination
    AmdSub destination _ -> modifies destination
    AmdAnd destination _ -> modifies destination
    AmdOr destination _ -> modifies destination
    AmdXor destination _ -> modifies destination
    AmdImul destination _ -> writes [destination]
    AmdCmp _ _ -> Writes []
    AmdTest _ _ -> Writes []
    AmdShl destination -> modifies destination
    AmdShr destination -> modifies destination
    AmdShlImmediate destination _ -> modifies destination
    AmdShrImmediate destination _ -> modifies destination
    AmdSarImmediate destination _ -> modifies destination
    AmdNot destination -> modifies destination
    AmdMul _ -> writes [RAX, RDX]
    AmdDiv _ -> writes [RAX, RDX]
    AmdSet _ destination -> modifies destination
    AmdCmov _ destination _ -> writes [destination]
    AmdNeg destination -> modifies destination
    AmdSar destination -> modifies destination
    AmdIdiv _ -> writes [RAX, RDX]
    AmdImulWide _ -> writes [RAX, RDX]
    AmdCqo -> writes [RDX]
    -- The vector registers are outside the model, and no general register
    -- changes.
    AmdMovqToXmm _ _ -> Writes []
    AmdMovdToXmm _ _ -> Writes []
    AmdSse {} -> Writes []
    AmdUcomis {} -> Writes []
    AmdCvtsi2s {} -> Writes []
    AmdMovqFromXmm destination _ -> writes [destination]
    AmdMovdFromXmm destination _ -> writes [destination]
    AmdCvtts2si _ destination _ -> writes [destination]
    AmdBitCount _ destination _ -> writes [destination]
  where
    writes registers
      | RSP `elem` registers = Forgets
      | otherwise = Writes (map registerKey registers)
    modifies destination =
      case destination of
        Amd64RmRegister register -> writes [register]
        Amd64RmMemory (Amd64Memory RSP offset) -> WritesSlot offset
        Amd64RmMemory _ -> Writes []
    narrowAccess base
      | base == RSP = Forgets
      | otherwise = Writes []

-- | Whether a register name reads and writes all 64 bits. The stack pointer
-- is excluded: moving it is not a copy the pass may follow.
quadWord :: Amd64Register -> Bool
quadWord register = register `elem` quadRegisters && register /= RSP

quadRegisters, dwordRegisters, byteRegisters :: [Amd64Register]
quadRegisters = [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15]
dwordRegisters = [EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]
byteRegisters = [AL, CL, DL, BL, SPL, BPL, SIL, DIL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

-- | The key of the 64-bit register a name refers to. Writing a narrow name
-- changes the whole register, so every name has to invalidate the same
-- entry.
registerKey :: Amd64Register -> Int
registerKey register =
  case elemIndex register quadRegisters of
    Just index -> index
    Nothing ->
      case elemIndex register dwordRegisters of
        Just index -> index
        Nothing -> fromMaybe 0 (elemIndex register byteRegisters)

-- | Split the parameters of a C function into the integer class and the
-- float class. Each list pairs the parameter index with its type.
classify :: [Type] -> ([(Int, Type)], [(Int, Type)])
classify types =
  ( [(index, ty) | (index, ty) <- zip [0 ..] types, not (isFloatType ty)],
    [(index, ty) | (index, ty) <- zip [0 ..] types, isFloatType ty]
  )

-- | Place the frame of one function. The allocator decides which values need
-- a slot at all; the rest of the frame holds the saved preserved registers
-- and the stack allocations, in that order.
--
-- A function has a frame when it has anything to put in one or when it
-- calls: a call needs the stack aligned, which the saved frame pointer
-- arranges. An aihc function that spills nothing and calls nothing has none.
functionLayout :: Map Symbol Signature -> Function -> M Layout
functionLayout signatures function = do
  let blocks = functionBlocks function
      convention = functionConvention function
      allocation = allocateRegistersFor (registersFor convention (functionScratch function)) signatures function
      calls = [operation | block <- blocks, Instruction _ operation <- blockInstructions block, isCall operation]
      callsAihc = any ((== AihcConvention) . callConvention signatures) calls
      savedRegisters =
        case convention of
          AihcConvention -> []
          CConvention
            | callsAihc -> preservedRegisters
            | otherwise -> [register | register <- allocationUsed allocation, register `elem` preservedRegisters]
      slots = Map.fromList (zip (allocationSpills allocation) [0, 8 ..])
      slotsEnd = 8 * Map.size slots
      saved = zip savedRegisters [slotsEnd, slotsEnd + 8 ..]
      allocsStart = slotsEnd + 8 * length saved
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
        layoutAllocs = allocs,
        layoutSize = size,
        layoutFramed = size > 0 || not (null calls)
      }

isCall :: Operation -> Bool
isCall operation =
  case operation of
    Call _ _ -> True
    CallIndirect {} -> True
    _ -> False

-- | The convention of the callee of a call operation. A direct call to a
-- symbol the module does not declare is an aihc call.
callConvention :: Map Symbol Signature -> Operation -> CallingConvention
callConvention signatures operation =
  case operation of
    Call symbol _ -> maybe AihcConvention signatureConvention (Map.lookup symbol signatures)
    CallIndirect _ _ signature -> signatureConvention signature
    _ -> AihcConvention

placeAllocations :: Int -> [(Var, Integer, Integer)] -> M (Map Var (Int, Int))
placeAllocations = go Map.empty
  where
    go placed _ [] = pure placed
    go placed next ((var, size, alignment) : rest) = do
      when (alignment > 16) $ unsupported "stack.alloc alignment above 16"
      let start = roundUp (fromInteger alignment) next
      go (Map.insert var (start, fromInteger size) placed) (start + fromInteger size) rest
    roundUp alignment value = ((value + alignment - 1) `div` alignment) * alignment

-- | Save the frame pointer, reserve the frame, save the preserved registers,
-- zero the stack allocations, and move the parameters into their homes. A
-- parameter the allocator left in its argument register costs nothing.
functionPrologue :: Ctx -> M [Amd64Statement]
functionPrologue ctx = do
  let parameters = functionParameters function
      moves =
        case functionConvention function of
          AihcConvention ->
            parallelMove
              [ (home ctx var, SourceLocation (parameterLocation index))
              | (index, (var, _)) <- zip [0 ..] parameters
              ]
          CConvention ->
            let (integers, floats) = classify (map snd parameters)
                names = map fst parameters
             in concat [canonicalizeRegister ty register | ((_, ty), register) <- zip integers argumentRegisters]
                  <> parallelMove [(home ctx (names !! index), SourceLocation (LocRegister register)) | ((index, _), register) <- zip integers argumentRegisters]
                  <> concat
                    [ [amd64Instruction (AmdMovqFromXmm scratchLeft xmm)]
                        <> canonicalizeRegister ty scratchLeft
                        <> parallelMove [(home ctx (names !! index), SourceLocation (LocRegister scratchLeft))]
                    | ((index, ty), xmm) <- zip floats [0 ..]
                    ]
  pure
    ( frame
        <> saveRegisters ctx
        <> concatMap zeroAllocation (Map.elems (layoutAllocs layout))
        <> moves
    )
  where
    function = ctxFunction ctx
    layout = ctxLayout ctx
    frame
      | layoutFramed layout =
          [ amd64Instruction (AmdPush RBP),
            amd64Instruction (AmdMov RBP (Amd64MoveRegister RSP))
          ]
            <> adjustStack AmdSub (layoutSize layout)
      | otherwise = []
    parameterLocation index
      | index < length argumentRegisters = LocRegister (argumentRegisters !! index)
      -- The overflow block sits above the return address.
      | otherwise = LocSlot (frameBytes layout + 8 + 8 * (index - length argumentRegisters))
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

-- | Give back the frame: restore the preserved registers, then the saved
-- frame pointer. The stack pointer ends on the return address. The
-- displacement is the number of bytes the stack pointer currently sits
-- below the frame.
leaveFrame :: Ctx -> Int -> [Amd64Statement]
leaveFrame ctx displacement =
  restoreRegisters ctx displacement
    <> ( if layoutFramed (ctxLayout ctx)
           then [amd64Instruction (AmdMov RSP (Amd64MoveRegister RBP)), amd64Instruction (AmdPop RBP)]
           else adjustStack AmdAdd displacement
       )

-- | Pop the incoming overflow block on the way out.
returnInstruction :: Ctx -> Amd64Statement
returnInstruction ctx =
  amd64Instruction (if ctxIncomingOverflow ctx == 0 then AmdRet else AmdRetImm (ctxIncomingOverflow ctx))

-- | Save the preserved registers the function uses into the frame. The
-- prologue runs this before it moves any parameter into a register.
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

-- Values

-- | Where a value lives: a register, or a frame slot at a byte offset above
-- the stack pointer after the prologue.
data Location
  = LocRegister !Amd64Register
  | LocSlot !Int
  deriving (Eq, Ord, Show)

-- | The source of a move: a location, or a literal of a type.
data MoveSource
  = SourceLocation !Location
  | SourceLiteral !Type !Literal
  deriving (Eq, Show)

home :: Ctx -> Var -> Location
home ctx var =
  case Map.lookup var (layoutRegisters (ctxLayout ctx)) of
    Just register -> LocRegister register
    Nothing ->
      case Map.lookup var (layoutSlots (ctxLayout ctx)) of
        Just offset -> LocSlot offset
        Nothing -> error ("Aihc.Amd64.Lir: unknown value " <> T.unpack (unVar var))

-- | The source of a move that reads an operand of a type.
operandSource :: Ctx -> Type -> Operand -> MoveSource
operandSource ctx ty operand =
  case operand of
    OperandVar var -> SourceLocation (home ctx var)
    OperandLiteral literal -> SourceLiteral ty literal

-- | Read an operand for an instruction: the register that holds it, or the
-- scratch register it was loaded into. The displacement is the number of
-- bytes the stack pointer currently sits below the frame.
operandIn :: Ctx -> Int -> Type -> Amd64Register -> Operand -> ([Amd64Statement], Amd64Register)
operandIn ctx displacement ty scratch operand =
  case operand of
    OperandVar var ->
      case home ctx var of
        LocRegister register -> ([], register)
        LocSlot offset -> ([loadSlot scratch (offset + displacement)], scratch)
    OperandLiteral literal -> (literalInto ty scratch literal, scratch)

-- | Read an operand into a given register.
operandTo :: Ctx -> Type -> Amd64Register -> Operand -> [Amd64Statement]
operandTo ctx ty destination operand =
  case operand of
    OperandVar var ->
      case home ctx var of
        LocRegister register -> move destination register
        LocSlot offset -> [loadSlot destination offset]
    OperandLiteral literal -> literalInto ty destination literal

-- | The register an instruction computes a result in, and the store that
-- puts it in its home afterwards when the home is a slot.
resultIn :: Ctx -> Amd64Register -> Var -> (Amd64Register, [Amd64Statement])
resultIn ctx scratch var =
  case home ctx var of
    LocRegister register -> (register, [])
    LocSlot offset -> (scratch, [storeSlot scratch offset])

loadSlot :: Amd64Register -> Int -> Amd64Statement
loadSlot register offset = amd64Instruction (AmdMov register (Amd64MoveMemory (slotMemory offset)))

storeSlot :: Amd64Register -> Int -> Amd64Statement
storeSlot register offset = amd64Instruction (AmdStore (slotMemory offset) (Amd64StoreRegister register))

slotMemory :: Int -> Amd64Memory
slotMemory offset = Amd64Memory RSP (fromIntegral offset)

-- | The 64-bit pattern of a literal of a type, when it is a number. Float
-- literals take the width of the type, and integer literals are canonical
-- for the type.
literalBits :: Type -> Literal -> Maybe Integer
literalBits ty literal =
  case (ty, literal) of
    (F32, LitFloat value) -> Just (toInteger (castFloatToWord32 (double2Float value)))
    (F32, LitInt value) -> Just (toInteger (castFloatToWord32 (fromInteger value)))
    (F64, LitInt value) -> Just (toInteger (castDoubleToWord64 (fromInteger value)))
    (_, LitFloat value) -> Just (toInteger (castDoubleToWord64 value))
    (_, LitInt value) -> Just (canonicalInteger ty value)
    (_, LitNull) -> Just 0
    (_, LitSymbol _) -> Nothing

-- | Load a literal with the encoding of its type.
literalInto :: Type -> Amd64Register -> Literal -> [Amd64Statement]
literalInto ty register literal =
  case literalBits ty literal of
    Just bits -> [immediate register bits]
    Nothing ->
      case literal of
        LitSymbol symbol -> [address register (lirSymbol symbol)]
        _ -> []

-- | A literal integer operand that fits the sign-extended 32-bit immediate
-- of an arithmetic instruction, as that immediate.
smallImmediate :: Type -> Operand -> Maybe Integer
smallImmediate ty operand =
  case operand of
    OperandLiteral (LitInt value)
      | not (isFloatType ty) -> signedImmediate (canonicalInteger ty value)
    _ -> Nothing

-- | A 64-bit pattern as a sign-extended 32-bit immediate, when it is one.
signedImmediate :: Integer -> Maybe Integer
signedImmediate bits
  | bits < 2 ^ (31 :: Int) = Just bits
  | bits >= 2 ^ (64 :: Int) - 2 ^ (31 :: Int) = Just (bits - 2 ^ (64 :: Int))
  | otherwise = Nothing

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

-- | A copy between registers, unless it would copy a register to itself.
move :: Amd64Register -> Amd64Register -> [Amd64Statement]
move destination source
  | destination == source = []
  | otherwise = [amd64Instruction (AmdMov destination (Amd64MoveRegister source))]

tshow :: (Show value) => value -> Text
tshow = T.pack . show

-- Parallel moves

-- | Perform a set of moves as if all at once: every source is read before
-- the destination it lives in is written. A move whose destination nothing
-- else reads goes first; when only cycles remain, the contents of one
-- destination are parked in 'scratchLeft' and the readers of that
-- destination read the parking place instead. A slot-to-slot move goes
-- through 'scratchRight'.
--
-- The destinations are distinct. A move to the location a source is already
-- in is dropped.
parallelMove :: [(Location, MoveSource)] -> [Amd64Statement]
parallelMove = concatMap emit . orderMoves locationOf SourceLocation (LocRegister scratchLeft)
  where
    locationOf source =
      case source of
        SourceLocation location -> Just location
        SourceLiteral _ _ -> Nothing
    emit (destination, source) =
      case (destination, source) of
        (LocRegister target, SourceLocation (LocRegister register)) -> move target register
        (LocRegister target, SourceLocation (LocSlot offset)) -> [loadSlot target offset]
        (LocRegister target, SourceLiteral ty literal) -> literalInto ty target literal
        (LocSlot offset, SourceLocation (LocRegister register)) -> [storeSlot register offset]
        (LocSlot offset, SourceLocation (LocSlot from)) -> [loadSlot scratchRight from, storeSlot scratchRight offset]
        (LocSlot offset, SourceLiteral ty literal) ->
          case literalBits ty literal >>= signedImmediate of
            Just value -> [amd64Instruction (AmdStore (slotMemory offset) (Amd64StoreImmediate value))]
            Nothing -> literalInto ty scratchRight literal <> [storeSlot scratchRight offset]

-- | A source displaced by the bytes the stack pointer currently sits below
-- the frame. Only a slot moves.
displaceSource :: Int -> MoveSource -> MoveSource
displaceSource displacement source =
  case source of
    SourceLocation (LocSlot offset) -> SourceLocation (LocSlot (offset + displacement))
    _ -> source

-- Blocks

-- | A comparison that the branch of the block consumes directly, so that
-- the block compares and branches on the flags instead of materializing a
-- boolean.
data Fused = Fused !CompareOp !Type !Operand !Operand

-- | Compile one block. The entry block has no label: nothing may jump to
-- it.
compileBlock :: Ctx -> (Bool, Block, Maybe Block) -> M [Amd64Statement]
compileBlock ctx (entry, block, next) = do
  let (instructions, fused) = fuseCompare ctx (blockInstructions block) (blockTerminator block)
  lines' <- concat <$> mapM (compileInstruction ctx) instructions
  terminator <- compileTerminator ctx (blockLabel <$> next) fused (blockTerminator block)
  pure ([amd64Label (ctxLabels ctx Map.! blockLabel block) | not entry] <> lines' <> terminator)

-- | Split off a comparison that only the branch of the block reads and that
-- sits right before it. Nothing else reads the boolean, so it is never
-- written. A float comparison fuses only in the ordered forms the flags
-- express directly.
fuseCompare :: Ctx -> [Instruction] -> Terminator -> ([Instruction], Maybe Fused)
fuseCompare ctx instructions terminator =
  case (reverse instructions, terminator) of
    (Instruction [var] (Compare op ty left right) : before, Branch (OperandVar condition) _ _)
      | condition == var,
        Map.lookup var (ctxReads ctx) == Just 1,
        not (isFloatType ty) || op `elem` [Eq, Ne, FLt, FLe, FGt, FGe] ->
          (reverse before, Just (Fused op ty left right))
    _ -> (instructions, Nothing)

-- | The test a block branches on: a register that holds the condition when
-- it is not zero, a register that holds it when it is zero, the flags, or
-- the flags of a float comparison for equality, where an unordered pair sets
-- the parity flag.
data Test
  = TestNonZero !Amd64Register
  | TestZero !Amd64Register
  | TestFlags !Amd64Condition
  | TestFloatEqual
  | TestFloatNotEqual

-- | Set up the test of a branch condition: the fused comparison when there
-- is one, otherwise the boolean itself.
conditionTest :: Ctx -> Maybe Fused -> Operand -> ([Amd64Statement], Test)
conditionTest ctx fused condition =
  case fused of
    Just (Fused op ty left right)
      | op `elem` [Eq, Ne],
        not (isFloatType ty),
        right == OperandLiteral (LitInt 0) ->
          let (loads, register) = operandIn ctx 0 ty scratchLeft left
           in (loads, if op == Eq then TestZero register else TestNonZero register)
      | isFloatType ty ->
          let (loads, flags) = floatFlags ctx op ty left right
           in ( loads,
                case op of
                  Eq -> TestFloatEqual
                  Ne -> TestFloatNotEqual
                  _ -> TestFlags flags
              )
      | otherwise ->
          let (loads, register) = operandIn ctx 0 ty scratchLeft left
              signed = op `elem` [LtS, LeS, GtS, GeS]
              (extendLeft, leftRegister) = if signed then signExtendInto ty scratchLeft register else ([], register)
           in (loads <> extendLeft <> compareWith ctx ty signed leftRegister right, TestFlags (integerCondition op))
    Nothing ->
      let (loads, register) = operandIn ctx 0 I1 scratchLeft condition
       in (loads, TestNonZero register)

-- | Compare two floats with @ucomis@ and say which condition holds when the
-- comparison does. @ucomis@ sets the carry flag for less than and every
-- flag for an unordered pair, so the ordered conditions are the unsigned
-- above conditions with the operands in the right order; the condition of
-- an equality is 'AmdEqual' with the parity still to be checked.
floatFlags :: Ctx -> CompareOp -> Type -> Operand -> Operand -> ([Amd64Statement], Amd64Condition)
floatFlags ctx op ty left right =
  let (loads, a) = operandIn ctx 0 ty scratchLeft left
      (loads', b) = operandIn ctx 0 ty scratchRight right
      ucomis first second = [toFloat ty 0 first, toFloat ty 1 second, amd64Instruction (AmdUcomis (ty == F64) 0 1)]
      (order, flags) =
        case op of
          FLt -> (ucomis b a, AmdAbove)
          FLe -> (ucomis b a, AmdAboveOrEqual)
          FGt -> (ucomis a b, AmdAbove)
          FGe -> (ucomis a b, AmdAboveOrEqual)
          Ne -> (ucomis a b, AmdNotEqual)
          _ -> (ucomis a b, AmdEqual)
   in (loads <> loads' <> order, flags)

-- | Compare a register with an operand: against an immediate when the
-- operand is a small literal, otherwise against a register. The right
-- operand is sign-extended into the right scratch register when asked.
compareWith :: Ctx -> Type -> Bool -> Amd64Register -> Operand -> [Amd64Statement]
compareWith ctx ty signed left right =
  case smallImmediate ty right of
    Just value
      | not signed || typeBits ty >= 64 || value < 2 ^ (typeBits ty - 1) ->
          [amd64Instruction (AmdCmp (Amd64RmRegister left) (Amd64BinaryImmediate value))]
    _ ->
      let (loads, register) = operandIn ctx 0 ty scratchRight right
          (extend, rightRegister) = if signed then signExtendInto ty scratchRight register else ([], register)
       in loads <> extend <> [amd64Instruction (AmdCmp (Amd64RmRegister left) (Amd64BinaryRegister rightRegister))]

-- | Branch to a label when the test fails.
branchUnless :: Test -> Text -> M [Amd64Statement]
branchUnless test label =
  case test of
    TestNonZero register -> pure [testZero register, amd64Instruction (AmdJe label)]
    TestZero register -> pure [testZero register, amd64Instruction (AmdJne label)]
    TestFlags condition -> pure [amd64Instruction (AmdJcc (inverseCondition condition) label)]
    TestFloatEqual -> pure [amd64Instruction (AmdJne label), amd64Instruction (AmdJcc AmdParity label)]
    TestFloatNotEqual -> do
      over <- freshLabel "unordered"
      pure [amd64Instruction (AmdJcc AmdParity over), amd64Instruction (AmdJe label), amd64Label over]

-- | Branch to a label when the test holds.
branchWhen :: Test -> Text -> M [Amd64Statement]
branchWhen test label =
  case test of
    TestNonZero register -> pure [testZero register, amd64Instruction (AmdJne label)]
    TestZero register -> pure [testZero register, amd64Instruction (AmdJe label)]
    TestFlags condition -> pure [amd64Instruction (AmdJcc condition label)]
    TestFloatEqual -> do
      over <- freshLabel "unordered"
      pure [amd64Instruction (AmdJcc AmdParity over), amd64Instruction (AmdJe label), amd64Label over]
    TestFloatNotEqual -> pure [amd64Instruction (AmdJne label), amd64Instruction (AmdJcc AmdParity label)]

compileTerminator :: Ctx -> Maybe Label -> Maybe Fused -> Terminator -> M [Amd64Statement]
compileTerminator ctx next fused terminator =
  case terminator of
    Jump target -> do
      moves <- blockArgumentMoves ctx target
      pure (moves <> branchTo target)
    Branch condition whenTrue whenFalse -> do
      let (setup, test) = conditionTest ctx fused condition
      trueMoves <- blockArgumentMoves ctx whenTrue
      falseMoves <- blockArgumentMoves ctx whenFalse
      if null trueMoves && null falseMoves && isNext whenFalse
        then do
          branch <- branchWhen test (labelOf whenTrue)
          pure (setup <> branch)
        else do
          falseLabel <- if null falseMoves then pure (labelOf whenFalse) else freshLabel "else"
          branch <- branchUnless test falseLabel
          pure
            ( setup
                <> branch
                <> trueMoves
                <> [jump (labelOf whenTrue) | not (null falseMoves) || not (isNext whenTrue)]
                <> (if null falseMoves then [] else amd64Label falseLabel : falseMoves <> branchTo whenFalse)
            )
    Switch ty scrutinee cases fallback -> do
      let (loads, register) = operandIn ctx 0 ty scratchLeft scrutinee
      edges <- forM cases $ \switchCase -> do
        moves <- blockArgumentMoves ctx (switchCaseTarget switchCase)
        label <-
          if null moves
            then pure (labelOf (switchCaseTarget switchCase))
            else freshLabel "case"
        pure (switchCase, label, moves)
      fallbackLines <-
        case fallback of
          Just target -> do
            moves <- blockArgumentMoves ctx target
            pure (moves <> branchTo target)
          Nothing -> do
            stub <- trapLabel "switch without a matching case"
            pure [jump stub]
      let checks =
            concat
              [ compareWith ctx ty False register (OperandLiteral (LitInt (resolvedSwitchCaseValue switchCase))) <> [amd64Instruction (AmdJe label)]
              | (switchCase, label, _) <- edges
              ]
          bodies =
            concat
              [ amd64Label label : moves <> [jump (labelOf (switchCaseTarget switchCase))]
              | (switchCase, label, moves) <- edges,
                not (null moves)
              ]
      pure (loads <> checks <> fallbackLines <> bodies)
    Return values -> do
      when (length values > length resultRegisters) $ unsupported "return of more than eight values"
      let moves =
            parallelMove
              [ (LocRegister register, operandSource ctx ty value)
              | (ty, register, value) <- zip3 (functionResults function) resultRegisters values
              ]
          floatMoves =
            case (functionConvention function, functionResults function) of
              (CConvention, [F64]) -> [amd64Instruction (AmdMovqToXmm 0 RAX)]
              (CConvention, [F32]) -> [amd64Instruction (AmdMovdToXmm 0 EAX)]
              _ -> []
      pure (moves <> floatMoves <> leaveFrame ctx 0 <> [returnInstruction ctx])
    TailCall symbol arguments ->
      let signature = Map.lookup symbol (ctxSignatures ctx)
       in tailCall (Left (lirSymbol symbol)) (maybe AihcConvention signatureConvention signature) (maybe [] signatureParameters signature) arguments
    TailCallIndirect target arguments signature -> tailCall (Right target) (signatureConvention signature) (signatureParameters signature) arguments
    Trap message -> do
      stub <- trapLabel message
      pure [jump stub]
  where
    function = ctxFunction ctx
    layout = ctxLayout ctx
    labelOf target = ctxLabels ctx Map.! targetLabel target
    isNext target = Just (targetLabel target) == next
    branchTo target = [jump (labelOf target) | not (isNext target)]
    jump label = amd64Instruction (AmdJmp (Amd64JumpLabel label))
    tailCall callee convention parameterTypes arguments =
      case convention of
        AihcConvention -> aihcTailCall callee parameterTypes arguments
        CConvention -> do
          argumentMoves <- cArgumentMoves ctx parameterTypes arguments
          targetLoad <- case callee of
            Left _ -> pure []
            Right operand -> do
              stub <- trapLabel "indirect call to a non-function"
              pure (operandTo ctx Code scratchRight operand <> [testZero scratchRight, amd64Instruction (AmdJe stub)])
          let branch = case callee of
                Left label -> jump label
                Right _ -> amd64Instruction (AmdJmp (Amd64JumpRegister scratchRight))
          pure (targetLoad <> argumentMoves <> leaveFrame ctx 0 <> [branch])
    -- The outgoing block and the return address replace the incoming block.
    -- When the outgoing block is no larger, the arguments are written in
    -- place above the frame and the return address moves up; when it is
    -- larger and the function has no frame, the return address moves down
    -- to make room; when it is larger and the function has a frame, the
    -- block is built below the frame and copied up once the frame is gone.
    aihcTailCall callee parameterTypes arguments = do
      let outgoing = overflowBytes (length arguments)
          incoming = ctxIncomingOverflow ctx
          types = parameterTypes <> repeat I64
          overflow = drop (length argumentRegisters) (zip types arguments)
          registerMoves displacement =
            parallelMove
              [ (LocRegister register, displaceSource displacement (operandSource ctx ty argument))
              | (register, (ty, argument)) <- zip argumentRegisters (zip types arguments)
              ]
          overflowStores displacement base =
            concat
              [ loads <> [storeSlot register (base + 8 * position)]
              | (position, (ty, argument)) <- zip [0 :: Int ..] overflow,
                let (loads, register) = operandIn ctx displacement ty scratchLeft argument
              ]
          branch = case callee of
            Left label -> jump label
            Right _ -> amd64Instruction (AmdJmp (Amd64JumpRegister scratchRight))
          returnSlot = frameBytes layout
      stub <-
        case callee of
          Left _ -> pure Nothing
          Right _ -> Just <$> trapLabel "indirect call to a non-function"
      let targetLoad displacement =
            case (callee, stub) of
              (Right operand, Just label) ->
                let (loads, register) = operandIn ctx displacement Code scratchRight operand
                 in loads <> move scratchRight register <> [testZero scratchRight, amd64Instruction (AmdJe label)]
              _ -> []
      pure $
        if outgoing <= incoming
          then
            overflowStores 0 (returnSlot + 8 + incoming - outgoing)
              <> targetLoad 0
              <> registerMoves 0
              <> ( if outgoing == incoming
                     then []
                     else
                       [ loadSlot scratchLeft returnSlot,
                         storeSlot scratchLeft (returnSlot + incoming - outgoing)
                       ]
                 )
              <> leaveFrame ctx 0
              <> adjustStack AmdAdd (incoming - outgoing)
              <> [branch]
          else
            if not (layoutFramed layout)
              then
                [loadSlot scratchLeft 0]
                  <> adjustStack AmdSub (outgoing - incoming)
                  <> [storeSlot scratchLeft 0]
                  <> overflowStores 0 8
                  <> targetLoad 0
                  <> registerMoves 0
                  <> [branch]
              else
                let temporary = 8 + outgoing
                    delta = fromIntegral (8 + incoming - outgoing) :: Int64
                 in adjustStack AmdSub temporary
                      <> overflowStores temporary 8
                      <> targetLoad temporary
                      <> registerMoves temporary
                      <> restoreRegisters ctx temporary
                      -- Read the frame pointer and the return address before
                      -- the copy overwrites them, then copy the return
                      -- address and the block to their final place, highest
                      -- word first because the destination lies above the
                      -- source. Every argument is in its register by now, so
                      -- @rax@ is free to hold the frame pointer.
                      <> [ amd64Instruction (AmdMov RAX (Amd64MoveRegister RBP)),
                           amd64Instruction (AmdMov scratchLeft (Amd64MoveMemory (Amd64Memory RAX 8))),
                           storeSlot scratchLeft 0,
                           amd64Instruction (AmdMov RBP (Amd64MoveMemory (Amd64Memory RAX 0)))
                         ]
                      <> concat
                        [ [ loadSlot scratchLeft (8 * position),
                            amd64Instruction (AmdStore (Amd64Memory RAX (delta + fromIntegral (8 * position))) (Amd64StoreRegister scratchLeft))
                          ]
                        | position <- reverse [0 .. length overflow]
                        ]
                      <> [amd64Instruction (AmdLea RSP (Amd64MemoryAddress (Amd64Memory RAX delta))), branch]

cArgumentMoves :: Ctx -> [Type] -> [Operand] -> M [Amd64Statement]
cArgumentMoves ctx parameterTypes arguments = do
  let (integers, floats) = classify (take (length arguments) (parameterTypes <> repeat I64))
  when (length integers > length argumentRegisters) $ unsupported "C call with more than six integer arguments"
  when (length floats > floatArgumentCount) $ unsupported "C call with more than eight float arguments"
  -- Move float arguments first. Integer moves can overwrite their source registers.
  pure
    ( concat
        [ loads <> [amd64Instruction (AmdMovqToXmm xmm register)]
        | ((index, ty), xmm) <- zip floats [0 ..],
          let (loads, register) = operandIn ctx 0 ty scratchLeft (arguments !! index)
        ]
        <> parallelMove [(LocRegister register, operandSource ctx ty (arguments !! index)) | ((index, ty), register) <- zip integers argumentRegisters]
        -- A variadic callee reads the number of vector registers
        -- from @al@.
        <> [immediate RAX (length floats)]
    )

-- | Move the arguments of a jump into the parameters of the target, all at
-- once.
blockArgumentMoves :: Ctx -> Target -> M [Amd64Statement]
blockArgumentMoves ctx (Target label arguments) = do
  let parameters = Map.findWithDefault [] label (ctxBlockParameters ctx)
  pure
    ( parallelMove
        [ (home ctx var, operandSource ctx ty argument)
        | ((var, ty), argument) <- zip parameters arguments
        ]
    )

-- Instructions

-- | The right operand of a two-operand instruction.
data RightOperand
  = RightRegister !Amd64Register
  | RightImmediate !Integer

binarySource :: RightOperand -> Amd64BinarySource
binarySource operand =
  case operand of
    RightRegister register -> Amd64BinaryRegister register
    RightImmediate value -> Amd64BinaryImmediate value

compileInstruction :: Ctx -> Instruction -> M [Amd64Statement]
compileInstruction ctx (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft left
      single $ \dst -> do
        body <- binary op ty dst a right
        pure (loads <> body)
    Unary op ty value -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft value
      single $ \dst -> pure (loads <> bitCount op ty dst a)
    Wide op ty left right -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft left
          (loads', b) = operandIn ctx 0 ty scratchRight right
      pair $ \low high -> pure (loads <> loads' <> wide op ty low high a b)
    Compare op ty left right ->
      single $ \dst -> pure (compare' op ty dst left right)
    FloatBinary op ty left right -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft left
          (loads', b) = operandIn ctx 0 ty scratchRight right
      single $ \dst ->
        pure (loads <> loads' <> [toFloat ty 0 a, toFloat ty 1 b, amd64Instruction (AmdSse (floatOp op) (ty == F64) 0 1), fromFloat ty dst 0])
    FloatUnary op ty value -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft value
      single $ \dst -> pure (loads <> floatUnary op ty dst a)
    Convert op from value to -> do
      let (loads, a) = operandIn ctx 0 from scratchLeft value
      single $ \dst -> do
        body <- convert op from to dst a
        pure (loads <> body)
    PtrToInt value -> single $ \dst -> pure (operandTo ctx Ptr dst value)
    PtrFromInt value -> single $ \dst -> pure (operandTo ctx Ptr dst value)
    Select ty condition left right -> single $ \dst -> pure (select ty dst condition left right)
    Load ty (Address base offset) _ -> do
      let (loads, baseRegister) = operandIn ctx 0 Ptr scratchRight base
      single $ \dst -> pure (loads <> [loadMemory ty dst (Amd64Memory baseRegister (fromInteger offset))] <> [mask | ty == I1, mask <- narrowRegister I1 dst])
    Store ty value (Address base offset) _ -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft value
          (loads', baseRegister) = operandIn ctx 0 Ptr scratchRight base
      pure (loads <> loads' <> [storeMemory ty (Amd64Memory baseRegister (fromInteger offset)) a])
    PtrAdd base offset -> do
      let (loads, a) = operandIn ctx 0 Ptr scratchLeft base
      single $ \dst ->
        pure
          ( loads
              <> case smallImmediate I64 offset of
                Just value -> [amd64Instruction (AmdLea dst (Amd64MemoryAddress (Amd64Memory a (fromInteger value))))]
                Nothing ->
                  let (loads', b) = operandIn ctx 0 I64 scratchRight offset
                   in loads' <> arith True AmdAdd dst a (RightRegister b)
          )
    StackAlloc _ _ ->
      case results of
        [var]
          | Just (offset, _) <- Map.lookup var (layoutAllocs (ctxLayout ctx)) ->
              single $ \dst -> pure [amd64Instruction (AmdLea dst (Amd64MemoryAddress (slotMemory offset)))]
        _ -> unsupported "stack.alloc without a placed result"
    GlobalGet symbol ->
      single $ \dst -> pure [address scratchRight (lirSymbol symbol), amd64Instruction (AmdMov dst (Amd64MoveMemory (Amd64Memory scratchRight 0)))]
    GlobalSet symbol value -> do
      let (loads, a) = operandIn ctx 0 I64 scratchLeft value
      pure (loads <> [address scratchRight (lirSymbol symbol), amd64Instruction (AmdStore (Amd64Memory scratchRight 0) (Amd64StoreRegister a))])
    Call symbol arguments -> call (Left symbol) arguments
    CallIndirect target arguments signature -> callIndirect target arguments signature
  where
    -- The result register of an instruction with one result, and the store
    -- that follows when the result lives in a slot. A result may alias an
    -- operand of its own instruction, so every body below reads its
    -- operands before it writes a result, or computes in a scratch register
    -- first.
    single body =
      case results of
        [var] -> do
          let (dst, store) = resultIn ctx scratchLeft var
          lines' <- body dst
          pure (lines' <> store)
        _ -> unsupported "instruction result count"
    pair body =
      case results of
        [first, second] -> do
          let (low, storeLow) = resultIn ctx scratchLeft first
              (high, storeHigh) = resultIn ctx scratchRight second
          lines' <- body low high
          pure (lines' <> storeLow <> storeHigh)
        _ -> unsupported "instruction result count"

    -- The right operand of a binary instruction: an immediate when the
    -- literal fits, otherwise a register.
    rightValue ty right =
      case smallImmediate ty right of
        Just value -> ([], RightImmediate value)
        Nothing ->
          let (loads, b) = operandIn ctx 0 ty scratchRight right
           in (loads, RightRegister b)
    rightRegister ty = operandIn ctx 0 ty scratchRight

    -- A two-operand instruction computes into its left operand, so the
    -- result is computed in place when it aliases the left operand, in
    -- place with the operands swapped when the instruction commutes and it
    -- aliases the right one, and otherwise in a copy of the left operand
    -- that the right one does not alias.
    arith commutative op dst a b
      | dst == a = [amd64Instruction (op (Amd64RmRegister dst) (binarySource b))]
      | RightRegister register <- b,
        register == dst =
          if commutative
            then [amd64Instruction (op (Amd64RmRegister dst) (Amd64BinaryRegister a))]
            else move scratchLeft a <> [amd64Instruction (op (Amd64RmRegister scratchLeft) (binarySource b))] <> move dst scratchLeft
      | otherwise = move dst a <> [amd64Instruction (op (Amd64RmRegister dst) (binarySource b))]

    -- An addition of an immediate into another register is one @lea@.
    addImmediate dst a value
      | dst /= a = [amd64Instruction (AmdLea dst (Amd64MemoryAddress (Amd64Memory a (fromInteger value))))]
      | otherwise = arith True AmdAdd dst a (RightImmediate value)

    binary op ty dst a right =
      case op of
        Add ->
          let (loads, b) = rightValue ty right
           in pure
                ( loads
                    <> narrow
                      ty
                      dst
                      ( case b of
                          RightImmediate value -> addImmediate dst a value
                          RightRegister _ -> arith True AmdAdd dst a b
                      )
                )
        Sub ->
          let (loads, b) = rightValue ty right
           in pure
                ( loads
                    <> narrow
                      ty
                      dst
                      ( case b of
                          RightImmediate value | value > negate (2 ^ (31 :: Int)) -> addImmediate dst a (negate value)
                          _ -> arith False AmdSub dst a b
                      )
                )
        Mul ->
          let (loads, b) = rightRegister ty right
           in pure (loads <> narrow ty dst (multiply dst a b))
        DivS -> do
          checks <- signedDivisionChecks op ty a right
          pure (checks <> [amd64Instruction AmdCqo, amd64Instruction (AmdIdiv (Amd64RmRegister scratchRight))] <> narrow ty dst (move dst RAX))
        DivU -> do
          zero <- trapLabel "integer division by zero"
          let (loads, b) = rightRegister ty right
          pure (loads <> move RAX a <> [testZero b, amd64Instruction (AmdJe zero), clearRdx, amd64Instruction (AmdDiv (Amd64RmRegister b))] <> move dst RAX)
        RemS -> do
          checks <- signedDivisionChecks op ty a right
          pure (checks <> [amd64Instruction AmdCqo, amd64Instruction (AmdIdiv (Amd64RmRegister scratchRight))] <> narrow ty dst (move dst RDX))
        RemU -> do
          zero <- trapLabel "integer division by zero"
          let (loads, b) = rightRegister ty right
          pure (loads <> move RAX a <> [testZero b, amd64Instruction (AmdJe zero), clearRdx, amd64Instruction (AmdDiv (Amd64RmRegister b))] <> move dst RDX)
        And ->
          let (loads, b) = rightValue ty right
           in pure (loads <> arith True AmdAnd dst a b)
        Or ->
          let (loads, b) = rightValue ty right
           in pure (loads <> arith True AmdOr dst a b)
        Xor ->
          let (loads, b) = rightValue ty right
           in pure (loads <> arith True AmdXor dst a b)
        Shl -> pure (shift ty dst (move dst a) AmdShl AmdShlImmediate right)
        ShrS -> pure (shift ty dst (signExtendTo dst ty a) AmdSar AmdSarImmediate right)
        ShrU -> pure (shift ty dst (move dst a) AmdShr AmdShrImmediate right)

    -- @imul@ reads a register or memory operand, so a literal right operand
    -- goes through a register.
    multiply dst a b
      | dst == a = [amd64Instruction (AmdImul dst (Amd64RmRegister b))]
      | dst == b = [amd64Instruction (AmdImul dst (Amd64RmRegister a))]
      | otherwise = move dst a <> [amd64Instruction (AmdImul dst (Amd64RmRegister b))]

    -- A shift by a literal count uses the immediate form. A variable count
    -- goes through @cl@, which the function then keeps out of the pool, and
    -- is read before the left operand reaches the destination in case the
    -- two share a register. A narrow shift count wraps at the width of the
    -- type, and the result of a narrow shift is masked back to it.
    shift ty dst prepare byCount byImmediate right =
      case right of
        OperandLiteral (LitInt count) ->
          prepare <> narrowShift ty dst [amd64Instruction (byImmediate (Amd64RmRegister dst) (fromInteger (count `mod` toInteger (typeBits ty))))]
        _ ->
          let (loads, b) = rightRegister ty right
           in loads
                <> move RCX b
                <> [amd64Instruction (AmdAnd (Amd64RmRegister RCX) (Amd64BinaryImmediate (toInteger (typeBits ty - 1)))) | typeBits ty /= 64]
                <> prepare
                <> narrowShift ty dst [amd64Instruction (byCount (Amd64RmRegister dst))]
    narrowShift ty dst body = body <> narrowRegister ty dst

    -- The dividend goes to @rax@ and the divisor to the right scratch
    -- register, both sign-extended. The divisor must not be zero, and the
    -- minimum value divided by minus one does not fit.
    signedDivisionChecks op ty a right = do
      zero <- trapLabel "integer division by zero"
      skip <- freshLabel "div"
      minusOne <-
        if op == DivS
          then do
            overflow <- trapLabel "integer overflow"
            pure
              [ immediate scratchLeft (minimumSigned ty),
                amd64Instruction (AmdCmp (Amd64RmRegister RAX) (Amd64BinaryRegister scratchLeft)),
                amd64Instruction (AmdJe overflow)
              ]
          -- A remainder with divisor minus one is zero. Use one to prevent
          -- the hardware overflow trap for the minimum signed integer.
          else pure [immediate scratchRight (1 :: Integer)]
      let (loads, b) = rightRegister ty right
      pure
        ( loads
            <> signExtendTo RAX ty a
            <> signExtendTo scratchRight ty b
            <> [ testZero scratchRight,
                 amd64Instruction (AmdJe zero),
                 amd64Instruction (AmdCmp (Amd64RmRegister scratchRight) (Amd64BinaryImmediate (-1))),
                 amd64Instruction (AmdJne skip)
               ]
            <> minusOne
            <> [amd64Label skip]
        )

    -- A narrow value is zero-extended in its register, so a leading-zero
    -- count includes the bits above the type and a trailing-zero count of
    -- zero would reach the top of the register. Setting the first bit above
    -- the type keeps the trailing count at the width of the type.
    bitCount op ty dst a =
      let bits = typeBits ty
       in case op of
            Popcount -> [amd64Instruction (AmdBitCount AmdPopcnt dst (Amd64RmRegister a))]
            Clz ->
              [amd64Instruction (AmdBitCount AmdLzcnt dst (Amd64RmRegister a))]
                <> [amd64Instruction (AmdSub (Amd64RmRegister dst) (Amd64BinaryImmediate (toInteger (64 - bits)))) | bits < 64]
            Ctz
              | bits < 64 ->
                  move scratchLeft a
                    <> ( case signedImmediate (2 ^ bits) of
                           Just value -> [amd64Instruction (AmdOr (Amd64RmRegister scratchLeft) (Amd64BinaryImmediate value))]
                           Nothing -> [immediate scratchRight (2 ^ bits :: Integer), amd64Instruction (AmdOr (Amd64RmRegister scratchLeft) (Amd64BinaryRegister scratchRight))]
                       )
                    <> [amd64Instruction (AmdBitCount AmdTzcnt dst (Amd64RmRegister scratchLeft))]
              | otherwise -> [amd64Instruction (AmdBitCount AmdTzcnt dst (Amd64RmRegister a))]

    -- Every body reads both operands before it writes either result. The
    -- 64-bit forms go through @rdx:rax@, which the function keeps out of
    -- the pool.
    wide op ty low high a b =
      case op of
        MulWideU
          | typeBits ty == 64 -> move RAX a <> [amd64Instruction (AmdMul (Amd64RmRegister b))] <> move low RAX <> move high RDX
          | otherwise ->
              move scratchLeft a
                <> [amd64Instruction (AmdImul scratchLeft (Amd64RmRegister b))]
                <> move high scratchLeft
                <> [amd64Instruction (AmdShrImmediate (Amd64RmRegister high) (typeBits ty))]
                <> narrowRegister ty high
                <> narrowRegister ty scratchLeft
                <> move low scratchLeft
        MulWideS
          | typeBits ty == 64 -> move RAX a <> [amd64Instruction (AmdImulWide (Amd64RmRegister b))] <> move low RAX <> move high RDX
          | otherwise ->
              signExtendTo scratchLeft ty a
                <> signExtendTo scratchRight ty b
                <> [amd64Instruction (AmdImul scratchLeft (Amd64RmRegister scratchRight))]
                <> move high scratchLeft
                <> [amd64Instruction (AmdSarImmediate (Amd64RmRegister high) (typeBits ty))]
                <> narrowRegister ty high
                <> narrowRegister ty scratchLeft
                <> move low scratchLeft
        AddCarry
          | typeBits ty == 64 ->
              move scratchLeft a
                <> [amd64Instruction (AmdAdd (Amd64RmRegister scratchLeft) (Amd64BinaryRegister b))]
                <> setFlag AmdCarry high
                <> move low scratchLeft
          | otherwise ->
              move scratchLeft a
                <> [amd64Instruction (AmdAdd (Amd64RmRegister scratchLeft) (Amd64BinaryRegister b))]
                <> move high scratchLeft
                <> [amd64Instruction (AmdShrImmediate (Amd64RmRegister high) (typeBits ty))]
                <> narrowRegister ty scratchLeft
                <> move low scratchLeft
        SubBorrow ->
          move scratchLeft a
            <> [amd64Instruction (AmdSub (Amd64RmRegister scratchLeft) (Amd64BinaryRegister b))]
            <> setFlag AmdCarry high
            <> narrowRegister ty scratchLeft
            <> move low scratchLeft

    compare' op ty dst left right
      | isFloatType ty = floatCompare op ty dst left right
      | otherwise =
          let signed = op `elem` [LtS, LeS, GtS, GeS]
              (loads, a) = operandIn ctx 0 ty scratchLeft left
              (extendLeft, a') = if signed then signExtendInto ty scratchLeft a else ([], a)
           in loads <> extendLeft <> compareWith ctx ty signed a' right <> setFlag (integerCondition op) dst

    -- An equality also checks the parity flag, which an unordered pair
    -- sets; the second flag goes through the right scratch register, which
    -- the comparison no longer needs.
    floatCompare op ty dst left right =
      let (loads, flags) = floatFlags ctx op ty left right
       in loads
            <> case op of
              Eq -> setFlag AmdEqual dst <> setFlag AmdNotParity scratchRight <> [amd64Instruction (AmdAnd (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
              Ne -> setFlag AmdNotEqual dst <> setFlag AmdParity scratchRight <> [amd64Instruction (AmdOr (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
              _ | op `elem` [FLt, FLe, FGt, FGe] -> setFlag flags dst
              _ -> [immediate dst (0 :: Integer)]

    floatUnary op ty dst a =
      case op of
        FNeg -> [immediate scratchRight (signBit ty)] <> move dst a <> [amd64Instruction (AmdXor (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
        FAbs -> [immediate scratchRight (signBit ty - 1)] <> move dst a <> [amd64Instruction (AmdAnd (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
        FSqrt -> [toFloat ty 0 a, amd64Instruction (AmdSse SseSqrt (ty == F64) 0 0), fromFloat ty dst 0]

    -- The condition is read in place, from its slot, or folded when it is a
    -- literal. The chosen operand reaches the destination directly, and the
    -- other one is moved in when the condition says so.
    select ty dst condition left right =
      case condition of
        OperandLiteral literal -> operandTo ctx ty dst (if literal == LitInt 0 || literal == LitNull then right else left)
        OperandVar var ->
          let (conditionLoads, test) =
                case home ctx var of
                  LocRegister register
                    | register == dst -> (move scratchRight register, testZero scratchRight)
                    | otherwise -> ([], testZero register)
                  LocSlot offset -> ([], amd64Instruction (AmdCmp (Amd64RmMemory (slotMemory offset)) (Amd64BinaryImmediate 0)))
              other = if dst == scratchLeft then scratchRight else scratchLeft
           in case right of
                OperandVar rightVar
                  | home ctx rightVar == LocRegister dst ->
                      let (loads, a) = operandIn ctx 0 ty other left
                       in conditionLoads <> loads <> [test, amd64Instruction (AmdCmov AmdNotEqual dst (Amd64RmRegister a))]
                _ ->
                  let (loads, b) = operandIn ctx 0 ty other right
                   in conditionLoads <> loads <> operandTo ctx ty dst left <> [test, amd64Instruction (AmdCmov AmdEqual dst (Amd64RmRegister b))]

    convert op from to dst a =
      case op of
        SExt -> pure (signExtendTo dst from a <> narrowRegister to dst)
        ZExt -> pure (move dst a)
        Trunc -> pure (truncateTo dst to a)
        IToFS ->
          let (extend, a') = signExtendInto from scratchLeft a
           in pure (extend <> [amd64Instruction (AmdCvtsi2s (to == F64) 0 a'), fromFloat to dst 0])
        IToFU
          | typeBits from == 64 -> unsignedToFloat to dst a
          | otherwise -> pure [amd64Instruction (AmdCvtsi2s (to == F64) 0 a), fromFloat to dst 0]
        FToIS -> floatToInteger True from to dst a
        FToIU -> floatToInteger False from to dst a
        FpExt -> pure [amd64Instruction (AmdMovdToXmm 0 (dwordRegister a)), amd64Instruction (AmdSse SseConvertWidth False 0 0), amd64Instruction (AmdMovqFromXmm dst 0)]
        FpTrunc -> pure [amd64Instruction (AmdMovqToXmm 0 a), amd64Instruction (AmdSse SseConvertWidth True 0 0), amd64Instruction (AmdMovdFromXmm (dwordRegister dst) 0)]
        Bitcast -> pure (move dst a)

    -- A 64-bit value with the top bit set is halved with its low bit kept
    -- as a sticky bit, converted, and doubled, which rounds correctly.
    unsignedToFloat to dst a = do
      large <- freshLabel "utof_large"
      done <- freshLabel "utof_done"
      pure
        ( [ testZero a,
            amd64Instruction (AmdJcc AmdSign large),
            amd64Instruction (AmdCvtsi2s (to == F64) 0 a),
            amd64Instruction (AmdJmp (Amd64JumpLabel done)),
            amd64Label large
          ]
            <> move scratchRight a
            <> [amd64Instruction (AmdShrImmediate (Amd64RmRegister scratchRight) 1)]
            <> move scratchLeft a
            <> [ amd64Instruction (AmdAnd (Amd64RmRegister scratchLeft) (Amd64BinaryImmediate 1)),
                 amd64Instruction (AmdOr (Amd64RmRegister scratchRight) (Amd64BinaryRegister scratchLeft)),
                 amd64Instruction (AmdCvtsi2s (to == F64) 0 scratchRight),
                 amd64Instruction (AmdSse SseAdd (to == F64) 0 0),
                 amd64Label done,
                 fromFloat to dst 0
               ]
        )

    -- Widen the source to double, reject NaN and values outside the target
    -- range, then convert with rounding toward zero. An unsigned 64-bit
    -- result above the signed range is converted from the value minus
    -- two to the sixty-third.
    floatToInteger signed from to dst a = do
      invalid <- trapLabel "invalid float to integer conversion"
      let widen = if from == F64 then [amd64Instruction (AmdMovqToXmm 0 a)] else [amd64Instruction (AmdMovdToXmm 0 (dwordRegister a)), amd64Instruction (AmdSse SseConvertWidth False 0 0)]
          bits = typeBits to
          (lower, excludeLower, upper) = integerConversionBounds signed from to
          bound value = [immediate scratchRight (toInteger (castDoubleToWord64 value)), amd64Instruction (AmdMovqToXmm 1 scratchRight), amd64Instruction (AmdUcomis True 0 1)]
          lowerCheck = bound lower <> [amd64Instruction (AmdJcc (if excludeLower then AmdBelowOrEqual else AmdBelow) invalid)]
          upperCheck = bound upper <> [amd64Instruction (AmdJcc AmdAboveOrEqual invalid)]
      body <-
        if signed || bits < 64
          then pure [amd64Instruction (AmdCvtts2si True dst 0)]
          else do
            large <- freshLabel "ftou_large"
            done <- freshLabel "ftou_done"
            pure
              [ immediate scratchRight (toInteger (castDoubleToWord64 (2 ^^ (63 :: Int)))),
                amd64Instruction (AmdMovqToXmm 1 scratchRight),
                amd64Instruction (AmdUcomis True 0 1),
                amd64Instruction (AmdJcc AmdAboveOrEqual large),
                amd64Instruction (AmdCvtts2si True dst 0),
                amd64Instruction (AmdJmp (Amd64JumpLabel done)),
                amd64Label large,
                amd64Instruction (AmdSse SseSub True 0 1),
                amd64Instruction (AmdCvtts2si True dst 0),
                immediate scratchRight (signBit F64),
                amd64Instruction (AmdXor (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight)),
                amd64Label done
              ]
      pure
        ( widen
            <> [amd64Instruction (AmdUcomis True 0 0), amd64Instruction (AmdJcc AmdParity invalid)]
            <> lowerCheck
            <> upperCheck
            <> body
            <> narrowRegister to dst
        )

    narrow ty dst body = body <> narrowRegister ty dst

    call callee arguments = do
      let (convention, resultTypes, parameterTypes) = calleeSignature callee
      let outgoing = case convention of
            CConvention -> 0
            AihcConvention -> overflowBytes (length arguments)
          types = parameterTypes <> repeat I64
      argumentMoves <-
        case convention of
          AihcConvention ->
            pure
              ( concat
                  [ loads <> [storeSlot register (8 * position)]
                  | (position, (ty, argument)) <- zip [0 :: Int ..] (drop (length argumentRegisters) (zip types arguments)),
                    let (loads, register) = operandIn ctx outgoing ty scratchLeft argument
                  ]
                  <> parallelMove
                    [ (LocRegister register, displaceSource outgoing (operandSource ctx ty argument))
                    | (register, (ty, argument)) <- zip argumentRegisters (zip types arguments)
                    ]
              )
          CConvention -> cArgumentMoves ctx parameterTypes arguments
      let branch = case callee of
            Left symbol -> [amd64Instruction (AmdCall (lirSymbol symbol))]
            Right _ -> [amd64Instruction (AmdCallRegister scratchRight)]
          resultMoves =
            case convention of
              AihcConvention -> parallelMove [(home ctx var, SourceLocation (LocRegister register)) | (var, register) <- zip results resultRegisters]
              CConvention ->
                concat
                  [ floatResult ty <> canonicalizeRegister ty RAX <> parallelMove [(home ctx var, SourceLocation (LocRegister RAX))]
                  | (var, ty) <- zip results resultTypes
                  ]
      pure (adjustStack AmdSub outgoing <> argumentMoves <> branch <> resultMoves)
    callIndirect target arguments signature = do
      stub <- trapLabel "indirect call to a non-function"
      body <- call (Right signature) arguments
      pure (operandTo ctx Code scratchRight target <> [testZero scratchRight, amd64Instruction (AmdJe stub)] <> body)
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

-- | Mask a value to the width of a narrow type into a register. A 64-bit
-- value is copied.
truncateTo :: Amd64Register -> Type -> Amd64Register -> [Amd64Statement]
truncateTo destination ty source =
  case ty of
    I1 -> move destination source <> [amd64Instruction (AmdAnd (Amd64RmRegister destination) (Amd64BinaryImmediate 1))]
    I8 -> [amd64Instruction (AmdMovzx destination (Amd64RmRegister (byteRegister source)))]
    I16 -> [amd64Instruction (AmdMovzxWord destination (Amd64RmRegister source))]
    I32 -> [amd64Instruction (AmdMov (dwordRegister destination) (Amd64MoveRegister (dwordRegister source)))]
    _ -> move destination source

-- | Sign-extend a canonical narrow value into a register. A 64-bit value is
-- copied.
signExtendTo :: Amd64Register -> Type -> Amd64Register -> [Amd64Statement]
signExtendTo destination ty source =
  case ty of
    I1 -> move destination source <> [amd64Instruction (AmdNeg (Amd64RmRegister destination))]
    I8 -> [amd64Instruction (AmdMovsxByte destination (Amd64RmRegister (byteRegister source)))]
    I16 -> [amd64Instruction (AmdMovsxWord destination (Amd64RmRegister source))]
    I32 -> [amd64Instruction (AmdMovsxd destination (Amd64RmRegister (dwordRegister source)))]
    _ -> move destination source

-- | Sign-extend a canonical narrow value into the scratch register, and say
-- which register now holds the extended value. A 64-bit value stays where
-- it is.
signExtendInto :: Type -> Amd64Register -> Amd64Register -> ([Amd64Statement], Amd64Register)
signExtendInto ty scratch source
  | typeBits ty >= 64 = ([], source)
  | otherwise = (signExtendTo scratch ty source, scratch)

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

-- | The condition that holds exactly when the given one does not.
inverseCondition :: Amd64Condition -> Amd64Condition
inverseCondition condition =
  case condition of
    AmdOverflow -> AmdNotOverflow
    AmdNotOverflow -> AmdOverflow
    AmdCarry -> AmdAboveOrEqual
    AmdBelow -> AmdAboveOrEqual
    AmdAboveOrEqual -> AmdBelow
    AmdEqual -> AmdNotEqual
    AmdNotEqual -> AmdEqual
    AmdBelowOrEqual -> AmdAbove
    AmdAbove -> AmdBelowOrEqual
    AmdLess -> AmdGreaterOrEqual
    AmdGreaterOrEqual -> AmdLess
    AmdLessOrEqual -> AmdGreater
    AmdGreater -> AmdLessOrEqual
    AmdSign -> AmdNotSign
    AmdNotSign -> AmdSign
    AmdParity -> AmdNotParity
    AmdNotParity -> AmdParity

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
