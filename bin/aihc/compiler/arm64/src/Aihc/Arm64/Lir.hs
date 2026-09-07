{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to AArch64 Mach-O objects for Darwin.
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
-- The @aihc@ calling convention passes the first eight arguments in @x0@ to
-- @x7@ and the rest in a 16-byte aligned block on the stack. The callee pops
-- that block, so a tail call restores the stack of the caller before it
-- pushes its own block and the stack does not grow. Results come back in
-- @x0@ to @x7@. An aihc function preserves no register: every call clobbers
-- them all, so an aihc function that makes no call and spills nothing needs
-- no frame at all. A C function preserves @x19@ to @x28@ and saves the ones
-- it touches, and it saves all of them when it calls into aihc code.
--
-- Narrow integers are canonical: an @iN@ value is zero-extended to 64 bits
-- wherever it lives. A float is its IEEE bit pattern.
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
import Aihc.Lir.RegAlloc (Allocation (..), Registers (..), allocateRegistersFor, readCounts)
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
    ( if null traps
        then []
        else [arm64Section TextSection] <> stubs <> reporter <> [arm64Section ReadOnlySection] <> messages
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
    -- | The preserved registers the function saves, with the frame slot the
    -- prologue saves each one in.
    layoutSaved :: ![(Arm64Register, Int)],
    layoutAllocs :: !(Map Var (Int, Int)),
    -- | The bytes below the saved frame pointer pair.
    layoutSize :: !Int,
    -- | Whether the function saves the frame pointer pair and reserves the
    -- frame at all. A function without a frame leaves the stack pointer
    -- where it found it.
    layoutFramed :: !Bool
  }

-- | The bytes between the stack pointer after the prologue and the stack
-- pointer at entry.
frameBytes :: Layout -> Int
frameBytes layout
  | layoutFramed layout = layoutSize layout + 16
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

argumentRegisters :: [Arm64Register]
argumentRegisters = [X0, X1, X2, X3, X4, X5, X6, X7]

-- | The registers every call clobbers, in the order the allocator prefers
-- them. The argument registers come last so that a value without a hint
-- leaves them for the values the conventions want there.
volatileRegisters :: [Arm64Register]
volatileRegisters = [X8, X9, X10, X11, X12, X13] <> argumentRegisters

-- | The procedure call standard passes the first eight floating point
-- arguments in v0 to v7.
floatArgumentCount :: Int
floatArgumentCount = 8

-- | Split the parameters of a C function into the integer class and the
-- float class. Each list pairs the parameter index with its type. The two
-- classes have separate registers and separate counters, so the position of
-- a parameter in its own class, not among all of them, selects its register.
classify :: [Type] -> ([(Int, Type)], [(Int, Type)])
classify types =
  ( [(index, ty) | (index, ty) <- zip [0 ..] types, not (isFloatType ty)],
    [(index, ty) | (index, ty) <- zip [0 ..] types, isFloatType ty]
  )

-- | The registers a C call preserves. An aihc call clobbers them too.
preservedRegisters :: [Arm64Register]
preservedRegisters = [X19, X20, X21, X22, X23, X24, X25, X26, X27, X28]

-- | The scratch registers of instruction selection. The left and the right
-- operand of an instruction land in the first two when they are not in a
-- register already, a result without a register is computed in the first
-- two, and the third is free for anything else an instruction needs. The
-- fourth holds the target of an indirect call, and the parallel moves break
-- a cycle through the third.
scratchLeft, scratchRight, scratchExtra, scratchTarget :: Arm64Register
scratchLeft = X16
scratchRight = X17
scratchExtra = X15
scratchTarget = X14

-- | What the allocator may hand out under one convention. A preserved
-- register costs a C function a save and a restore; an aihc function
-- preserves nothing and pays nothing.
registersFor :: CallingConvention -> Registers Arm64Register
registersFor convention =
  Registers
    { registersVolatile = volatileRegisters,
      registersPreserved = preservedRegisters,
      registersPreservedCost = convention == CConvention,
      registersArgument = argument,
      registersResult = argument
    }
  where
    argument index
      | index < length argumentRegisters = Just (argumentRegisters !! index)
      | otherwise = Nothing

overflowBytes :: Int -> Int
overflowBytes count = ((max 0 (count - length argumentRegisters) * 8 + 15) `div` 16) * 16

compileFunction :: Map Symbol Signature -> Int -> Function -> M [Arm64Statement]
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
      unsupported ("function " <> unSymbol (functionName function) <> " has more than eight integer C parameters")
    when (length floats > floatArgumentCount) $
      unsupported ("function " <> unSymbol (functionName function) <> " has more than eight float C parameters")
  prologue <- functionPrologue ctx
  body <- concat <$> mapM (compileBlock ctx) (zip3 (True : repeat False) blocks (map Just (drop 1 blocks) <> [Nothing]))
  pure
    ( [arm64Section TextSection, arm64Align 2]
        <> [arm64Global symbol | functionLinkage function == Export]
        <> [arm64Label symbol]
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
                -- held the previous contents of the slot is stale. A store
                -- of what the slot already holds changes nothing.
                StoresSlot register offset
                  | IntMap.lookup register held == Just (FromSlot offset) -> go held rest
                  | otherwise ->
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
-- a slot at all; the rest of the frame holds the saved preserved registers
-- and the stack allocations, in that order. The stack allocations stay last
-- so that no address that escapes into a register can reach a slot, which
-- is what 'elideSlotReloads' relies on.
--
-- A function has a frame when it has anything to put in one or when it
-- calls: a call overwrites the link register, which the frame pointer pair
-- keeps. An aihc function that spills nothing and calls nothing has none.
functionLayout :: Map Symbol Signature -> Function -> M Layout
functionLayout signatures function = do
  let blocks = functionBlocks function
      convention = functionConvention function
      allocation = allocateRegistersFor (registersFor convention) signatures function
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
  when (size > 32000) $ unsupported ("function " <> unSymbol (functionName function) <> " needs a frame larger than 32000 bytes")
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

-- | Save the frame pointer pair, reserve the frame, save the preserved
-- registers, zero the stack allocations, and move the parameters into their
-- homes. A parameter the allocator left in its argument register costs
-- nothing.
functionPrologue :: Ctx -> M [Arm64Statement]
functionPrologue ctx = do
  let parameters = functionParameters function
      moves =
        case functionConvention function of
          AihcConvention ->
            parallelMove
              [ (home ctx var, SourceLocation (parameterLocation index))
              | (index, (var, _)) <- zip [0 ..] parameters
              ]
          -- The C convention counts the integer and the float class
          -- separately, so a parameter takes the register at its position
          -- within its own class. A float moves through a scratch register
          -- rather than the argument register of the same number, which may
          -- hold an integer parameter of its own.
          CConvention ->
            let (integers, floats) = classify (map snd parameters)
                names = map fst parameters
             in concat [canonicalizeRegister ty register | ((_, ty), register) <- zip integers argumentRegisters]
                  <> parallelMove [(home ctx (names !! index), SourceLocation (LocRegister register)) | ((index, _), register) <- zip integers argumentRegisters]
                  <> concat
                    [ [arm64Instruction (ArmFmovFromFloat (ty == F64) scratchLeft slot)]
                        <> canonicalizeRegister ty scratchLeft
                        <> parallelMove [(home ctx (names !! index), SourceLocation (LocRegister scratchLeft))]
                    | ((index, ty), slot) <- zip floats [0 ..]
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
          [ arm64Instruction (ArmStp X29 X30 (Arm64PreIndex SP (-16))),
            arm64Instruction (ArmMov X29 (Arm64RegisterValue SP))
          ]
            <> adjustStack ArmSub (layoutSize layout)
      | otherwise = []
    parameterLocation index
      | index < length argumentRegisters = LocRegister (argumentRegisters !! index)
      -- The overflow block sits above the frame.
      | otherwise = LocSlot (frameBytes layout + 8 * (index - length argumentRegisters))
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
  | otherwise = [immediate scratchExtra bytes, arm64Instruction (operation SP SP (Arm64RegisterValue scratchExtra))]

-- | Give back the frame: restore the preserved registers, then the saved
-- pair. The stack pointer ends where it was at entry. The displacement is
-- the number of bytes the stack pointer currently sits below the frame.
leaveFrame :: Ctx -> Int -> [Arm64Statement]
leaveFrame ctx displacement =
  restoreRegisters ctx displacement
    <> ( if layoutFramed layout
           then
             [ arm64Instruction (ArmMov SP (Arm64RegisterValue X29)),
               arm64Instruction (ArmLdp X29 X30 (Arm64PostIndex SP 16))
             ]
           else adjustStack ArmAdd displacement
       )
  where
    layout = ctxLayout ctx

-- | Save the preserved registers the function uses into the frame. The
-- prologue runs this before it moves any parameter into a register.
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

-- Values

-- | Where a value lives: a register, or a frame slot at a byte offset above
-- the stack pointer after the prologue.
data Location
  = LocRegister !Arm64Register
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
        Nothing -> error ("Aihc.Arm64.Lir: unknown value " <> T.unpack (unVar var))

-- | The source of a move that reads an operand of a type.
operandSource :: Ctx -> Type -> Operand -> MoveSource
operandSource ctx ty operand =
  case operand of
    OperandVar var -> SourceLocation (home ctx var)
    OperandLiteral literal -> SourceLiteral ty literal

-- | Read an operand for an instruction: the register that holds it, or the
-- scratch register it was loaded into. The displacement is the number of
-- bytes the stack pointer currently sits below the frame.
operandIn :: Ctx -> Int -> Type -> Arm64Register -> Operand -> ([Arm64Statement], Arm64Register)
operandIn ctx displacement ty scratch operand =
  case operand of
    OperandVar var ->
      case home ctx var of
        LocRegister register -> ([], register)
        LocSlot offset -> ([loadSlot scratch (offset + displacement)], scratch)
    OperandLiteral literal -> (literalInto ty scratch literal, scratch)

-- | The register an instruction computes a result in, and the store that
-- puts it in its home afterwards when the home is a slot.
resultIn :: Ctx -> Arm64Register -> Var -> (Arm64Register, [Arm64Statement])
resultIn ctx scratch var =
  case home ctx var of
    LocRegister register -> (register, [])
    LocSlot offset -> (scratch, [storeSlot scratch offset])

loadSlot :: Arm64Register -> Int -> Arm64Statement
loadSlot register offset = arm64Instruction (ArmLdr register (Arm64Offset SP (fromIntegral offset)))

storeSlot :: Arm64Register -> Int -> Arm64Statement
storeSlot register offset = arm64Instruction (ArmStr register (Arm64Offset SP (fromIntegral offset)))

-- | Load a literal with the encoding of its type. Float literals need the
-- width of the type, and integer literals are canonical for the type.
literalInto :: Type -> Arm64Register -> Literal -> [Arm64Statement]
literalInto ty register literal =
  case (ty, literal) of
    (F32, LitFloat value) -> [immediate register (toInteger (castFloatToWord32 (double2Float value)))]
    (F32, LitInt value) -> [immediate register (toInteger (castFloatToWord32 (fromInteger value)))]
    (F64, LitInt value) -> [immediate register (toInteger (castDoubleToWord64 (fromInteger value)))]
    (_, LitFloat value) -> [immediate register (toInteger (castDoubleToWord64 value))]
    (_, LitInt value)
      | typeBits ty < 64 -> [immediate register (canonicalInteger ty value)]
      | otherwise -> [immediate register value]
    (_, LitNull) -> [arm64Instruction (ArmMov register (Arm64RegisterValue XZR))]
    (_, LitSymbol symbol) -> address register (lirSymbol symbol)

-- | A literal integer operand that fits the immediate of an arithmetic
-- instruction.
smallImmediate :: Type -> Operand -> Maybe Integer
smallImmediate ty operand =
  case operand of
    OperandLiteral (LitInt value)
      | not (isFloatType ty),
        let canonical = if typeBits ty < 64 then canonicalInteger ty value else value,
        canonical >= 0 && canonical < 4096 ->
          Just canonical
    _ -> Nothing

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

-- | A copy between registers, unless it would copy a register to itself.
move :: Arm64Register -> Arm64Register -> [Arm64Statement]
move destination source
  | destination == source = []
  | otherwise = [arm64Instruction (ArmMov destination (Arm64RegisterValue source))]

tshow :: (Show value) => value -> Text
tshow = T.pack . show

-- Parallel moves

-- | Perform a set of moves as if all at once: every source is read before
-- the destination it lives in is written. A move whose destination nothing
-- else reads goes first; when only cycles remain, the contents of one
-- destination are parked in 'scratchExtra' and the readers of that
-- destination read the parking place instead. A slot-to-slot move goes
-- through 'scratchLeft'.
--
-- The destinations are distinct. A move to the location a source is already
-- in is dropped.
parallelMove :: [(Location, MoveSource)] -> [Arm64Statement]
parallelMove = go . filter (not . identity)
  where
    identity (destination, source) = source == SourceLocation destination
    go [] = []
    go pending =
      case partitionReady pending of
        ([], blocked) ->
          -- Every destination is still read by another move, so the moves
          -- form cycles. Park the destination of the first one.
          case blocked of
            (destination, _) : _ ->
              let park = LocRegister scratchExtra
                  redirect (target, source)
                    | source == SourceLocation destination = (target, SourceLocation park)
                    | otherwise = (target, source)
               in emit (park, SourceLocation destination) <> go (map redirect blocked)
            [] -> []
        (ready, blocked) -> concatMap emit ready <> go blocked
    partitionReady pending =
      let readLocations = [location | (_, SourceLocation location) <- pending]
       in ( [pair | pair@(destination, _) <- pending, destination `notElem` readLocations],
            [pair | pair@(destination, _) <- pending, destination `elem` readLocations]
          )
    emit (destination, source) =
      case (destination, source) of
        (LocRegister target, SourceLocation (LocRegister register)) -> move target register
        (LocRegister target, SourceLocation (LocSlot offset)) -> [loadSlot target offset]
        (LocRegister target, SourceLiteral ty literal) -> literalInto ty target literal
        (LocSlot offset, SourceLocation (LocRegister register)) -> [storeSlot register offset]
        (LocSlot offset, SourceLocation (LocSlot from)) -> [loadSlot scratchLeft from, storeSlot scratchLeft offset]
        (LocSlot offset, SourceLiteral ty literal) -> literalInto ty scratchLeft literal <> [storeSlot scratchLeft offset]

-- | A location displaced by the bytes the stack pointer currently sits
-- below the frame. Only a slot moves.
displace :: Int -> Location -> Location
displace displacement location =
  case location of
    LocSlot offset -> LocSlot (offset + displacement)
    LocRegister _ -> location

displaceSource :: Int -> MoveSource -> MoveSource
displaceSource displacement source =
  case source of
    SourceLocation location -> SourceLocation (displace displacement location)
    SourceLiteral _ _ -> source

-- Blocks

-- | A comparison that the branch of the block consumes directly, so that
-- the block compares and branches on the flags instead of materializing a
-- boolean.
data Fused = Fused !CompareOp !Type !Operand !Operand

-- | Compile one block. The entry block has no label: nothing may jump to
-- it, and without a label the reload pass sees through the prologue into
-- the first instructions.
compileBlock :: Ctx -> (Bool, Block, Maybe Block) -> M [Arm64Statement]
compileBlock ctx (entry, block, next) = do
  let (instructions, fused) = fuseCompare ctx (blockInstructions block) (blockTerminator block)
  lines' <- concat <$> mapM (compileInstruction ctx) instructions
  terminator <- compileTerminator ctx (blockLabel <$> next) fused (blockTerminator block)
  pure ([arm64Label (ctxLabels ctx Map.! blockLabel block) | not entry] <> lines' <> terminator)

-- | Split off a comparison that only the branch of the block reads and that
-- sits right before it. Nothing else reads the boolean, so it is never
-- written.
fuseCompare :: Ctx -> [Instruction] -> Terminator -> ([Instruction], Maybe Fused)
fuseCompare ctx instructions terminator =
  case (reverse instructions, terminator) of
    (Instruction [var] (Compare op ty left right) : before, Branch (OperandVar condition) _ _)
      | condition == var,
        Map.lookup var (ctxReads ctx) == Just 1 ->
          (reverse before, Just (Fused op ty left right))
    _ -> (instructions, Nothing)

-- | The test a block branches on: a register that holds the condition when
-- it is not zero, a register that holds it when it is zero, or the flags.
data Test
  = TestNonZero !Arm64Register
  | TestZero !Arm64Register
  | TestFlags !Arm64Condition

-- | Set up the test of a branch condition: the fused comparison when there
-- is one, otherwise the boolean itself.
conditionTest :: Ctx -> Maybe Fused -> Operand -> ([Arm64Statement], Test)
conditionTest ctx fused condition =
  case fused of
    Just (Fused op ty left right)
      | op `elem` [Eq, Ne],
        not (isFloatType ty),
        isZero right ->
          let (loads, register) = operandIn ctx 0 ty scratchLeft left
           in (loads, if op == Eq then TestZero register else TestNonZero register)
      | isFloatType ty ->
          let (leftLoads, leftRegister) = operandIn ctx 0 ty scratchLeft left
              (rightLoads, rightRegister) = operandIn ctx 0 ty scratchRight right
           in ( leftLoads
                  <> rightLoads
                  <> [toFloat ty 16 leftRegister, toFloat ty 17 rightRegister, arm64Instruction (ArmFcmp (ty == F64) 16 17)],
                TestFlags (floatCondition op)
              )
      | otherwise ->
          let (loads, register) = operandIn ctx 0 ty scratchLeft left
              signed = op `elem` [LtS, LeS, GtS, GeS]
              (extendLeft, leftRegister) = if signed then signExtendInto ty scratchLeft register else ([], register)
           in (loads <> extendLeft <> compareWith ctx ty signed leftRegister right, TestFlags (integerCondition op))
    Nothing ->
      let (loads, register) = operandIn ctx 0 I1 scratchLeft condition
       in (loads, TestNonZero register)
  where
    isZero operand = operand == OperandLiteral (LitInt 0)

-- | Compare a register with an operand: against an immediate when the
-- operand is a small literal, otherwise against a register. The right
-- operand is sign-extended into the right scratch register when asked.
compareWith :: Ctx -> Type -> Bool -> Arm64Register -> Operand -> [Arm64Statement]
compareWith ctx ty signed left right =
  case smallImmediate ty right of
    Just value
      | not signed || typeBits ty >= 64 || value < 2 ^ (typeBits ty - 1) ->
          [arm64Instruction (ArmCmp left (Arm64ImmediateValue value))]
    _ ->
      let (loads, register) = operandIn ctx 0 ty scratchRight right
          (extend, rightRegister) = if signed then signExtendInto ty scratchRight register else ([], register)
       in loads <> extend <> [arm64Instruction (ArmCmp left (Arm64RegisterValue rightRegister))]

-- | Branch to a label when the test fails, or when it holds.
branchUnless, branchWhen :: Test -> Text -> Arm64Statement
branchUnless test label =
  case test of
    TestNonZero register -> arm64Instruction (ArmCbz register label)
    TestZero register -> arm64Instruction (ArmCbnz register label)
    TestFlags condition -> arm64Instruction (ArmBCond (inverseCondition condition) label)
branchWhen test label =
  case test of
    TestNonZero register -> arm64Instruction (ArmCbnz register label)
    TestZero register -> arm64Instruction (ArmCbz register label)
    TestFlags condition -> arm64Instruction (ArmBCond condition label)

compileTerminator :: Ctx -> Maybe Label -> Maybe Fused -> Terminator -> M [Arm64Statement]
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
        then pure (setup <> [branchWhen test (labelOf whenTrue)])
        else do
          falseLabel <- if null falseMoves then pure (labelOf whenFalse) else freshLabel "else"
          pure
            ( setup
                <> [branchUnless test falseLabel]
                <> trueMoves
                <> [arm64Instruction (ArmB (labelOf whenTrue)) | not (null falseMoves) || not (isNext whenTrue)]
                <> (if null falseMoves then [] else arm64Label falseLabel : falseMoves <> branchTo whenFalse)
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
            pure [arm64Instruction (ArmB stub)]
      let checks =
            concat
              [ compareWith ctx ty False register (OperandLiteral (LitInt (switchCaseValue switchCase))) <> [arm64Instruction (ArmBCond ArmEq label)]
              | (switchCase, label, _) <- edges
              ]
          bodies =
            concat
              [ arm64Label label : moves <> [arm64Instruction (ArmB (labelOf (switchCaseTarget switchCase)))]
              | (switchCase, label, moves) <- edges,
                not (null moves)
              ]
      pure (loads <> checks <> fallbackLines <> bodies)
    Return values -> do
      when (length values > length argumentRegisters) $ unsupported "return of more than eight values"
      let moves =
            parallelMove
              [ (LocRegister register, operandSource ctx ty value)
              | (ty, register, value) <- zip3 (functionResults function) argumentRegisters values
              ]
          floatMoves =
            case (functionConvention function, functionResults function) of
              (CConvention, [ty]) | isFloatType ty -> [arm64Instruction (ArmFmovToFloat (ty == F64) 0 X0)]
              _ -> []
      pure (moves <> floatMoves <> leaveFrame ctx 0 <> adjustStack ArmAdd (ctxIncomingOverflow ctx) <> [arm64Instruction ArmRet])
    TailCall symbol arguments ->
      tailCall (Left (lirSymbol symbol)) (maybe [] signatureParameters (Map.lookup symbol (ctxSignatures ctx))) arguments
    TailCallIndirect target arguments signature -> tailCall (Right target) (signatureParameters signature) arguments
    Trap message -> do
      stub <- trapLabel message
      pure [arm64Instruction (ArmB stub)]
  where
    function = ctxFunction ctx
    layout = ctxLayout ctx
    labelOf target = ctxLabels ctx Map.! targetLabel target
    isNext target = Just (targetLabel target) == next
    branchTo target = [arm64Instruction (ArmB (labelOf target)) | not (isNext target)]
    -- The outgoing block replaces the incoming one. When it is no larger,
    -- it is written in place above the frame; when it is larger and the
    -- function has no frame, the stack pointer just moves down to make
    -- room; when it is larger and the function has a frame, it is built
    -- below the frame and copied up once the frame is gone.
    tailCall callee parameterTypes arguments = do
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
            Left label -> arm64Instruction (ArmB label)
            Right _ -> arm64Instruction (ArmBr scratchTarget)
      stub <-
        case callee of
          Left _ -> pure Nothing
          Right _ -> Just <$> trapLabel "indirect call to a non-function"
      let targetLoad displacement =
            case (callee, stub) of
              (Right operand, Just label) ->
                let (loads, register) = operandIn ctx displacement Code scratchTarget operand
                 in loads <> move scratchTarget register <> [arm64Instruction (ArmCbz scratchTarget label)]
              _ -> []
      pure $
        if outgoing <= incoming
          then
            overflowStores 0 (frameBytes layout + incoming - outgoing)
              <> targetLoad 0
              <> registerMoves 0
              <> leaveFrame ctx 0
              <> adjustStack ArmAdd (incoming - outgoing)
              <> [branch]
          else
            if not (layoutFramed layout)
              then
                adjustStack ArmSub (outgoing - incoming)
                  <> overflowStores 0 0
                  <> targetLoad 0
                  <> registerMoves 0
                  <> [branch]
              else
                adjustStack ArmSub outgoing
                  <> overflowStores outgoing 0
                  <> targetLoad outgoing
                  <> registerMoves outgoing
                  <> restoreRegisters ctx outgoing
                  -- Restore the saved pair, then copy the outgoing block to
                  -- its final place just below the stack of the caller,
                  -- highest word first because the destination lies above
                  -- the source.
                  <> [ arm64Instruction (ArmMov scratchExtra (Arm64RegisterValue X29)),
                       arm64Instruction (ArmLdp X29 X30 (Arm64Offset scratchExtra 0))
                     ]
                  <> destination (16 + incoming - outgoing)
                  <> concat
                    [ [ loadSlot scratchRight (8 * position),
                        arm64Instruction (ArmStr scratchRight (Arm64Offset scratchLeft (fromIntegral (8 * position))))
                      ]
                    | position <- reverse [0 .. length overflow - 1]
                    ]
                  <> [arm64Instruction (ArmMov SP (Arm64RegisterValue scratchLeft)), branch]
    destination delta
      | delta >= 0 = [arm64Instruction (ArmAdd scratchLeft scratchExtra (Arm64ImmediateValue (fromIntegral delta)))]
      | otherwise = [arm64Instruction (ArmSub scratchLeft scratchExtra (Arm64ImmediateValue (fromIntegral (negate delta))))]

-- | Move the arguments of a jump into the parameters of the target, all at
-- once.
blockArgumentMoves :: Ctx -> Target -> M [Arm64Statement]
blockArgumentMoves ctx (Target label arguments) = do
  let parameters = Map.findWithDefault [] label (ctxBlockParameters ctx)
  pure
    ( parallelMove
        [ (home ctx var, operandSource ctx ty argument)
        | ((var, ty), argument) <- zip parameters arguments
        ]
    )

-- Instructions

compileInstruction :: Ctx -> Instruction -> M [Arm64Statement]
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
        pure (loads <> loads' <> [toFloat ty 16 a, toFloat ty 17 b, arm64Instruction (ArmFloat (floatOp op) (ty == F64) 16 16 17), fromFloat ty dst 16])
    FloatUnary op ty value -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft value
      single $ \dst -> pure (loads <> [toFloat ty 16 a, arm64Instruction (ArmFloat (floatUnaryOp op) (ty == F64) 16 16 16), fromFloat ty dst 16])
    Convert op from value to -> do
      let (loads, a) = operandIn ctx 0 from scratchLeft value
      single $ \dst -> do
        body <- convert op from to dst a
        pure (loads <> body)
    PtrToInt value -> copy value
    PtrFromInt value -> copy value
    Select ty condition left right -> do
      let (conditionLoads, c) = operandIn ctx 0 I1 scratchExtra condition
          (loads, a) = operandIn ctx 0 ty scratchLeft left
          (loads', b) = operandIn ctx 0 ty scratchRight right
      single $ \dst ->
        pure (conditionLoads <> loads <> loads' <> [arm64Instruction (ArmCmp c (Arm64ImmediateValue 0)), arm64Instruction (ArmCsel dst a b ArmNe)])
    Load ty (Address base offset) _ -> do
      let (addressLines, baseRegister) = effectiveAddress base offset ty
      single $ \dst -> pure (addressLines <> [loadMemory ty dst baseRegister (memoryOffset offset ty)])
    Store ty value (Address base offset) _ -> do
      let (loads, a) = operandIn ctx 0 ty scratchLeft value
          (addressLines, baseRegister) = effectiveAddress base offset ty
      pure (loads <> addressLines <> [storeMemory ty a baseRegister (memoryOffset offset ty)])
    PtrAdd base offset -> do
      let (loads, a) = operandIn ctx 0 Ptr scratchLeft base
      single $ \dst ->
        pure
          ( loads
              <> case smallImmediate I64 offset of
                Just value -> [arm64Instruction (ArmAdd dst a (Arm64ImmediateValue value))]
                Nothing ->
                  let (loads', b) = operandIn ctx 0 I64 scratchRight offset
                   in loads' <> [arm64Instruction (ArmAdd dst a (Arm64RegisterValue b))]
          )
    StackAlloc _ _ ->
      case results of
        [var]
          | Just (offset, _) <- Map.lookup var (layoutAllocs (ctxLayout ctx)) ->
              single $ \dst ->
                pure
                  ( if offset < 4096
                      then [arm64Instruction (ArmAdd dst SP (Arm64ImmediateValue (fromIntegral offset)))]
                      else [immediate scratchExtra offset, arm64Instruction (ArmAdd dst SP (Arm64RegisterValue scratchExtra))]
                  )
        _ -> unsupported "stack.alloc without a placed result"
    GlobalGet symbol ->
      single $ \dst -> pure (address scratchRight (lirSymbol symbol) <> [arm64Instruction (ArmLdr dst (Arm64Offset scratchRight 0))])
    GlobalSet symbol value -> do
      let (loads, a) = operandIn ctx 0 I64 scratchLeft value
      pure (loads <> address scratchRight (lirSymbol symbol) <> [arm64Instruction (ArmStr a (Arm64Offset scratchRight 0))])
    Call symbol arguments -> call (Left symbol) arguments
    CallIndirect target arguments signature -> callIndirect target arguments signature
  where
    -- The result register of an instruction with one result, and the store
    -- that follows when the result lives in a slot. A result never aliases
    -- an operand of its own instruction unless the instruction reads every
    -- operand before it writes, which every body below does.
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
    copy value = do
      let (loads, a) = operandIn ctx 0 Ptr scratchLeft value
      single $ \dst -> pure (loads <> move dst a)

    -- The offset of a load or a store: folded into the instruction when the
    -- scaled immediate form permits it, otherwise added to the base.
    memoryOffset offset ty
      | fitsScaled offset ty = fromInteger offset
      | otherwise = 0
    fitsScaled offset ty =
      let size = toInteger (typeBytes ty)
       in offset >= 0 && offset `mod` size == 0 && offset `div` size < 4096
    effectiveAddress base offset ty =
      let (loads, register) = operandIn ctx 0 Ptr scratchRight base
       in if fitsScaled offset ty
            then (loads, register)
            else (loads <> [immediate scratchExtra offset, arm64Instruction (ArmAdd scratchRight register (Arm64RegisterValue scratchExtra))], scratchRight)

    -- A narrow value is zero-extended in its register, so a leading-zero
    -- count includes the bits above the type and a trailing-zero count of
    -- zero would reach the top of the register. Setting the first bit above
    -- the type keeps the trailing count at the width of the type.
    bitCount op ty dst a =
      let bits = typeBits ty
       in case op of
            Popcount ->
              [ toFloat F64 16 a,
                arm64Instruction (ArmCnt 16 16),
                arm64Instruction (ArmAddv 16 16),
                fromFloat F64 dst 16
              ]
            Clz ->
              arm64Instruction (ArmClz dst a)
                : [arm64Instruction (ArmSub dst dst (Arm64ImmediateValue (toInteger (64 - bits)))) | bits < 64]
            Ctz
              | bits < 64 ->
                  [ immediate scratchExtra (2 ^ bits :: Integer),
                    arm64Instruction (ArmOrr dst a (Arm64RegisterValue scratchExtra)),
                    arm64Instruction (ArmRbit dst dst),
                    arm64Instruction (ArmClz dst dst)
                  ]
              | otherwise -> [arm64Instruction (ArmRbit dst a), arm64Instruction (ArmClz dst dst)]

    -- The right operand of a binary instruction: an immediate when the
    -- literal fits, otherwise a register.
    rightValue ty right =
      case smallImmediate ty right of
        Just value -> ([], Arm64ImmediateValue value)
        Nothing ->
          let (loads, b) = operandIn ctx 0 ty scratchRight right
           in (loads, Arm64RegisterValue b)
    rightRegister ty = operandIn ctx 0 ty scratchRight

    binary op ty dst a right =
      case op of
        Add ->
          let (loads, b) = rightValue ty right
           in pure (loads <> narrow ty dst [arm64Instruction (ArmAdd dst a b)])
        Sub ->
          let (loads, b) = rightValue ty right
           in pure (loads <> narrow ty dst [arm64Instruction (ArmSub dst a b)])
        Mul ->
          let (loads, b) = rightRegister ty right
           in pure (loads <> narrow ty dst [arm64Instruction (ArmMul dst a b)])
        DivS -> do
          zero <- trapLabel "integer division by zero"
          overflow <- trapLabel "integer overflow"
          skip <- freshLabel "div"
          let (loads, b) = rightRegister ty right
              (extendLeft, a') = signExtendInto ty scratchLeft a
              (extendRight, b') = signExtendInto ty scratchRight b
          pure
            ( loads
                <> extendLeft
                <> extendRight
                <> [ arm64Instruction (ArmCbz b' zero),
                     immediate scratchExtra (-1 :: Integer),
                     arm64Instruction (ArmCmp b' (Arm64RegisterValue scratchExtra)),
                     arm64Instruction (ArmBCond ArmNe skip),
                     immediate scratchExtra (minimumSigned ty),
                     arm64Instruction (ArmCmp a' (Arm64RegisterValue scratchExtra)),
                     arm64Instruction (ArmBCond ArmEq overflow),
                     arm64Label skip
                   ]
                <> narrow ty dst [arm64Instruction (ArmSdiv dst a' b')]
            )
        DivU -> do
          zero <- trapLabel "integer division by zero"
          let (loads, b) = rightRegister ty right
          pure (loads <> [arm64Instruction (ArmCbz b zero), arm64Instruction (ArmUdiv dst a b)])
        RemS -> do
          zero <- trapLabel "integer division by zero"
          let (loads, b) = rightRegister ty right
              (extendLeft, a') = signExtendInto ty scratchLeft a
              (extendRight, b') = signExtendInto ty scratchRight b
          pure
            ( loads
                <> extendLeft
                <> extendRight
                <> [arm64Instruction (ArmCbz b' zero), arm64Instruction (ArmSdiv scratchExtra a' b')]
                <> narrow ty dst [arm64Instruction (ArmMsub dst scratchExtra b' a')]
            )
        RemU -> do
          zero <- trapLabel "integer division by zero"
          let (loads, b) = rightRegister ty right
          pure (loads <> [arm64Instruction (ArmCbz b zero), arm64Instruction (ArmUdiv scratchExtra a b), arm64Instruction (ArmMsub dst scratchExtra b a)])
        And ->
          let (loads, b) = rightRegister ty right
           in pure (loads <> [arm64Instruction (ArmAnd dst a b)])
        Or ->
          let (loads, b) = rightRegister ty right
           in pure (loads <> [arm64Instruction (ArmOrr dst a (Arm64RegisterValue b))])
        Xor ->
          let (loads, b) = rightRegister ty right
           in pure (loads <> [arm64Instruction (ArmEor dst a b)])
        Shl ->
          let (loads, b) = rightRegister ty right
              (mask, b') = shiftCount ty b
           in pure (loads <> mask <> narrow ty dst [arm64Instruction (ArmLsl dst a (Arm64RegisterShift b'))])
        ShrS ->
          let (loads, b) = rightRegister ty right
              (extendLeft, a') = signExtendInto ty scratchLeft a
              (mask, b') = shiftCount ty b
           in pure (loads <> extendLeft <> mask <> narrow ty dst [arm64Instruction (ArmAsr dst a' (Arm64RegisterShift b'))])
        ShrU ->
          let (loads, b) = rightRegister ty right
              (mask, b') = shiftCount ty b
           in pure (loads <> mask <> [arm64Instruction (ArmLsr dst a (Arm64RegisterShift b'))])

    -- Every body reads both operands before it writes either result, and
    -- computes through 'scratchExtra' when it needs a third register.
    wide op ty low high a b =
      case op of
        MulWideU
          | typeBits ty == 64 ->
              [arm64Instruction (ArmUmulh scratchExtra a b), arm64Instruction (ArmMul low a b)] <> move high scratchExtra
          | otherwise ->
              [arm64Instruction (ArmMul scratchExtra a b), arm64Instruction (ArmLsr high scratchExtra (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
                <> narrowRegister ty high
                <> move low scratchExtra
                <> narrowRegister ty low
        MulWideS
          | typeBits ty == 64 ->
              [arm64Instruction (ArmSmulh scratchExtra a b), arm64Instruction (ArmMul low a b)] <> move high scratchExtra
          | otherwise ->
              let (extendLeft, a') = signExtendInto ty scratchLeft a
                  (extendRight, b') = signExtendInto ty scratchRight b
               in extendLeft
                    <> extendRight
                    <> [arm64Instruction (ArmMul scratchExtra a' b'), arm64Instruction (ArmAsr high scratchExtra (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
                    <> narrowRegister ty high
                    <> move low scratchExtra
                    <> narrowRegister ty low
        AddCarry
          | typeBits ty == 64 -> [arm64Instruction (ArmAdds low a (Arm64RegisterValue b)), arm64Instruction (ArmCset high ArmCs)]
          | otherwise ->
              [arm64Instruction (ArmAdd low a (Arm64RegisterValue b)), arm64Instruction (ArmLsr high low (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
                <> narrowRegister ty low
        SubBorrow
          | typeBits ty == 64 -> [arm64Instruction (ArmSubs low a (Arm64RegisterValue b)), arm64Instruction (ArmCset high ArmCc)]
          | otherwise ->
              [ arm64Instruction (ArmCmp a (Arm64RegisterValue b)),
                arm64Instruction (ArmCset scratchExtra ArmCc),
                arm64Instruction (ArmSub low a (Arm64RegisterValue b))
              ]
                <> narrowRegister ty low
                <> move high scratchExtra

    compare' op ty dst left right
      | isFloatType ty =
          let (loads, a) = operandIn ctx 0 ty scratchLeft left
              (loads', b) = operandIn ctx 0 ty scratchRight right
           in loads <> loads' <> [toFloat ty 16 a, toFloat ty 17 b, arm64Instruction (ArmFcmp (ty == F64) 16 17), arm64Instruction (ArmCset dst (floatCondition op))]
      | otherwise =
          let signed = op `elem` [LtS, LeS, GtS, GeS]
              (loads, a) = operandIn ctx 0 ty scratchLeft left
              (extendLeft, a') = if signed then signExtendInto ty scratchLeft a else ([], a)
           in loads <> extendLeft <> compareWith ctx ty signed a' right <> [arm64Instruction (ArmCset dst (integerCondition op))]

    convert op from to dst a =
      case op of
        SExt -> pure (signExtendTo dst from a <> narrowRegister to dst)
        ZExt -> pure (move dst a)
        Trunc -> pure (truncateTo dst to a)
        IToFS ->
          let (extend, a') = signExtendInto from scratchLeft a
           in pure (extend <> [arm64Instruction (ArmScvtf (to == F64) 16 a'), fromFloat to dst 16])
        IToFU -> pure [arm64Instruction (ArmUcvtf (to == F64) 16 a), fromFloat to dst 16]
        FToIS -> floatToInteger True from to dst a
        FToIU -> floatToInteger False from to dst a
        FpExt -> pure [arm64Instruction (ArmFmovToFloat False 16 (wordRegister a)), arm64Instruction (ArmFcvt True 16 16), arm64Instruction (ArmFmovFromFloat True dst 16)]
        FpTrunc -> pure [arm64Instruction (ArmFmovToFloat True 16 a), arm64Instruction (ArmFcvt False 16 16), arm64Instruction (ArmFmovFromFloat False (wordRegister dst) 16)]
        Bitcast -> pure (move dst a)

    -- Widen the source to double, reject NaN and values outside the target
    -- range, then convert with rounding toward zero.
    floatToInteger signed from to dst a = do
      invalid <- trapLabel "invalid float to integer conversion"
      let widen = if from == F64 then [arm64Instruction (ArmFmovToFloat True 16 a)] else [arm64Instruction (ArmFmovToFloat False 16 (wordRegister a)), arm64Instruction (ArmFcvt True 16 16)]
          bits = typeBits to
          lower = if signed then negate (2 ^^ (bits - 1)) else -1 :: Double
          upper = if signed then 2 ^^ (bits - 1) else 2 ^^ bits :: Double
          lowerCondition = if signed then ArmMi else ArmLe
          convertOp = if signed then ArmFcvtzs True dst 16 else ArmFcvtzu True dst 16
      pure
        ( widen
            <> [ arm64Instruction (ArmFcmp True 16 16),
                 arm64Instruction (ArmBCond ArmVs invalid),
                 immediate scratchExtra (toInteger (castDoubleToWord64 lower)),
                 arm64Instruction (ArmFmovToFloat True 17 scratchExtra),
                 arm64Instruction (ArmFcmp True 16 17),
                 arm64Instruction (ArmBCond lowerCondition invalid),
                 immediate scratchExtra (toInteger (castDoubleToWord64 upper)),
                 arm64Instruction (ArmFmovToFloat True 17 scratchExtra),
                 arm64Instruction (ArmFcmp True 16 17),
                 arm64Instruction (ArmBCond ArmGe invalid),
                 arm64Instruction convertOp
               ]
            <> narrowRegister to dst
        )

    -- A shift count of a narrow type wraps at the width of the type.
    shiftCount ty b
      | typeBits ty == 64 = ([], b)
      | otherwise = ([arm64Instruction (ArmAndMask scratchRight b (log2 (toInteger (typeBits ty))))], scratchRight)
    narrow ty dst body = body <> narrowRegister ty dst

    call callee arguments = do
      let (convention, resultTypes, parameterTypes) = calleeSignature callee
      let outgoing = case convention of
            CConvention -> 0
            AihcConvention -> overflowBytes (length arguments)
          types = parameterTypes <> repeat I64
          branch = case callee of
            Left symbol -> [arm64Instruction (ArmBl (lirSymbol symbol))]
            Right _ -> [arm64Instruction (ArmBlr scratchTarget)]
          resultMoves =
            concat [floatResult convention ty register <> canonicalResult convention ty register | (ty, register) <- zip resultTypes argumentRegisters]
              <> parallelMove [(home ctx var, SourceLocation (LocRegister register)) | (var, register) <- zip results argumentRegisters]
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
          CConvention -> do
            let (integers, floats) = classify (take (length arguments) types)
            when (length integers > length argumentRegisters) $ unsupported "C call with more than eight integer arguments"
            when (length floats > floatArgumentCount) $ unsupported "C call with more than eight float arguments"
            -- The floats go first: a float register is never a home, while
            -- the integer moves may overwrite one.
            pure
              ( concat
                  [ loads <> [arm64Instruction (ArmFmovToFloat (ty == F64) slot register)]
                  | ((index, ty), slot) <- zip floats [0 ..],
                    let (loads, register) = operandIn ctx 0 ty scratchLeft (arguments !! index)
                  ]
                  <> parallelMove [(LocRegister register, operandSource ctx ty (arguments !! index)) | ((index, ty), register) <- zip integers argumentRegisters]
              )
      pure (adjustStack ArmSub outgoing <> argumentMoves <> branch <> resultMoves)
    callIndirect target arguments signature = do
      stub <- trapLabel "indirect call to a non-function"
      body <- call (Right signature) arguments
      let (loads, register) = operandIn ctx 0 Code scratchTarget target
      pure (loads <> move scratchTarget register <> [arm64Instruction (ArmCbz scratchTarget stub)] <> body)
    calleeSignature callee =
      case callee of
        Left symbol ->
          case Map.lookup symbol (ctxSignatures ctx) of
            Just signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)
            Nothing -> (AihcConvention, [], [])
        Right signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)
    floatResult convention ty register =
      case convention of
        CConvention | isFloatType ty -> [arm64Instruction (ArmFmovFromFloat (ty == F64) register 0)]
        _ -> []
    canonicalResult convention ty register =
      case convention of
        CConvention -> canonicalizeRegister ty register
        AihcConvention -> []

minimumSigned :: Type -> Integer
minimumSigned ty = negate (2 ^ (typeBits ty - 1))

-- | Mask a register to the width of a narrow type.
narrowRegister :: Type -> Arm64Register -> [Arm64Statement]
narrowRegister ty register
  | typeBits ty >= 64 = []
  | otherwise = [arm64Instruction (ArmAndMask register register (typeBits ty))]

-- | Sign-extend a canonical narrow value into a register. A 64-bit value is
-- copied.
signExtendTo :: Arm64Register -> Type -> Arm64Register -> [Arm64Statement]
signExtendTo destination ty source =
  case ty of
    I1 -> [arm64Instruction (ArmSub destination XZR (Arm64RegisterValue source))]
    I8 -> [arm64Instruction (ArmSxtb destination source)]
    I16 -> [arm64Instruction (ArmSxth destination source)]
    I32 -> [arm64Instruction (ArmSxtw destination source)]
    _ -> move destination source

-- | Sign-extend a canonical narrow value into the scratch register, and say
-- which register now holds the extended value. A 64-bit value stays where
-- it is.
signExtendInto :: Type -> Arm64Register -> Arm64Register -> ([Arm64Statement], Arm64Register)
signExtendInto ty scratch source
  | typeBits ty >= 64 = ([], source)
  | otherwise = (signExtendTo scratch ty source, scratch)

-- | Mask a value to the width of a narrow type into a register. A 64-bit
-- value is copied.
truncateTo :: Arm64Register -> Type -> Arm64Register -> [Arm64Statement]
truncateTo destination ty source
  | typeBits ty >= 64 = move destination source
  | otherwise = [arm64Instruction (ArmAndMask destination source (typeBits ty))]

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

-- | The condition that holds exactly when the given one does not. The
-- unordered case flips with it, so a fused float branch falls the same way
-- as the boolean it replaces.
inverseCondition :: Arm64Condition -> Arm64Condition
inverseCondition condition =
  case condition of
    ArmEq -> ArmNe
    ArmNe -> ArmEq
    ArmCs -> ArmCc
    ArmCc -> ArmCs
    ArmMi -> ArmPl
    ArmPl -> ArmMi
    ArmVs -> ArmVc
    ArmVc -> ArmVs
    ArmHi -> ArmLs
    ArmLs -> ArmHi
    ArmGe -> ArmLt
    ArmLt -> ArmGe
    ArmGt -> ArmLe
    ArmLe -> ArmGt

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
