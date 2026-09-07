-- | Reference interpreter for Lir. It implements the semantics in
-- @docs/lir.md@ with a 64-bit word and a flat address space.
module Aihc.Lir.Interpret
  ( Value (..),
    InterpretError (..),
    runFunction,
    renderValue,
    renderValues,
    renderInterpretError,
  )
where

import Aihc.Lir.Pretty (prettySymbol, renderDoc)
import Aihc.Lir.Resolve (resolveConstants, resolvedSwitchCaseValue)
import Aihc.Lir.Syntax
import Control.Monad (foldM, unless, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify', put)
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble, double2Float, float2Double)
import Numeric (showHex)

-- | Integers hold their bits in the low @N@ bits of the word.
data Value
  = VInt !Word64
  | VF32 !Float
  | VF64 !Double
  | VPtr !Word64
  | -- | The address of a function. Code addresses are not readable memory.
    VCode !Word64
  deriving (Eq, Show)

data InterpretError
  = -- | The program stopped with a trap message.
    InterpretTrap !Text
  | -- | The interpreter cannot execute the program.
    InterpretFailure !Text
  deriving (Eq, Show)

renderInterpretError :: InterpretError -> Text
renderInterpretError err =
  case err of
    InterpretTrap message -> "trap: " <> message
    InterpretFailure message -> "interpreter failure: " <> message

data Program = Program
  { programFunctions :: !(Map Symbol Function),
    programSignatures :: !(Map Symbol Signature),
    programGlobalTypes :: !(Map Symbol Type),
    programAddresses :: !(Map Symbol Word64),
    programCode :: !(Map Word64 Symbol),
    programReadOnly :: ![(Word64, Word64)]
  }

data Machine = Machine
  { machineMemory :: !(IntMap Word8),
    machineGlobals :: !(Map Symbol Value),
    machineStack :: !Word64
  }

type M = StateT Machine (Either InterpretError)

type Locals = Map Var Value

data Outcome
  = Returned ![Value]
  | TailCalled !Symbol ![Value]

codeBase, codeStride, dataBase, stackBase, stackLimit :: Word64
codeBase = 0x1000
codeStride = 16
dataBase = 0x10000
stackBase = 0x10000000
stackLimit = stackBase + 0x100000

trap :: Text -> M a
trap = lift . Left . InterpretTrap

failure :: Text -> M a
failure = lift . Left . InterpretFailure

-- | Execute a function of the module with the given arguments.
runFunction :: Module -> Symbol -> [Value] -> Either InterpretError [Value]
runFunction lirModule entry arguments = do
  (program, machine) <- buildProgram (resolveConstants lirModule)
  evalStateT (callFunction program entry arguments) machine

-- Program setup

buildProgram :: Module -> Either InterpretError (Program, Machine)
buildProgram (Module items) = do
  let functions = Map.fromList [(functionName function, function) | ItemFunction function <- items]
      signatures =
        Map.fromList
          ( [(functionName function, functionSignature function) | ItemFunction function <- items]
              <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
          )
      codeSymbols = [symbol | item <- items, Just symbol <- [codeSymbol item]]
      codeAddresses = zip codeSymbols [codeBase, codeBase + codeStride ..]
      dataItems = [dataItem | ItemData dataItem <- items]
      (dataAddresses, _) = foldl' placeData ([], dataBase) dataItems
      externDataAddresses = zip [symbol | ItemExternData symbol <- items] [codeBase + codeStride * fromIntegral (length codeSymbols) + 8, codeBase + codeStride * fromIntegral (length codeSymbols) + 8 + codeStride ..]
      addresses = Map.fromList (codeAddresses <> dataAddresses <> externDataAddresses)
      program =
        Program
          { programFunctions = functions,
            programSignatures = signatures,
            programGlobalTypes = Map.fromList [(globalName global, globalType global) | ItemGlobal global <- items],
            programAddresses = addresses,
            programCode = Map.fromList [(address, symbol) | (symbol, address) <- codeAddresses],
            programReadOnly =
              [ (address, address + dataSize dataItem)
              | dataItem <- dataItems,
                not (dataMutable dataItem),
                Just address <- [Map.lookup (dataName dataItem) addresses]
              ]
          }
  memory <- foldM (writeData addresses) IntMap.empty [(dataItem, address) | dataItem <- dataItems, Just address <- [Map.lookup (dataName dataItem) addresses]]
  let globals = Map.fromList [(globalName global, zeroValue (globalType global)) | ItemGlobal global <- items]
  pure (program, Machine {machineMemory = memory, machineGlobals = globals, machineStack = stackBase})
  where
    codeSymbol item =
      case item of
        ItemFunction function -> Just (functionName function)
        ItemExternFunction external -> Just (externFunctionName external)
        _ -> Nothing
    placeData (placed, next) dataItem =
      let alignment = fromInteger (max 1 (dataAlignment dataItem))
          address = alignUp alignment next
       in (placed <> [(dataName dataItem, address)], address + dataSize dataItem)

alignUp :: Word64 -> Word64 -> Word64
alignUp alignment address = (address + alignment - 1) .&. negate alignment

dataSize :: DataItem -> Word64
dataSize = sum . map fieldSize . dataFields

fieldSize :: DataField -> Word64
fieldSize field =
  case field of
    DataInt ty _ -> typeBytes ty
    DataIntConstant ty _ -> typeBytes ty
    DataFloat ty _ -> typeBytes ty
    DataSymbol _ _ -> 8
    DataNull -> 8
    DataWord _ -> 8
    DataWordConstant _ -> 8
    DataCode _ -> 8
    DataBytes bytes -> fromIntegral (BS.length bytes)
    DataZero count -> fromInteger count

typeBytes :: Type -> Word64
typeBytes ty = fromIntegral (max 8 (typeBits ty) `div` 8)

writeData :: Map Symbol Word64 -> IntMap Word8 -> (DataItem, Word64) -> Either InterpretError (IntMap Word8)
writeData addresses memory (dataItem, address) = do
  bytes <- concat <$> mapM fieldBytes (dataFields dataItem)
  pure (writeBytes address bytes memory)
  where
    fieldBytes field =
      case field of
        DataInt ty value -> pure (littleEndian (typeBytes ty) (fromInteger value))
        DataIntConstant _ symbol -> unresolved symbol
        DataFloat ty value -> pure (encodeValue ty (floatValue ty value))
        DataSymbol symbol addend -> symbolBytes symbol addend
        DataNull -> pure (replicate 8 0)
        DataWord value -> pure (littleEndian 8 (fromInteger value))
        DataWordConstant symbol -> unresolved symbol
        DataCode Nothing -> pure (replicate 8 0)
        DataCode (Just symbol) -> symbolBytes symbol 0
        DataBytes bytes -> pure (BS.unpack bytes)
        DataZero count -> pure (replicate (fromInteger count) 0)
    unresolved symbol = Left (InterpretFailure ("unknown constant " <> renderSymbol symbol))
    symbolBytes symbol addend =
      case Map.lookup symbol addresses of
        Nothing -> Left (InterpretFailure ("unknown symbol " <> renderSymbol symbol))
        Just target -> pure (littleEndian 8 (target + fromInteger addend))

writeBytes :: Word64 -> [Word8] -> IntMap Word8 -> IntMap Word8
writeBytes address bytes memory =
  foldl' (\acc (offset, byte) -> IntMap.insert (fromIntegral (address + offset)) byte acc) memory (zip [0 ..] bytes)

littleEndian :: Word64 -> Word64 -> [Word8]
littleEndian count value = [fromIntegral (value `shiftR` (8 * fromIntegral index)) | index <- [0 .. count - 1]]

fromLittleEndian :: [Word8] -> Word64
fromLittleEndian = foldr (\byte acc -> acc `shiftL` 8 .|. fromIntegral byte) 0

zeroValue :: Type -> Value
zeroValue ty =
  case ty of
    F32 -> VF32 0
    F64 -> VF64 0
    Ptr -> VPtr 0
    Code -> VCode 0
    _ -> VInt 0

floatValue :: Type -> Double -> Value
floatValue ty value =
  case ty of
    F32 -> VF32 (double2Float value)
    _ -> VF64 value

-- Values

mask :: Type -> Word64 -> Word64
mask ty value
  | bits >= 64 = value
  | otherwise = value .&. ((1 `shiftL` bits) - 1)
  where
    bits = typeBits ty

fromIntegerBits :: Type -> Integer -> Word64
fromIntegerBits ty value = mask ty (fromInteger value)

signed :: Type -> Word64 -> Integer
signed ty value
  | bits < 64 && value >= (1 `shiftL` (bits - 1)) = toInteger value - (1 `shiftL` bits)
  | bits == 64 && value >= (1 `shiftL` 63) = toInteger value - (1 `shiftL` 64)
  | otherwise = toInteger value
  where
    bits = typeBits ty

unsigned :: Word64 -> Integer
unsigned = toInteger

encodeValue :: Type -> Value -> [Word8]
encodeValue ty value =
  case value of
    VInt bits -> littleEndian (typeBytes ty) bits
    VF32 float -> littleEndian 4 (fromIntegral (castFloatToWord32 float))
    VF64 double -> littleEndian 8 (castDoubleToWord64 double)
    VPtr address -> littleEndian 8 address
    VCode address -> littleEndian 8 address

decodeValue :: Type -> [Word8] -> Value
decodeValue ty bytes =
  case ty of
    F32 -> VF32 (castWord32ToFloat (fromIntegral word))
    F64 -> VF64 (castWord64ToDouble word)
    Ptr -> VPtr word
    Code -> VCode word
    _ -> VInt (mask ty word)
  where
    word = fromLittleEndian bytes

literalValue :: Program -> Type -> Literal -> M Value
literalValue program ty literal =
  case literal of
    LitInt value
      | isFloatType ty -> pure (floatValue ty (fromInteger value))
      | ty `elem` [Ptr, Code] -> failure "integer literal used as an address"
      | otherwise -> pure (VInt (fromIntegerBits ty value))
    LitFloat value -> pure (floatValue ty value)
    LitNull -> pure (address 0)
    LitSymbol symbol ->
      case Map.lookup symbol (programAddresses program) of
        Nothing -> failure ("unknown symbol " <> renderSymbol symbol)
        Just target -> pure (address target)
  where
    address = if ty == Code then VCode else VPtr

-- | Render a result with its declared type. Integers are signed decimals and
-- @i1@ is @0@ or @1@.
renderValue :: Type -> Value -> Text
renderValue ty value =
  case value of
    VInt bits
      | ty == I1 -> T.pack (show bits)
      | otherwise -> T.pack (show (signed ty bits))
    VF32 float -> T.pack (show float)
    VF64 double -> T.pack (show double)
    VPtr address -> T.pack ("0x" <> showHex address "")
    VCode address -> T.pack ("0x" <> showHex address "")

renderValues :: [Type] -> [Value] -> Text
renderValues types values = T.intercalate ", " (zipWith renderValue types values)

-- Execution

callFunction :: Program -> Symbol -> [Value] -> M [Value]
callFunction program = loop
  where
    loop symbol arguments = do
      function <- lookupFunction program symbol
      when (length (functionParameters function) /= length arguments) $
        failure ("call of " <> renderSymbol symbol <> " with " <> T.pack (show (length arguments)) <> " arguments")
      stackMark <- gets machineStack
      let locals = Map.fromList (zip (map fst (functionParameters function)) arguments)
      outcome <- case functionBlocks function of
        [] -> failure (renderSymbol symbol <> " has no blocks")
        entry : _ -> execBlock program function locals entry []
      releaseStack stackMark
      case outcome of
        Returned values -> pure values
        TailCalled next nextArguments -> loop next nextArguments

lookupFunction :: Program -> Symbol -> M Function
lookupFunction program symbol =
  case Map.lookup symbol (programFunctions program) of
    Just function -> pure function
    Nothing
      | Map.member symbol (programSignatures program) -> failure ("call to extern function " <> renderSymbol symbol)
      | otherwise -> failure ("unknown function " <> renderSymbol symbol)

releaseStack :: Word64 -> M ()
releaseStack mark = do
  machine <- get
  let current = machineStack machine
      released = foldl' (\memory address -> IntMap.delete (fromIntegral address) memory) (machineMemory machine) [mark .. current - 1]
  when (current > mark) $ put machine {machineMemory = released, machineStack = mark}

execBlock :: Program -> Function -> Locals -> Block -> [Value] -> M Outcome
execBlock program function locals block arguments = do
  let bound = foldl' (\acc ((var, _), value) -> Map.insert var value acc) locals (zip (blockParameters block) arguments)
  afterInstructions <- foldM (execInstruction program function) bound (blockInstructions block)
  execTerminator program function afterInstructions (blockTerminator block)

execInstruction :: Program -> Function -> Locals -> Instruction -> M Locals
execInstruction program _ locals instruction = do
  results <- execOperation program locals (instructionOperation instruction)
  pure (foldl' (\acc (var, value) -> Map.insert var value acc) locals (zip (instructionResults instruction) results))

execTerminator :: Program -> Function -> Locals -> Terminator -> M Outcome
execTerminator program function locals terminator =
  case terminator of
    Jump target -> jumpTo target
    Branch condition whenTrue whenFalse -> do
      value <- operand I1 condition
      jumpTo (if value == VInt 1 then whenTrue else whenFalse)
    Switch ty scrutinee cases fallback -> do
      value <- operand ty scrutinee
      case find (\switchCase -> VInt (fromIntegerBits ty (resolvedSwitchCaseValue switchCase)) == value) cases of
        Just switchCase -> jumpTo (switchCaseTarget switchCase)
        Nothing -> maybe (trap "switch without a matching case") jumpTo fallback
    Return values -> Returned <$> zipWithM operand (functionResults function) values
    TailCall symbol arguments -> do
      signature <- lookupSignature program symbol
      TailCalled symbol <$> zipWithM operand (signatureParameters signature) arguments
    TailCallIndirect target arguments signature -> do
      symbol <- resolveIndirect program locals target signature
      TailCalled symbol <$> zipWithM operand (signatureParameters signature) arguments
    Trap message -> trap message
  where
    operand = evalOperand program locals
    jumpTo (Target label arguments) =
      case find ((== label) . blockLabel) (functionBlocks function) of
        Nothing -> failure ("unknown block " <> unLabel label)
        Just block -> do
          values <- zipWithM operand (map snd (blockParameters block)) arguments
          execBlock program function locals block values

lookupSignature :: Program -> Symbol -> M Signature
lookupSignature program symbol =
  case Map.lookup symbol (programSignatures program) of
    Just signature -> pure signature
    Nothing -> failure ("unknown function " <> renderSymbol symbol)

resolveIndirect :: Program -> Locals -> Operand -> Signature -> M Symbol
resolveIndirect program locals target signature = do
  address <- evalOperand program locals Code target
  case address of
    VCode code
      | Just symbol <- Map.lookup code (programCode program) -> do
          actual <- lookupSignature program symbol
          unless (actual == signature) $ trap "indirect call signature mismatch"
          pure symbol
    _ -> trap "indirect call to a non-function"

evalOperand :: Program -> Locals -> Type -> Operand -> M Value
evalOperand program locals ty operand =
  case operand of
    OperandVar var ->
      case Map.lookup var locals of
        Just value -> pure value
        Nothing -> failure ("undefined value %" <> unVar var)
    OperandLiteral literal -> literalValue program ty literal

execOperation :: Program -> Locals -> Operation -> M [Value]
execOperation program locals operation =
  case operation of
    Binary op ty left right -> do
      a <- intOperand ty left
      b <- intOperand ty right
      (: []) . VInt <$> binary op ty a b
    Unary op ty value -> do
      a <- intOperand ty value
      pure [VInt (unary op ty a)]
    Wide op ty left right -> do
      a <- intOperand ty left
      b <- intOperand ty right
      pure (wide op ty a b)
    Compare op ty left right -> do
      a <- operand ty left
      b <- operand ty right
      pure [VInt (if compareValues op ty a b then 1 else 0)]
    FloatBinary op ty left right -> do
      a <- operand ty left
      b <- operand ty right
      pure [floatBinary op a b]
    FloatUnary op ty value -> (: []) . floatUnary op <$> operand ty value
    Convert op from value to -> (: []) <$> (operand from value >>= convert op from to)
    PtrToInt value -> do
      address <- ptrOperand value
      pure [VInt address]
    PtrFromInt value -> do
      bits <- intOperand I64 value
      pure [VPtr bits]
    Select ty condition left right -> do
      c <- operand I1 condition
      a <- operand ty left
      b <- operand ty right
      pure [if c == VInt 1 then a else b]
    Load ty address alignment -> do
      target <- effectiveAddress address alignment
      bytes <- readBytes target (typeBytes ty)
      pure [decodeValue ty bytes]
    Store ty value address alignment -> do
      stored <- operand ty value
      target <- effectiveAddress address alignment
      storeBytes program target (encodeValue ty stored)
      pure []
    PtrAdd base offset -> do
      address <- ptrOperand base
      delta <- intOperand I64 offset
      pure [VPtr (address + delta)]
    StackAlloc size alignment -> do
      machine <- get
      let start = alignUp (fromInteger (max 1 alignment)) (machineStack machine)
          end = start + fromInteger size
      when (end > stackLimit) $ trap "stack overflow"
      put machine {machineMemory = writeBytes start (replicate (fromInteger size) 0) (machineMemory machine), machineStack = end}
      pure [VPtr start]
    GlobalGet symbol -> do
      globals <- gets machineGlobals
      case Map.lookup symbol globals of
        Just value -> pure [value]
        Nothing -> failure ("unknown global " <> renderSymbol symbol)
    GlobalSet symbol value ->
      case Map.lookup symbol (programGlobalTypes program) of
        Nothing -> failure ("unknown global " <> renderSymbol symbol)
        Just ty -> do
          stored <- operand ty value
          modify' (\machine -> machine {machineGlobals = Map.insert symbol stored (machineGlobals machine)})
          pure []
    Call symbol arguments -> do
      signature <- lookupSignature program symbol
      values <- zipWithM operand (signatureParameters signature) arguments
      callFunction program symbol values
    CallIndirect target arguments signature -> do
      symbol <- resolveIndirect program locals target signature
      values <- zipWithM operand (signatureParameters signature) arguments
      callFunction program symbol values
  where
    operand = evalOperand program locals
    intOperand ty value = do
      result <- operand ty value
      case result of
        VInt bits -> pure bits
        _ -> failure "expected an integer value"
    ptrOperand value = do
      result <- operand Ptr value
      case result of
        VPtr address -> pure address
        _ -> failure "expected a pointer value"
    effectiveAddress (Address base offset) alignment = do
      address <- ptrOperand base
      let target = address + fromInteger offset
      when (alignment > 1 && target `mod` fromInteger alignment /= 0) $ trap "misaligned memory access"
      pure target

readBytes :: Word64 -> Word64 -> M [Word8]
readBytes address count = do
  memory <- gets machineMemory
  case mapM (\offset -> IntMap.lookup (fromIntegral (address + offset)) memory) [0 .. count - 1] of
    Just bytes -> pure bytes
    Nothing -> trap "memory access out of bounds"

storeBytes :: Program -> Word64 -> [Word8] -> M ()
storeBytes program address bytes = do
  memory <- gets machineMemory
  let count = fromIntegral (length bytes)
      mapped = all (\offset -> IntMap.member (fromIntegral (address + offset)) memory) [0 .. count - 1]
      readOnly = any (\(start, end) -> address < end && address + count > start) (programReadOnly program)
  unless mapped $ trap "memory access out of bounds"
  when readOnly $ trap "store to read-only data"
  modify' (\machine -> machine {machineMemory = writeBytes address bytes memory})

-- Arithmetic

binary :: BinaryOp -> Type -> Word64 -> Word64 -> M Word64
binary op ty a b =
  case op of
    Add -> pure (mask ty (a + b))
    Sub -> pure (mask ty (a - b))
    Mul -> pure (mask ty (a * b))
    DivS -> do
      when (b == 0) $ trap "integer division by zero"
      when (signed ty a == negate (2 ^ (typeBits ty - 1)) && signed ty b == -1) $ trap "integer overflow"
      pure (fromIntegerBits ty (signed ty a `quot` signed ty b))
    DivU -> do
      when (b == 0) $ trap "integer division by zero"
      pure (a `div` b)
    RemS -> do
      when (b == 0) $ trap "integer division by zero"
      pure (fromIntegerBits ty (signed ty a `rem` signed ty b))
    RemU -> do
      when (b == 0) $ trap "integer division by zero"
      pure (a `mod` b)
    And -> pure (a .&. b)
    Or -> pure (a .|. b)
    Xor -> pure (a `xor` b)
    Shl -> pure (mask ty (a `shiftL` count))
    ShrS -> pure (fromIntegerBits ty (signed ty a `shiftR` count))
    ShrU -> pure (a `shiftR` count)
  where
    count = fromIntegral (b `mod` fromIntegral (typeBits ty))

-- | The bit-count operations. A narrow value is canonical in its low bits,
-- so counting leading zeros starts at the width of the type.
unary :: UnaryOp -> Type -> Word64 -> Word64
unary op ty a =
  case op of
    Clz -> fromIntegral (length (takeWhile not [testBit a index | index <- [bits - 1, bits - 2 .. 0]]))
    Ctz -> fromIntegral (length (takeWhile not [testBit a index | index <- [0 .. bits - 1]]))
    Popcount -> fromIntegral (popCount (mask ty a))
  where
    bits = typeBits ty

wide :: WideOp -> Type -> Word64 -> Word64 -> [Value]
wide op ty a b =
  case op of
    MulWideS -> halves (signed ty a * signed ty b)
    MulWideU -> halves (unsigned a * unsigned b)
    AddCarry ->
      let total = unsigned a + unsigned b
       in [VInt (fromIntegerBits ty total), VInt (if total >= 2 ^ typeBits ty then 1 else 0)]
    SubBorrow ->
      let difference = unsigned a - unsigned b
       in [VInt (fromIntegerBits ty difference), VInt (if difference < 0 then 1 else 0)]
  where
    halves wideProduct = [VInt (fromIntegerBits ty wideProduct), VInt (fromIntegerBits ty (wideProduct `shiftR` typeBits ty))]

compareValues :: CompareOp -> Type -> Value -> Value -> Bool
compareValues op ty a b =
  case op of
    Eq -> equal
    Ne -> not equal
    LtS -> signedCompare (<)
    LeS -> signedCompare (<=)
    GtS -> signedCompare (>)
    GeS -> signedCompare (>=)
    LtU -> unsignedCompare (<)
    LeU -> unsignedCompare (<=)
    GtU -> unsignedCompare (>)
    GeU -> unsignedCompare (>=)
    FLt -> floatCompare (<)
    FLe -> floatCompare (<=)
    FGt -> floatCompare (>)
    FGe -> floatCompare (>=)
  where
    equal =
      case (a, b) of
        (VF32 x, VF32 y) -> x == y
        (VF64 x, VF64 y) -> x == y
        _ -> a == b
    signedCompare relation =
      case (a, b) of
        (VInt x, VInt y) -> relation (signed ty x) (signed ty y)
        _ -> False
    unsignedCompare relation =
      case (a, b) of
        (VInt x, VInt y) -> relation x y
        (VPtr x, VPtr y) -> relation x y
        _ -> False
    floatCompare :: (forall f. (Ord f) => f -> f -> Bool) -> Bool
    floatCompare relation =
      case (a, b) of
        (VF32 x, VF32 y) -> relation x y
        (VF64 x, VF64 y) -> relation x y
        _ -> False

floatBinary :: FloatBinaryOp -> Value -> Value -> Value
floatBinary op a b =
  case (a, b) of
    (VF32 x, VF32 y) -> VF32 (apply x y)
    (VF64 x, VF64 y) -> VF64 (apply x y)
    _ -> a
  where
    apply :: (Fractional f) => f -> f -> f
    apply x y =
      case op of
        FAdd -> x + y
        FSub -> x - y
        FMul -> x * y
        FDiv -> x / y

floatUnary :: FloatUnaryOp -> Value -> Value
floatUnary op value =
  case value of
    VF32 x -> VF32 (apply x)
    VF64 x -> VF64 (apply x)
    _ -> value
  where
    apply :: (Floating f) => f -> f
    apply x =
      case op of
        FNeg -> negate x
        FAbs -> abs x
        FSqrt -> sqrt x

convert :: ConvertOp -> Type -> Type -> Value -> M Value
convert op from to value =
  case (op, value) of
    (SExt, VInt bits) -> pure (VInt (fromIntegerBits to (signed from bits)))
    (ZExt, VInt bits) -> pure (VInt bits)
    (Trunc, VInt bits) -> pure (VInt (mask to bits))
    (IToFS, VInt bits) -> pure (floatValue to (fromInteger (signed from bits)))
    (IToFU, VInt bits) -> pure (floatValue to (fromInteger (unsigned bits)))
    (FToIS, _) -> floatToInteger True
    (FToIU, _) -> floatToInteger False
    (FpExt, VF32 x) -> pure (VF64 (float2Double x))
    (FpTrunc, VF64 x) -> pure (VF32 (double2Float x))
    (Bitcast, VInt bits) | to == F32 -> pure (VF32 (castWord32ToFloat (fromIntegral bits)))
    (Bitcast, VInt bits) | to == F64 -> pure (VF64 (castWord64ToDouble bits))
    (Bitcast, VF32 x) -> pure (VInt (fromIntegral (castFloatToWord32 x)))
    (Bitcast, VF64 x) -> pure (VInt (castDoubleToWord64 x))
    _ -> failure "invalid conversion operand"
  where
    floatToInteger isSigned = do
      let asDouble =
            case value of
              VF32 x -> float2Double x
              VF64 x -> x
              _ -> 0 / 0
      when (isNaN asDouble || isInfinite asDouble) $ trap "invalid float to integer conversion"
      let truncated = truncate asDouble :: Integer
          bits = typeBits to
          inRange =
            if isSigned
              then truncated >= negate (2 ^ (bits - 1)) && truncated < 2 ^ (bits - 1)
              else truncated >= 0 && truncated < 2 ^ bits
      unless inRange $ trap "invalid float to integer conversion"
      pure (VInt (fromIntegerBits to truncated))

renderSymbol :: Symbol -> Text
renderSymbol = renderDoc . prettySymbol
