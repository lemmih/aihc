{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim
  ( (+#),
    (-#),
    (*#),
    (<#),
    (==#),
    addIntC#,
    addWordC#,
    and#,
    Addr#,
    Array#,
    awaitIO#,
    ByteArray#,
    byteArrayContents#,
    catch#,
    casMutVar#,
    Char#,
    compareInt#,
    divInt#,
    copyByteArray#,
    copyAddrToByteArray#,
    Double#,
    Float#,
    fork#,
    getSizeofMutableByteArray#,
    indexArray#,
    indexWord8OffAddr#,
    indexWord32OffAddr#,
    indexWord64OffAddr#,
    indexWordArray#,
    int2Word#,
    Int#,
    Int8#,
    Int16#,
    Int32#,
    Int64#,
    chr#,
    isByteArrayPinned#,
    isMutableByteArrayPinned#,
    MVar#,
    MutableArray#,
    MutableByteArray#,
    mutableByteArrayContents#,
    MutVar#,
    newAlignedPinnedByteArray#,
    newArray#,
    newByteArray#,
    newMVar#,
    newMutVar#,
    newPinnedByteArray#,
    noDuplicate#,
    not#,
    ord#,
    or#,
    plusWord#,
    popCnt#,
    quotRemWord#,
    quotRemWord2#,
    quotWord#,
    raise#,
    reallyUnsafePtrEquality#,
    Proxy#,
    proxy#,
    runRW#,
    readWordArray#,
    realWorld#,
    readMVar#,
    readArray#,
    readMutVar#,
    resizeMutableByteArray#,
    seq,
    sameMutVar#,
    sameMutableArray#,
    shrinkMutableByteArray#,
    sizeofByteArray#,
    subIntC#,
    subWordC#,
    State#,
    StableName#,
    StablePtr#,
    takeMVar#,
    ThreadId#,
    RealWorld,
    TYPE,
    unsafeFreezeByteArray#,
    unsafeFreezeArray#,
    unsafeThawByteArray#,
    unsafeThawArray#,
    makeStableName#,
    stableNameToInt#,
    putMVar#,
    uncheckedShiftL#,
    uncheckedShiftRL#,
    unsafeCoerce#,
    word2Int#,
    word8ToWord#,
    word32ToWord#,
    word64ToWord#,
    writeWordArray#,
    writeArray#,
    writeMutVar#,
    Word#,
    Word8#,
    Word16#,
    Word32#,
    Word64#,
    xor#,
    nullAddr#,
    plusAddr#,
    minusAddr#,
    eqAddr#,
    neAddr#,
    ltAddr#,
    leAddr#,
    gtAddr#,
    geAddr#,
    addr2Int#,
    int2Addr#,
    indexWord16OffAddr#,
    indexWord8OffAddrAsWord16#,
    indexWord8OffAddrAsWord32#,
    indexWord8OffAddrAsWord64#,
    indexWord8OffAddrAsFloat#,
    indexWord8OffAddrAsDouble#,
    readWord8OffAddr#,
    readWord16OffAddr#,
    readWord32OffAddr#,
    readWord64OffAddr#,
    readWord8OffAddrAsWord16#,
    readWord8OffAddrAsWord32#,
    readWord8OffAddrAsWord64#,
    readWord8OffAddrAsFloat#,
    readWord8OffAddrAsDouble#,
    writeWord8OffAddr#,
    writeWord16OffAddr#,
    writeWord32OffAddr#,
    writeWord64OffAddr#,
    writeWord8OffAddrAsWord16#,
    writeWord8OffAddrAsWord32#,
    writeWord8OffAddrAsWord64#,
    writeWord8OffAddrAsFloat#,
    writeWord8OffAddrAsDouble#,
    cstringLength#,
    compareByteArrays#,
    copyMutableByteArray#,
    copyByteArrayToAddr#,
    copyMutableByteArrayToAddr#,
    wordToWord8#,
    wordToWord16#,
    wordToWord32#,
    wordToWord64#,
    word16ToWord#,
    eqWord64#,
    neWord64#,
    ltWord64#,
    leWord64#,
    gtWord64#,
    geWord64#,
    (>#),
    (>=#),
    (<=#),
    (/=#),
    touch#,
    sameMutableByteArray#,
    setByteArray#,
    indexAddrArray#,
    indexDoubleArray#,
    indexFloatArray#,
    indexInt8Array#,
    indexInt16Array#,
    indexInt32Array#,
    indexInt64Array#,
    indexIntArray#,
    indexStablePtrArray#,
    indexCharArray#,
    indexWideCharArray#,
    indexWord8Array#,
    indexWord16Array#,
    indexWord32Array#,
    indexWord64Array#,
    indexWord8ArrayAsWord16#,
    indexWord8ArrayAsWord32#,
    indexWord8ArrayAsWord64#,
    readAddrArray#,
    readDoubleArray#,
    readFloatArray#,
    readInt8Array#,
    readInt16Array#,
    readInt32Array#,
    readInt64Array#,
    readIntArray#,
    readStablePtrArray#,
    readWideCharArray#,
    readWord8Array#,
    readWord16Array#,
    readWord32Array#,
    readWord64Array#,
    writeAddrArray#,
    writeDoubleArray#,
    writeFloatArray#,
    writeInt8Array#,
    writeInt16Array#,
    writeInt32Array#,
    writeInt64Array#,
    writeIntArray#,
    writeStablePtrArray#,
    writeWideCharArray#,
    writeWord8Array#,
    writeWord16Array#,
    writeWord32Array#,
    writeWord64Array#,
    uncheckedIShiftL#,
    uncheckedIShiftRA#,
    uncheckedIShiftRL#,
    clz#,
    intToInt8#,
    int8ToInt#,
    intToInt16#,
    int16ToInt#,
    intToInt32#,
    int32ToInt#,
    intToInt64#,
    int64ToInt#,
    plusFloat#,
    minusFloat#,
    timesFloat#,
    sqrtFloat#,
    expFloat#,
    logFloat#,
    sinFloat#,
    cosFloat#,
    tanFloat#,
    asinFloat#,
    acosFloat#,
    atanFloat#,
    sinhFloat#,
    coshFloat#,
    tanhFloat#,
    asinhFloat#,
    acoshFloat#,
    atanhFloat#,
    divideFloat#,
    powerFloat#,
    negateFloat#,
    fabsFloat#,
    int2Float#,
    float2Int#,
    gtFloat#,
    ltFloat#,
    eqFloat#,
    (+##),
    (-##),
    (*##),
    sqrtDouble#,
    expDouble#,
    logDouble#,
    sinDouble#,
    cosDouble#,
    tanDouble#,
    asinDouble#,
    acosDouble#,
    atanDouble#,
    sinhDouble#,
    coshDouble#,
    tanhDouble#,
    asinhDouble#,
    acoshDouble#,
    atanhDouble#,
    (/##),
    (**##),
    negateDouble#,
    fabsDouble#,
    int2Double#,
    double2Int#,
    (>##),
    (<##),
    (==##),
    ctz#,
    eqWord#,
    geWord#,
    gtWord#,
    leWord#,
    ltWord#,
    minusWord#,
    neWord#,
    remWord#,
    timesWord#,
    timesWord2#,
    timesInt2#,
    byteSwap#,
    byteSwap16#,
    byteSwap32#,
    byteSwap64#,
    double2Float#,
    float2Double#,
    castFloatToWord32#,
    castWord32ToFloat#,
    castDoubleToWord64#,
    castWord64ToDouble#,
    yield#,
  )
where

import GHC.Types (Levity (..), RuntimeRep (..), TYPE, Type, UnliftedType)

type Int# :: TYPE 'IntRep
data Int#

type Int8# :: TYPE 'Int8Rep
data Int8#

type Int16# :: TYPE 'Int16Rep
data Int16#

type Int32# :: TYPE 'Int32Rep
data Int32#

type Int64# :: TYPE 'Int64Rep
data Int64#

type Word# :: TYPE 'WordRep
data Word#

type Word8# :: TYPE 'Word8Rep
data Word8#

type Word16# :: TYPE 'Word16Rep
data Word16#

type Word32# :: TYPE 'Word32Rep
data Word32#

type Word64# :: TYPE 'Word64Rep
data Word64#

type Char# :: TYPE 'WordRep
data Char#

type Float# :: TYPE 'FloatRep
data Float#

type Double# :: TYPE 'DoubleRep
data Double#

type State# :: Type -> TYPE ('TupleRep '[])
data State# s

type Addr# :: TYPE 'AddrRep
data Addr#

type Array# :: Type -> UnliftedType
data Array# a

type ByteArray# :: UnliftedType
data ByteArray#

type MutableArray# :: Type -> Type -> UnliftedType
data MutableArray# d a

type MutableByteArray# :: Type -> UnliftedType
data MutableByteArray# d

type MVar# :: Type -> Type -> UnliftedType
data MVar# d a

type MutVar# :: Type -> Type -> UnliftedType
data MutVar# d a

type ThreadId# :: UnliftedType
data ThreadId#

type StableName# :: Type -> UnliftedType
data StableName# a

type StablePtr# :: Type -> TYPE 'AddrRep
data StablePtr# a

type RealWorld :: Type
data RealWorld

type Proxy# :: forall k. k -> TYPE ('TupleRep '[])
data Proxy# (a :: k)

foreign import prim proxy# :: forall k (a :: k). Proxy# a

foreign import prim reallyUnsafePtrEquality# :: a -> b -> Int#

foreign import prim raise# :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b

foreign import prim unsafeCoerce# :: forall (q :: RuntimeRep) (r :: RuntimeRep) (a :: TYPE q) (b :: TYPE r). a -> b

foreign import prim seq :: forall (r :: RuntimeRep) a (b :: TYPE r). a -> b -> b

infixr 0 `seq`

foreign import prim realWorld# :: State# RealWorld

-- | Apply a state transformer to the real world token. The result can have
-- any runtime representation.
foreign import prim runRW# :: forall (r :: RuntimeRep) (o :: TYPE r). (State# RealWorld -> o) -> o

foreign import prim noDuplicate# :: State# d -> State# d

foreign import prim makeStableName# :: a -> State# RealWorld -> (# State# RealWorld, StableName# a #)

foreign import prim stableNameToInt# :: StableName# a -> Int#

foreign import prim compareInt# :: Int# -> Int# -> Int#

foreign import prim divInt# :: Int# -> Int# -> Int#

foreign import prim (+#) :: Int# -> Int# -> Int#

foreign import prim (-#) :: Int# -> Int# -> Int#

foreign import prim (*#) :: Int# -> Int# -> Int#

foreign import prim (<#) :: Int# -> Int# -> Int#

foreign import prim (==#) :: Int# -> Int# -> Int#

foreign import prim ord# :: Char# -> Int#

foreign import prim chr# :: Int# -> Char#

foreign import prim addIntC# :: Int# -> Int# -> (# Int#, Int# #)

foreign import prim subIntC# :: Int# -> Int# -> (# Int#, Int# #)

foreign import prim plusWord# :: Word# -> Word# -> Word#

foreign import prim minusWord# :: Word# -> Word# -> Word#

foreign import prim timesWord# :: Word# -> Word# -> Word#

foreign import prim addWordC# :: Word# -> Word# -> (# Word#, Int# #)

foreign import prim subWordC# :: Word# -> Word# -> (# Word#, Int# #)

foreign import prim timesWord2# :: Word# -> Word# -> (# Word#, Word# #)

foreign import prim timesInt2# :: Int# -> Int# -> (# Int#, Int#, Int# #)

foreign import prim byteSwap# :: Word# -> Word#

foreign import prim byteSwap16# :: Word# -> Word#

foreign import prim byteSwap32# :: Word# -> Word#

foreign import prim byteSwap64# :: Word64# -> Word64#

foreign import prim double2Float# :: Double# -> Float#

foreign import prim float2Double# :: Float# -> Double#

foreign import prim castFloatToWord32# :: Float# -> Word32#

foreign import prim castWord32ToFloat# :: Word32# -> Float#

foreign import prim castDoubleToWord64# :: Double# -> Word64#

foreign import prim castWord64ToDouble# :: Word64# -> Double#

foreign import prim quotWord# :: Word# -> Word# -> Word#

foreign import prim remWord# :: Word# -> Word# -> Word#

foreign import prim quotRemWord# :: Word# -> Word# -> (# Word#, Word# #)

foreign import prim quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#, Word# #)

foreign import prim and# :: Word# -> Word# -> Word#

foreign import prim or# :: Word# -> Word# -> Word#

foreign import prim xor# :: Word# -> Word# -> Word#

foreign import prim not# :: Word# -> Word#

foreign import prim uncheckedShiftL# :: Word# -> Int# -> Word#

foreign import prim uncheckedShiftRL# :: Word# -> Int# -> Word#

foreign import prim int2Word# :: Int# -> Word#

foreign import prim word2Int# :: Word# -> Int#

foreign import prim word8ToWord# :: Word8# -> Word#

foreign import prim word32ToWord# :: Word32# -> Word#

foreign import prim word64ToWord# :: Word64# -> Word#

foreign import prim eqWord# :: Word# -> Word# -> Int#

foreign import prim neWord# :: Word# -> Word# -> Int#

foreign import prim ltWord# :: Word# -> Word# -> Int#

foreign import prim leWord# :: Word# -> Word# -> Int#

foreign import prim gtWord# :: Word# -> Word# -> Int#

foreign import prim geWord# :: Word# -> Word# -> Int#

foreign import prim clz# :: Word# -> Word#

-- Sized integer conversions and floating point arithmetic.

foreign import prim intToInt8# :: Int# -> Int8#

foreign import prim int8ToInt# :: Int8# -> Int#

foreign import prim intToInt16# :: Int# -> Int16#

foreign import prim int16ToInt# :: Int16# -> Int#

foreign import prim intToInt32# :: Int# -> Int32#

foreign import prim int32ToInt# :: Int32# -> Int#

foreign import prim intToInt64# :: Int# -> Int64#

foreign import prim int64ToInt# :: Int64# -> Int#

foreign import prim plusFloat# :: Float# -> Float# -> Float#

foreign import prim minusFloat# :: Float# -> Float# -> Float#

foreign import prim timesFloat# :: Float# -> Float# -> Float#

foreign import prim sqrtFloat# :: Float# -> Float#

foreign import prim expFloat# :: Float# -> Float#

foreign import prim logFloat# :: Float# -> Float#

foreign import prim sinFloat# :: Float# -> Float#

foreign import prim cosFloat# :: Float# -> Float#

foreign import prim tanFloat# :: Float# -> Float#

foreign import prim asinFloat# :: Float# -> Float#

foreign import prim acosFloat# :: Float# -> Float#

foreign import prim atanFloat# :: Float# -> Float#

foreign import prim sinhFloat# :: Float# -> Float#

foreign import prim coshFloat# :: Float# -> Float#

foreign import prim tanhFloat# :: Float# -> Float#

foreign import prim asinhFloat# :: Float# -> Float#

foreign import prim acoshFloat# :: Float# -> Float#

foreign import prim atanhFloat# :: Float# -> Float#

foreign import prim divideFloat# :: Float# -> Float# -> Float#

foreign import prim powerFloat# :: Float# -> Float# -> Float#

foreign import prim negateFloat# :: Float# -> Float#

foreign import prim fabsFloat# :: Float# -> Float#

foreign import prim int2Float# :: Int# -> Float#

foreign import prim float2Int# :: Float# -> Int#

foreign import prim gtFloat# :: Float# -> Float# -> Int#

foreign import prim ltFloat# :: Float# -> Float# -> Int#

foreign import prim eqFloat# :: Float# -> Float# -> Int#

foreign import prim (+##) :: Double# -> Double# -> Double#

foreign import prim (-##) :: Double# -> Double# -> Double#

foreign import prim (*##) :: Double# -> Double# -> Double#

foreign import prim negateDouble# :: Double# -> Double#

foreign import prim fabsDouble# :: Double# -> Double#

foreign import prim int2Double# :: Int# -> Double#

foreign import prim double2Int# :: Double# -> Int#

foreign import prim (>##) :: Double# -> Double# -> Int#

foreign import prim (<##) :: Double# -> Double# -> Int#

foreign import prim (==##) :: Double# -> Double# -> Int#

foreign import prim sqrtDouble# :: Double# -> Double#

foreign import prim expDouble# :: Double# -> Double#

foreign import prim logDouble# :: Double# -> Double#

foreign import prim sinDouble# :: Double# -> Double#

foreign import prim cosDouble# :: Double# -> Double#

foreign import prim tanDouble# :: Double# -> Double#

foreign import prim asinDouble# :: Double# -> Double#

foreign import prim acosDouble# :: Double# -> Double#

foreign import prim atanDouble# :: Double# -> Double#

foreign import prim sinhDouble# :: Double# -> Double#

foreign import prim coshDouble# :: Double# -> Double#

foreign import prim tanhDouble# :: Double# -> Double#

foreign import prim asinhDouble# :: Double# -> Double#

foreign import prim acoshDouble# :: Double# -> Double#

foreign import prim atanhDouble# :: Double# -> Double#

foreign import prim (/##) :: Double# -> Double# -> Double#

foreign import prim (**##) :: Double# -> Double# -> Double#

foreign import prim ctz# :: Word# -> Word#

foreign import prim popCnt# :: Word# -> Word#

foreign import prim newMutVar# :: a -> State# d -> (# State# d, MutVar# d a #)

foreign import prim newMVar# :: State# d -> (# State# d, MVar# d a #)

foreign import prim readMVar# :: MVar# d a -> State# d -> (# State# d, a #)

foreign import prim takeMVar# :: MVar# d a -> State# d -> (# State# d, a #)

foreign import prim putMVar# :: MVar# d a -> a -> State# d -> State# d

foreign import prim readMutVar# :: MutVar# d a -> State# d -> (# State# d, a #)

foreign import prim writeMutVar# :: MutVar# d a -> a -> State# d -> State# d

-- | Replace a mutable variable when its current value is pointer-identical to
-- the expected value. The flag is @0#@ when the swap succeeds and @1#@ when it
-- fails; the final field is the value left in the mutable variable.
foreign import prim casMutVar# :: MutVar# d a -> a -> a -> State# d -> (# State# d, Int#, a #)

foreign import prim sameMutVar# :: MutVar# d a -> MutVar# d a -> Int#

foreign import prim newArray# :: Int# -> a -> State# d -> (# State# d, MutableArray# d a #)

foreign import prim indexArray# :: Array# a -> Int# -> a

foreign import prim readArray# :: MutableArray# d a -> Int# -> State# d -> (# State# d, a #)

foreign import prim writeArray# :: MutableArray# d a -> Int# -> a -> State# d -> State# d

foreign import prim unsafeFreezeArray# :: MutableArray# d a -> State# d -> (# State# d, Array# a #)

foreign import prim unsafeThawArray# :: Array# a -> State# d -> (# State# d, MutableArray# d a #)

foreign import prim sameMutableArray# :: MutableArray# d a -> MutableArray# d a -> Int#

foreign import prim sameMutableByteArray# :: MutableByteArray# d -> MutableByteArray# d -> Int#

foreign import prim newByteArray# :: Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim newPinnedByteArray# :: Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim newAlignedPinnedByteArray# :: Int# -> Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim isMutableByteArrayPinned# :: MutableByteArray# d -> Int#

foreign import prim isByteArrayPinned# :: ByteArray# -> Int#

foreign import prim byteArrayContents# :: ByteArray# -> Addr#

foreign import prim mutableByteArrayContents# :: MutableByteArray# d -> Addr#

foreign import prim shrinkMutableByteArray# :: MutableByteArray# d -> Int# -> State# d -> State# d

foreign import prim resizeMutableByteArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim unsafeFreezeByteArray# :: MutableByteArray# d -> State# d -> (# State# d, ByteArray# #)

foreign import prim unsafeThawByteArray# :: ByteArray# -> State# d -> (# State# d, MutableByteArray# d #)

foreign import prim sizeofByteArray# :: ByteArray# -> Int#

foreign import prim getSizeofMutableByteArray# :: MutableByteArray# d -> State# d -> (# State# d, Int# #)

foreign import prim copyAddrToByteArray# :: Addr# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

foreign import prim indexWord8OffAddr# :: Addr# -> Int# -> Word8#

foreign import prim indexWord32OffAddr# :: Addr# -> Int# -> Word32#

foreign import prim indexWord64OffAddr# :: Addr# -> Int# -> Word64#

foreign import prim indexWordArray# :: ByteArray# -> Int# -> Word#

foreign import prim readWordArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)

foreign import prim writeWordArray# :: MutableByteArray# d -> Int# -> Word# -> State# d -> State# d

foreign import prim copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

foreign import prim nullAddr# :: Addr#

foreign import prim plusAddr# :: Addr# -> Int# -> Addr#

foreign import prim minusAddr# :: Addr# -> Addr# -> Int#

foreign import prim eqAddr# :: Addr# -> Addr# -> Int#

foreign import prim neAddr# :: Addr# -> Addr# -> Int#

foreign import prim ltAddr# :: Addr# -> Addr# -> Int#

foreign import prim leAddr# :: Addr# -> Addr# -> Int#

foreign import prim gtAddr# :: Addr# -> Addr# -> Int#

foreign import prim geAddr# :: Addr# -> Addr# -> Int#

foreign import prim addr2Int# :: Addr# -> Int#

foreign import prim int2Addr# :: Int# -> Addr#

foreign import prim indexWord16OffAddr# :: Addr# -> Int# -> Word16#

foreign import prim indexWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16#

foreign import prim indexWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32#

foreign import prim indexWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64#

foreign import prim indexWord8OffAddrAsFloat# :: Addr# -> Int# -> Float#

foreign import prim indexWord8OffAddrAsDouble# :: Addr# -> Int# -> Double#

foreign import prim readWord8OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word8# #)

foreign import prim readWord16OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word16# #)

foreign import prim readWord32OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word32# #)

foreign import prim readWord64OffAddr# :: Addr# -> Int# -> State# d -> (# State# d, Word64# #)

foreign import prim readWord8OffAddrAsWord16# :: Addr# -> Int# -> State# d -> (# State# d, Word16# #)

foreign import prim readWord8OffAddrAsWord32# :: Addr# -> Int# -> State# d -> (# State# d, Word32# #)

foreign import prim readWord8OffAddrAsWord64# :: Addr# -> Int# -> State# d -> (# State# d, Word64# #)

foreign import prim readWord8OffAddrAsFloat# :: Addr# -> Int# -> State# d -> (# State# d, Float# #)

foreign import prim readWord8OffAddrAsDouble# :: Addr# -> Int# -> State# d -> (# State# d, Double# #)

foreign import prim writeWord8OffAddr# :: Addr# -> Int# -> Word8# -> State# d -> State# d

foreign import prim writeWord16OffAddr# :: Addr# -> Int# -> Word16# -> State# d -> State# d

foreign import prim writeWord32OffAddr# :: Addr# -> Int# -> Word32# -> State# d -> State# d

foreign import prim writeWord64OffAddr# :: Addr# -> Int# -> Word64# -> State# d -> State# d

foreign import prim writeWord8OffAddrAsWord16# :: Addr# -> Int# -> Word16# -> State# d -> State# d

foreign import prim writeWord8OffAddrAsWord32# :: Addr# -> Int# -> Word32# -> State# d -> State# d

foreign import prim writeWord8OffAddrAsWord64# :: Addr# -> Int# -> Word64# -> State# d -> State# d

foreign import prim writeWord8OffAddrAsFloat# :: Addr# -> Int# -> Float# -> State# d -> State# d

foreign import prim writeWord8OffAddrAsDouble# :: Addr# -> Int# -> Double# -> State# d -> State# d

foreign import prim cstringLength# :: Addr# -> Int#

foreign import prim compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#

foreign import prim copyMutableByteArray# :: MutableByteArray# d -> Int# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

foreign import prim copyByteArrayToAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> State# d -> State# d

foreign import prim copyMutableByteArrayToAddr# :: MutableByteArray# d -> Int# -> Addr# -> Int# -> State# d -> State# d

foreign import prim wordToWord8# :: Word# -> Word8#

foreign import prim wordToWord16# :: Word# -> Word16#

foreign import prim wordToWord32# :: Word# -> Word32#

foreign import prim wordToWord64# :: Word# -> Word64#

foreign import prim word16ToWord# :: Word16# -> Word#

foreign import prim eqWord64# :: Word64# -> Word64# -> Int#

foreign import prim neWord64# :: Word64# -> Word64# -> Int#

foreign import prim ltWord64# :: Word64# -> Word64# -> Int#

foreign import prim leWord64# :: Word64# -> Word64# -> Int#

foreign import prim gtWord64# :: Word64# -> Word64# -> Int#

foreign import prim geWord64# :: Word64# -> Word64# -> Int#

foreign import prim (>#) :: Int# -> Int# -> Int#

foreign import prim (>=#) :: Int# -> Int# -> Int#

foreign import prim (<=#) :: Int# -> Int# -> Int#

foreign import prim (/=#) :: Int# -> Int# -> Int#

-- | Keep a value alive until this point. The runtime does not move or free
-- byte arrays that are still referenced, so the primitive has no effect.
-- The value can have any runtime representation, as in GHC.
foreign import prim touch# :: forall (r :: RuntimeRep) (a :: TYPE r) d. a -> State# d -> State# d

foreign import prim setByteArray# :: MutableByteArray# d -> Int# -> Int# -> Int# -> State# d -> State# d

foreign import prim indexAddrArray# :: ByteArray# -> Int# -> Addr#

foreign import prim indexDoubleArray# :: ByteArray# -> Int# -> Double#

foreign import prim indexFloatArray# :: ByteArray# -> Int# -> Float#

foreign import prim indexInt8Array# :: ByteArray# -> Int# -> Int8#

foreign import prim indexInt16Array# :: ByteArray# -> Int# -> Int16#

foreign import prim indexInt32Array# :: ByteArray# -> Int# -> Int32#

foreign import prim indexInt64Array# :: ByteArray# -> Int# -> Int64#

foreign import prim indexIntArray# :: ByteArray# -> Int# -> Int#

foreign import prim indexStablePtrArray# :: ByteArray# -> Int# -> StablePtr# a

foreign import prim indexCharArray# :: ByteArray# -> Int# -> Char#

foreign import prim indexWideCharArray# :: ByteArray# -> Int# -> Char#

foreign import prim indexWord8Array# :: ByteArray# -> Int# -> Word8#

foreign import prim indexWord16Array# :: ByteArray# -> Int# -> Word16#

foreign import prim indexWord32Array# :: ByteArray# -> Int# -> Word32#

foreign import prim indexWord64Array# :: ByteArray# -> Int# -> Word64#

foreign import prim indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word16#

foreign import prim indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> Word32#

foreign import prim indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word64#

foreign import prim readAddrArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Addr# #)

foreign import prim readDoubleArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)

foreign import prim readFloatArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Float# #)

foreign import prim readInt8Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int8# #)

foreign import prim readInt16Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int16# #)

foreign import prim readInt32Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int32# #)

foreign import prim readInt64Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int64# #)

foreign import prim readIntArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)

foreign import prim readStablePtrArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, StablePtr# a #)

foreign import prim readWideCharArray# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)

foreign import prim readWord8Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word8# #)

foreign import prim readWord16Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word16# #)

foreign import prim readWord32Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word32# #)

foreign import prim readWord64Array# :: MutableByteArray# d -> Int# -> State# d -> (# State# d, Word64# #)

foreign import prim writeAddrArray# :: MutableByteArray# d -> Int# -> Addr# -> State# d -> State# d

foreign import prim writeDoubleArray# :: MutableByteArray# d -> Int# -> Double# -> State# d -> State# d

foreign import prim writeFloatArray# :: MutableByteArray# d -> Int# -> Float# -> State# d -> State# d

foreign import prim writeInt8Array# :: MutableByteArray# d -> Int# -> Int8# -> State# d -> State# d

foreign import prim writeInt16Array# :: MutableByteArray# d -> Int# -> Int16# -> State# d -> State# d

foreign import prim writeInt32Array# :: MutableByteArray# d -> Int# -> Int32# -> State# d -> State# d

foreign import prim writeInt64Array# :: MutableByteArray# d -> Int# -> Int64# -> State# d -> State# d

foreign import prim writeIntArray# :: MutableByteArray# d -> Int# -> Int# -> State# d -> State# d

foreign import prim writeStablePtrArray# :: MutableByteArray# d -> Int# -> StablePtr# a -> State# d -> State# d

foreign import prim writeWideCharArray# :: MutableByteArray# d -> Int# -> Char# -> State# d -> State# d

foreign import prim writeWord8Array# :: MutableByteArray# d -> Int# -> Word8# -> State# d -> State# d

foreign import prim writeWord16Array# :: MutableByteArray# d -> Int# -> Word16# -> State# d -> State# d

foreign import prim writeWord32Array# :: MutableByteArray# d -> Int# -> Word32# -> State# d -> State# d

foreign import prim writeWord64Array# :: MutableByteArray# d -> Int# -> Word64# -> State# d -> State# d

foreign import prim uncheckedIShiftL# :: Int# -> Int# -> Int#

foreign import prim uncheckedIShiftRA# :: Int# -> Int# -> Int#

foreign import prim uncheckedIShiftRL# :: Int# -> Int# -> Int#

foreign import prim
  fork# ::
    (State# RealWorld -> (# State# RealWorld, a #)) ->
    State# RealWorld ->
    (# State# RealWorld, ThreadId# #)

foreign import prim yield# :: State# RealWorld -> State# RealWorld

-- | Suspend the current green thread until an opaque runtime IO request has
-- completed. Concrete IO operations are ordinary runtime foreign calls.
foreign import prim awaitIO# :: Addr# -> State# RealWorld -> State# RealWorld

foreign import prim
  catch# ::
    (State# RealWorld -> (# State# RealWorld, a #)) ->
    (b -> State# RealWorld -> (# State# RealWorld, a #)) ->
    State# RealWorld ->
    (# State# RealWorld, a #)
