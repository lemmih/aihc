{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Foreign.Storable
  ( Storable (..),
  )
where

import GHC.Err (undefined)
import GHC.IO (IO (..))
import GHC.Int (Int (..), Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Num (Num (..))
import GHC.Prim
  ( chr#,
    int16ToInt#,
    int2Word#,
    int32ToInt#,
    int64ToInt#,
    int8ToInt#,
    intToInt16#,
    intToInt32#,
    intToInt64#,
    intToInt8#,
    ord#,
    readWord16OffAddr#,
    readWord32OffAddr#,
    readWord64OffAddr#,
    readWord8OffAddr#,
    readWord8OffAddrAsDouble#,
    readWord8OffAddrAsFloat#,
    word16ToWord#,
    word2Int#,
    word32ToWord#,
    word64ToWord#,
    word8ToWord#,
    wordToWord16#,
    wordToWord32#,
    wordToWord64#,
    wordToWord8#,
    writeWord16OffAddr#,
    writeWord32OffAddr#,
    writeWord64OffAddr#,
    writeWord8OffAddr#,
    writeWord8OffAddrAsDouble#,
    writeWord8OffAddrAsFloat#,
  )
import GHC.Ptr (Ptr (..), plusPtr)
import GHC.Types (Char (..), Double (..), Float (..))
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))

class Storable a where
  sizeOf :: a -> Int
  alignment :: a -> Int
  peekElemOff :: Ptr a -> Int -> IO a
  pokeElemOff :: Ptr a -> Int -> a -> IO ()
  peekByteOff :: Ptr b -> Int -> IO a
  pokeByteOff :: Ptr b -> Int -> a -> IO ()
  peek :: Ptr a -> IO a
  poke :: Ptr a -> a -> IO ()

  peekElemOff address index = peek (address `plusPtr` (index * sizeOf (pointerElement address)))
  pokeElemOff address index value = poke (address `plusPtr` (index * sizeOf value)) value
  peekByteOff address offset = peek (address `plusPtr` offset)
  pokeByteOff address offset = poke (address `plusPtr` offset)
  peek address = peekElemOff address 0
  poke address = pokeElemOff address 0

pointerElement :: Ptr a -> a
pointerElement _ = undefined

instance Storable Word8 where
  sizeOf _ = 1
  alignment _ = 1
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord8OffAddr# address index state of
            (# readState, value #) -> (# readState, W8# value #)
      )
  pokeElemOff (Ptr address) (I# index) (W8# value) =
    IO
      ( \state ->
          case writeWord8OffAddr# address index value state of
            nextState -> (# nextState, () #)
      )

instance Storable Word16 where
  sizeOf _ = 2
  alignment _ = 2
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord16OffAddr# address index state of
            (# readState, value #) -> (# readState, W16# value #)
      )
  pokeElemOff (Ptr address) (I# index) (W16# value) =
    IO
      ( \state ->
          case writeWord16OffAddr# address index value state of
            nextState -> (# nextState, () #)
      )

instance Storable Word32 where
  sizeOf _ = 4
  alignment _ = 4
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord32OffAddr# address index state of
            (# readState, value #) -> (# readState, W32# value #)
      )
  pokeElemOff (Ptr address) (I# index) (W32# value) =
    IO
      ( \state ->
          case writeWord32OffAddr# address index value state of
            nextState -> (# nextState, () #)
      )

instance Storable Word64 where
  sizeOf _ = 8
  alignment _ = 8
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord64OffAddr# address index state of
            (# readState, value #) -> (# readState, W64# value #)
      )
  pokeElemOff (Ptr address) (I# index) (W64# value) =
    IO
      ( \state ->
          case writeWord64OffAddr# address index value state of
            nextState -> (# nextState, () #)
      )

instance Storable Word where
  sizeOf _ = 8
  alignment _ = 8
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord64OffAddr# address index state of
            (# readState, value #) -> (# readState, W# (word64ToWord# value) #)
      )
  pokeElemOff (Ptr address) (I# index) (W# value) =
    IO
      ( \state ->
          case writeWord64OffAddr# address index (wordToWord64# value) state of
            nextState -> (# nextState, () #)
      )

instance Storable Int where
  sizeOf _ = 8
  alignment _ = 8
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord64OffAddr# address index state of
            (# readState, value #) -> (# readState, I# (word2Int# (word64ToWord# value)) #)
      )
  pokeElemOff (Ptr address) (I# index) (I# value) =
    IO
      ( \state ->
          case writeWord64OffAddr# address index (wordToWord64# (int2Word# value)) state of
            nextState -> (# nextState, () #)
      )

-- | The signed types have no dedicated address primops, so they round-trip
-- through the unsigned accessor of the same width and narrow back to the
-- signed representation.
instance Storable Int8 where
  sizeOf _ = 1
  alignment _ = 1
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord8OffAddr# address index state of
            (# readState, value #) -> (# readState, I8# (intToInt8# (word2Int# (word8ToWord# value))) #)
      )
  pokeElemOff (Ptr address) (I# index) (I8# value) =
    IO
      ( \state ->
          case writeWord8OffAddr# address index (wordToWord8# (int2Word# (int8ToInt# value))) state of
            nextState -> (# nextState, () #)
      )

instance Storable Int16 where
  sizeOf _ = 2
  alignment _ = 2
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord16OffAddr# address index state of
            (# readState, value #) -> (# readState, I16# (intToInt16# (word2Int# (word16ToWord# value))) #)
      )
  pokeElemOff (Ptr address) (I# index) (I16# value) =
    IO
      ( \state ->
          case writeWord16OffAddr# address index (wordToWord16# (int2Word# (int16ToInt# value))) state of
            nextState -> (# nextState, () #)
      )

instance Storable Int32 where
  sizeOf _ = 4
  alignment _ = 4
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord32OffAddr# address index state of
            (# readState, value #) -> (# readState, I32# (intToInt32# (word2Int# (word32ToWord# value))) #)
      )
  pokeElemOff (Ptr address) (I# index) (I32# value) =
    IO
      ( \state ->
          case writeWord32OffAddr# address index (wordToWord32# (int2Word# (int32ToInt# value))) state of
            nextState -> (# nextState, () #)
      )

instance Storable Int64 where
  sizeOf _ = 8
  alignment _ = 8
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord64OffAddr# address index state of
            (# readState, value #) -> (# readState, I64# (intToInt64# (word2Int# (word64ToWord# value))) #)
      )
  pokeElemOff (Ptr address) (I# index) (I64# value) =
    IO
      ( \state ->
          case writeWord64OffAddr# address index (wordToWord64# (int2Word# (int64ToInt# value))) state of
            nextState -> (# nextState, () #)
      )

-- | 'Float' has no dedicated address primop, so the unaligned byte-offset
-- accessors carry the four-byte IEEE payload.
instance Storable Float where
  sizeOf _ = 4
  alignment _ = 4
  peekElemOff address index = peekByteOff address (index * 4)
  pokeElemOff address index = pokeByteOff address (index * 4)
  peekByteOff (Ptr address) (I# offset) =
    IO
      ( \state ->
          case readWord8OffAddrAsFloat# address offset state of
            (# readState, value #) -> (# readState, F# value #)
      )
  pokeByteOff (Ptr address) (I# offset) (F# value) =
    IO
      ( \state ->
          case writeWord8OffAddrAsFloat# address offset value state of
            nextState -> (# nextState, () #)
      )

instance Storable Double where
  sizeOf _ = 8
  alignment _ = 8
  peekElemOff address index = peekByteOff address (index * 8)
  pokeElemOff address index = pokeByteOff address (index * 8)
  peekByteOff (Ptr address) (I# offset) =
    IO
      ( \state ->
          case readWord8OffAddrAsDouble# address offset state of
            (# readState, value #) -> (# readState, D# value #)
      )
  pokeByteOff (Ptr address) (I# offset) (D# value) =
    IO
      ( \state ->
          case writeWord8OffAddrAsDouble# address offset value state of
            nextState -> (# nextState, () #)
      )

instance Storable Char where
  sizeOf _ = 4
  alignment _ = 4
  peekElemOff (Ptr address) (I# index) =
    IO
      ( \state ->
          case readWord32OffAddr# address index state of
            (# readState, value #) -> (# readState, C# (chr# (word2Int# (word32ToWord# value))) #)
      )
  pokeElemOff (Ptr address) (I# index) (C# value) =
    IO
      ( \state ->
          case writeWord32OffAddr# address index (wordToWord32# (int2Word# (ord# value))) state of
            nextState -> (# nextState, () #)
      )
