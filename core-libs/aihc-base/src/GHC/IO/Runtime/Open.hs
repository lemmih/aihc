{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Opening files through the runtime. Paths are marshalled to UTF-8 bytes
-- before the open request is submitted.
module GHC.IO.Runtime.Open
  ( openIOHandle,
    openUtf8FilePath,
  )
where

import Data.Bool (Bool (..), (&&))
import Data.Either (Either (..))
import GHC.Base (List (..), Monad (..), String, ord)
import GHC.IO (IO (..))
import GHC.IO.Runtime (IOHandle, awaitIO, openResultError, submitOpen, takeOpenResult, writeMemoryByte)
import GHC.Int (Int (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Prim (Addr#, MutableByteArray#, RealWorld, mutableByteArrayContents#, newPinnedByteArray#)
import GHC.Ptr (Ptr)

-- | Marshal a 'String' to stable UTF-8 bytes. Embedded NUL and surrogate
-- code points are rejected before the action runs.
openUtf8FilePath :: String -> Int -> IO (Either Int (Ptr IOHandle))
openUtf8FilePath path mode =
  case utf8Length path of
    Left pathError -> return (Left pathError)
    Right length ->
      do
        buffer <- newPathBuffer length
        case buffer of
          PathBuffer rawBuffer -> do
            writeUtf8 (mutableByteArrayContents# rawBuffer) 0 path
            openIOHandle (mutableByteArrayContents# rawBuffer) length mode

data PathBuffer = PathBuffer (MutableByteArray# RealWorld)

newPathBuffer :: Int -> IO PathBuffer
newPathBuffer (I# size) =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, PathBuffer buffer #)
    )

utf8Length :: String -> Either Int Int
utf8Length = go 0
  where
    go :: Int -> String -> Either Int Int
    go length [] = Right length
    go length (character : rest) =
      case utf8Width (ord character) of
        Left widthError -> Left widthError
        Right width -> go (length + width) rest

utf8Width :: Int -> Either Int Int
utf8Width codePoint =
  case codePoint == 0 of
    True -> Left 22
    False ->
      case codePoint <= 127 of
        True -> Right 1
        False ->
          case codePoint <= 2047 of
            True -> Right 2
            False ->
              case codePoint >= 55296 && codePoint <= 57343 of
                True -> Left 84
                False ->
                  case codePoint <= 65535 of
                    True -> Right 3
                    False -> Right 4

writeUtf8 :: Addr# -> Int -> String -> IO ()
writeUtf8 _ _ [] = return ()
writeUtf8 address offset (character : rest) = do
  nextOffset <- writeCodePoint address offset (ord character)
  writeUtf8 address nextOffset rest

writeCodePoint :: Addr# -> Int -> Int -> IO Int
writeCodePoint address offset codePoint =
  case codePoint <= 127 of
    True -> do
      writeByte address offset codePoint
      return (offset + 1)
    False ->
      case codePoint <= 2047 of
        True -> do
          case divide64 codePoint of
            (high, low) -> do
              writeByte address offset (192 + high)
              writeByte address (offset + 1) (128 + low)
              return (offset + 2)
        False ->
          case codePoint <= 65535 of
            True -> do
              case divide64 codePoint of
                (middleAndHigh, low) ->
                  case divide64 middleAndHigh of
                    (high, middle) -> do
                      writeByte address offset (224 + high)
                      writeByte address (offset + 1) (128 + middle)
                      writeByte address (offset + 2) (128 + low)
                      return (offset + 3)
            False -> do
              case divide64 codePoint of
                (tailBytes, low) ->
                  case divide64 tailBytes of
                    (headBytes, middleLow) ->
                      case divide64 headBytes of
                        (high, middleHigh) -> do
                          writeByte address offset (240 + high)
                          writeByte address (offset + 1) (128 + middleHigh)
                          writeByte address (offset + 2) (128 + middleLow)
                          writeByte address (offset + 3) (128 + low)
                          return (offset + 4)

writeByte :: Addr# -> Int -> Int -> IO ()
writeByte address offset value = do
  writeMemoryByte address offset value
  return ()

divide64 :: Int -> (Int, Int)
divide64 value = go value 0
  where
    go :: Int -> Int -> (Int, Int)
    go remainder quotient =
      case remainder >= 64 of
        True -> go (remainder - 64) (quotient + 1)
        False -> (quotient, remainder)

-- | Open a file through the runtime. The result is the error number of a
-- failed open.
openIOHandle :: Addr# -> Int -> Int -> IO (Either Int (Ptr IOHandle))
openIOHandle path length mode = do
  request <- submitOpen path length mode
  awaitIO request
  result <- takeOpenResult request
  openCode <- openResultError result
  case openCode == 0 of
    True -> return (Right result)
    False -> return (Left openCode)
