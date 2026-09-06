-- | Byte and UTF-8 reads from a handle's byte buffer, shared by the text
-- reads of @GHC.IO.Handle.Text@ and the lookahead of
-- @GHC.IO.Handle.Internals@.
module GHC.Internal.IO.Handle.Read
  ( fillIfEmpty,
    readByte,
    readUtf8Char,
  )
where

import Data.Bool (Bool (..), (&&))
import Data.Maybe (Maybe (..))
import GHC.Base (Monad (..))
import GHC.Char (chr)
import GHC.IO (IO)
import GHC.IO.Buffer (Buffer (..), isEmptyBuffer, readWord8Buf)
import GHC.IORef (readIORef, writeIORef)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (Handle__ (..))
import GHC.Internal.IO.Types qualified as Buffered (fillReadBuffer)
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import GHC.Types (Char)

-- | Fill the byte buffer when it is empty. The result is 'False' at the
-- end of the input.
fillIfEmpty :: Handle__ -> IO Bool
fillIfEmpty Handle__ {haByteBuffer, haDevice} = do
  buffer <- readIORef haByteBuffer
  case isEmptyBuffer buffer of
    False -> return True
    True -> do
      (count, buffer') <- Buffered.fillReadBuffer haDevice buffer
      writeIORef haByteBuffer buffer'
      return (count > 0)

-- | Take one byte from the byte buffer.
readByte :: Handle__ -> IO (Maybe Int)
readByte handle_@Handle__ {haByteBuffer} = do
  filled <- fillIfEmpty handle_
  case filled of
    False -> return Nothing
    True -> do
      buffer <- readIORef haByteBuffer
      value <- readWord8Buf (bufRaw buffer) (bufL buffer)
      writeIORef haByteBuffer buffer {bufL = bufL buffer + 1}
      return (Just (fromIntegral value))

-- | Decode one UTF-8 character. A bad sequence gives the replacement
-- character.
readUtf8Char :: Handle__ -> IO (Maybe Char)
readUtf8Char handle_ = do
  first <- readByte handle_
  case first of
    Nothing -> return Nothing
    Just leading
      | leading < 0x80 -> return (Just (chr leading))
      | leading < 0xC0 -> return (Just replacement)
      | leading < 0xE0 -> continuation (leading - 0xC0) 1
      | leading < 0xF0 -> continuation (leading - 0xE0) 2
      | otherwise -> continuation (leading - 0xF0) 3
  where
    otherwise = True
    continuation code count =
      case count == 0 of
        True -> return (Just (chr code))
        False -> do
          next <- readByte handle_
          case next of
            Nothing -> return (Just replacement)
            Just byte ->
              case byte >= 0x80 && byte < 0xC0 of
                True -> continuation (code * 64 + (byte - 0x80)) (count - 1)
                False -> return (Just replacement)
    replacement = chr 0xFFFD
