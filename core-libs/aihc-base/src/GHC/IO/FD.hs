{-# LANGUAGE MagicHash #-}

{- HLINT ignore "Use camelCase" -}

-- | Raw IO over runtime-owned IO resources. 'FD' wraps one runtime
-- resource and gives it the device classes that the handle layer uses.
module GHC.IO.FD
  ( FD (..),
    stdin,
    stdout,
    stderr,
    openFile,
    release,
    readRawBufferPtr,
    writeRawBufferPtr,
  )
where

import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Foreign.C.Error (Errno (..), eIO, errnoToIOError)
import GHC.Base (Monad (..), String)
import GHC.IO (FilePath, IO (..))
import GHC.IO.Buffer (newByteBuffer)
import GHC.IO.BufferedIO (readBuf, readBufNonBlocking, writeBuf, writeBufNonBlocking)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.Runtime
  ( IOHandle,
    IORequest,
    awaitIO,
    closeIOHandle,
    stderrHandle,
    stdinHandle,
    stdoutHandle,
    submitRead,
    submitWrite,
    takeResult,
  )
import GHC.IO.Runtime.Open (openUtf8FilePath)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Int (Int (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (BufferedIO (..), IODevice (..), IODeviceType (..), RawIO (..), ioError)
import GHC.Num (Num (..))
import GHC.Prim (Addr#)
import GHC.Ptr (Ptr (..), plusPtr)
import GHC.Real (fromIntegral)
import GHC.Show (Show (..), showString)
import GHC.Word (Word8)

-- | A runtime IO resource. The runtime has no non-blocking mode, so the
-- flag is always zero.
data FD = FD
  { fdFD :: !(Ptr IOHandle),
    fdIsNonBlocking :: !Int
  }

instance Show FD where
  showsPrec _ _ = showString "<fd>"

-- | The default byte buffer size of a handle.
dEFAULT_BUFFER_SIZE :: Int
dEFAULT_BUFFER_SIZE = 8192

stdin :: FD
stdin = FD (unsafePerformIO stdinHandle) 0

stdout :: FD
stdout = FD (unsafePerformIO stdoutHandle) 0

stderr :: FD
stderr = FD (unsafePerformIO stderrHandle) 0

-- | The mode number of the runtime open request.
ioModeNumber :: IOMode -> Int
ioModeNumber mode =
  case mode of
    ReadMode -> 0
    WriteMode -> 1
    AppendMode -> 2
    ReadWriteMode -> 3

-- | Open a file. The runtime opens every file as a byte stream.
openFile :: FilePath -> IOMode -> Bool -> IO (FD, IODeviceType)
openFile path mode _nonBlocking = do
  result <- openUtf8FilePath path (ioModeNumber mode)
  case result of
    Left errno -> ioError (errnoToIOError "openFile" (Errno (fromIntegral errno)) Nothing (Just path))
    Right rawHandle -> return (FD rawHandle 0, Stream)

-- | Release the resource without a close error.
release :: FD -> IO ()
release fd = do
  _ <- closeIOHandle (fdFD fd)
  return ()

instance RawIO FD where
  read fd buffer _ = readRawBufferPtr "GHC.IO.FD.read" fd buffer 0
  readNonBlocking fd buffer _ count = do
    transferred <- readRawBufferPtr "GHC.IO.FD.readNonBlocking" fd buffer 0 count
    return (Just transferred)
  write fd buffer _ = writeAll "GHC.IO.FD.write" fd buffer 0
  writeNonBlocking fd buffer _ = writeRawBufferPtr "GHC.IO.FD.writeNonBlocking" fd buffer 0

instance BufferedIO FD where
  newBuffer _ = newByteBuffer dEFAULT_BUFFER_SIZE
  fillReadBuffer = readBuf
  fillReadBuffer0 = readBufNonBlocking
  flushWriteBuffer = writeBuf
  flushWriteBuffer0 = writeBufNonBlocking

instance IODevice FD where
  ready _ _ _ = return True
  close fd = do
    result <- closeIOHandle (fdFD fd)
    case result < 0 of
      True -> ioError (errnoToIOError "GHC.IO.FD.close" (Errno (fromIntegral (decodeError result))) Nothing Nothing)
      False -> return ()
  devType _ = return Stream

-- | Read up to @count@ bytes. The result is zero at the end of the input.
readRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> Int -> IO Int
readRawBufferPtr location fd buffer offset count = do
  result <- readIntoPtr (fdFD fd) buffer offset count
  case result < 0 of
    True -> ioError (errnoToIOError location (Errno (fromIntegral (decodeError result))) Nothing Nothing)
    False -> return result

-- | Write up to @count@ bytes and give the number of bytes written.
writeRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> Int -> IO Int
writeRawBufferPtr location fd buffer offset count = do
  result <- writeFromPtr (fdFD fd) buffer offset count
  case result < 0 of
    True -> ioError (errnoToIOError location (Errno (fromIntegral (decodeError result))) Nothing Nothing)
    False -> return result

writeAll :: String -> FD -> Ptr Word8 -> Int -> Int -> IO ()
writeAll location fd buffer offset count =
  case count <= 0 of
    True -> return ()
    False -> do
      written <- writeRawBufferPtr location fd buffer offset count
      case written == 0 of
        True -> ioError (errnoToIOError location eIO Nothing Nothing)
        False -> writeAll location fd buffer (offset + written) (count - written)

-- | The runtime encodes an error number @e@ as @-(e + 1)@.
decodeError :: Int -> Int
decodeError result = negate result - 1

readIntoPtr :: Ptr IOHandle -> Ptr a -> Int -> Int -> IO Int
readIntoPtr handle (Ptr address) = readIntoAddress handle address

writeFromPtr :: Ptr IOHandle -> Ptr a -> Int -> Int -> IO Int
writeFromPtr handle (Ptr address) = writeFromAddress handle address

readIntoAddress :: Ptr IOHandle -> Addr# -> Int -> Int -> IO Int
readIntoAddress handle address offset length =
  awaitRequest (submitRead handle address offset length)

writeFromAddress :: Ptr IOHandle -> Addr# -> Int -> Int -> IO Int
writeFromAddress handle address offset length =
  awaitRequest (submitWrite handle address offset length)

awaitRequest :: IO (Ptr IORequest) -> IO Int
awaitRequest submission = do
  request <- submission
  awaitIO request
  takeResult request
