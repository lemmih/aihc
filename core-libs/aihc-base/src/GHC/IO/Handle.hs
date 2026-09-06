{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Use camelCase" -}

-- | Handle operations.
module GHC.IO.Handle
  ( Handle,
    BufferMode (..),
    mkFileHandle,
    mkDuplexHandle,
    hFileSize,
    hSetFileSize,
    hIsEOF,
    isEOF,
    hLookAhead,
    hSetBuffering,
    hSetBinaryMode,
    hSetEncoding,
    hGetEncoding,
    hFlush,
    hFlushAll,
    hDuplicate,
    hDuplicateTo,
    hClose,
    hClose_help,
    LockMode (..),
    hLock,
    hTryLock,
    HandlePosition,
    HandlePosn (..),
    hGetPosn,
    hSetPosn,
    SeekMode (..),
    hSeek,
    hTell,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hGetBuffering,
    hIsSeekable,
    hSetEcho,
    hGetEcho,
    hWaitForInput,
    hGetChar,
    hGetLine,
    hGetContents,
    hGetContents',
    hPutChar,
    hPutStr,
    hPutStrLn,
    hGetBuf,
    hGetBufNonBlocking,
    hPutBuf,
    hPutBufNonBlocking,
    hShow,
    hGetBufSome,
    hSetNewlineMode,
    Newline (..),
    NewlineMode (..),
    nativeNewline,
    noNewlineTranslation,
    universalNewlineMode,
    nativeNewlineMode,
  )
where

import Data.Bool (Bool (..), not)
import Data.Maybe (Maybe (..))
import GHC.Base (Monad (..), String, ($), (++), (.))
import GHC.Exception.Type (SomeException)
import GHC.IO (IO, throwIO)
import GHC.IO.Buffer (Buffer (..), BufferState (..), isEmptyBuffer, newCharBuffer)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Text (hGetBuf, hGetBufNonBlocking, hGetBufSome, hGetChar, hGetContents, hGetContents', hGetLine, hPutBuf, hPutBufNonBlocking, hPutChar, hPutStr, hPutStrLn, hWaitForInput)
import GHC.IO.Handle.Types
import GHC.IO.StdHandles (stdin)
import GHC.IORef (readIORef, writeIORef)
import GHC.Int (Int)
import GHC.Integer (Integer)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (IODevice (..), IOErrorType (..), IOException (..), SeekMode (..), ioException)
import GHC.Internal.IO.Types qualified as Buffered (fillReadBuffer)
import GHC.MVar (MVar)
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import GHC.Show (Show (..), showString, shows)
import GHC.Types (Char)

-- ---------------------------------------------------------------------
-- Closing

-- | Flush and close the handle. A closed handle stays closed. A close
-- error reaches the caller with the handle in it.
hClose :: Handle -> IO ()
hClose handle@(FileHandle _ state) = do
  exception <- hClose' handle state
  maybeThrow exception
hClose handle@(DuplexHandle _ readSide writeSide) = do
  readException <- hClose' handle readSide
  writeException <- hClose' handle writeSide
  maybeThrow readException
  maybeThrow writeException

maybeThrow :: Maybe SomeException -> IO ()
maybeThrow Nothing = return ()
maybeThrow (Just exception) = throwIO exception

hClose' :: Handle -> MVar Handle__ -> IO (Maybe SomeException)
hClose' handle state = withHandle' "hClose" handle state hClose_help

-- ---------------------------------------------------------------------
-- Sizes and positions

hFileSize :: Handle -> IO Integer
hFileSize handle =
  withHandle_ "hFileSize" handle $ \handle_@Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> do
        flushWriteBuffer handle_
        getSize haDevice

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize handle size =
  withHandle_ "hSetFileSize" handle $ \handle_@Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> do
        flushWriteBuffer handle_
        setSize haDevice size

hIsEOF :: Handle -> IO Bool
hIsEOF handle =
  wantReadableHandle_ "hIsEOF" handle $ \handle_@Handle__ {haByteBuffer, haDevice} -> do
    flushCharReadBuffer handle_
    buffer <- readIORef haByteBuffer
    case isEmptyBuffer buffer of
      False -> return False
      True -> do
        (count, buffer') <- Buffered.fillReadBuffer haDevice buffer
        writeIORef haByteBuffer buffer'
        return (count == 0)

isEOF :: IO Bool
isEOF = hIsEOF stdin

type HandlePosition = Integer

data HandlePosn = HandlePosn Handle HandlePosition

instance Eq HandlePosn where
  HandlePosn _ left == HandlePosn _ right = left == right

instance Show HandlePosn where
  showsPrec precedence (HandlePosn handle position) =
    showsPrec precedence handle . showString " at position " . shows position

hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = do
  position <- hTell handle
  return (HandlePosn handle position)

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn handle position) = hSeek handle AbsoluteSeek position

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek handle mode offset =
  wantSeekableHandle "hSeek" handle $ \handle_@Handle__ {haDevice} -> do
    flushBuffer handle_
    _ <- seek haDevice mode offset
    return ()

hTell :: Handle -> IO Integer
hTell handle =
  wantSeekableHandle "hTell" handle $ \handle_@Handle__ {haDevice, haByteBuffer} -> do
    position <- tell haDevice
    buffer <- readIORef haByteBuffer
    let buffered = fromIntegral (bufR buffer - bufL buffer)
    return
      ( case bufState buffer of
          WriteBuffer -> position + buffered
          ReadBuffer -> position - buffered
      )

-- ---------------------------------------------------------------------
-- Buffering

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering handle mode =
  withAllHandles__ "hSetBuffering" handle $ \handle_@Handle__ {haType, haBufferMode, haCharBuffer} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ ->
        case mode == haBufferMode of
          True -> return handle_
          False -> do
            case mode of
              BlockBuffering (Just size) | size <= 0 -> ioe_bufsiz size
              _ -> return ()
            flushCharBuffer handle_
            let state = initBufferState haType
            newBuffer <- newCharBuffer dEFAULT_CHAR_BUFFER_SIZE state
            writeIORef haCharBuffer newBuffer
            return handle_ {haBufferMode = mode}

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle =
  withHandle_ "hGetBuffering" handle $ \Handle__ {haType, haBufferMode} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return haBufferMode

-- | Handles are always binary in this runtime.
hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode handle _ =
  withAllHandles__ "hSetBinaryMode" handle $ \handle_@Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return handle_

hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding handle encoding =
  withAllHandles__ "hSetEncoding" handle $ \handle_@Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return handle_ {haCodec = Just encoding}

hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetEncoding handle =
  withHandle_ "hGetEncoding" handle $ \Handle__ {haType, haCodec} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return haCodec

hSetNewlineMode :: Handle -> NewlineMode -> IO ()
hSetNewlineMode handle (NewlineMode input output) =
  withAllHandles__ "hSetNewlineMode" handle $ \handle_@Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return handle_ {haInputNL = input, haOutputNL = output}

hFlush :: Handle -> IO ()
hFlush handle = wantWritableHandle "hFlush" handle flushWriteBuffer

hFlushAll :: Handle -> IO ()
hFlushAll handle = withHandle_ "hFlushAll" handle flushBuffer

-- ---------------------------------------------------------------------
-- Duplication and locks

hDuplicate :: Handle -> IO Handle
hDuplicate handle =
  withHandle_ "hDuplicate" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> ioException (IOError (Just handle) UnsupportedOperation "hDuplicate" "handles cannot be duplicated" Nothing Nothing)

hDuplicateTo :: Handle -> Handle -> IO ()
hDuplicateTo source target =
  withHandle_ "hDuplicateTo" target $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> ioException (IOError (Just source) UnsupportedOperation "hDuplicateTo" "handles cannot be duplicated" Nothing Nothing)

data LockMode = SharedLock | ExclusiveLock

hLock :: Handle -> LockMode -> IO ()
hLock handle _ =
  withHandle_ "hLock" handle $ \_ ->
    ioException (IOError (Just handle) UnsupportedOperation "hLock" "file locks are not available" Nothing Nothing)

hTryLock :: Handle -> LockMode -> IO Bool
hTryLock handle _ =
  withHandle_ "hTryLock" handle $ \_ ->
    ioException (IOError (Just handle) UnsupportedOperation "hTryLock" "file locks are not available" Nothing Nothing)

-- ---------------------------------------------------------------------
-- Queries

hIsOpen :: Handle -> IO Bool
hIsOpen handle =
  withHandle_ "hIsOpen" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> return False
      SemiClosedHandle -> return False
      _ -> return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle =
  withHandle_ "hIsClosed" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> return True
      _ -> return False

hIsReadable :: Handle -> IO Bool
hIsReadable DuplexHandle {} = return True
hIsReadable handle =
  withHandle_ "hIsReadable" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> return (isReadableHandleType haType)

hIsWritable :: Handle -> IO Bool
hIsWritable DuplexHandle {} = return True
hIsWritable handle =
  withHandle_ "hIsWritable" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> return (isWritableHandleType haType)

hIsSeekable :: Handle -> IO Bool
hIsSeekable handle =
  withHandle_ "hIsSeekable" handle $ \Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      AppendHandle -> return False
      _ -> isSeekable haDevice

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho handle on = do
  terminal <- hIsTerminalDevice handle
  case terminal of
    False -> return ()
    True ->
      withHandle_ "hSetEcho" handle $ \Handle__ {haType, haDevice} ->
        case haType of
          ClosedHandle -> ioe_closedHandle
          _ -> setEcho haDevice on

hGetEcho :: Handle -> IO Bool
hGetEcho handle = do
  terminal <- hIsTerminalDevice handle
  case terminal of
    False -> return False
    True ->
      withHandle_ "hGetEcho" handle $ \Handle__ {haType, haDevice} ->
        case haType of
          ClosedHandle -> ioe_closedHandle
          _ -> getEcho haDevice

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice handle =
  withHandle_ "hIsTerminalDevice" handle $ \Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> isTerminal haDevice

-- | A description of the handle state.
hShow :: Handle -> IO String
hShow handle@(FileHandle path state) = showHandle' path False handle state
hShow handle@(DuplexHandle path _ state) = showHandle' path True handle state

showHandle' :: String -> Bool -> Handle -> MVar Handle__ -> IO String
showHandle' path duplex handle state =
  withHandle_' "showHandle" handle state $ \Handle__ {haType, haBufferMode, haByteBuffer} -> do
    buffer <- readIORef haByteBuffer
    return
      ( "{loc="
          ++ path
          ++ ",type="
          ++ show haType
          ++ (case duplex of True -> ",duplex"; False -> "")
          ++ ",buffering="
          ++ show haBufferMode
          ++ ",bytes="
          ++ show (bufR buffer - bufL buffer)
          ++ "}"
      )

-- | The next character of a handle without consuming it.
hLookAhead :: Handle -> IO Char
hLookAhead handle = wantReadableHandle_ "hLookAhead" handle hLookAhead_
