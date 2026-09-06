{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Use camelCase" -}

-- | The handle operations that the public handle modules share: the
-- locking wrappers, the buffer flushes, the handle constructors, and the
-- IO errors of the handle layer.
module GHC.IO.Handle.Internals
  ( withHandle,
    withHandle',
    withHandle_,
    withHandle_',
    withHandle__',
    withAllHandles__,
    wantWritableHandle,
    wantReadableHandle,
    wantReadableHandle_,
    wantSeekableHandle,
    mkHandle,
    mkFileHandle,
    mkDuplexHandle,
    openTextEncoding,
    closeTextCodecs,
    initBufferState,
    dEFAULT_CHAR_BUFFER_SIZE,
    flushBuffer,
    flushWriteBuffer,
    flushCharReadBuffer,
    flushCharBuffer,
    flushByteReadBuffer,
    flushByteWriteBuffer,
    augmentIOError,
    ioe_closedHandle,
    ioe_semiclosedHandle,
    ioe_EOF,
    ioe_notReadable,
    ioe_notWritable,
    ioe_finalizedHandle,
    ioe_bufsiz,
    hClose_help,
    hLookAhead_,
    HandleFinalizer,
    handleFinalizer,
    debugIO,
    traceIO,
  )
where

import Data.Bool (Bool (..), not)
import Data.Maybe (Maybe (..), isJust)
import Data.Typeable (Typeable)
import GHC.Base (Monad (..), String, ($), (++))
import GHC.Exception.Type (SomeException)
import GHC.IO (FilePath, IO, catchException, onException, throwIO)
import GHC.IO.Buffer
  ( Buffer (..),
    BufferState (..),
    CharBuffer,
    bufferElems,
    isEmptyBuffer,
    isWriteBuffer,
    newByteBuffer,
    newCharBuffer,
  )
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Handle.Read (fillIfEmpty, readUtf8Char)
import GHC.Internal.IO.Types hiding (flushWriteBuffer)
import GHC.Internal.IO.Types qualified as Buffered (emptyWriteBuffer, flushWriteBuffer, newBuffer)
import GHC.MVar (MVar, newMVar, putMVar, takeMVar)
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import GHC.Show (Show (..))
import GHC.Types (Char)
import GHC.Word (Word8)

-- ---------------------------------------------------------------------
-- Handle locking

-- | Run an action with the state of a handle. The action gives the new
-- state. An exception in the action restores the old state and gets the
-- handle and the operation name.
withHandle :: String -> Handle -> (Handle__ -> IO (Handle__, a)) -> IO a
withHandle function handle@(FileHandle _ state) action = withHandle' function handle state action
withHandle function handle@(DuplexHandle _ state _) action = withHandle' function handle state action

withHandle' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO (Handle__, a)) -> IO a
withHandle' function handle state action = do
  old <- takeMVar state
  (new, result) <-
    catchException
      (action old `onException` putMVar state old)
      (rethrowAugmented function handle)
  checkHandleInvariants new
  putMVar state new
  return result

withHandle_ :: String -> Handle -> (Handle__ -> IO a) -> IO a
withHandle_ function handle@(FileHandle _ state) action = withHandle_' function handle state action
withHandle_ function handle@(DuplexHandle _ state _) action = withHandle_' function handle state action

withHandle_' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO a) -> IO a
withHandle_' function handle state action =
  withHandle' function handle state $ \handle_ -> do
    result <- action handle_
    return (handle_, result)

withAllHandles__ :: String -> Handle -> (Handle__ -> IO Handle__) -> IO ()
withAllHandles__ function handle@(FileHandle _ state) action = withHandle__' function handle state action
withAllHandles__ function handle@(DuplexHandle _ readSide writeSide) action = do
  withHandle__' function handle readSide action
  withHandle__' function handle writeSide action

withHandle__' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO Handle__) -> IO ()
withHandle__' function handle state action =
  withHandle' function handle state $ \handle_ -> do
    new <- action handle_
    return (new, ())

rethrowAugmented :: String -> Handle -> IOException -> IO a
rethrowAugmented function handle exception = throwIO (augmentIOError exception function handle)

-- | Put the handle and the operation name into an IO error.
augmentIOError :: IOException -> String -> Handle -> IOException
augmentIOError exception function handle =
  exception {ioe_handle = Just handle, ioe_location = function, ioe_filename = Just (handlePath handle)}

handlePath :: Handle -> FilePath
handlePath (FileHandle path _) = path
handlePath (DuplexHandle path _ _) = path

-- ---------------------------------------------------------------------
-- Access checks

wantWritableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantWritableHandle function handle@(FileHandle _ state) action = wantWritableHandle' function handle state action
wantWritableHandle function handle@(DuplexHandle _ _ state) action = wantWritableHandle' function handle state action

wantWritableHandle' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO a) -> IO a
wantWritableHandle' function handle state action =
  withHandle_' function handle state (checkWritableHandle action)

checkWritableHandle :: (Handle__ -> IO a) -> Handle__ -> IO a
checkWritableHandle action handle_@Handle__ {haType, haByteBuffer, haCharBuffer, haDevice} =
  case haType of
    ClosedHandle -> ioe_closedHandle
    SemiClosedHandle -> ioe_semiclosedHandle
    ReadHandle -> ioe_notWritable
    ReadWriteHandle -> do
      charBuffer <- readIORef haCharBuffer
      case isWriteBuffer charBuffer of
        True -> return ()
        False -> do
          flushCharReadBuffer handle_
          flushByteReadBuffer handle_
          charBuffer' <- readIORef haCharBuffer
          writeIORef haCharBuffer charBuffer' {bufState = WriteBuffer}
          byteBuffer <- readIORef haByteBuffer
          byteBuffer' <- Buffered.emptyWriteBuffer haDevice byteBuffer
          writeIORef haByteBuffer byteBuffer'
      action handle_
    _ -> action handle_

wantReadableHandle :: String -> Handle -> (Handle__ -> IO (Handle__, a)) -> IO a
wantReadableHandle function handle action = withHandle function handle (checkReadableHandle action)

wantReadableHandle_ :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantReadableHandle_ function handle@(FileHandle _ state) action = wantReadableHandle' function handle state action
wantReadableHandle_ function handle@(DuplexHandle _ state _) action = wantReadableHandle' function handle state action

wantReadableHandle' :: String -> Handle -> MVar Handle__ -> (Handle__ -> IO a) -> IO a
wantReadableHandle' function handle state action =
  withHandle_' function handle state (checkReadableHandle action)

checkReadableHandle :: (Handle__ -> IO a) -> Handle__ -> IO a
checkReadableHandle action handle_@Handle__ {haType, haByteBuffer, haCharBuffer} =
  case haType of
    ClosedHandle -> ioe_closedHandle
    SemiClosedHandle -> ioe_semiclosedHandle
    AppendHandle -> ioe_notReadable
    WriteHandle -> ioe_notReadable
    ReadWriteHandle -> do
      byteBuffer <- readIORef haByteBuffer
      case isWriteBuffer byteBuffer of
        False -> return ()
        True -> do
          case bufR byteBuffer > bufL byteBuffer of
            True -> flushWriteBuffer handle_
            False -> return ()
          charBuffer <- readIORef haCharBuffer
          writeIORef haCharBuffer charBuffer {bufState = ReadBuffer}
          byteBuffer' <- readIORef haByteBuffer
          writeIORef haByteBuffer byteBuffer' {bufState = ReadBuffer}
      action handle_
    _ -> action handle_

wantSeekableHandle :: String -> Handle -> (Handle__ -> IO a) -> IO a
wantSeekableHandle function handle@DuplexHandle {} _ =
  ioException (IOError (Just handle) IllegalOperation function "handle is not seekable" Nothing Nothing)
wantSeekableHandle function handle@(FileHandle _ state) action =
  withHandle_' function handle state (checkSeekableHandle action)

checkSeekableHandle :: (Handle__ -> IO a) -> Handle__ -> IO a
checkSeekableHandle action handle_@Handle__ {haType, haDevice} =
  case haType of
    ClosedHandle -> ioe_closedHandle
    SemiClosedHandle -> ioe_semiclosedHandle
    AppendHandle -> ioe_notSeekable
    _ -> do
      seekable <- isSeekable haDevice
      case seekable of
        True -> action handle_
        False -> ioe_notSeekable

-- ---------------------------------------------------------------------
-- Errors

ioe_closedHandle, ioe_semiclosedHandle, ioe_EOF, ioe_notReadable, ioe_notWritable, ioe_notSeekable, ioe_finalizedHandle :: IO a
ioe_closedHandle = ioException (IOError Nothing IllegalOperation "" "handle is closed" Nothing Nothing)
ioe_semiclosedHandle = ioException (IOError Nothing IllegalOperation "" "handle is semi-closed" Nothing Nothing)
ioe_EOF = ioException (IOError Nothing EOF "" "" Nothing Nothing)
ioe_notReadable = ioException (IOError Nothing IllegalOperation "" "handle is not open for reading" Nothing Nothing)
ioe_notWritable = ioException (IOError Nothing IllegalOperation "" "handle is not open for writing" Nothing Nothing)
ioe_notSeekable = ioException (IOError Nothing IllegalOperation "" "handle is not seekable" Nothing Nothing)
ioe_finalizedHandle = ioException (IOError Nothing IllegalOperation "" "handle is finalized" Nothing Nothing)

ioe_bufsiz :: Int -> IO a
ioe_bufsiz size =
  ioException (IOError Nothing InvalidArgument "hSetBuffering" ("illegal buffer size " ++ show size) Nothing Nothing)

-- ---------------------------------------------------------------------
-- Buffers

-- | Write the buffered bytes to the device and drop the buffered
-- characters.
flushBuffer :: Handle__ -> IO ()
flushBuffer handle_@Handle__ {haCharBuffer} = do
  charBuffer <- readIORef haCharBuffer
  case bufState charBuffer of
    ReadBuffer -> do
      flushCharReadBuffer handle_
      flushByteReadBuffer handle_
    WriteBuffer -> flushByteWriteBuffer handle_

-- | Write the buffered bytes of a write handle to the device.
flushWriteBuffer :: Handle__ -> IO ()
flushWriteBuffer handle_@Handle__ {haByteBuffer} = do
  byteBuffer <- readIORef haByteBuffer
  case isWriteBuffer byteBuffer of
    True -> flushByteWriteBuffer handle_
    False -> return ()

flushByteWriteBuffer :: Handle__ -> IO ()
flushByteWriteBuffer Handle__ {haByteBuffer, haDevice} = do
  byteBuffer <- readIORef haByteBuffer
  case isEmptyBuffer byteBuffer of
    True -> return ()
    False -> do
      byteBuffer' <- Buffered.flushWriteBuffer haDevice byteBuffer
      writeIORef haByteBuffer byteBuffer'

-- | Text goes to the byte buffer at once, so the character buffer of a
-- write handle is always empty.
flushCharBuffer :: Handle__ -> IO ()
flushCharBuffer handle_@Handle__ {haCharBuffer} = do
  charBuffer <- readIORef haCharBuffer
  case bufState charBuffer of
    ReadBuffer -> flushCharReadBuffer handle_
    WriteBuffer -> writeIORef haCharBuffer charBuffer {bufL = 0, bufR = 0}

-- | Text reads take their bytes from the byte buffer at once, so the
-- character buffer of a read handle is always empty.
flushCharReadBuffer :: Handle__ -> IO ()
flushCharReadBuffer Handle__ {haCharBuffer} = do
  charBuffer <- readIORef haCharBuffer
  writeIORef haCharBuffer charBuffer {bufL = 0, bufR = 0}

-- | Drop the unread bytes. A seekable device moves back by the same
-- count.
flushByteReadBuffer :: Handle__ -> IO ()
flushByteReadBuffer Handle__ {haByteBuffer, haDevice} = do
  byteBuffer <- readIORef haByteBuffer
  case isEmptyBuffer byteBuffer of
    True -> return ()
    False -> do
      seekable <- isSeekable haDevice
      case seekable of
        True -> do
          _ <- seek haDevice RelativeSeek (negate (fromIntegral (bufferElems byteBuffer)))
          return ()
        False -> return ()
      writeIORef haByteBuffer byteBuffer {bufL = 0, bufR = 0}

-- ---------------------------------------------------------------------
-- Handle construction

type HandleFinalizer = FilePath -> MVar Handle__ -> IO ()

-- | Close the handle. GHC runs this function when the garbage collector
-- drops a handle. This runtime has no finalizers.
handleFinalizer :: HandleFinalizer
handleFinalizer _ state = do
  handle_ <- takeMVar state
  (closed, _) <- hClose_help handle_
  putMVar state closed

mkHandle ::
  (RawIO dev, IODevice dev, BufferedIO dev, Typeable dev) =>
  dev ->
  FilePath ->
  HandleType ->
  Bool ->
  Maybe TextEncoding ->
  NewlineMode ->
  Maybe HandleFinalizer ->
  Maybe (MVar Handle__) ->
  IO Handle
mkHandle device path handleType buffered codec newlines _finalizer otherSide = do
  let state = initBufferState handleType
  byteBuffer <- Buffered.newBuffer device state
  byteBufferRef <- newIORef byteBuffer
  (charBufferRef, mode) <-
    case buffered of
      True -> getCharBuffer device state
      False -> mkUnBuffer state
  spares <- newIORef BufferListNil
  let handle_ =
        Handle__
          { haDevice = device,
            haType = handleType,
            haByteBuffer = byteBufferRef,
            haBufferMode = mode,
            haCharBuffer = charBufferRef,
            haBuffers = spares,
            haCodec = codec,
            haInputNL = inputNL newlines,
            haOutputNL = outputNL newlines,
            haOtherSide = otherSide
          }
  stateVar <- newMVar handle_
  return (FileHandle path stateVar)

mkFileHandle ::
  (RawIO dev, IODevice dev, BufferedIO dev, Typeable dev) =>
  dev ->
  FilePath ->
  IOMode ->
  Maybe TextEncoding ->
  NewlineMode ->
  IO Handle
mkFileHandle device path mode codec newlines =
  mkHandle device path (ioModeToHandleType mode) True codec newlines Nothing Nothing

mkDuplexHandle ::
  (RawIO dev, IODevice dev, BufferedIO dev, Typeable dev) =>
  dev ->
  FilePath ->
  Maybe TextEncoding ->
  NewlineMode ->
  IO Handle
mkDuplexHandle device path codec newlines = do
  writeSide <- mkHandle device path WriteHandle True codec newlines Nothing Nothing
  writeVar <-
    case writeSide of
      FileHandle _ var -> return var
      DuplexHandle _ _ var -> return var
  readSide <- mkHandle device path ReadHandle True codec newlines Nothing (Just writeVar)
  readVar <-
    case readSide of
      FileHandle _ var -> return var
      DuplexHandle _ var _ -> return var
  return (DuplexHandle path readVar writeVar)

ioModeToHandleType :: IOMode -> HandleType
ioModeToHandleType ReadMode = ReadHandle
ioModeToHandleType WriteMode = WriteHandle
ioModeToHandleType ReadWriteMode = ReadWriteHandle
ioModeToHandleType AppendMode = AppendHandle

initBufferState :: HandleType -> BufferState
initBufferState ReadHandle = ReadBuffer
initBufferState _ = WriteBuffer

-- | Text encodings have no state in this runtime.
openTextEncoding :: Maybe TextEncoding -> HandleType -> (Maybe TextEncoding -> IO a) -> IO a
openTextEncoding codec _ action = action codec

closeTextCodecs :: Handle__ -> IO ()
closeTextCodecs _ = return ()

dEFAULT_CHAR_BUFFER_SIZE :: Int
dEFAULT_CHAR_BUFFER_SIZE = 2048

getCharBuffer :: (IODevice dev) => dev -> BufferState -> IO (IORef CharBuffer, BufferMode)
getCharBuffer device state = do
  buffer <- newCharBuffer dEFAULT_CHAR_BUFFER_SIZE state
  ref <- newIORef buffer
  terminal <- isTerminal device
  let mode =
        case terminal of
          True -> LineBuffering
          False -> BlockBuffering Nothing
  return (ref, mode)

mkUnBuffer :: BufferState -> IO (IORef CharBuffer, BufferMode)
mkUnBuffer state = do
  buffer <- newCharBuffer dEFAULT_CHAR_BUFFER_SIZE state
  ref <- newIORef buffer
  return (ref, NoBuffering)

-- ---------------------------------------------------------------------
-- Closing

-- | Flush and close a handle. The result carries the first exception.
hClose_help :: Handle__ -> IO (Handle__, Maybe SomeException)
hClose_help handle_ =
  case haType handle_ of
    ClosedHandle -> return (handle_, Nothing)
    _ -> do
      flushException <- trymaybe (flushWriteBuffer handle_)
      (closed, closeException) <- hClose_handle_ handle_
      return
        ( closed,
          case isJust flushException of
            True -> flushException
            False -> closeException
        )

hClose_handle_ :: Handle__ -> IO (Handle__, Maybe SomeException)
hClose_handle_ handle_@Handle__ {haDevice, haOtherSide, haBuffers, haCharBuffer, haByteBuffer} = do
  closeException <-
    case haOtherSide of
      Nothing -> trymaybe (close haDevice)
      Just _ -> return Nothing
  writeIORef haBuffers BufferListNil
  writeIORef haCharBuffer noCharBuffer
  writeIORef haByteBuffer noByteBuffer
  return (handle_ {haType = ClosedHandle}, closeException)

noCharBuffer :: CharBuffer
noCharBuffer = unsafePerformIO (newCharBuffer 1 ReadBuffer)
{-# NOINLINE noCharBuffer #-}

noByteBuffer :: Buffer Word8
noByteBuffer = unsafePerformIO (newByteBuffer 1 ReadBuffer)
{-# NOINLINE noByteBuffer #-}

trymaybe :: IO () -> IO (Maybe SomeException)
trymaybe action =
  catchException
    ( do
        action
        return Nothing
    )
    keepException

keepException :: SomeException -> IO (Maybe SomeException)
keepException exception = return (Just exception)

-- ---------------------------------------------------------------------
-- Debug output

debugIO :: String -> IO ()
debugIO _ = return ()

traceIO :: String -> IO ()
traceIO _ = return ()

-- | The next character of a readable handle, left in the buffer.
hLookAhead_ :: Handle__ -> IO Char
hLookAhead_ handle_@Handle__ {haByteBuffer} = do
  flushCharReadBuffer handle_
  filled <- fillIfEmpty handle_
  case filled of
    False -> ioe_EOF
    True -> do
      buffer <- readIORef haByteBuffer
      result <- readUtf8Char handle_
      case result of
        Nothing -> ioe_EOF
        Just character -> do
          writeIORef haByteBuffer buffer
          return character
