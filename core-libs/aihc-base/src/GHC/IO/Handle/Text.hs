{-# LANGUAGE NamedFieldPuns #-}

-- | Text and block transfers on handles. Text goes through the byte
-- buffer as UTF-8.
module GHC.IO.Handle.Text
  ( hPutChar,
    hPutStr,
    hPutStrLn,
    hGetBuf,
    hGetBufSome,
    hGetBufNonBlocking,
    hPutBuf,
    hPutBufNonBlocking,
    hGetChar,
    hGetLine,
    hGetContents,
    hGetContents',
    hWaitForInput,
  )
where

import Data.Bool (Bool (..), not, (&&), (||))
import Data.Maybe (Maybe (..), maybe)
import Foreign.Storable (Storable (..))
import GHC.Base (Monad (..), String, ord, ($), (++), (.))
import GHC.Char (chr)
import GHC.IO (IO)
import GHC.IO.Buffer
  ( Buffer (..),
    RawBuffer,
    bufferAvailable,
    bufferElems,
    isEmptyBuffer,
    readWord8Buf,
    withRawBuffer,
  )
import GHC.IO.Handle.Internals
  ( flushByteWriteBuffer,
    flushCharReadBuffer,
    flushWriteBuffer,
    ioe_EOF,
    wantReadableHandle,
    wantReadableHandle_,
    wantWritableHandle,
  )
import GHC.IO.Unsafe (unsafeInterleaveIO)
import GHC.IORef (readIORef, writeIORef)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Handle.Read (fillIfEmpty, readByte, readUtf8Char)
import GHC.Internal.IO.Types
  ( BufferMode (..),
    Handle (..),
    HandleType (..),
    Handle__ (..),
    IOErrorType (..),
    IOException (..),
    ioException,
  )
import GHC.Internal.IO.Types qualified as Buffered (fillReadBuffer, flushWriteBuffer)
import GHC.Internal.IO.Types qualified as RawIO (write)
import GHC.Num (Num (..))
import GHC.Ptr (Ptr, castPtr, plusPtr)
import GHC.Real (Integral (..), fromIntegral)
import GHC.Show (Show (..))
import GHC.Types (Char)
import GHC.Word (Word8)

-- ---------------------------------------------------------------------
-- Text output

hPutChar :: Handle -> Char -> IO ()
hPutChar handle character =
  wantWritableHandle "hPutChar" handle $ \handle_ -> do
    writeChars handle_ [character]
    flushForMode handle_ (character == '\n')

hPutStr :: Handle -> String -> IO ()
hPutStr handle text = hPutStr' handle text False

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle text = hPutStr' handle text True

hPutStr' :: Handle -> String -> Bool -> IO ()
hPutStr' handle text addNewline =
  wantWritableHandle "hPutStr" handle $ \handle_ -> do
    newline <- writeChars handle_ text
    case addNewline of
      True -> do
        _ <- writeChars handle_ "\n"
        flushForMode handle_ True
      False -> flushForMode handle_ newline

-- | Flush the byte buffer when the buffer mode asks for it.
flushForMode :: Handle__ -> Bool -> IO ()
flushForMode handle_@Handle__ {haBufferMode} sawNewline =
  case haBufferMode of
    NoBuffering -> flushByteWriteBuffer handle_
    LineBuffering ->
      case sawNewline of
        True -> flushByteWriteBuffer handle_
        False -> return ()
    BlockBuffering _ -> return ()

-- | Encode the characters as UTF-8 into the byte buffer. The result says
-- whether the text had a newline.
writeChars :: Handle__ -> String -> IO Bool
writeChars _ [] = return False
writeChars handle_@Handle__ {haByteBuffer} (character : rest) = do
  buffer <- readIORef haByteBuffer
  buffer' <-
    case bufferAvailable buffer < 4 of
      True -> do
        flushByteWriteBuffer handle_
        readIORef haByteBuffer
      False -> return buffer
  written <- withRawBuffer (bufRaw buffer') $ \pointer -> encodeUtf8 pointer (bufR buffer') (ord character)
  writeIORef haByteBuffer buffer' {bufR = written}
  newline <- writeChars handle_ rest
  return (character == '\n' || newline)

-- | Write one code point as UTF-8 and give the next offset.
encodeUtf8 :: Ptr Word8 -> Int -> Int -> IO Int
encodeUtf8 pointer offset code
  | code < 0x80 = do
      byte offset code
      return (offset + 1)
  | code < 0x800 = do
      byte offset (0xC0 + quot code 64)
      byte (offset + 1) (0x80 + rem code 64)
      return (offset + 2)
  | code < 0x10000 = do
      byte offset (0xE0 + quot code 4096)
      byte (offset + 1) (0x80 + rem (quot code 64) 64)
      byte (offset + 2) (0x80 + rem code 64)
      return (offset + 3)
  | otherwise = do
      byte offset (0xF0 + quot code 262144)
      byte (offset + 1) (0x80 + rem (quot code 4096) 64)
      byte (offset + 2) (0x80 + rem (quot code 64) 64)
      byte (offset + 3) (0x80 + rem code 64)
      return (offset + 4)
  where
    otherwise = True
    byte index value = pokeByteOff pointer index (fromIntegral value :: Word8)

-- ---------------------------------------------------------------------
-- Text input

-- | Read one character. The bytes are UTF-8.
hGetChar :: Handle -> IO Char
hGetChar handle =
  wantReadableHandle_ "hGetChar" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    result <- readUtf8Char handle_
    maybe ioe_EOF return result

hGetLine :: Handle -> IO String
hGetLine handle =
  wantReadableHandle_ "hGetLine" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    first <- readUtf8Char handle_
    case first of
      Nothing -> ioe_EOF
      Just character -> readLine handle_ character

readLine :: Handle__ -> Char -> IO String
readLine handle_ character =
  case character == '\n' of
    True -> return []
    False -> do
      next <- readUtf8Char handle_
      case next of
        Nothing -> return [character]
        Just following -> do
          rest <- readLine handle_ following
          return (character : rest)

-- | Read the rest of the input lazily and semi-close the handle.
hGetContents :: Handle -> IO String
hGetContents handle =
  wantReadableHandle "hGetContents" handle $ \handle_ -> do
    text <- lazyRead handle_
    return (handle_ {haType = SemiClosedHandle}, text)

lazyRead :: Handle__ -> IO String
lazyRead handle_ =
  unsafeInterleaveIO $ do
    next <- readUtf8Char handle_
    case next of
      Nothing -> return []
      Just character -> do
        rest <- lazyRead handle_
        return (character : rest)

-- | Read the rest of the input strictly.
hGetContents' :: Handle -> IO String
hGetContents' handle =
  wantReadableHandle_ "hGetContents'" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    strictRead handle_

strictRead :: Handle__ -> IO String
strictRead handle_ = do
  next <- readUtf8Char handle_
  case next of
    Nothing -> return []
    Just character -> do
      rest <- strictRead handle_
      return (character : rest)

-- | Input is always ready or at the end.
hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput handle _ =
  wantReadableHandle_ "hWaitForInput" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    fillIfEmpty handle_

-- ---------------------------------------------------------------------
-- Block input

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf handle pointer count
  | count == 0 = return 0
  | count < 0 = illegalBufferSize handle "hGetBuf" count
  | otherwise =
      wantReadableHandle_ "hGetBuf" handle $ \handle_@Handle__ {haByteBuffer} -> do
        flushCharReadBuffer handle_
        buffer <- readIORef haByteBuffer
        case isEmptyBuffer buffer of
          True -> bufReadEmpty handle_ buffer (castPtr pointer) 0 count
          False -> bufReadNonEmpty handle_ buffer (castPtr pointer) 0 count
  where
    otherwise = True

bufReadNonEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadNonEmpty handle_@Handle__ {haByteBuffer} buffer@Buffer {bufRaw, bufR, bufL} pointer soFar count = do
  let available = bufR - bufL
  case count < available of
    True -> do
      copyFromRawBuffer pointer bufRaw bufL count
      writeIORef haByteBuffer buffer {bufL = bufL + count}
      return (soFar + count)
    False -> do
      copyFromRawBuffer pointer bufRaw bufL available
      let buffer' = buffer {bufR = 0, bufL = 0}
      writeIORef haByteBuffer buffer'
      let remaining = count - available
          soFar' = soFar + available
          pointer' = pointer `plusPtr` available
      case remaining == 0 of
        True -> return soFar'
        False -> bufReadEmpty handle_ buffer' pointer' soFar' remaining

bufReadEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadEmpty handle_@Handle__ {haByteBuffer, haDevice} buffer pointer soFar count = do
  (transferred, buffer') <- Buffered.fillReadBuffer haDevice buffer
  case transferred == 0 of
    True -> return soFar
    False -> do
      writeIORef haByteBuffer buffer'
      bufReadNonEmpty handle_ buffer' pointer soFar count

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome handle pointer count
  | count == 0 = return 0
  | count < 0 = illegalBufferSize handle "hGetBufSome" count
  | otherwise =
      wantReadableHandle_ "hGetBufSome" handle $ \handle_@Handle__ {haByteBuffer, haDevice} -> do
        flushCharReadBuffer handle_
        buffer <- readIORef haByteBuffer
        case isEmptyBuffer buffer of
          True -> do
            (transferred, buffer') <- Buffered.fillReadBuffer haDevice buffer
            case transferred == 0 of
              True -> return 0
              False -> do
                writeIORef haByteBuffer buffer'
                bufReadNBNonEmpty handle_ buffer' (castPtr pointer) 0 count
          False -> bufReadNBNonEmpty handle_ buffer (castPtr pointer) 0 count
  where
    otherwise = True

-- | The runtime has no non-blocking reads, so this is 'hGetBufSome'.
hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = hGetBufSome

bufReadNBNonEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadNBNonEmpty Handle__ {haByteBuffer} buffer@Buffer {bufRaw, bufR, bufL} pointer soFar count = do
  let available = bufR - bufL
  case count < available of
    True -> do
      copyFromRawBuffer pointer bufRaw bufL count
      writeIORef haByteBuffer buffer {bufL = bufL + count}
      return (soFar + count)
    False -> do
      copyFromRawBuffer pointer bufRaw bufL available
      writeIORef haByteBuffer buffer {bufR = 0, bufL = 0}
      return (soFar + available)

-- | Copy @count@ bytes from the raw buffer at @offset@ to the pointer.
copyFromRawBuffer :: Ptr Word8 -> RawBuffer Word8 -> Int -> Int -> IO ()
copyFromRawBuffer pointer raw offset count =
  withRawBuffer raw $ \source -> copyBytesLoop pointer source offset 0 count

copyBytesLoop :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int -> IO ()
copyBytesLoop target source sourceOffset targetOffset count =
  case count <= 0 of
    True -> return ()
    False -> do
      value <- peekByteOff source sourceOffset :: IO Word8
      pokeByteOff target targetOffset value
      copyBytesLoop target source (sourceOffset + 1) (targetOffset + 1) (count - 1)

-- | Copy @count@ bytes from the pointer into the raw buffer at @offset@.
copyToRawBuffer :: RawBuffer Word8 -> Int -> Ptr Word8 -> Int -> IO ()
copyToRawBuffer raw offset pointer count =
  withRawBuffer raw $ \target -> copyBytesLoop target pointer 0 offset count

-- ---------------------------------------------------------------------
-- Block output

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf handle pointer count = do
  _ <- hPutBuf' handle pointer count True
  return ()

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking handle pointer count = hPutBuf' handle pointer count False

hPutBuf' :: Handle -> Ptr a -> Int -> Bool -> IO Int
hPutBuf' handle pointer count canBlock
  | count == 0 = return 0
  | count < 0 = illegalBufferSize handle "hPutBuf" count
  | otherwise =
      wantWritableHandle "hPutBuf" handle $ \handle_@Handle__ {haBufferMode} -> do
        written <- bufWrite handle_ (castPtr pointer) count canBlock
        -- A handle without block buffering flushes after each write.
        case haBufferMode of
          BlockBuffering _ -> return ()
          _ -> flushWriteBuffer handle_
        return written
  where
    otherwise = True

bufWrite :: Handle__ -> Ptr Word8 -> Int -> Bool -> IO Int
bufWrite handle_@Handle__ {haByteBuffer, haDevice} pointer count canBlock = do
  old@Buffer {bufRaw, bufR, bufSize} <- readIORef haByteBuffer
  case bufSize - bufR > count of
    True -> do
      -- The data fits in the buffer.
      copyToRawBuffer bufRaw bufR pointer count
      writeIORef haByteBuffer old {bufR = bufR + count}
      return count
    False -> do
      -- Flush the buffer. Data that fits in the empty buffer goes into
      -- it. Larger data goes to the device at once.
      old' <- Buffered.flushWriteBuffer haDevice old
      writeIORef haByteBuffer old'
      case count < bufSize of
        True -> bufWrite handle_ pointer count canBlock
        False -> do
          RawIO.write haDevice pointer (bufOffset old') count
          writeIORef haByteBuffer old' {bufOffset = bufOffset old' + fromIntegral count}
          return count

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle function size =
  ioException (IOError (Just handle) InvalidArgument function ("illegal buffer size " ++ show size) Nothing Nothing)
