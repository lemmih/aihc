-- | The standard IO interface.
module System.IO
  ( IO,
    fixIO,
    FilePath,
    Handle,
    stdin,
    stdout,
    stderr,
    withFile,
    openFile,
    IOMode (..),
    hClose,
    readFile,
    readFile',
    writeFile,
    appendFile,
    hFileSize,
    hSetFileSize,
    hIsEOF,
    isEOF,
    BufferMode (..),
    hSetBuffering,
    hGetBuffering,
    hFlush,
    hGetPosn,
    hSetPosn,
    HandlePosn,
    hSeek,
    SeekMode (..),
    hTell,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    hIsTerminalDevice,
    hSetEcho,
    hGetEcho,
    hShow,
    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hLookAhead,
    hGetContents,
    hGetContents',
    hPutChar,
    hPutStr,
    hPutStrLn,
    hPrint,
    interact,
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    getContents',
    readIO,
    readLn,
    withBinaryFile,
    openBinaryFile,
    hSetBinaryMode,
    hPutBuf,
    hGetBuf,
    hGetBufSome,
    hPutBufNonBlocking,
    hGetBufNonBlocking,
    hSetEncoding,
    hGetEncoding,
    TextEncoding,
    latin1,
    utf8,
    utf8_bom,
    utf16,
    utf16le,
    utf16be,
    utf32,
    utf32le,
    utf32be,
    localeEncoding,
    char8,
    mkTextEncoding,
    hSetNewlineMode,
    Newline (..),
    nativeNewline,
    NewlineMode (..),
    noNewlineTranslation,
    universalNewlineMode,
    nativeNewlineMode,
  )
where

import GHC.IO.Encoding (TextEncoding, char8, latin1, mkTextEncoding, utf8, utf8_bom)
import GHC.IO.Handle
import GHC.IO.Handle.Text (hGetBuf, hGetBufNonBlocking, hGetBufSome, hGetChar, hGetContents, hGetContents', hGetLine, hPutBuf, hPutBufNonBlocking, hPutChar, hPutStr, hPutStrLn, hWaitForInput)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openBinaryFile, openFile, stderr, stdin, stdout, withBinaryFile, withFile)
import Prelude (Bool (..), Char, FilePath, IO, Read (..), Show (..), String, error, pure, read, return, (>>=))

-- | Handles cannot tell whether they are terminals.
hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice _ = pure False

hReady :: Handle -> IO Bool
hReady handle = hWaitForInput handle 0

hPrint :: (Show a) => Handle -> a -> IO ()
hPrint handle value = hPutStrLn handle (show value)

putChar :: Char -> IO ()
putChar = hPutChar stdout

putStr :: String -> IO ()
putStr = hPutStr stdout

putStrLn :: String -> IO ()
putStrLn = hPutStrLn stdout

print :: (Show a) => a -> IO ()
print value = putStrLn (show value)

getChar :: IO Char
getChar = hGetChar stdin

getLine :: IO String
getLine = hGetLine stdin

getContents :: IO String
getContents = hGetContents stdin

getContents' :: IO String
getContents' = hGetContents' stdin

interact :: (String -> String) -> IO ()
interact function = do
  input <- getContents
  putStr (function input)

readFile :: FilePath -> IO String
readFile path = openFile path ReadMode >>= hGetContents

readFile' :: FilePath -> IO String
readFile' path = withFile path ReadMode hGetContents'

writeFile :: FilePath -> String -> IO ()
writeFile path text = withFile path WriteMode (`hPutStr` text)

appendFile :: FilePath -> String -> IO ()
appendFile path text = withFile path AppendMode (`hPutStr` text)

readLn :: (Read a) => IO a
readLn = getLine >>= readIO

readIO :: (Read a) => String -> IO a
readIO text = return (read text)

fixIO :: (a -> IO a) -> IO a
fixIO _ = error "System.IO.fixIO: not available"

-- The runtime only has UTF-8. The other encodings carry their names.
utf16, utf16le, utf16be, utf32, utf32le, utf32be, localeEncoding :: TextEncoding
utf16 = utf8
utf16le = utf8
utf16be = utf8
utf32 = utf8
utf32le = utf8
utf32be = utf8
localeEncoding = utf8
