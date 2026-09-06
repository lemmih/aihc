{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Access to the runtime-owned complete program argument vector. The ABI uses
-- one UTF-8 byte string per argument, terminated by a zero byte.
module GHC.Internal.Environment
  ( getFullArgs,
    setFullArgs,
  )
where

import Data.Foldable (any, elem, foldr)
import GHC.Base (ord)
import GHC.Char (chr)
import GHC.IO (IO (..))
import GHC.IO.Exception (IOErrorType (..), ioError)
import GHC.IO.Runtime (readMemoryByte, writeMemoryByte)
import GHC.Int (Int (..))
import GHC.Prim (Addr#, MutableByteArray#, RealWorld, mutableByteArrayContents#, newPinnedByteArray#)
import System.IO.Error (mkIOError)
import Prelude

data ArgumentBuffer = ArgumentBuffer (MutableByteArray# RealWorld)

foreign import ccall unsafe "aihc_program_arguments_size"
  argumentSize :: IO Int

foreign import ccall unsafe "aihc_program_arguments_copy"
  copyArguments :: Addr# -> Int -> IO Int

foreign import ccall unsafe "aihc_program_arguments_replace"
  replaceArguments :: Addr# -> Int -> IO Int

getFullArgs :: IO [String]
getFullArgs = do
  required <- argumentSize
  readSnapshot required

readSnapshot :: Int -> IO [String]
readSnapshot requested = do
  buffer <- newArgumentBuffer (atLeastOne requested)
  actual <- copyArgumentBuffer buffer requested
  case actual > requested of
    True -> readSnapshot actual
    False -> do
      bytes <- readBytes buffer 0 actual
      return (decodeArguments bytes)

setFullArgs :: [String] -> IO ()
setFullArgs arguments =
  case anyContainsNul arguments of
    True -> ioError (mkIOError InvalidArgument "setArgs" Nothing Nothing)
    False -> do
      let bytes = encodeArguments arguments
          size = byteCount bytes
      buffer <- newArgumentBuffer (atLeastOne size)
      writeBytes buffer 0 bytes
      result <- replaceArgumentBuffer buffer size
      case result == 0 of
        True -> return ()
        False -> ioError (mkIOError InvalidArgument "setArgs" Nothing Nothing)

newArgumentBuffer :: Int -> IO ArgumentBuffer
newArgumentBuffer (I# size) =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# nextState, buffer #) -> (# nextState, ArgumentBuffer buffer #)
    )

atLeastOne :: Int -> Int
atLeastOne size =
  case size < 1 of
    True -> 1
    False -> size

copyArgumentBuffer :: ArgumentBuffer -> Int -> IO Int
copyArgumentBuffer (ArgumentBuffer buffer) = copyArguments (mutableByteArrayContents# buffer)

replaceArgumentBuffer :: ArgumentBuffer -> Int -> IO Int
replaceArgumentBuffer (ArgumentBuffer buffer) = replaceArguments (mutableByteArrayContents# buffer)

readArgumentByte :: ArgumentBuffer -> Int -> IO Int
readArgumentByte (ArgumentBuffer buffer) = readMemoryByte (mutableByteArrayContents# buffer)

writeArgumentByte :: ArgumentBuffer -> Int -> Int -> IO Int
writeArgumentByte (ArgumentBuffer buffer) = writeMemoryByte (mutableByteArrayContents# buffer)

readBytes :: ArgumentBuffer -> Int -> Int -> IO [Int]
readBytes buffer offset length =
  case offset == length of
    True -> return []
    False -> do
      byte <- readArgumentByte buffer offset
      rest <- readBytes buffer (offset + 1) length
      return (byte : rest)

writeBytes :: ArgumentBuffer -> Int -> [Int] -> IO ()
writeBytes _ _ [] = return ()
writeBytes buffer offset (byte : rest) = do
  result <- writeArgumentByte buffer offset byte
  case result == 0 of
    True -> writeBytes buffer (offset + 1) rest
    False -> ioError (mkIOError InvalidArgument "setArgs" Nothing Nothing)

encodeArguments :: [String] -> [Int]
encodeArguments [] = []
encodeArguments (argument : rest) = encodeUtf8 argument ++ (0 : encodeArguments rest)

decodeArguments :: [Int] -> [String]
decodeArguments [] = []
decodeArguments bytes =
  case takeArgumentBytes bytes of
    (argument, rest) -> decodeUtf8 argument : decodeArguments rest

takeArgumentBytes :: [Int] -> ([Int], [Int])
takeArgumentBytes [] = ([], [])
takeArgumentBytes (byte : rest) =
  case byte == 0 of
    True -> ([], rest)
    False ->
      case takeArgumentBytes rest of
        (argument, remaining) -> (byte : argument, remaining)

encodeUtf8 :: String -> [Int]
encodeUtf8 = foldr (\character rest -> encodeCodePoint (ord character) ++ rest) []

encodeCodePoint :: Int -> [Int]
encodeCodePoint codePoint =
  case codePoint <= 127 of
    True -> [codePoint]
    False ->
      case codePoint <= 2047 of
        True -> [192 + quot codePoint 64, 128 + rem codePoint 64]
        False ->
          case codePoint <= 65535 of
            True ->
              [ 224 + quot codePoint 4096,
                128 + rem (quot codePoint 64) 64,
                128 + rem codePoint 64
              ]
            False ->
              [ 240 + quot codePoint 262144,
                128 + rem (quot codePoint 4096) 64,
                128 + rem (quot codePoint 64) 64,
                128 + rem codePoint 64
              ]

decodeUtf8 :: [Int] -> String
decodeUtf8 [] = []
decodeUtf8 (first : rest) =
  case first < 128 of
    True -> chr first : decodeUtf8 rest
    False -> decodeMultibyte first rest

decodeMultibyte :: Int -> [Int] -> String
decodeMultibyte first rest =
  case first >= 194 && first <= 223 of
    True ->
      case rest of
        second : remaining ->
          case continuation second of
            True -> chr ((first - 192) * 64 + second - 128) : decodeUtf8 remaining
            False -> replacement : decodeUtf8 rest
        [] -> [replacement]
    False ->
      case first >= 224 && first <= 239 of
        True -> decodeThree first rest
        False ->
          case first >= 240 && first <= 244 of
            True -> decodeFour first rest
            False -> replacement : decodeUtf8 rest

decodeThree :: Int -> [Int] -> String
decodeThree first rest =
  case rest of
    second : (third : remaining) ->
      let codePoint = (first - 224) * 4096 + (second - 128) * 64 + third - 128
       in case continuation second && continuation third && codePoint >= 2048 && notSurrogate codePoint of
            True -> chr codePoint : decodeUtf8 remaining
            False -> replacement : decodeUtf8 rest
    _ -> replacement : decodeUtf8 rest

decodeFour :: Int -> [Int] -> String
decodeFour first rest =
  case rest of
    second : (third : (fourth : remaining)) ->
      let codePoint = (first - 240) * 262144 + (second - 128) * 4096 + (third - 128) * 64 + fourth - 128
       in case continuation second && continuation third && continuation fourth && codePoint >= 65536 && codePoint <= 1114111 of
            True -> chr codePoint : decodeUtf8 remaining
            False -> replacement : decodeUtf8 rest
    _ -> replacement : decodeUtf8 rest

continuation :: Int -> Bool
continuation byte = byte >= 128 && byte <= 191

notSurrogate :: Int -> Bool
notSurrogate codePoint = codePoint < 55296 || codePoint > 57343

replacement :: Char
replacement = chr 65533

byteCount :: [a] -> Int
byteCount [] = 0
byteCount (_ : rest) = 1 + byteCount rest

anyContainsNul :: [String] -> Bool
anyContainsNul = any containsNul

containsNul :: String -> Bool
containsNul = elem (chr 0)
