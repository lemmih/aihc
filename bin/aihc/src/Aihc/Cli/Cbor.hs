-- | The CBOR primitives that the store artifacts use.
--
-- The artifact encoders write a small subset of CBOR: arrays, text strings,
-- unsigned integers, and negative integers. This module holds one copy of
-- that subset for each artifact module.
module Aihc.Cli.Cbor
  ( cborArray,
    cborText,
    cborWord,
    cborInt,
    cborMajor,
    getArrayLength,
    getText,
    getWord,
    getInt,
    getMajor,
  )
where

import Control.Monad (unless)
import Data.Binary.Get qualified as Get
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)

cborArray :: Int -> Builder.Builder
cborArray = cborMajor 4 . fromIntegral

cborText :: Text -> Builder.Builder
cborText value = cborMajor 3 (fromIntegral (BS.length bytes)) <> Builder.byteString bytes
  where
    bytes = TE.encodeUtf8 value

cborWord :: Word64 -> Builder.Builder
cborWord = cborMajor 0

cborInt :: Int -> Builder.Builder
cborInt value
  | value >= 0 = cborMajor 0 (fromIntegral value)
  | otherwise = cborMajor 1 (fromIntegral (-1 - value))

cborMajor :: Word8 -> Word64 -> Builder.Builder
cborMajor major value
  | value < 24 = Builder.word8 (major * 32 + fromIntegral value)
  | value <= 255 = Builder.word8 (major * 32 + 24) <> Builder.word8 (fromIntegral value)
  | value <= 65535 = Builder.word8 (major * 32 + 25) <> Builder.word16BE (fromIntegral value)
  | value <= 4294967295 = Builder.word8 (major * 32 + 26) <> Builder.word32BE (fromIntegral value)
  | otherwise = Builder.word8 (major * 32 + 27) <> Builder.word64BE value

getArrayLength :: Get.Get Int
getArrayLength = fromIntegral <$> getMajor 4

getText :: Get.Get Text
getText = do
  length' <- getMajor 3
  TE.decodeUtf8 <$> Get.getByteString (fromIntegral length')

getWord :: Get.Get Word64
getWord = getMajor 0

getInt :: Get.Get Int
getInt = do
  initial <- Get.lookAhead Get.getWord8
  let major = initial `shiftR` 5
  value <- getMajor major
  case major of
    0 -> pure (fromIntegral value)
    1 -> pure (-1 - fromIntegral value)
    _ -> fail "unexpected CBOR integer"

getMajor :: Word8 -> Get.Get Word64
getMajor expected = do
  initial <- Get.getWord8
  let major = initial `shiftR` 5
      info = initial `mod` 32
  unless (major == expected) (fail "unexpected CBOR major type")
  case info of
    value | value < 24 -> pure (fromIntegral value)
    24 -> fromIntegral <$> Get.getWord8
    25 -> fromIntegral <$> Get.getWord16be
    26 -> fromIntegral <$> Get.getWord32be
    27 -> Get.getWord64be
    _ -> fail "unsupported CBOR length"
