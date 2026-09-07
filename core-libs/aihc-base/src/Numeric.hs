module Numeric
  ( showSigned,
    showInt,
    showIntAtBase,
    showHex,
    showOct,
    showBin,
    readInt,
    readBin,
    readOct,
    readDec,
    readHex,
    readSigned,
    lexDigits,
  )
where

import Data.Char (digitToInt, isDigit, isHexDigit, isOctDigit, isSpace)
import GHC.Show (intToDigit)
import Prelude (Bool (..), Char, Eq (..), Int, Integral (..), Num (..), Ord (..), ReadS, Real (..), Show (..), ShowS, String, dropWhile, errorWithoutStackTrace, fromIntegral, negate, readParen, showChar, showParen, span, (.), (||))

showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPositive precedence value =
  if value < 0
    then showParen (precedence > 6) (showChar '-' . showPositive (negate value))
    else showPositive value

showInt :: (Integral a) => a -> ShowS
showInt = showIntAtBase 10 intToDigit

showIntAtBase :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toDigit value rest =
  case (base <= 1, value < 0) of
    (True, _) -> errorWithoutStackTrace "Numeric.showIntAtBase: unsupported base"
    (_, True) -> errorWithoutStackTrace "Numeric.showIntAtBase: negative number"
    _ -> showDigits base toDigit value rest

showDigits :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showDigits base toDigit value rest =
  case quotRem value base of
    (quotient, remainder) ->
      let digits = toDigit (fromIntegral remainder) : rest
       in if quotient == 0
            then digits
            else showDigits base toDigit quotient digits

showHex :: (Integral a) => a -> ShowS
showHex = showIntAtBase 16 intToDigit

showOct :: (Integral a) => a -> ShowS
showOct = showIntAtBase 8 intToDigit

showBin :: (Integral a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit

-- | Read an unsigned number written in a positional base.
--
-- @isDigitChar@ accepts a digit of the base and @digitValue@ gives its
-- value. The reader fails when the input does not start with a digit.
readInt :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt base isDigitChar digitValue input =
  case span isDigitChar input of
    ([], _) -> []
    (digits, rest) -> [(accumulate 0 digits, rest)]
  where
    accumulate accumulator remaining =
      case remaining of
        [] -> accumulator
        digit : more -> accumulate (accumulator * base + fromIntegral (digitValue digit)) more

-- | Read an unsigned binary number.
readBin :: (Num a) => ReadS a
readBin = readInt 2 isBinDigit digitToInt

-- | Read an unsigned octal number.
readOct :: (Num a) => ReadS a
readOct = readInt 8 isOctDigit digitToInt

-- | Read an unsigned decimal number.
readDec :: (Num a) => ReadS a
readDec = readInt 10 isDigit digitToInt

-- | Read an unsigned hexadecimal number. Both letter cases are accepted.
readHex :: (Num a) => ReadS a
readHex = readInt 16 isHexDigit digitToInt

-- | Add an optional sign and optional parentheses to an unsigned reader.
--
-- The reader skips the space before the sign and before the number.
readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPositive = readParen False readSigned'
  where
    readSigned' input =
      case dropWhile isSpace input of
        '-' : afterSign -> [(negate value, rest) | (value, rest) <- readPositive (dropWhile isSpace afterSign)]
        withoutSpace -> readPositive withoutSpace

-- | Read the longest non-empty run of decimal digits.
lexDigits :: ReadS String
lexDigits input =
  case span isDigit input of
    ([], _) -> []
    (digits, rest) -> [(digits, rest)]

isBinDigit :: Char -> Bool
isBinDigit character = character == '0' || character == '1'
