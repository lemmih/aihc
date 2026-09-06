{-# LANGUAGE MagicHash #-}

module GHC.Float
  ( Double (..),
    Float (..),
    FFFormat (..),
    Floating (..),
    RealFloat (..),
    castDoubleToWord64,
    castFloatToWord32,
    castWord32ToFloat,
    castWord64ToDouble,
    double2Float,
    double2Int,
    float2Double,
    float2Int,
    int2Double,
    int2Float,
    floatToDigits,
    formatRealFloat,
    roundTo,
    showFloat,
    showSignedFloat,
  )
where

import Data.Bool (Bool (..), not, otherwise, (&&), (||))
import GHC.Base (Maybe (..), String, (++), (.))
import GHC.Err (errorWithoutStackTrace)
import GHC.Int (Int (..))
import GHC.Integer (Integer)
import GHC.Internal.Char (Char (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Integer (Integer (..), integerToInt#)
import GHC.Num (Num (..))
import GHC.Prim
  ( Double#,
    Float#,
    Int#,
    Word#,
    Word32#,
    Word64#,
    acosDouble#,
    acosFloat#,
    acoshDouble#,
    acoshFloat#,
    and#,
    asinDouble#,
    asinFloat#,
    asinhDouble#,
    asinhFloat#,
    atanDouble#,
    atanFloat#,
    atanhDouble#,
    atanhFloat#,
    castDoubleToWord64#,
    castFloatToWord32#,
    castWord32ToFloat#,
    castWord64ToDouble#,
    cosDouble#,
    cosFloat#,
    coshDouble#,
    coshFloat#,
    divideFloat#,
    double2Float#,
    double2Int#,
    eqFloat#,
    eqWord#,
    expDouble#,
    expFloat#,
    fabsDouble#,
    fabsFloat#,
    float2Double#,
    float2Int#,
    gtFloat#,
    int2Double#,
    int2Float#,
    logDouble#,
    logFloat#,
    ltFloat#,
    minusFloat#,
    negateDouble#,
    negateFloat#,
    or#,
    plusFloat#,
    powerFloat#,
    sinDouble#,
    sinFloat#,
    sinhDouble#,
    sinhFloat#,
    sqrtDouble#,
    sqrtFloat#,
    tanDouble#,
    tanFloat#,
    tanhDouble#,
    tanhFloat#,
    timesFloat#,
    uncheckedShiftRL#,
    word2Int#,
    word32ToWord#,
    word64ToWord#,
    wordToWord32#,
    wordToWord64#,
    (*##),
    (**##),
    (+##),
    (-#),
    (-##),
    (/##),
    (<##),
    (==#),
    (==##),
    (>#),
    (>##),
  )
import GHC.Real (Fractional (..), Integral (..), Rational, Real (..), RealFrac (..), denominator, even, fromIntegral, numerator, (%), (^), (^^))
import GHC.Show (Show (..), ShowS, intToDigit, showChar, showList__, showParen, showString, shows)
import GHC.Types (Double (..), Float (..), Ordering (..))
import GHC.Word (Word32 (..), Word64 (..))

-- | Trigonometric and transcendental operations.
class (Fractional a) => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
  log1p :: a -> a
  expm1 :: a -> a
  log1pexp :: a -> a
  log1mexp :: a -> a

infixr 8 **

-- | Machine-independent decomposition of real floating-point values.
class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a

-- | Round a list of digits in the given base to the given number of digits.
-- The result carries a leading one when the rounding overflows.
roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base digitCount digits =
  case roundDigits base digitCount digits of
    (0, rounded) -> (0, rounded)
    (_, rounded) -> (1, 1 : rounded)

roundDigits :: Int -> Int -> [Int] -> (Int, [Int])
roundDigits base remaining digits =
  case remaining == 0 of
    True -> (roundCarry base digits, [])
    False ->
      case digits of
        [] -> (0, replicateZero remaining)
        (digit : rest) ->
          case roundDigits base (remaining - 1) rest of
            (carry, roundedRest) ->
              let total = digit + carry
               in case total == base of
                    True -> (1, 0 : roundedRest)
                    False -> (0, total : roundedRest)

roundCarry :: Int -> [Int] -> Int
roundCarry _ [] = 0
roundCarry base (digit : rest) =
  let half = base `quot` 2
   in case digit > half of
        True -> 1
        False ->
          case digit == half of
            True -> roundEven rest
            False -> 0

roundEven :: [Int] -> Int
roundEven [] = 0
roundEven (digit : rest) =
  case digit == 0 of
    True -> roundEven rest
    False -> 1

replicateZero :: Int -> [Int]
replicateZero count =
  case count <= 0 of
    True -> []
    False -> 0 : replicateZero (count - 1)

-- | Give the IEEE 754 bit pattern of a single-precision value.
castFloatToWord32 :: Float -> Word32
castFloatToWord32 (F# value) = W32# (castFloatToWord32# value)

-- | Give the single-precision value of an IEEE 754 bit pattern.
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat (W32# value) = F# (castWord32ToFloat# value)

-- | Give the IEEE 754 bit pattern of a double-precision value.
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 (D# value) = W64# (castDoubleToWord64# value)

-- | Give the double-precision value of an IEEE 754 bit pattern.
castWord64ToDouble :: Word64 -> Double
castWord64ToDouble (W64# value) = D# (castWord64ToDouble# value)

int2Double :: Int -> Double
int2Double (I# value) = D# (int2Double# value)

int2Float :: Int -> Float
int2Float (I# value) = F# (int2Float# value)

double2Int :: Double -> Int
double2Int (D# value) = I# (double2Int# value)

float2Int :: Float -> Int
float2Int (F# value) = I# (float2Int# value)

double2Float :: Double -> Float
double2Float (D# value) = F# (double2Float# value)

float2Double :: Float -> Double
float2Double (F# value) = D# (float2Double# value)

-- | Convert a primitive comparison result to a 'Bool'.
isTrue :: Int# -> Bool
isTrue value =
  case value of
    0# -> False
    _ -> True

instance Eq Float where
  F# left == F# right = isTrue (eqFloat# left right)
  left /= right = not (left == right)

instance Ord Float where
  compare (F# left) (F# right) =
    case ltFloat# left right of
      0# ->
        case gtFloat# left right of
          0# -> EQ
          _ -> GT
      _ -> LT
  F# left < F# right = isTrue (ltFloat# left right)
  F# left > F# right = isTrue (gtFloat# left right)
  left <= right = not (left > right)
  left >= right = not (left < right)
  max left right = if left < right then right else left
  min left right = if left < right then left else right

floatSignum :: Float# -> Float#
floatSignum value =
  case gtFloat# value (int2Float# 0#) of
    0# ->
      case ltFloat# value (int2Float# 0#) of
        0# -> value
        _ -> int2Float# ((-#) 0# 1#)
    _ -> int2Float# 1#

instance Num Float where
  F# left + F# right = F# (plusFloat# left right)
  F# left - F# right = F# (minusFloat# left right)
  F# left * F# right = F# (timesFloat# left right)
  negate (F# value) = F# (negateFloat# value)
  abs (F# value) = F# (fabsFloat# value)
  signum (F# value) = F# (floatSignum value)
  fromInteger value = F# (int2Float# (integerToInt# value))

instance Eq Double where
  D# left == D# right = isTrue ((==##) left right)
  left /= right = not (left == right)

instance Ord Double where
  compare (D# left) (D# right) =
    case (<##) left right of
      0# ->
        case (>##) left right of
          0# -> EQ
          _ -> GT
      _ -> LT
  D# left < D# right = isTrue ((<##) left right)
  D# left > D# right = isTrue ((>##) left right)
  left <= right = not (left > right)
  left >= right = not (left < right)
  max left right = if left < right then right else left
  min left right = if left < right then left else right

doubleSignum :: Double# -> Double#
doubleSignum value =
  case (>##) value (int2Double# 0#) of
    0# ->
      case (<##) value (int2Double# 0#) of
        0# -> value
        _ -> int2Double# ((-#) 0# 1#)
    _ -> int2Double# 1#

instance Num Double where
  D# left + D# right = D# ((+##) left right)
  D# left - D# right = D# ((-##) left right)
  D# left * D# right = D# ((*##) left right)
  negate (D# value) = D# (negateDouble# value)
  abs (D# value) = D# (fabsDouble# value)
  signum (D# value) = D# (doubleSignum value)
  fromInteger value = D# (int2Double# (integerToInt# value))

-- The Fractional, Floating, Real, RealFrac, and RealFloat instances of the
-- floating point types. Truncation goes through the machine integer, so a
-- value outside the Int range wraps; that is a limit of the standin.

instance Fractional Float where
  F# left / F# right = F# (divideFloat# left right)
  recip (F# value) = F# (divideFloat# (int2Float# 1#) value)
  fromRational value = fromInteger (numerator value) / fromInteger (denominator value)

instance Fractional Double where
  D# left / D# right = D# ((/##) left right)
  recip (D# value) = D# ((/##) (int2Double# 1#) value)
  fromRational value = fromInteger (numerator value) / fromInteger (denominator value)

instance Floating Float where
  pi = F# (castWord32ToFloat# (wordToWord32# 0x40490FDB##))
  exp (F# value) = F# (expFloat# value)
  log (F# value) = F# (logFloat# value)
  sqrt (F# value) = F# (sqrtFloat# value)
  F# left ** F# right = F# (powerFloat# left right)
  logBase base value = log value / log base
  sin (F# value) = F# (sinFloat# value)
  cos (F# value) = F# (cosFloat# value)
  tan (F# value) = F# (tanFloat# value)
  asin (F# value) = F# (asinFloat# value)
  acos (F# value) = F# (acosFloat# value)
  atan (F# value) = F# (atanFloat# value)
  sinh (F# value) = F# (sinhFloat# value)
  cosh (F# value) = F# (coshFloat# value)
  tanh (F# value) = F# (tanhFloat# value)
  asinh (F# value) = F# (asinhFloat# value)
  acosh (F# value) = F# (acoshFloat# value)
  atanh (F# value) = F# (atanhFloat# value)
  log1p value = log (1 + value)
  expm1 value = exp value - 1
  log1pexp value = log1p (exp value)
  log1mexp value = log1p (negate (exp value))

instance Floating Double where
  pi = D# (castWord64ToDouble# (wordToWord64# 0x400921FB54442D18##))
  exp (D# value) = D# (expDouble# value)
  log (D# value) = D# (logDouble# value)
  sqrt (D# value) = D# (sqrtDouble# value)
  D# left ** D# right = D# ((**##) left right)
  logBase base value = log value / log base
  sin (D# value) = D# (sinDouble# value)
  cos (D# value) = D# (cosDouble# value)
  tan (D# value) = D# (tanDouble# value)
  asin (D# value) = D# (asinDouble# value)
  acos (D# value) = D# (acosDouble# value)
  atan (D# value) = D# (atanDouble# value)
  sinh (D# value) = D# (sinhDouble# value)
  cosh (D# value) = D# (coshDouble# value)
  tanh (D# value) = D# (tanhDouble# value)
  asinh (D# value) = D# (asinhDouble# value)
  acosh (D# value) = D# (acoshDouble# value)
  atanh (D# value) = D# (atanhDouble# value)
  log1p value = log (1 + value)
  expm1 value = exp value - 1
  log1pexp value = log1p (exp value)
  log1mexp value = log1p (negate (exp value))

-- | The mantissa and the exponent of a double, like decodeFloat.
decodeDouble :: Double -> (Integer, Int)
decodeDouble (D# value) =
  let bits = word64ToWord# (castDoubleToWord64# value)
      exponentField = word2Int# (and# (uncheckedShiftRL# bits 52#) 0x7FF##)
      mantissaField = and# bits 0xFFFFFFFFFFFFF##
      negative = isTrue ((>#) (word2Int# (uncheckedShiftRL# bits 63#)) 0#)
      signed mantissa = if negative then IS ((-#) 0# mantissa) else IS mantissa
   in case isTrue ((==#) exponentField 0#) of
        True ->
          case isTrue (eqWord# mantissaField 0##) of
            True -> (IS 0#, 0)
            False -> (signed (word2Int# mantissaField), -1074)
        False -> (signed (word2Int# (or# mantissaField 0x10000000000000##)), I# ((-#) exponentField 1075#))

-- | The mantissa and the exponent of a float, like decodeFloat.
decodeFloatValue :: Float -> (Integer, Int)
decodeFloatValue (F# value) =
  let bits = word32ToWord# (castFloatToWord32# value)
      exponentField = word2Int# (and# (uncheckedShiftRL# bits 23#) 0xFF##)
      mantissaField = and# bits 0x7FFFFF##
      negative = isTrue ((>#) (word2Int# (uncheckedShiftRL# bits 31#)) 0#)
      signed mantissa = if negative then IS ((-#) 0# mantissa) else IS mantissa
   in case isTrue ((==#) exponentField 0#) of
        True ->
          case isTrue (eqWord# mantissaField 0##) of
            True -> (IS 0#, 0)
            False -> (signed (word2Int# mantissaField), -149)
        False -> (signed (word2Int# (or# mantissaField 0x800000##)), I# ((-#) exponentField 150#))

rationalFromDecoded :: (Integer, Int) -> Rational
rationalFromDecoded (mantissa, exponent')
  | exponent' >= 0 = (mantissa * (2 ^ exponent')) % 1
  | otherwise = mantissa % (2 ^ negate exponent')

instance Real Float where
  toRational value = rationalFromDecoded (decodeFloatValue value)

instance Real Double where
  toRational value = rationalFromDecoded (decodeDouble value)

truncateDouble :: Double -> Integer
truncateDouble (D# value) = IS (double2Int# value)

truncateFloat :: Float -> Integer
truncateFloat (F# value) = IS (float2Int# value)

-- | Round half to even, like GHC.
roundFromTruncation :: (RealFrac a) => (a -> Integer) -> a -> Integer
roundFromTruncation truncation value =
  let whole = truncation value
      fraction = value - fromInteger whole
      away = if fraction < 0 then whole - 1 else whole + 1
      twice = abs fraction * 2
   in case compare twice 1 of
        LT -> whole
        EQ -> if even whole then whole else away
        GT -> away

ceilingFromTruncation :: (RealFrac a) => (a -> Integer) -> a -> Integer
ceilingFromTruncation truncation value =
  let whole = truncation value
   in if value - fromInteger whole > 0 then whole + 1 else whole

floorFromTruncation :: (RealFrac a) => (a -> Integer) -> a -> Integer
floorFromTruncation truncation value =
  let whole = truncation value
   in if value - fromInteger whole < 0 then whole - 1 else whole

instance RealFrac Float where
  properFraction value =
    let whole = truncateFloat value
     in (fromInteger whole, value - fromInteger whole)
  truncate value = fromInteger (truncateFloat value)
  round value = fromInteger (roundFromTruncation truncateFloat value)
  ceiling value = fromInteger (ceilingFromTruncation truncateFloat value)
  floor value = fromInteger (floorFromTruncation truncateFloat value)

instance RealFrac Double where
  properFraction value =
    let whole = truncateDouble value
     in (fromInteger whole, value - fromInteger whole)
  truncate value = fromInteger (truncateDouble value)
  round value = fromInteger (roundFromTruncation truncateDouble value)
  ceiling value = fromInteger (ceilingFromTruncation truncateDouble value)
  floor value = fromInteger (floorFromTruncation truncateDouble value)

-- | The atan2 of GHC.Float, for both floating point types.
atan2Value :: (RealFloat a) => a -> a -> a
atan2Value y x
  | x > 0 = atan (y / x)
  | x == 0 && y > 0 = pi / 2
  | x < 0 && y > 0 = pi + atan (y / x)
  | (x <= 0 && y < 0) || (x < 0 && isNegativeZero y) || (isNegativeZero x && isNegativeZero y) = negate (atan2Value (negate y) x)
  | y == 0 && (x < 0 || isNegativeZero x) = pi
  | x == 0 && y == 0 = y
  | otherwise = x + y

instance RealFloat Float where
  floatRadix _ = 2
  floatDigits _ = 24
  floatRange _ = (-125, 128)
  decodeFloat = decodeFloatValue
  encodeFloat mantissa exponent' = fromInteger mantissa * (2 ^^ exponent')
  exponent value =
    case decodeFloatValue value of
      (mantissa, exponent') -> if mantissa == 0 then 0 else exponent' + floatDigits value
  significand value =
    case decodeFloatValue value of
      (mantissa, _) -> encodeFloat mantissa (negate (floatDigits value))
  scaleFloat count value = value * (2 ^^ count)
  isNaN value = value /= value
  isInfinite (F# value) = isTrue (eqWord# (and# (word32ToWord# (castFloatToWord32# value)) 0x7FFFFFFF##) 0x7F800000##)
  isDenormalized (F# value) =
    let bits = word32ToWord# (castFloatToWord32# value)
     in isTrue (eqWord# (and# bits 0x7F800000##) 0##) && not (isTrue (eqWord# (and# bits 0x7FFFFF##) 0##))
  isNegativeZero (F# value) = isTrue (eqWord# (word32ToWord# (castFloatToWord32# value)) 0x80000000##)
  isIEEE _ = True
  atan2 = atan2Value

instance RealFloat Double where
  floatRadix _ = 2
  floatDigits _ = 53
  floatRange _ = (-1021, 1024)
  decodeFloat = decodeDouble
  encodeFloat mantissa exponent' = fromInteger mantissa * (2 ^^ exponent')
  exponent value =
    case decodeDouble value of
      (mantissa, exponent') -> if mantissa == 0 then 0 else exponent' + floatDigits value
  significand value =
    case decodeDouble value of
      (mantissa, _) -> encodeFloat mantissa (negate (floatDigits value))
  scaleFloat count value = value * (2 ^^ count)
  isNaN value = value /= value
  isInfinite (D# value) = isTrue (eqWord# (and# (word64ToWord# (castDoubleToWord64# value)) 0x7FFFFFFFFFFFFFFF##) 0x7FF0000000000000##)
  isDenormalized (D# value) =
    let bits = word64ToWord# (castDoubleToWord64# value)
     in isTrue (eqWord# (and# bits 0x7FF0000000000000##) 0##) && not (isTrue (eqWord# (and# bits 0xFFFFFFFFFFFFF##) 0##))
  isNegativeZero (D# value) = isTrue (eqWord# (word64ToWord# (castDoubleToWord64# value)) 0x8000000000000000##)
  isIEEE _ = True
  atan2 = atan2Value

-- Showing floating-point numbers

-- | The layout of a formatted floating-point number.
data FFFormat = FFExponent | FFFixed | FFGeneric

instance Show Float where
  showsPrec = showSignedFloat showFloat
  showList = showList__ shows

instance Show Double where
  showsPrec = showSignedFloat showFloat
  showList = showList__ shows

-- | Show a signed floating-point number, parenthesising a negative number
-- above precedence 6 like GHC does.
showSignedFloat :: (RealFloat a) => (a -> ShowS) -> Int -> a -> ShowS
showSignedFloat showPositive precedence value
  | value < 0 || isNegativeZero value =
      showParen (precedence > 6) (showChar '-' . showPositive (negate value))
  | otherwise = showPositive value

-- | Show a non-negative floating-point number in the shortest decimal form
-- that reads back to the same value.
showFloat :: (RealFloat a) => a -> ShowS
showFloat value = showString (formatRealFloat FFGeneric Nothing value)

-- | Format a floating-point number. The digit count only applies to the
-- exponent and fixed layouts; @Nothing@ prints every significant digit.
formatRealFloat :: (RealFloat a) => FFFormat -> Maybe Int -> a -> String
formatRealFloat format decimals value
  | isNaN value = "NaN"
  | isInfinite value = if value < 0 then "-Infinity" else "Infinity"
  | value < 0 || isNegativeZero value = '-' : formatDigits format decimals (floatToDigits 10 (negate value))
  | otherwise = formatDigits format decimals (floatToDigits 10 value)

formatDigits :: FFFormat -> Maybe Int -> ([Int], Int) -> String
formatDigits format decimals (digits, exponent') =
  case format of
    FFGeneric ->
      formatDigits (if exponent' < 0 || exponent' > 7 then FFExponent else FFFixed) decimals (digits, exponent')
    FFExponent -> formatExponent decimals digits exponent'
    FFFixed -> formatFixed decimals digits exponent'

formatExponent :: Maybe Int -> [Int] -> Int -> String
formatExponent decimals digits exponent' =
  case decimals of
    Nothing ->
      case mapList intToDigit digits of
        [] -> errorWithoutStackTrace "formatRealFloat/formatExponent: []"
        [digit] ->
          if digit == '0'
            then "0.0e0"
            else digit : ".0e" ++ show (exponent' - 1)
        (digit : rest) -> digit : '.' : rest ++ "e" ++ show (exponent' - 1)
    Just requested ->
      let count = max 1 requested
       in case digits of
            [0] -> '0' : '.' : replicateChar count '0' ++ "e0"
            _ ->
              case roundTo 10 (count + 1) digits of
                (carry, rounded) ->
                  case mapList intToDigit (if carry > 0 then initList rounded else rounded) of
                    [] -> errorWithoutStackTrace "formatRealFloat/formatExponent: rounded to nothing"
                    (digit : rest) -> digit : '.' : rest ++ 'e' : show (exponent' - 1 + carry)

formatFixed :: Maybe Int -> [Int] -> Int -> String
formatFixed decimals digits exponent' =
  case decimals of
    Nothing
      | exponent' <= 0 -> "0." ++ replicateChar (negate exponent') '0' ++ mapList intToDigit digits
      | otherwise -> splitFixed exponent' [] (mapList intToDigit digits)
    Just requested ->
      let count = max 0 requested
       in if exponent' >= 0
            then case roundTo 10 (count + exponent') digits of
              (carry, rounded) ->
                case splitAtList (exponent' + carry) (mapList intToDigit rounded) of
                  (whole, fraction) -> nonEmptyDigits whole ++ (if null fraction then "" else '.' : fraction)
            else case roundTo 10 count (replicateZero (negate exponent') ++ digits) of
              (carry, rounded) ->
                case mapList intToDigit (if carry > 0 then rounded else 0 : rounded) of
                  [] -> errorWithoutStackTrace "formatRealFloat/formatFixed: rounded to nothing"
                  (digit : fraction) -> digit : (if null fraction then "" else '.' : fraction)

-- | Place the decimal point after the given number of digits, padding the
-- whole part with zeros when the digits run out first.
splitFixed :: Int -> String -> String -> String
splitFixed remaining whole fraction
  | remaining == 0 = nonEmptyDigits (reverseList whole) ++ '.' : nonEmptyDigits fraction
  | otherwise =
      case fraction of
        [] -> splitFixed (remaining - 1) ('0' : whole) []
        (digit : rest) -> splitFixed (remaining - 1) (digit : whole) rest

nonEmptyDigits :: String -> String
nonEmptyDigits [] = "0"
nonEmptyDigits digits = digits

-- | The shortest digit sequence in the given base that uniquely identifies
-- the number, with the exponent of the first digit, following Burger and
-- Dybvig's free-format algorithm as in GHC.
floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits base value
  | value == 0 = ([0], 0)
  | otherwise =
      case decodeFloat value of
        (mantissa0, exponent0) ->
          let precision = floatDigits value
              radix = floatRadix value
              minExponent = case floatRange value of (lowest, _) -> lowest - precision
              -- A denormalized mantissa gets its exponent raised so the
              -- exponent never drops below the minimum.
              shift = minExponent - exponent0
              mantissa = if shift > 0 then mantissa0 `quot` expt radix shift else mantissa0
              exponent' = if shift > 0 then exponent0 + shift else exponent0
              scaled
                | exponent' >= 0 =
                    let radixPower = expt radix exponent'
                     in if mantissa == expt radix (precision - 1)
                          then (mantissa * radixPower * radix * 2, 2 * radix, radixPower * radix, radixPower)
                          else (mantissa * radixPower * 2, 2, radixPower, radixPower)
                | exponent' > minExponent && mantissa == expt radix (precision - 1) =
                    (mantissa * radix * 2, expt radix (negate exponent' + 1) * 2, radix, 1)
                | otherwise = (mantissa * 2, expt radix (negate exponent') * 2, 1, 1)
           in case scaled of
                (r, s, mUp, mDown) ->
                  let estimate =
                        if radix == 2 && base == 10
                          then
                            -- logBase 10 2 is slightly above 8651/28738, so a
                            -- non-negative estimate is one too small.
                            let logValue = precision - 1 + exponent0
                                approx = (logValue * 8651) `quot` 28738
                             in if logValue >= 0 then approx + 1 else approx
                          else
                            ceiling
                              ( (log (fromInteger (mantissa + 1) :: Float) + fromIntegral exponent' * log (fromInteger radix))
                                  / log (fromInteger base)
                              )
                      fixup n
                        | n >= 0 = if r + mUp <= expt base n * s then n else fixup (n + 1)
                        | otherwise = if expt base (negate n) * (r + mUp) <= s then n else fixup (n + 1)
                      k = fixup estimate
                      digits =
                        if k >= 0
                          then generateDigits base [] r (s * expt base k) mUp mDown
                          else
                            let scale = expt base (negate k)
                             in generateDigits base [] (r * scale) s (mUp * scale) (mDown * scale)
                   in (mapList fromInteger (reverseList digits), k)

generateDigits :: Integer -> [Integer] -> Integer -> Integer -> Integer -> Integer -> [Integer]
generateDigits base acc r s mUp mDown =
  case (r * base) `quotRem` s of
    (digit, remainder) ->
      let mUp' = mUp * base
          mDown' = mDown * base
       in case (remainder < mDown', remainder + mUp' > s) of
            (True, False) -> digit : acc
            (False, True) -> digit + 1 : acc
            (True, True) -> if remainder * 2 < s then digit : acc else digit + 1 : acc
            (False, False) -> generateDigits base (digit : acc) remainder s mUp' mDown'

expt :: Integer -> Int -> Integer
expt base power = base ^ power

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x : xs) = f x : mapList f xs

reverseList :: [a] -> [a]
reverseList = reverseOnto []
  where
    reverseOnto acc [] = acc
    reverseOnto acc (x : xs) = reverseOnto (x : acc) xs

replicateChar :: Int -> Char -> String
replicateChar count char =
  case count <= 0 of
    True -> []
    False -> char : replicateChar (count - 1) char

initList :: [a] -> [a]
initList [] = []
initList [_] = []
initList (x : xs) = x : initList xs

splitAtList :: Int -> [a] -> ([a], [a])
splitAtList count list
  | count <= 0 = ([], list)
  | otherwise =
      case list of
        [] -> ([], [])
        (x : xs) -> case splitAtList (count - 1) xs of (front, back) -> (x : front, back)

null :: [a] -> Bool
null [] = True
null _ = False
