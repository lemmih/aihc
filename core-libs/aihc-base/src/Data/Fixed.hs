{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use map" #-}

module Data.Fixed
  ( Fixed (..),
    HasResolution (..),
    showFixed,
    E0,
    Uni,
    E1,
    Deci,
    E2,
    Centi,
    E3,
    Milli,
    E6,
    Micro,
    E9,
    Nano,
    E12,
    Pico,
    div',
    mod',
    divMod',
  )
where

import Prelude

-- | Generalized integral division for real values.
div' :: (Real a, Integral b) => a -> a -> b
div' numeratorValue denominatorValue =
  floor (toRational numeratorValue / toRational denominatorValue)

-- | Generalized division with a remainder in the input type.
divMod' :: (Real a, Integral b) => a -> a -> (b, a)
divMod' numeratorValue denominatorValue =
  let quotient = div' numeratorValue denominatorValue
   in (quotient, numeratorValue - fromIntegral quotient * denominatorValue)

-- | Generalized modulus for real values.
mod' :: (Real a) => a -> a -> a
mod' numeratorValue denominatorValue =
  let quotient = div' numeratorValue denominatorValue
   in numeratorValue - fromInteger quotient * denominatorValue

-- | Fixed-point values store an integer count of resolution-sized units.
newtype Fixed (a :: k) = MkFixed Integer
  deriving newtype (Eq, Ord, Enum)

-- | Types used as a fixed-point parameter supply its scaling factor.
class HasResolution (a :: k) where
  resolution :: p a -> Integer

instance (HasResolution a) => Num (Fixed a) where
  MkFixed left + MkFixed right = MkFixed (left + right)
  MkFixed left - MkFixed right = MkFixed (left - right)
  fixed@(MkFixed left) * MkFixed right =
    MkFixed (div (left * right) (resolution fixed))
  negate (MkFixed value) = MkFixed (negate value)
  abs (MkFixed value) = MkFixed (abs value)
  signum (MkFixed value) = fromInteger (signum value)
  fromInteger value = withResolutionFixed (\scale -> MkFixed (value * scale))

instance (HasResolution a) => Real (Fixed a) where
  toRational fixed@(MkFixed value) =
    toRational value / toRational (resolution fixed)

instance (HasResolution a) => Fractional (Fixed a) where
  fixed@(MkFixed left) / MkFixed right =
    MkFixed (div (left * resolution fixed) right)
  recip fixed@(MkFixed value) =
    let scale = resolution fixed
     in MkFixed (div (scale * scale) value)
  fromRational value =
    withResolutionFixed
      (\scale -> MkFixed (floor (value * toRational scale)))

instance (HasResolution a) => RealFrac (Fixed a) where
  properFraction value =
    let whole = truncate value
     in (whole, value - fromIntegral whole)
  truncate value = truncate (toRational value)
  round value = round (toRational value)
  ceiling value = ceiling (toRational value)
  floor value = floor (toRational value)

withResolutionFixed :: (HasResolution a) => (Integer -> Fixed a) -> Fixed a
withResolutionFixed make = make (resolution (fixedWitness make))

fixedWitness :: (Integer -> Fixed a) -> Fixed a
fixedWitness _ = MkFixed 0

-- | Render a fixed-point value, optionally dropping trailing fractional
-- zeroes. The digit count is computed with integers, so formatting does not
-- depend on a concrete floating-point implementation.
showFixed :: (HasResolution a) => Bool -> Fixed a -> String
showFixed chopTrailingZeros fixed@(MkFixed value) =
  case value < 0 of
    True -> '-' : showFixed chopTrailingZeros (sameFixedType (MkFixed (negate value)) fixed)
    False ->
      let scale = resolution fixed
          digits = decimalDigits scale
          maximumFraction = 10 ^ digits
       in case divMod value scale of
            (whole, fraction) ->
              let displayedFraction = divCeiling (fraction * maximumFraction) scale
                  fractionText = showIntegerZeros chopTrailingZeros digits displayedFraction
               in show whole ++ addDecimalPoint fractionText

sameFixedType :: Fixed a -> Fixed a -> Fixed a
sameFixedType value _ = value

decimalDigits :: Integer -> Int
decimalDigits = decimalDigitsFrom 0 1

decimalDigitsFrom :: Int -> Integer -> Integer -> Int
decimalDigitsFrom digits power scale =
  case power >= scale of
    True -> digits
    False -> decimalDigitsFrom (digits + 1) (power * 10) scale

divCeiling :: Integer -> Integer -> Integer
divCeiling numeratorValue denominatorValue =
  div (numeratorValue + denominatorValue - 1) denominatorValue

showIntegerZeros :: Bool -> Int -> Integer -> String
showIntegerZeros chopTrailingZeros digits value =
  let text = show value
      padded = replicateFixed (digits - stringLength text) '0' ++ text
   in case chopTrailingZeros of
        True -> chopFixedZeros padded
        False -> padded

replicateFixed :: Int -> a -> [a]
replicateFixed count value =
  case count <= 0 of
    True -> []
    False -> value : replicateFixed (count - 1) value

stringLength :: String -> Int
stringLength [] = 0
stringLength (_ : rest) = 1 + stringLength rest

chopFixedZeros :: String -> String
chopFixedZeros = reverseFixed . dropLeadingFixedZeros . reverseFixed

dropLeadingFixedZeros :: String -> String
dropLeadingFixedZeros ('0' : rest) = dropLeadingFixedZeros rest
dropLeadingFixedZeros text = text

reverseFixed :: [a] -> [a]
reverseFixed = reverseOnto []

reverseOnto :: [a] -> [a] -> [a]
reverseOnto result [] = result
reverseOnto result (value : values) = reverseOnto (value : result) values

addDecimalPoint :: String -> String
addDecimalPoint [] = []
addDecimalPoint text = '.' : text

instance (HasResolution a) => Show (Fixed a) where
  showsPrec precedence value =
    showParen (precedence > 6 && value < 0) (showString (showFixed False value))

data E0

instance HasResolution E0 where
  resolution _ = 1

type Uni = Fixed E0

data E1

instance HasResolution E1 where
  resolution _ = 10

type Deci = Fixed E1

data E2

instance HasResolution E2 where
  resolution _ = 100

type Centi = Fixed E2

data E3

instance HasResolution E3 where
  resolution _ = 1000

type Milli = Fixed E3

data E6

instance HasResolution E6 where
  resolution _ = 1000000

type Micro = Fixed E6

data E9

instance HasResolution E9 where
  resolution _ = 1000000000

type Nano = Fixed E9

data E12

instance HasResolution E12 where
  resolution _ = 1000000000000

type Pico = Fixed E12
