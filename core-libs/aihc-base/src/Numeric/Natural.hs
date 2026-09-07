{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Natural
  ( Natural,
    minusNaturalMaybe,
  )
where

import Data.Bits (Bits (..))
import GHC.Exception (ArithException (..), throw)
import GHC.Read ()
import Text.ParserCombinators.ReadPrec (ReadPrec, pfail, step)
import Prelude

newtype Natural = Natural Integer
  deriving newtype (Eq, Ord, Real, Integral, Show)

underflow :: a
underflow = throw Underflow

minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe (Natural left) (Natural right) =
  case left < right of
    True -> Nothing
    False -> Just (Natural (left - right))

instance Num Natural where
  Natural left + Natural right = Natural (left + right)
  Natural left - Natural right =
    case left < right of
      True -> underflow
      False -> Natural (left - right)
  Natural left * Natural right = Natural (left * right)
  negate (Natural value) =
    case value == 0 of
      True -> Natural 0
      False -> underflow
  abs value = value
  signum (Natural value) = Natural (signum value)
  fromInteger value =
    case value < 0 of
      True -> underflow
      False -> Natural value

instance Enum Natural where
  succ value = value + 1
  pred value = value - 1
  toEnum value = fromInteger (toInteger value)
  fromEnum (Natural value) = fromInteger value
  enumFrom (Natural first) = naturalsFromThen first (first + 1)
  enumFromThen (Natural first) (Natural second) = naturalsFromThen first second
  enumFromTo (Natural first) (Natural last) = naturalsFromThenTo first (first + 1) last
  enumFromThenTo (Natural first) (Natural second) (Natural last) = naturalsFromThenTo first second last

naturalsFromThen :: Integer -> Integer -> [Natural]
naturalsFromThen first second =
  case second >= first of
    True -> Natural first : naturalsFromThen second (second + (second - first))
    False -> naturalsFromThenTo first second 0

naturalsFromThenTo :: Integer -> Integer -> Integer -> [Natural]
naturalsFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            True -> Natural value : go (value + step)
            False -> []
        False ->
          case value >= last && value >= 0 of
            True -> Natural value : go (value + step)
            False -> []

instance Read Natural where
  readPrec = do
    value <- readPrec :: ReadPrec Integer
    case value < 0 of
      True -> pfail
      False -> return (Natural value)

instance Bits Natural where
  Natural left .&. Natural right = Natural (left .&. right)
  Natural left .|. Natural right = Natural (left .|. right)
  xor (Natural left) (Natural right) = Natural (xor left right)
  complement _ = underflow
  shift (Natural value) amount = Natural (shift value amount)
  rotate (Natural value) amount = Natural (rotate value amount)
  zeroBits = Natural zeroBits
  bit index = Natural (bit index)
  testBit (Natural value) = testBit value
  bitSizeMaybe _ = Nothing
  bitSize _ = bitSize (0 :: Integer)
  isSigned _ = False
  shiftL (Natural value) amount = Natural (shiftL value amount)
  unsafeShiftL (Natural value) amount = Natural (unsafeShiftL value amount)
  shiftR (Natural value) amount = Natural (shiftR value amount)
  unsafeShiftR (Natural value) amount = Natural (unsafeShiftR value amount)
  popCount (Natural value) = popCount value
