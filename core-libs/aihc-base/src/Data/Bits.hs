{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bits
  ( Bits (..),
    FiniteBits (..),
    bitDefault,
    testBitDefault,
    popCountDefault,
    toIntegralSized,
    oneBits,
    (.^.),
    (.>>.),
    (.<<.),
    (!>>.),
    (!<<.),
    And (And),
    getAnd,
    Ior (Ior),
    getIor,
    Xor (Xor),
    getXor,
    Iff (Iff),
    getIff,
  )
where

import GHC.Bits
import Prelude (Eq (..), Int)

oneBits :: (FiniteBits a) => a
oneBits = complement zeroBits

(.^.) :: (Bits a) => a -> a -> a
(.^.) = xor

infixl 6 .^.

(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .>>.

(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL

infixl 8 .<<.

(!>>.) :: (Bits a) => a -> Int -> a
(!>>.) = unsafeShiftR

infixl 8 !>>.

(!<<.) :: (Bits a) => a -> Int -> a
(!<<.) = unsafeShiftL

infixl 8 !<<.

newtype And a = And a
  deriving newtype (Eq, Bits, FiniteBits)

getAnd :: And a -> a
getAnd (And value) = value

newtype Ior a = Ior a
  deriving newtype (Eq, Bits, FiniteBits)

getIor :: Ior a -> a
getIor (Ior value) = value

newtype Xor a = Xor a
  deriving newtype (Eq, Bits, FiniteBits)

getXor :: Xor a -> a
getXor (Xor value) = value

newtype Iff a = Iff a
  deriving newtype (Eq, Bits, FiniteBits)

getIff :: Iff a -> a
getIff (Iff value) = value
