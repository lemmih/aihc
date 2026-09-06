-- | The types that a fractional literal desugars to.
--
-- A fractional literal becomes @fromRational (Ratio numerator denominator)@.
-- The compiler builds that expression from the built-in scope, so the
-- ratio type and the Fractional class must live in the primitive package.
-- @GHC.Real@ of @aihc-base@ exports these declarations again.
module GHC.Prim.Real
  ( Fractional (..),
    Ratio (..),
    Rational,
  )
where

import GHC.Prim.Integer (Integer)
import GHC.Prim.Num (Num)

data Ratio a = Ratio a a

type Rational = Ratio Integer

class (Num a) => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

infixl 7 /
