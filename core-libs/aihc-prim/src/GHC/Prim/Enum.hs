-- | The 'Enum' class that arithmetic sequences desugar through, and the
-- 'Bounded' class beside it. GHC defines both in @GHC.Internal.Enum@; aihc
-- keeps them in the prim package because the compiler's built-in scope only
-- loads prim modules, and because the type checker recognizes a stock class
-- by its package, module and name, and it may name the prim package only.
--
-- The instances and the helper functions stay in @GHC.Enum@.
module GHC.Prim.Enum
  ( Enum (..),
    Bounded (..),
  )
where

import GHC.Types (Int)

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

class Bounded a where
  minBound :: a
  maxBound :: a
