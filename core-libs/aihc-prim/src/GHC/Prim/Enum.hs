-- | The 'Enum' class that arithmetic sequences desugar through. GHC defines
-- it in @GHC.Internal.Enum@; aihc keeps it in the prim package because the
-- compiler's built-in scope only loads prim modules.
module GHC.Prim.Enum
  ( Enum (..),
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
