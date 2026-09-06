module Data.Coerce (Coercible, coerce) where

import GHC.Types (Coercible)
import Unsafe.Coerce (unsafeCoerce)

-- | Change the type of a value that has the same runtime representation.
-- The type checker solves the constraint for every pair of types after it
-- unifies them through newtypes, so the caller must keep the property that
-- the two representations agree.
coerce :: (Coercible a b) => a -> b
coerce = unsafeCoerce
