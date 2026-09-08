module Data.Coerce (Coercible, coerce) where

import GHC.Types (Coercible)
import Unsafe.Coerce (unsafeCoerce)

-- | Change the type of a value with the same representation.
coerce :: (Coercible a b) => a -> b
coerce = unsafeCoerce
