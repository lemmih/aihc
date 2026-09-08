{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Coerce (Coercible, coerce) where

import Unsafe.Coerce (unsafeCoerce)

-- | The type checker proves that both types have the same representation.
class Coercible a b

-- | Change the type of a value with the same representation.
coerce :: (Coercible a b) => a -> b
coerce = unsafeCoerce
