{-# LANGUAGE StandaloneKindSignatures #-}

module StandaloneKindClass where

import Data.Kind (Constraint, Type)

type HasValue :: Type -> Constraint
class HasValue a where
  getValue :: a -> a
