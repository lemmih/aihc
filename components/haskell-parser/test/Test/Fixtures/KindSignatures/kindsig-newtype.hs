{-# LANGUAGE KindSignatures #-}

module KindSignaturesNewtype where

import Data.Kind (Type)

newtype Wrapped (f :: Type -> Type) a = Wrapped (f a)
