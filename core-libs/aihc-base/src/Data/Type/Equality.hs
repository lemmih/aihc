{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Equality
  ( (:~:) (..),
    (:~~:) (..),
  )
where

infix 4 :~:, :~~:

-- | Propositional equality. A value of type a :~: b proves that a and b are the same type.
data (a :: k) :~: (b :: k) where
  Refl :: forall k (a :: k). a :~: a

-- | Kind-heterogeneous propositional equality.
data (a :: k1) :~~: (b :: k2) where
  HRefl :: forall k (a :: k). a :~~: a
