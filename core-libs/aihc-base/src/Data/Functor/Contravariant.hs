{-# LANGUAGE KindSignatures #-}

module Data.Functor.Contravariant
  ( -- * Contravariant Functors
    Contravariant (..),
    phantom,

    -- * Operators
    (>$<),
    (>$$<),
    ($<),

    -- * Predicates
    Predicate (..),

    -- * Comparisons
    Comparison (..),
    defaultComparison,

    -- * Equivalence Relations
    Equivalence (..),
    defaultEquivalence,
    comparisonEquivalence,

    -- * Dual arrows
    Op (..),
  )
where

import Control.Applicative (Const (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Prelude

-- | The class of contravariant functors.
class Contravariant (f :: Type -> Type) where
  contramap :: (a' -> a) -> f a -> f a'

  -- | Replace all locations in the output with the same value.
  (>$) :: b -> f b -> f a
  value >$ structure = contramap (const value) structure

infixl 4 >$, $<, >$<, >$$<

{- HLINT ignore phantom "Use void" -}

-- | If @f@ is both 'Functor' and 'Contravariant' then by the time you
-- compose them, the result is phantom in its argument.
phantom :: (Functor f, Contravariant f) => f a -> f b
phantom x = contramap (const ()) (fmap (const ()) x)

-- | This is '>$' with its arguments flipped.
($<) :: (Contravariant f) => f b -> b -> f a
($<) = flip (>$)

-- | This is an infix alias for 'contramap'.
(>$<) :: (Contravariant f) => (a -> b) -> f b -> f a
(>$<) = contramap

-- | This is an infix version of 'contramap' with the arguments flipped.
(>$$<) :: (Contravariant f) => f b -> (a -> b) -> f a
(>$$<) = flip contramap

instance Contravariant (Const a) where
  contramap _ (Const value) = Const value

instance Contravariant Proxy where
  contramap _ _ = Proxy

newtype Predicate a = Predicate {getPredicate :: a -> Bool}

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)

instance Semigroup (Predicate a) where
  Predicate p <> Predicate q = Predicate (\a -> p a && q a)

instance Monoid (Predicate a) where
  mempty = Predicate (const True)

-- | Defines a total ordering on a type as per 'compare'.
newtype Comparison a = Comparison {getComparison :: a -> a -> Ordering}

instance Contravariant Comparison where
  contramap f (Comparison c) = Comparison (\a b -> c (f a) (f b))

instance Semigroup (Comparison a) where
  Comparison p <> Comparison q = Comparison (\a b -> p a b <> q a b)

instance Monoid (Comparison a) where
  mempty = Comparison (\_ _ -> EQ)

-- | Compare using 'compare'.
defaultComparison :: (Ord a) => Comparison a
defaultComparison = Comparison compare

-- | This data type represents an equivalence relation.
newtype Equivalence a = Equivalence {getEquivalence :: a -> a -> Bool}

instance Contravariant Equivalence where
  contramap f (Equivalence e) = Equivalence (\a b -> e (f a) (f b))

instance Semigroup (Equivalence a) where
  Equivalence p <> Equivalence q = Equivalence (\a b -> p a b && q a b)

instance Monoid (Equivalence a) where
  mempty = Equivalence (\_ _ -> True)

-- | Check for equivalence with '=='.
defaultEquivalence :: (Eq a) => Equivalence a
defaultEquivalence = Equivalence (==)

comparisonEquivalence :: Comparison a -> Equivalence a
comparisonEquivalence (Comparison p) = Equivalence (\a b -> p a b == EQ)

-- | Dual function arrows.
newtype Op a b = Op {getOp :: b -> a}

instance Contravariant (Op a) where
  contramap f (Op g) = Op (g . f)

instance (Semigroup a) => Semigroup (Op a b) where
  Op p <> Op q = Op (\a -> p a <> q a)

instance (Monoid a) => Monoid (Op a b) where
  mempty = Op (const mempty)
