{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Monoid
  ( Monoid (..),
    (<>),
    Dual (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    First (..),
    Last (..),
  )
where

import Data.Bool (Bool (..), (&&), (||))
import Data.Semigroup
  ( Max (..),
    Min (..),
    WrappedMonoid (..),
  )
import Data.Semigroup.Internal (Monoid (..), Semigroup (..))
import GHC.Base (Maybe (..))
import GHC.Enum (Bounded (..))
import GHC.Internal.Classes (Ord (..))
import GHC.Num (Num (..))

newtype Dual a = Dual {getDual :: a}

newtype All = All {getAll :: Bool}

newtype Any = Any {getAny :: Bool}

newtype Sum a = Sum {getSum :: a}

newtype Product a = Product {getProduct :: a}

newtype First a = First {getFirst :: Maybe a}

newtype Last a = Last {getLast :: Maybe a}

instance (Semigroup a) => Semigroup (Dual a) where
  Dual left <> Dual right = Dual (right <> left)

instance (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty

instance Semigroup All where
  All left <> All right = All (left && right)

instance Monoid All where
  mempty = All True

instance Semigroup Any where
  Any left <> Any right = Any (left || right)

instance Monoid Any where
  mempty = Any False

instance (Num a) => Semigroup (Sum a) where
  Sum left <> Sum right = Sum (left + right)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

instance (Num a) => Semigroup (Product a) where
  Product left <> Product right = Product (left * right)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

instance Semigroup (First a) where
  First Nothing <> right = right
  left <> _ = left

instance Monoid (First a) where
  mempty = First Nothing

instance Semigroup (Last a) where
  left <> Last Nothing = left
  _ <> right = right

instance Monoid (Last a) where
  mempty = Last Nothing

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

deriving newtype instance (Monoid m) => Monoid (WrappedMonoid m)
