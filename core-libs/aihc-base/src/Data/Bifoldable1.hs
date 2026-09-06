{-# LANGUAGE KindSignatures #-}

module Data.Bifoldable1
  ( Bifoldable1 (..),
  )
where

import Control.Applicative (Const (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Kind (Type)
import Data.Semigroup (Semigroup (..))
import Prelude

class (Bifoldable t) => Bifoldable1 (t :: Type -> Type -> Type) where
  bifold1 :: (Semigroup m) => t m m -> m
  bifoldMap1 :: (Semigroup m) => (a -> m) -> (b -> m) -> t a b -> m

  bifold1 = bifoldMap1 id id
  bifoldMap1 f g structure =
    case bifoldMap (Just . f) (Just . g) structure of
      Nothing -> errorWithoutStackTrace "bifoldMap1: empty structure"
      Just result -> result

instance Bifoldable1 (,) where
  bifoldMap1 f g (left, right) = f left <> g right

instance Bifoldable1 Const where
  bifoldMap1 f _ (Const value) = f value

instance Bifoldable1 Either where
  bifoldMap1 f _ (Left value) = f value
  bifoldMap1 _ g (Right value) = g value

instance Bifoldable1 ((,,) x) where
  bifoldMap1 f g (_, left, right) = f left <> g right
