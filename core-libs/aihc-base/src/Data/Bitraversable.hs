{-# LANGUAGE KindSignatures #-}

module Data.Bitraversable
  ( Bitraversable (..),
    bisequenceA,
    bisequence,
    bimapM,
    bifor,
    biforM,
    bimapDefault,
    bifoldMapDefault,
  )
where

import Control.Applicative (Const (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Prelude

class (Bifunctor t, Bifoldable t) => Bitraversable (t :: Type -> Type -> Type) where
  bitraverse :: (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bitraverse f g = bisequenceA . bimap f g

bisequenceA :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequenceA = bitraverse id id

bisequence :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequence = bisequenceA

bimapM :: (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM = bitraverse

bifor :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor structure f g = bitraverse f g structure

biforM :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
biforM = bifor

instance Bitraversable (,) where
  bitraverse f g (left, right) = liftA2 (,) (f left) (g right)

instance Bitraversable Either where
  bitraverse f _ (Left value) = fmap Left (f value)
  bitraverse _ g (Right value) = fmap Right (g value)

instance Bitraversable Const where
  bitraverse f _ (Const value) = fmap Const (f value)

instance Bitraversable ((,,) x) where
  bitraverse f g (extra, left, right) = liftA2 (tripleWith extra) (f left) (g right)

tripleWith :: x -> a -> b -> (x, a, b)
tripleWith extra left right = (extra, left, right)

-- | A default definition of 'bimap' for a 'Bitraversable' type.
bimapDefault :: (Bitraversable t) => (a -> b) -> (c -> d) -> t a c -> t b d
bimapDefault f g = runBiIdentity . bitraverse (BiIdentity . f) (BiIdentity . g)

-- | A default definition of 'bifoldMap' for a 'Bitraversable' type.
bifoldMapDefault :: (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault f g = getConst . bitraverse (Const . f) (Const . g)

newtype BiIdentity a = BiIdentity a

runBiIdentity :: BiIdentity a -> a
runBiIdentity (BiIdentity value) = value

instance Functor BiIdentity where
  fmap f (BiIdentity value) = BiIdentity (f value)

instance Applicative BiIdentity where
  pure = BiIdentity
  BiIdentity f <*> BiIdentity value = BiIdentity (f value)
