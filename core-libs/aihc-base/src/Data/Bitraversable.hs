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
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Prelude

{- HLINT ignore "Use (,)" -}

-- | 'Bitraversable' identifies bifunctorial data structures whose elements
-- can be traversed in order, performing 'Applicative' or 'Monad' actions at
-- each element, and collecting a result structure with the same shape.
class (Bifunctor t, Bifoldable t) => Bitraversable (t :: Type -> Type -> Type) where
  -- | Evaluates the relevant functions at each element in the structure,
  -- running the action, and builds a new structure with the same shape,
  -- using the results produced from sequencing the actions.
  bitraverse :: (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bitraverse f g = bisequenceA . bimap f g

-- | Sequences all the actions in a structure, building a new structure with
-- the same shape using the results of the actions.
bisequenceA :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequenceA = bitraverse id id

-- | Alias for 'bisequenceA'.
bisequence :: (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
bisequence = bisequenceA

-- | Alias for 'bitraverse'.
bimapM :: (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM = bitraverse

-- | 'bifor' is 'bitraverse' with the structure as the first argument.
bifor :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor t f g = bitraverse f g t

-- | Alias for 'bifor'.
biforM :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
biforM = bifor

-- | A default definition of 'bimap' in terms of the 'Bitraversable' operations.
bimapDefault :: (Bitraversable t) => (a -> b) -> (c -> d) -> t a c -> t b d
bimapDefault f g = runIdentity . bitraverse (Identity . f) (Identity . g)

-- | A default definition of 'bifoldMap' in terms of the 'Bitraversable' operations.
bifoldMapDefault :: (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault f g = getConst . bitraverse (Const . f) (Const . g)

instance Bitraversable (,) where
  bitraverse f g (a, b) = (\c d -> (c, d)) <$> f a <*> g b

instance Bitraversable ((,,) x) where
  bitraverse f g (x, a, b) = (\c d -> (x, c, d)) <$> f a <*> g b

instance Bitraversable ((,,,) x y) where
  bitraverse f g (x, y, a, b) = (\c d -> (x, y, c, d)) <$> f a <*> g b

instance Bitraversable Either where
  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b

instance Bitraversable Const where
  bitraverse f _ (Const a) = Const <$> f a
