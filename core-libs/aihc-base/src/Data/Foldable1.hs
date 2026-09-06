{-# LANGUAGE KindSignatures #-}

module Data.Foldable1
  ( Foldable1 (..),
    foldr1,
    foldr1',
    foldl1,
    foldl1',
    intercalate1,
    foldrM1,
    foldlM1,
    foldrMapM1,
    foldlMapM1,
    maximumBy,
    minimumBy,
  )
where

import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Max (..), Min (..), Semigroup (..))
import Prelude hiding (foldl1, foldr1, head, last, maximum, minimum)

class (Foldable t) => Foldable1 (t :: Type -> Type) where
  fold1 :: (Semigroup m) => t m -> m
  foldMap1 :: (Semigroup m) => (a -> m) -> t a -> m
  foldMap1' :: (Semigroup m) => (a -> m) -> t a -> m
  toNonEmpty :: t a -> NonEmpty a
  maximum :: (Ord a) => t a -> a
  minimum :: (Ord a) => t a -> a
  head :: t a -> a
  last :: t a -> a
  foldrMap1 :: (a -> b) -> (a -> b -> b) -> t a -> b
  foldlMap1' :: (a -> b) -> (b -> a -> b) -> t a -> b
  foldlMap1 :: (a -> b) -> (b -> a -> b) -> t a -> b
  foldrMap1' :: (a -> b) -> (a -> b -> b) -> t a -> b

  fold1 = foldMap1 id
  foldMap1 f = foldrMap1 f (\value rest -> f value <> rest)
  foldMap1' f = foldlMap1' f (\rest value -> rest <> f value)
  toNonEmpty = foldMap1 singleton
  maximum = getMax . foldMap1' Max
  minimum = getMin . foldMap1' Min
  head = foldrMap1 id const
  last = foldlMap1 id (\_ value -> value)
  foldrMap1 f g structure = runFromMaybe (foldMap1 (FromMaybe . step) structure) Nothing
    where
      step value Nothing = f value
      step value (Just rest) = g value rest
  foldlMap1' f g structure =
    case toNonEmpty structure of
      value :| values -> go (f value) values
    where
      go accumulator [] = accumulator
      go accumulator (value : values) = (go $! g accumulator value) values
  foldlMap1 f g structure = runFromMaybe (getFlipped (foldMap1 (Flipped . FromMaybe . step) structure)) Nothing
    where
      step value Nothing = f value
      step value (Just rest) = g rest value
  foldrMap1' f g structure =
    case reverseNonEmpty (toNonEmpty structure) of
      value :| values -> go (f value) values
    where
      go accumulator [] = accumulator
      go accumulator (value : values) = (go $! g value accumulator) values

singleton :: a -> NonEmpty a
singleton value = value :| []

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (value :| values) = go value [] values
  where
    go current accumulator [] = current :| accumulator
    go current accumulator (next : rest) = go next (current : accumulator) rest

-- | A semigroup on functions from an optional accumulator, used to derive
-- the right folds from 'foldMap1'.
newtype FromMaybe b = FromMaybe (Maybe b -> b)

runFromMaybe :: FromMaybe b -> Maybe b -> b
runFromMaybe (FromMaybe f) = f

instance Semigroup (FromMaybe b) where
  FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

-- | Reverses the order of a semigroup.
newtype Flipped a = Flipped a

getFlipped :: Flipped a -> a
getFlipped (Flipped value) = value

instance (Semigroup a) => Semigroup (Flipped a) where
  Flipped left <> Flipped right = Flipped (right <> left)

instance Foldable1 NonEmpty where
  foldMap1 f (value :| values) = go (f value) values
    where
      go accumulator [] = accumulator
      go accumulator (next : rest) = accumulator <> go (f next) rest
  toNonEmpty = id
  head (value :| _) = value

instance Foldable1 Identity where
  foldMap1 f (Identity value) = f value
  toNonEmpty (Identity value) = value :| []

instance Foldable1 ((,) a) where
  foldMap1 f (_, value) = f value
  toNonEmpty (_, value) = value :| []

foldr1 :: (Foldable1 t) => (a -> a -> a) -> t a -> a
foldr1 = foldrMap1 id

foldr1' :: (Foldable1 t) => (a -> a -> a) -> t a -> a
foldr1' = foldrMap1' id

foldl1 :: (Foldable1 t) => (a -> a -> a) -> t a -> a
foldl1 = foldlMap1 id

foldl1' :: (Foldable1 t) => (a -> a -> a) -> t a -> a
foldl1' = foldlMap1' id

intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
intercalate1 separator = foldrMap1 id (\value rest -> value <> separator <> rest)

foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrM1 = foldrMapM1 return

foldrMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (a -> b -> m b) -> t a -> m b
foldrMapM1 f g structure =
  case reverseNonEmpty (toNonEmpty structure) of
    value :| values -> f value >>= \result -> go result values
  where
    go accumulator [] = return accumulator
    go accumulator (value : values) = g value accumulator >>= \result -> go result values

foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 = foldlMapM1 return

foldlMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (b -> a -> m b) -> t a -> m b
foldlMapM1 f g structure =
  case toNonEmpty structure of
    value :| values -> f value >>= \result -> go result values
  where
    go accumulator [] = return accumulator
    go accumulator (value : values) = g accumulator value >>= \result -> go result values

maximumBy :: (Foldable1 t) => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1 pick
  where
    pick left right =
      case cmp left right of
        GT -> left
        _ -> right

minimumBy :: (Foldable1 t) => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 pick
  where
    pick left right =
      case cmp left right of
        GT -> right
        _ -> left
