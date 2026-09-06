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
    maximumBy,
    minimumBy,
  )
where

import Data.Foldable (Foldable, foldl')
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Semigroup (Semigroup (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import Prelude hiding (foldl1, foldr1, head, last, maximum, minimum)
import qualified Prelude

-- | Non-empty data structures that fold to a semigroup.
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
  toNonEmpty = foldrMap1 (\value -> value :| []) (\value (first :| rest) -> value :| (first : rest))
  maximum = foldlMap1' id max
  minimum = foldlMap1' id min
  head = foldrMap1 id (\value _ -> value)
  last = foldlMap1 id (\_ value -> value)
  foldrMap1 f g structure =
    case foldMap1 (\value -> FromMaybe (step value)) structure of
      FromMaybe finish -> finish Nothing
    where
      step value Nothing = f value
      step value (Just rest) = g value rest
  foldlMap1' f g structure =
    case toNonEmpty structure of
      first :| rest -> foldl' g (f first) rest
  foldlMap1 f g structure =
    case toNonEmpty structure of
      first :| rest -> Prelude.foldl g (f first) rest
  foldrMap1' f g structure =
    case reverseNonEmpty (toNonEmpty structure) of
      first :| rest -> foldl' (\acc value -> g value acc) (f first) rest

-- | A function that gives a value for the empty remainder or the folded
-- remainder. Its semigroup threads the remainder from right to left.
newtype FromMaybe b = FromMaybe (Maybe b -> b)

instance Semigroup (FromMaybe b) where
  FromMaybe left <> FromMaybe right = FromMaybe (\rest -> left (Just (right rest)))

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (first :| rest) = go first [] rest
  where
    go value acc [] = value :| acc
    go value acc (next : others) = go next (value : acc) others

instance Foldable1 NonEmpty where
  foldMap1 f (first :| rest) = go first rest
    where
      go value [] = f value
      go value (next : others) = f value <> go next others
  toNonEmpty = id
  head (first :| _) = first

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
foldrM1 f = foldrMap1 return (\value rest -> rest >>= f value)

foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 f = foldlMap1 return (\rest value -> rest >>= \acc -> f acc value)

maximumBy :: (Foldable1 t) => (a -> a -> Ordering) -> t a -> a
maximumBy comparison = foldl1' pick
  where
    pick left right =
      case comparison left right of
        GT -> left
        _ -> right

minimumBy :: (Foldable1 t) => (a -> a -> Ordering) -> t a -> a
minimumBy comparison = foldl1' pick
  where
    pick left right =
      case comparison left right of
        GT -> right
        _ -> left
