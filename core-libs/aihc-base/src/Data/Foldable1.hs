{-# LANGUAGE KindSignatures #-}

module Data.Foldable1
  ( Foldable1 (..),
    foldr1,
    foldl1,
    intercalate1,
    foldrM1,
    foldlM1,
    foldrMapM1,
    foldlMapM1,
    maximumBy,
    minimumBy,
  )
where

import Data.Foldable (Foldable, foldlM)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup (..))
import Prelude hiding (foldl1, foldr1, head, last, maximum, minimum)

-- | Non-empty data structures that can be folded.
--
-- Instances define at least one of 'foldMap1' or 'foldrMap1'.
class (Foldable t) => Foldable1 (t :: Type -> Type) where
  -- | Given a structure with elements whose type is a 'Semigroup', combine
  -- them via the semigroup's @('<>')@ operator.
  fold1 :: (Semigroup m) => t m -> m
  fold1 = foldMap1 id

  -- | Map each element of the structure to a semigroup, and combine the results.
  foldMap1 :: (Semigroup m) => (a -> m) -> t a -> m
  foldMap1 f = foldrMap1 f (\a m -> f a <> m)

  -- | A left-associative variant of 'foldMap1' that is strict in the accumulator.
  foldMap1' :: (Semigroup m) => (a -> m) -> t a -> m
  foldMap1' f = foldlMap1' f (\m a -> m <> f a)

  -- | List of elements of a structure, from left to right.
  toNonEmpty :: t a -> NonEmpty a
  toNonEmpty = foldrMap1 singleton consNonEmpty

  -- | The largest element of a non-empty structure.
  maximum :: (Ord a) => t a -> a
  maximum = foldlMap1' id max

  -- | The least element of a non-empty structure.
  minimum :: (Ord a) => t a -> a
  minimum = foldlMap1' id min

  -- | The first element of the structure.
  head :: t a -> a
  head = foldrMap1 id const

  -- | The last element of the structure.
  last :: t a -> a
  last = foldlMap1 id (\_ a -> a)

  -- | Generalized 'foldr1'.
  foldrMap1 :: (a -> b) -> (a -> b -> b) -> t a -> b
  foldrMap1 f g structure = applyFromMaybe (foldMap1 (FromMaybe . step) structure) Nothing
    where
      step a Nothing = f a
      step a (Just b) = g a b

  -- | Generalized 'foldl1'', strict in the accumulator.
  foldlMap1' :: (a -> b) -> (b -> a -> b) -> t a -> b
  foldlMap1' f g structure = foldlNonEmpty' f g (toNonEmpty structure)

  -- | Generalized 'foldl1'.
  foldlMap1 :: (a -> b) -> (b -> a -> b) -> t a -> b
  foldlMap1 f g structure = foldlNonEmpty f g (toNonEmpty structure)

  -- | Generalized 'foldr1'', strict in the accumulator.
  foldrMap1' :: (a -> b) -> (a -> b -> b) -> t a -> b
  foldrMap1' f g structure = foldrNonEmpty' f g (toNonEmpty structure)

-- | Right-associative fold of a structure.
foldr1 :: (Foldable1 t) => (a -> a -> a) -> t a -> a
foldr1 = foldrMap1 id

-- | Left-associative fold of a structure.
foldl1 :: (Foldable1 t) => (a -> a -> a) -> t a -> a
foldl1 = foldlMap1 id

-- | Insert an @m@ between each pair of @t m@.
intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
intercalate1 separator = foldrMap1 id (\a b -> a <> separator <> b)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right.
foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrM1 = foldrMapM1 return

-- | Map variant of 'foldrM1'.
foldrMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (a -> b -> m b) -> t a -> m b
foldrMapM1 g f structure = go (toNonEmpty structure)
  where
    go (e :| es) =
      case es of
        [] -> g e
        x : xs -> go (x :| xs) >>= f e

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the left.
foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 = foldlMapM1 return

-- | Map variant of 'foldlM1'.
foldlMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (b -> a -> m b) -> t a -> m b
foldlMapM1 g f structure =
  case toNonEmpty structure of
    x :| xs -> g x >>= \y -> foldlM f y xs

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.
maximumBy :: (Foldable1 t) => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1 (\x y -> case cmp x y of GT -> x; _ -> y)

-- | The least element of a non-empty structure with respect to the
-- given comparison function.
minimumBy :: (Foldable1 t) => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 (\x y -> case cmp x y of GT -> y; _ -> x)

instance Foldable1 NonEmpty where
  toNonEmpty = id
  foldMap1 f (x :| xs) = foldrList f (\a m -> f a <> m) x xs
  head (x :| _) = x

instance Foldable1 Identity where
  toNonEmpty (Identity x) = x :| []
  foldMap1 f (Identity x) = f x

instance Foldable1 ((,) a) where
  toNonEmpty (_, x) = x :| []
  foldMap1 f (_, x) = f x

singleton :: a -> NonEmpty a
singleton x = x :| []

consNonEmpty :: a -> NonEmpty a -> NonEmpty a
consNonEmpty x (y :| ys) = x :| (y : ys)

-- | A right fold whose semigroup operation threads the fold continuation.
newtype FromMaybe b = FromMaybe (Maybe b -> b)

applyFromMaybe :: FromMaybe b -> Maybe b -> b
applyFromMaybe (FromMaybe f) = f

instance Semigroup (FromMaybe b) where
  FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

foldrList :: (a -> b) -> (a -> b -> b) -> a -> [a] -> b
foldrList f _ x [] = f x
foldrList f g x (y : ys) = g x (foldrList f g y ys)

foldrNonEmpty' :: (a -> b) -> (a -> b -> b) -> NonEmpty a -> b
foldrNonEmpty' f g (x :| xs) =
  case reverse (x : xs) of
    [] -> f x
    y : ys -> foldlList' f (flip g) y ys

foldlNonEmpty :: (a -> b) -> (b -> a -> b) -> NonEmpty a -> b
foldlNonEmpty f g (x :| xs) = foldlList f g x xs

foldlList :: (a -> b) -> (b -> a -> b) -> a -> [a] -> b
foldlList f g x = go (f x)
  where
    go acc [] = acc
    go acc (y : ys) = go (g acc y) ys

foldlNonEmpty' :: (a -> b) -> (b -> a -> b) -> NonEmpty a -> b
foldlNonEmpty' f g (x :| xs) = foldlList' f g x xs

foldlList' :: (a -> b) -> (b -> a -> b) -> a -> [a] -> b
foldlList' f g x = go (f x)
  where
    go acc [] = acc
    go acc (y : ys) =
      case g acc y of
        acc' -> go acc' ys
