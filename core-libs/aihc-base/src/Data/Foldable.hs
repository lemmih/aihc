{-# LANGUAGE KindSignatures #-}

module Data.Foldable
  ( Foldable (..),
    foldrM,
    foldlM,
    traverse_,
    for_,
    sequenceA_,
    asum,
    mapM_,
    forM_,
    sequence_,
    msum,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import GHC.Internal.Foldable (Foldable (..), all, and, any, concat, concatMap, mapM_, notElem, or, sequence_)
import Prelude
  ( Applicative (..),
    Bool (..),
    Either (..),
    Eq (..),
    Functor (..),
    Int,
    Maybe (..),
    Monad (..),
    Num (..),
    Ord (..),
    Ordering (..),
    id,
    not,
    (&&),
    (++),
    (.),
    (||),
  )

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f initial structure = foldl step pure structure initial
  where
    step continuation value rest = f value rest >>= continuation

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f initial structure = foldr step pure structure initial
  where
    step value continuation rest = f rest value >>= continuation

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (thenApplicative . f) (pure ())

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ structure f = traverse_ f structure

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr thenApplicative (pure ())

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = for_

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

maximumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy compareValues = foldr1 choose
  where
    choose left right =
      case compareValues left right of
        GT -> left
        _ -> right

minimumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
minimumBy compareValues = foldr1 choose
  where
    choose left right =
      case compareValues left right of
        GT -> right
        _ -> left

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find predicate = foldr choose Nothing
  where
    choose value rest =
      case predicate value of
        True -> Just value
        False -> rest

thenApplicative :: (Applicative f) => f a -> f b -> f b
thenApplicative first second = fmap (\_ value -> value) first <*> second
