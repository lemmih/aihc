module Data.Traversable
  ( Traversable (..),
    for,
    forM,
    mapAccumL,
    mapAccumR,
    fmapDefault,
    foldMapDefault,
  )
where

import Data.Monoid (Monoid (..))
import GHC.Internal.Traversable (Traversable (..))
import Prelude
  ( Applicative (..),
    Functor (..),
    Monad,
    (.),
  )

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for structure f = traverse f structure

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM structure f = mapM f structure

-- | A state transformer whose effects run from left to right.
newtype StateL s a = StateL (s -> (s, a))

runStateL :: StateL s a -> s -> (s, a)
runStateL (StateL step) = step

instance Functor (StateL s) where
  fmap f (StateL step) =
    StateL
      ( \state ->
          case step state of
            (state', value) -> (state', f value)
      )

instance Applicative (StateL s) where
  pure value = StateL (\state -> (state, value))
  StateL stepFunction <*> StateL stepValue =
    StateL
      ( \state ->
          case stepFunction state of
            (state', function) ->
              case stepValue state' of
                (state'', value) -> (state'', function value)
      )

-- | A state transformer whose effects run from right to left.
newtype StateR s a = StateR (s -> (s, a))

runStateR :: StateR s a -> s -> (s, a)
runStateR (StateR step) = step

instance Functor (StateR s) where
  fmap f (StateR step) =
    StateR
      ( \state ->
          case step state of
            (state', value) -> (state', f value)
      )

instance Applicative (StateR s) where
  pure value = StateR (\state -> (state, value))
  StateR stepFunction <*> StateR stepValue =
    StateR
      ( \state ->
          case stepValue state of
            (state', value) ->
              case stepFunction state' of
                (state'', function) -> (state'', function value)
      )

mapAccumL :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumL f initial structure =
  runStateL (traverse (\value -> StateL (\state -> f state value)) structure) initial

mapAccumR :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumR f initial structure =
  runStateR (traverse (\value -> StateR (\state -> f state value)) structure) initial

-- | The identity applicative that fmapDefault uses.
newtype Plain a = Plain a

runPlain :: Plain a -> a
runPlain (Plain value) = value

instance Functor Plain where
  fmap f (Plain value) = Plain (f value)

instance Applicative Plain where
  pure = Plain
  Plain function <*> Plain value = Plain (function value)

-- | The constant applicative that foldMapDefault uses.
newtype Accumulate m a = Accumulate m

runAccumulate :: Accumulate m a -> m
runAccumulate (Accumulate value) = value

instance Functor (Accumulate m) where
  fmap _ (Accumulate value) = Accumulate value

instance (Monoid m) => Applicative (Accumulate m) where
  pure _ = Accumulate mempty
  Accumulate left <*> Accumulate right = Accumulate (left `mappend` right)

fmapDefault :: (Traversable t) => (a -> b) -> t a -> t b
fmapDefault f = runPlain . traverse (Plain . f)

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = runAccumulate . traverse (Accumulate . f)
