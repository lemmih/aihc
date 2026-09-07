{-# LANGUAGE TupleSections #-}

module Data.Traversable
  ( Traversable (..),
    for,
    forM,
    mapAccumL,
    mapAccumR,
    foldMapDefault,
  )
where

import Control.Applicative (Const (..))
import Data.Monoid (Monoid)
import GHC.Internal.Traversable (Traversable (..))
import Prelude (Applicative (..), Functor (..), Monad, (.))

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for structure f = traverse f structure

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM structure f = mapM f structure

newtype StateL s a = StateL {runStateL :: s -> (s, a)}

instance Functor (StateL s) where
  fmap f (StateL action) = StateL (\s -> let ~(next, value) = action s in (next, f value))

instance Applicative (StateL s) where
  pure value = StateL (,value)
  StateL function <*> StateL argument =
    StateL
      ( \s ->
          let ~(next, f) = function s
              ~(final, value) = argument next
           in (final, f value)
      )

newtype StateR s a = StateR {runStateR :: s -> (s, a)}

instance Functor (StateR s) where
  fmap f (StateR action) = StateR (\s -> let ~(next, value) = action s in (next, f value))

instance Applicative (StateR s) where
  pure value = StateR (,value)
  StateR function <*> StateR argument =
    StateR
      ( \s ->
          let ~(next, value) = argument s
              ~(final, f) = function next
           in (final, f value)
      )

mapAccumL :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumL f initial values = runStateL (traverse (\value -> StateL (`f` value)) values) initial

mapAccumR :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumR f initial values = runStateR (traverse (\value -> StateR (`f` value)) values) initial

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f values = getConst (traverse (Const . f) values)
