module Data.Traversable
  ( Traversable (..),
    for,
    forM,
  )
where

import GHC.Internal.Traversable (Traversable (..))
import Prelude (Applicative, Monad)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for structure f = traverse f structure

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM structure f = mapM f structure
