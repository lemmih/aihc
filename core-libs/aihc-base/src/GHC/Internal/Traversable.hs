{-# LANGUAGE KindSignatures #-}

{- HLINT ignore "Use traverse" -}
{- HLINT ignore "Use sequenceA" -}

-- | The Traversable class. This module does not import Prelude, so Prelude
-- can export the class methods; the instances for the Prelude types live in
-- Prelude next to their Functor instances.
module GHC.Internal.Traversable
  ( Traversable (..),
  )
where

import Data.Kind (Type)
import GHC.Base (Applicative (..), Functor (..), Monad, id, (.))
import GHC.Internal.Foldable (Foldable)

class (Functor t, Foldable t) => Traversable (t :: Type -> Type) where
  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  sequenceA :: (Applicative f) => t (f a) -> f (t a)
  mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
  sequence :: (Monad m) => t (m a) -> m (t a)

  traverse f = sequenceA . fmap f
  sequenceA = traverse id
  mapM = traverse
  sequence = sequenceA
