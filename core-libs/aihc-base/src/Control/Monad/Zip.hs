{-# LANGUAGE KindSignatures #-}

module Control.Monad.Zip
  ( MonadZip (..),
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Prelude

{- HLINT ignore "Use (,)" -}
{- HLINT ignore "Avoid NonEmpty.unzip" -}

-- | Instances should satisfy the laws:
--
-- [Naturality] @'fmap' (f '***' g) ('mzip' ma mb) = 'mzip' ('fmap' f ma) ('fmap' g mb)@
-- [Information preservation] if @'fmap' ('const' ()) ma = 'fmap' ('const' ()) mb@ then @'munzip' ('mzip' ma mb) = (ma, mb)@
class (Monad m) => MonadZip (m :: Type -> Type) where
  mzip :: m a -> m b -> m (a, b)
  mzip = mzipWith (\a b -> (a, b))

  mzipWith :: (a -> b -> c) -> m a -> m b -> m c
  mzipWith f ma mb = fmap (uncurry f) (mzip ma mb)

  munzip :: m (a, b) -> (m a, m b)
  munzip mab = (fmap fst mab, fmap snd mab)

instance MonadZip [] where
  mzip = zip
  mzipWith = zipWith
  munzip = unzip

instance MonadZip Maybe where
  mzipWith f (Just a) (Just b) = Just (f a b)
  mzipWith _ Nothing _ = Nothing
  mzipWith _ (Just _) Nothing = Nothing

instance MonadZip Identity where
  mzipWith f (Identity a) (Identity b) = Identity (f a b)
  munzip (Identity (a, b)) = (Identity a, Identity b)

instance MonadZip NonEmpty where
  mzip = NonEmpty.zip
  mzipWith = NonEmpty.zipWith
  munzip = NonEmpty.unzip
