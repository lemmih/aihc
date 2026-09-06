module Control.Monad.Zip
  ( MonadZip (..),
  )
where

import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Prelude

-- | Monads that support zipping.
class (Monad m) => MonadZip m where
  mzip :: m a -> m b -> m (a, b)
  mzipWith :: (a -> b -> c) -> m a -> m b -> m c
  munzip :: m (a, b) -> (m a, m b)

  mzip = mzipWith (\left right -> (left, right))
  mzipWith f left right = fmap (\pair -> f (fst pair) (snd pair)) (mzip left right)
  munzip pairs = (fmap fst pairs, fmap snd pairs)

instance MonadZip [] where
  mzip = zip
  mzipWith = zipWith
  munzip = unzip

instance MonadZip Maybe where
  mzipWith f (Just left) (Just right) = Just (f left right)
  mzipWith _ _ _ = Nothing

instance MonadZip Identity where
  mzipWith f (Identity left) (Identity right) = Identity (f left right)
  munzip (Identity (left, right)) = (Identity left, Identity right)

instance MonadZip NonEmpty where
  mzip (left :| lefts) (right :| rights) = (left, right) :| zip lefts rights
  mzipWith f (left :| lefts) (right :| rights) = f left right :| zipWith f lefts rights
  munzip ((left, right) :| pairs) =
    case unzip pairs of
      (lefts, rights) -> (left :| lefts, right :| rights)
