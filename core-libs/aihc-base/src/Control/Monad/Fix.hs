module Control.Monad.Fix
  ( MonadFix (..),
    fix,
  )
where

import Data.Function (fix)
import Prelude (Monad)

class (Monad m) => MonadFix m where
  mfix :: (a -> m a) -> m a
