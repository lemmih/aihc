{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | The lazy state-thread monad. This milestone runs each action in
-- sequence, so the lazy 'ST' has the semantics of the strict 'ST'. The
-- type is distinct, so instances for both monads can exist together.
module Control.Monad.ST.Lazy
  ( ST,
    runST,
    strictToLazyST,
    lazyToStrictST,
  )
where

import GHC.Base (Applicative (..), Functor (..), Monad (..))
import GHC.ST qualified as Strict

newtype ST s a = ST (Strict.ST s a)
  deriving newtype (Functor, Applicative, Monad)

-- | Run a lazy state thread and return its result.
runST :: (forall s. ST s a) -> a
runST action = Strict.runST (lazyToStrictST action)

-- | Convert a strict 'Strict.ST' action to a lazy 'ST' action.
strictToLazyST :: Strict.ST s a -> ST s a
strictToLazyST = ST

-- | Convert a lazy 'ST' action to a strict 'Strict.ST' action.
lazyToStrictST :: ST s a -> Strict.ST s a
lazyToStrictST (ST action) = action
