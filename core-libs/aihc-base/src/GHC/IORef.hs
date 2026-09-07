{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IORef
  ( IORef (..),
    newIORef,
    readIORef,
    writeIORef,
  )
where

import Data.Kind (Type)
import GHC.IO (IO (..))
import GHC.Internal.Classes (Eq (..))
import GHC.Prim (RealWorld, newMutVar#, readMutVar#, writeMutVar#)
import GHC.STRef (STRef (..))

-- | A mutable variable in the 'IO' monad. The representation deliberately
-- shares the 'STRef' boundary used by @base@.
newtype IORef (a :: Type) = IORef (STRef RealWorld a)
  deriving newtype (Eq)

-- | Build a new 'IORef'.
newIORef :: a -> IO (IORef a)
newIORef initial =
  IO
    ( \state ->
        case newMutVar# initial state of
          (# nextState, reference #) -> (# nextState, IORef (STRef reference) #)
    )

-- | Read the value of an 'IORef'.
readIORef :: IORef a -> IO a
readIORef (IORef (STRef reference)) = IO (readMutVar# reference)

-- | Write a new value into an 'IORef' without forcing it.
writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef (STRef reference)) value =
  IO
    ( \state ->
        case writeMutVar# reference value state of
          nextState -> (# nextState, () #)
    )
