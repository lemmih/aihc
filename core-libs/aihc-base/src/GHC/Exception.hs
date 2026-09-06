{-# LANGUAGE MagicHash #-}

module GHC.Exception
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
    throw,
    prettyCallStackLines,
  )
where

import GHC.Exception.Type
  ( ArithException (..),
    Exception (..),
    SomeException (..),
    divZeroException,
    overflowException,
    ratioZeroDenomException,
    underflowException,
  )
import GHC.Internal.Stack (prettyCallStackLines)
import GHC.Prim (raise#)

throw :: (Exception e) => e -> a
throw exception = raise# (toException exception)
