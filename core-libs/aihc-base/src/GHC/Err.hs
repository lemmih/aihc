{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}

module GHC.Err
  ( error,
    errorWithoutStackTrace,
    undefined,
  )
where

import GHC.Base (String)
import GHC.Internal.Stack (appendCallStack)
import GHC.Prim (raise#)
import GHC.Stack.Types (HasCallStack)
import GHC.Types (RuntimeRep, TYPE)

-- | Stop the program with a message and the call stack of the call site.
error :: forall (r :: RuntimeRep) (a :: TYPE r). (HasCallStack) => String -> a
error message = raise# (appendCallStack message ?callStack)

errorWithoutStackTrace :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
errorWithoutStackTrace = raise#

undefined :: forall (r :: RuntimeRep) (a :: TYPE r). (HasCallStack) => a
undefined = raise# (appendCallStack "Prelude.undefined" ?callStack)
