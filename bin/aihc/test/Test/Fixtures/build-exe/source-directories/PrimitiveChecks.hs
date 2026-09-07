{-# LANGUAGE MagicHash #-}

module PrimitiveChecks (primitiveChecks) where

import GHC.Exts (Proxy#, isTrue#, proxy#, reallyUnsafePtrEquality#)

proxy :: a -> Proxy# ()
proxy _ = proxy#

consume :: Proxy# () -> Int
consume _ = 42

loop :: Int
loop = loop

primitiveChecks :: Bool
primitiveChecks =
  consume (proxy True) == 42
    && isTrue# (reallyUnsafePtrEquality# loop loop)
    && not (isTrue# (reallyUnsafePtrEquality# (Just True) (Just False)))
