{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module X where

import GHC.Exts (Int#)

f :: Int# -> (# Int#, Int# #)
f x = (# x, x #)
