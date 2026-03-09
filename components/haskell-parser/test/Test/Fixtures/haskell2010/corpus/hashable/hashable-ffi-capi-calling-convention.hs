{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
module X where

import GHC.Exts (ByteArray#)

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_withSeed_offset"
  unsafe_xxh3_64bit_withSeed_ba :: ByteArray# -> Int
