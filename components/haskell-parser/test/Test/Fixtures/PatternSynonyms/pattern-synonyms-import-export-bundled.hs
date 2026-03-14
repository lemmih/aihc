{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module PatternSynonymsImportExportBundled
  ( Nat (Zero, Succ),
  ) where

import PatternSynonymsSource (Nat (Zero, Succ))

fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n
