{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsImportExportBundledData
  ( Nat (data Zero, data Succ),
  ) where

import PatternSynonymsSource (Nat (data Zero, data Succ))

fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n
