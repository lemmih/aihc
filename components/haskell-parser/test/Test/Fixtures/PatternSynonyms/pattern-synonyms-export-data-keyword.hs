{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsExportDataKeyword
  ( data Zero,
    data Succ,
  ) where

data Nat = Z | S Nat

pattern Zero :: Nat
pattern Zero = Z

pattern Succ :: Nat -> Nat
pattern Succ n = S n
