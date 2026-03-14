{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsCompletePragma where

data Nat = Z | S Nat

pattern Zero :: Nat
pattern Zero = Z

pattern Succ :: Nat -> Nat
pattern Succ n = S n

{-# COMPLETE Zero, Succ #-}

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n
