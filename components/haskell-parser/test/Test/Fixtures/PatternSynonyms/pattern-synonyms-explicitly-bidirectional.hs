{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsExplicitlyBidirectional where

pattern NonEmpty :: a -> [a] -> [a]
pattern NonEmpty x xs <- (x : xs)
  where
    NonEmpty x xs = x : xs

headOr :: a -> [a] -> a
headOr d (NonEmpty x _) = x
headOr d [] = d
