{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsSignatureBasic where

pattern Head :: a -> [a]
pattern Head x <- (x : _)

firstOr :: a -> [a] -> a
firstOr d (Head x) = x
firstOr d [] = d
