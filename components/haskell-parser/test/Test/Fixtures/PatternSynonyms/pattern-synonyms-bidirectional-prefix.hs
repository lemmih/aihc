{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsBidirectionalPrefix where

data T = T Int

pattern P :: Int -> T
pattern P x = T x

mk :: Int -> T
mk x = P x
