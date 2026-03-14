{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsUnidirectionalPrefix where

data T = T Int

pattern P :: Int -> T
pattern P x <- T x

isP :: T -> Bool
isP (P _) = True
isP _ = False
