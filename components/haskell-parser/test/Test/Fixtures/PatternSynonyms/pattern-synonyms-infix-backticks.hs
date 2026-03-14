{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsInfixBackticks where

data Pair a = Pair a a

pattern PairP :: a -> a -> Pair a
pattern x `PairP` y = Pair x y
infix 5 `PairP`

swap :: Pair a -> Pair a
swap (x `PairP` y) = y `PairP` x
