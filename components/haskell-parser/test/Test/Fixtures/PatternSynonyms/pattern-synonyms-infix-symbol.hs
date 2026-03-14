{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsInfixSymbol where

data Pair a = Pair a a

pattern (:*:) :: a -> a -> Pair a
pattern x :*: y = Pair x y
infix 6 :*:

swap :: Pair a -> Pair a
swap (x :*: y) = y :*: x
