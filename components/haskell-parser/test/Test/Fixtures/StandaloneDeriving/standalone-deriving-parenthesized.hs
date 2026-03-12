{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingParenthesized where

data Tree a = Leaf a | Branch (Tree a) (Tree a)

deriving instance (Eq a) => Eq (Tree a)
