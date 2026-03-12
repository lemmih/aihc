{-# LANGUAGE ExistentialQuantification #-}

module ExistentialInfixConstructor where

infixr 5 :&:

data PairBox = forall a b. (Show a, Show b) => a :&: b

pairRender :: PairBox -> String
pairRender (x :&: y) = show x ++ show y
