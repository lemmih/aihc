{-# LANGUAGE TypeOperators #-}

module TypeOperatorSignature where

infixr 5 :+:
data a :+: b = L a | R b

foldEither :: (a -> c) -> (b -> c) -> (a :+: b) -> c
foldEither f _ (L a) = f a
foldEither _ g (R b) = g b
