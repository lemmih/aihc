{-# LANGUAGE TypeOperators #-}

module TypeOperatorDataInfix where

infixr 5 :+:
data a :+: b = InL a | InR b
