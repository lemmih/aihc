{-# LANGUAGE TypeOperators #-}

module TypeOperatorTypeSynonym where

infixr 6 :*:
type a :*: b = (a, b)
