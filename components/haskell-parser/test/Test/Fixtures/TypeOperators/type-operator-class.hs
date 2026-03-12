{-# LANGUAGE TypeOperators #-}

module TypeOperatorClass where

infix 4 :=:
class a :=: b where
  proof :: a -> b -> ()
