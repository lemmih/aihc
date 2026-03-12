{-# LANGUAGE InstanceSigs #-}

module InstanceSigsMultiMethod where

class Ops a where
  incr :: a -> a
  asString :: a -> String

instance Ops Int where
  incr :: Int -> Int
  incr x = x + 1

  asString :: Int -> String
  asString = show
