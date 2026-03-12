{-# LANGUAGE InstanceSigs #-}

module InstanceSigsConstrained where

class Collect a where
  collect :: a -> [String]

instance Show a => Collect [a] where
  collect :: [a] -> [String]
  collect = map show
