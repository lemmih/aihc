{-# LANGUAGE InstanceSigs #-}

module InstanceSigsBasic where

class Render a where
  render :: a -> String

instance Render Int where
  render :: Int -> String
  render = show
