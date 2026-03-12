{-# LANGUAGE MultiParamTypeClasses #-}

module MultiParamTypeClassesInstanceBasic where

class Combine a b where
  combine :: a -> b -> (a, b)

instance Combine Int Bool where
  combine a b = (a, b)
