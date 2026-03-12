{-# LANGUAGE MultiParamTypeClasses #-}

module MultiParamTypeClassesThreeParams where

class Rel a b c where
  relate :: a -> b -> c

instance Rel Int Int Int where
  relate x y = x + y
