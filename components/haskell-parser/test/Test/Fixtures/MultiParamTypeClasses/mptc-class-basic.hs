{-# LANGUAGE MultiParamTypeClasses #-}

module MultiParamTypeClassesClassBasic where

class Converts a b where
  convert :: a -> b
