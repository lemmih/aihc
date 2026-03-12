{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesClassInstance where

class Convert a b | a -> b where
  convert :: a -> b

instance Convert Int Integer where
  convert = toInteger
