{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesBidirectional where

class Iso a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a
