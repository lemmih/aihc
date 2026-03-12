{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesMultiLeft where

class Merge a b c | a b -> c where
  merge :: a -> b -> c
