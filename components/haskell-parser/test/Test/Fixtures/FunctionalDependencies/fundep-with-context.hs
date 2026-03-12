{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesWithContext where

class Show a => Pretty a b | a -> b where
  pretty :: a -> b
