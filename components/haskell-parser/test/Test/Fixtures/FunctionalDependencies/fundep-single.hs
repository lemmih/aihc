{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesSingle where

class Collects e c | c -> e where
  insert :: e -> c -> c
