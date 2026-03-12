{-# LANGUAGE NamedWildCards #-}

module NamedWildcardForall where

poly :: forall _a. _a -> _a
poly x = x
