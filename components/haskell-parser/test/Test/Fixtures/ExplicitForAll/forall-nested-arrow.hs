{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllNestedArrow where

apply :: (forall a. a -> a) -> Int
apply f = f 3
