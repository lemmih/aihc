{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllTopLevel where

identity :: forall a. a -> a
identity x = x
