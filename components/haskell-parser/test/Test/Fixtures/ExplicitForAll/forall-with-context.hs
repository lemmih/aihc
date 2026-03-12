{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllWithContext where

render :: forall a. Show a => a -> String
render = show
