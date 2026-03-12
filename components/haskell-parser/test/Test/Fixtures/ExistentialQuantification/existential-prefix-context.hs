{-# LANGUAGE ExistentialQuantification #-}

module ExistentialPrefixContext where

data Box = forall a. Show a => Box a

render :: Box -> String
render (Box x) = show x
