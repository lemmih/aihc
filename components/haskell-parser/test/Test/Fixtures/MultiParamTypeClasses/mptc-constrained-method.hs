{-# LANGUAGE MultiParamTypeClasses #-}

module MultiParamTypeClassesConstrainedMethod where

class Render a b where
  render :: Show a => a -> b -> String
