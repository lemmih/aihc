{-# LANGUAGE InstanceSigs #-}

module InstanceSigsDefault where

class Pretty a where
  pretty :: a -> String

instance Pretty Bool where
  pretty :: Bool -> String
  pretty True = "true"
  pretty False = "false"
