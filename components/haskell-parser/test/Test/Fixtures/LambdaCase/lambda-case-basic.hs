{-# LANGUAGE LambdaCase #-}

module LambdaCaseBasic where

describeBool :: Bool -> String
describeBool = \case
  True -> "yes"
  False -> "no"

