{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesMultipleClauses where

data Box a = Box a
  deriving stock (Eq)
  deriving stock (Show)
