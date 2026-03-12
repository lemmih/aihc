{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesStockMulti where

data Pair a = Pair a a
  deriving stock (Eq, Show)
