{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategiesNewtype where

newtype Age = Age Int
  deriving newtype (Eq, Ord, Show)
