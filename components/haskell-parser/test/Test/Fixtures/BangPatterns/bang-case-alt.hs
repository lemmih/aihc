{-# LANGUAGE BangPatterns #-}

module BangPatternsCaseAlt where

headStrict :: [Int] -> Maybe Int
headStrict xs =
  case xs of
    ![] -> Nothing
    !(y : _) -> Just y
