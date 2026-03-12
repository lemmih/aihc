{-# LANGUAGE BangPatterns #-}

module BangPatternsCaseAlt where

headStrict :: [Int] -> Maybe Int
headStrict xs =
  case xs of
    !(y : _) -> Just y
    _ -> Nothing
