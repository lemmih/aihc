{-# LANGUAGE PatternGuards #-}

module PatternGuardMultiQualifiers where

firstPositive :: [Int] -> Int
firstPositive xs
  | y : _ <- xs, y > 0 = y
  | otherwise = 0
