{-# LANGUAGE PatternGuards #-}

module PatternGuardSingle where

headOrZero :: [Int] -> Int
headOrZero xs
  | y : _ <- xs = y
  | otherwise = 0
