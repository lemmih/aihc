{-# LANGUAGE PatternGuards #-}

module PatternGuardWithLet where

safeDivHead :: Int -> [Int] -> Int
safeDivHead n xs
  | y : _ <- xs, let q = quot n y, q >= 0 = q
  | otherwise = 0
