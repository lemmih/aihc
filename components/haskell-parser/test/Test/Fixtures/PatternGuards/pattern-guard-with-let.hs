{-# LANGUAGE PatternGuards #-}

module PatternGuardWithLet where

safeDivHead :: Int -> [Int] -> Int
safeDivHead n xs
  | y : _ <- xs, let q = n `div` y, q >= 0 = q
  | otherwise = 0
