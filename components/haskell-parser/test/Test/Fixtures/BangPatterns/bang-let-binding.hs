{-# LANGUAGE BangPatterns #-}

module BangPatternsLetBinding where

strictPair :: (Int, Int) -> Int
strictPair pair =
  let !(x, y) = pair
   in x + y
