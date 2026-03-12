{-# LANGUAGE BangPatterns #-}

module BangPatternsWhere where

scale :: Int -> Int -> Int
scale factor input = go input
  where
    go !x = factor * x
