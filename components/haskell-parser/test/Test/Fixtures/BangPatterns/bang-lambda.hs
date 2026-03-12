{-# LANGUAGE BangPatterns #-}

module BangPatternsLambda where

applyStrict :: (Int -> Int) -> Int -> Int
applyStrict f = (\ !x -> f x)
