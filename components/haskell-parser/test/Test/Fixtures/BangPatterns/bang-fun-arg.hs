{-# LANGUAGE BangPatterns #-}

module BangPatternsFunArg where

strictId :: Int -> Int
strictId !x = x
