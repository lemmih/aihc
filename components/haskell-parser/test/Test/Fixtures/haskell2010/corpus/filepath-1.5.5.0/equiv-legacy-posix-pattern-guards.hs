{-# LANGUAGE PatternGuards #-}
module Repro where

f x
  | Just y <- x
  = y
f _ = 0
