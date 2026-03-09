{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Repro where

data WindowsFilePaths = WindowsFilePaths [Int]

g = \(xs :: WindowsFilePaths) ->
  let f x = x
   in f xs
