{-# LANGUAGE ImportQualifiedPost #-}

module ImportQualifiedPostBasic where

import Data.List qualified as List

sorted :: [Int] -> [Int]
sorted = List.sort
