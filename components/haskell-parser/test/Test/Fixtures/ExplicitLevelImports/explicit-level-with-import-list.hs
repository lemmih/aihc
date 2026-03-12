{-# LANGUAGE ExplicitLevelImports #-}

module ExplicitLevelWithImportList where

import quote Data.List (map)
import splice Data.Maybe (fromMaybe)

useMap :: [Int] -> [Int]
useMap = map (+ 1)
