{-# LANGUAGE ExplicitLevelImports #-}

module ExplicitLevelWithHiding where

import quote Prelude hiding (map)
import splice Data.List hiding (foldl)

useFilter :: (a -> Bool) -> [a] -> [a]
useFilter = filter
