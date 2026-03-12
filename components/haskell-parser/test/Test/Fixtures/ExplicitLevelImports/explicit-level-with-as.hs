{-# LANGUAGE ExplicitLevelImports #-}

module ExplicitLevelWithAs where

import quote Data.List as L
import splice Data.Maybe as M

useAliases :: [a] -> a -> a
useAliases xs fallback = M.fromMaybe fallback (L.listToMaybe xs)
