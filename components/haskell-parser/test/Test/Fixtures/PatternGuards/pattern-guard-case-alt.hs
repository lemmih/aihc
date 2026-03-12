{-# LANGUAGE PatternGuards #-}

module PatternGuardCaseAlt where

asPair :: Maybe a -> [a]
asPair value =
  case value of
    m
      | Just x <- m -> [x, x]
      | otherwise -> []
