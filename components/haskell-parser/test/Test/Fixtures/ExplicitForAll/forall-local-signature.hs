{-# LANGUAGE ExplicitForAll #-}

module ExplicitForAllLocalSignature where

outer :: Int -> Int
outer n =
  let local :: forall a. a -> a
      local x = x
   in local n
