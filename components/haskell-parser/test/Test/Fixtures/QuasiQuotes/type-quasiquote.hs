{-# LANGUAGE QuasiQuotes #-}
module TypeQuasiQuote where

f :: [sql|INT|] -> Int
f _ = 0
