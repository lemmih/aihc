{-# LANGUAGE QuasiQuotes #-}
module PatQuasiQuote where

isMatch [sql|user:{id}|] = True
isMatch _ = False
