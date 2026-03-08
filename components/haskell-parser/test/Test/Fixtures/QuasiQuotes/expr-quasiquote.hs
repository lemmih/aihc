{-# LANGUAGE QuasiQuotes #-}
module ExprQuasiQuote where

x = [sql|select * from users where id = 1|]
