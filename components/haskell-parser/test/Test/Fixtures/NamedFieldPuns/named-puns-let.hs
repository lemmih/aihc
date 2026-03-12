{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsLet where

data User = User {userName :: String, userId :: Int}

render :: User -> String
render u =
  let User {userName, userId} = u
   in userName ++ "#" ++ show userId
