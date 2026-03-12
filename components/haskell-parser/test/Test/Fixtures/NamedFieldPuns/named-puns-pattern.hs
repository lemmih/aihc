{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsPattern where

data Person = Person {name :: String, age :: Int}

greet :: Person -> String
greet Person {name} = "hello " ++ name
