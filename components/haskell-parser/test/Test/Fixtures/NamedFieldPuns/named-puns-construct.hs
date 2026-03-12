{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsConstruct where

data Person = Person {name :: String, age :: Int}

mkPerson :: String -> Int -> Person
mkPerson name age = Person {name, age}
