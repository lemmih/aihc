module TypeApplicationsNoSpace where

f :: a -> a
f x = x

x :: Int
x = f @Int 1
