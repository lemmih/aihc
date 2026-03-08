module TypeApplicationsExprBasic where

f :: a -> a
f x = x

h :: (Int, Int, Int, Int, Int)
h = (f @Int 1, f 2 @Int, f @Int @Int 3, f @(Maybe Int) (Just 4), ((+) @Int) 5 6)
