module ParallelListCompBasic where

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = [ (x, y) | x <- xs | y <- ys ]
