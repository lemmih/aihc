module D2 where
f [] = 0
f (_:xs) = 1 + f xs
