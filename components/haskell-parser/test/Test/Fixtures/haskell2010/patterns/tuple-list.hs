module P2 where
x (a,b) = a + b
y (h:t) = h + length t
y [] = 0
