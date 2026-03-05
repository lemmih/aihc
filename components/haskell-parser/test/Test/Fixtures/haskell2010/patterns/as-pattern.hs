module P3 where
x s@(h:_) = (h, s)
x [] = ('_', [])
