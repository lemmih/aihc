module T6 where
f :: Int -> Int
f n = helper n
  where
    helper :: Int -> Int
    helper x = x + 1
