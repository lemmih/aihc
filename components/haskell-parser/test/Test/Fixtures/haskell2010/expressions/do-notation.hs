module X7 where
x = do
  a <- Just 1
  b <- Just 2
  return (a + b)
