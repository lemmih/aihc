module T1 where
f :: (Eq a, Show a) => a -> String
f x = if x == x then show x else "no"
