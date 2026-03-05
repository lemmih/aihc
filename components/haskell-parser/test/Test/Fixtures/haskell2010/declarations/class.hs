module D7 where
class Eq a => Named a where
  nameOf :: a -> String
