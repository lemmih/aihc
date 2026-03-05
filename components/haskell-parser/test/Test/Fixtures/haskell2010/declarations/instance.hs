module D8 where
data Box a = Box a
instance Eq a => Eq (Box a) where
  Box x == Box y = x == y
