module T15 where
toMaybe :: (->) a (Maybe a)
toMaybe x = Just x
