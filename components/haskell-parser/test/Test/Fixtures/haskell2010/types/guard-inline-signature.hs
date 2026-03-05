module T11 where
choose :: Bool -> Bool
choose b
  | (not b :: Bool) = True
  | otherwise = False
