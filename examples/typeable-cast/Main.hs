module Main where

import Data.Typeable (Typeable, cast, tyConName, typeOf, typeRepTyCon)

-- | A newtype over 'Int'. At runtime a 'Meters' value is just an 'Int',
-- but 'cast' compares types, so casting between the two must fail.
newtype Meters = Meters Int

-- | Another newtype over 'Int' with the same runtime representation.
newtype Seconds = Seconds Int

-- | A container whose type depends on its element type.
data Box a = Box a

-- | The name of the outermost type constructor of a value's type.
typeName :: (Typeable a) => a -> String
typeName value = tyConName (typeRepTyCon (typeOf value))

-- | Report whether a value of type @a@ can be cast to type @b@.
attempt :: String -> Maybe b -> String
attempt label result =
  label ++ ": " ++ case result of
    Just _ -> "cast succeeded"
    Nothing -> "cast failed"

-- | Sum every 'Int' in a list of heterogeneous values, skipping the rest.
sumInts :: [Maybe Int] -> Int
sumInts = foldr (\entry total -> maybe total (+ total) entry) 0

main :: IO ()
main = do
  putStrLn "-- same type"
  putStrLn (attempt "Int -> Int" (cast (42 :: Int) :: Maybe Int))
  putStrLn (attempt "Bool -> Bool" (cast True :: Maybe Bool))
  case cast (Box True) :: Maybe (Box Bool) of
    Just (Box flag) -> putStrLn ("Box Bool -> Box Bool: unpacked " ++ show flag)
    Nothing -> putStrLn "Box Bool -> Box Bool: cast failed"

  putStrLn "-- different types"
  putStrLn (attempt "Int -> Bool" (cast (42 :: Int) :: Maybe Bool))
  putStrLn (attempt "Box Bool -> Box Int" (cast (Box True) :: Maybe (Box Int)))

  putStrLn "-- newtypes share a representation but not a type"
  putStrLn (attempt "Meters -> Int" (cast (Meters 7) :: Maybe Int))
  putStrLn (attempt "Int -> Meters" (cast (7 :: Int) :: Maybe Meters))
  putStrLn (attempt "Meters -> Seconds" (cast (Meters 7) :: Maybe Seconds))
  putStrLn (attempt "Meters -> Meters" (cast (Meters 7) :: Maybe Meters))

  putStrLn "-- type constructor names"
  putStrLn (typeName (Meters 7))
  putStrLn (typeName (Seconds 7))
  putStrLn (typeName (7 :: Int))
  putStrLn (typeName (Box 'x'))

  putStrLn "-- picking the Ints out of a mixed bag"
  let ints = sumInts [cast (1 :: Int), cast True, cast (Meters 100), cast (2 :: Int), cast "three"]
  putStrLn ("sum of the Ints: " ++ show ints)
