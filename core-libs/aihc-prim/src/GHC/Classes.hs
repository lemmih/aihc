{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use min" #-}

module GHC.Classes
  ( Eq (..),
    Ord (..),
  )
where

import GHC.Types (Bool (..), Ordering (..))

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  left == right = classesNot (left /= right)
  left /= right = classesNot (left == right)

infix 4 ==, /=

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  compare left right =
    if left == right
      then EQ
      else
        if left <= right
          then LT
          else GT
  left < right =
    case compare left right of
      LT -> True
      _ -> False
  left <= right =
    case compare left right of
      GT -> False
      _ -> True
  left > right =
    case compare left right of
      GT -> True
      _ -> False
  left >= right =
    case compare left right of
      LT -> False
      _ -> True
  max left right = if left <= right then right else left
  min left right = if left <= right then left else right

infix 4 <, <=, >, >=

-- | Boolean negation for the class default methods.
classesNot :: Bool -> Bool
classesNot True = False
classesNot False = True
