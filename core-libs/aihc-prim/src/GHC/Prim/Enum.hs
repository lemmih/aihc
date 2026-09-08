{-# LANGUAGE MagicHash #-}

-- | The 'Enum' class that arithmetic sequences desugar through, and the
-- 'Bounded' class beside it. GHC defines both in @GHC.Internal.Enum@; aihc
-- keeps them in the prim package because the compiler's built-in scope only
-- loads prim modules, and because the type checker recognizes a stock class
-- by its package, module and name, and it may name the prim package only.
--
-- The instances stay in @GHC.Enum@.
module GHC.Prim.Enum
  ( Enum (..),
    Bounded (..),
  )
where

import GHC.Prim (Int#, not#, uncheckedShiftRL#, word2Int#, (+#), (-#), (<#), (>#))
import GHC.Types (Int (..))

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

  succ value = case fromEnum value of
    I# index -> toEnum (I# ((+#) index 1#))
  pred value = case fromEnum value of
    I# index -> toEnum (I# ((-#) index 1#))
  enumFrom value = enumMap toEnum (enumIndices 0# (fromEnum value) (I# 1#) maximumInt)
  enumFromThen first second = case fromEnum first of
    I# x -> case fromEnum second of
      I# y -> case (<#) y x of
        1# -> enumMap toEnum (enumIndices 1# (I# x) (I# ((-#) y x)) minimumInt)
        _ -> enumMap toEnum (enumIndices 0# (I# x) (I# ((-#) y x)) maximumInt)
  enumFromTo first last = enumMap toEnum (enumIndices 0# (fromEnum first) (I# 1#) (fromEnum last))
  enumFromThenTo first second last = case fromEnum first of
    I# x -> case fromEnum second of
      I# y -> enumMap toEnum (enumIndices ((<#) y x) (I# x) (I# ((-#) y x)) (fromEnum last))

maximumInt :: Int
maximumInt = I# (word2Int# (uncheckedShiftRL# (not# 0##) 1#))

minimumInt :: Int
minimumInt = case maximumInt of
  I# value -> I# ((-#) ((-#) 0# value) 1#)

enumMap :: (a -> b) -> [a] -> [b]
enumMap _ [] = []
enumMap f (x : xs) = f x : enumMap f xs

-- Stop before an Int step can wrap.
enumIndices :: Int# -> Int -> Int -> Int -> [Int]
enumIndices descending (I# value) (I# step) (I# last) =
  case descending of
    1# -> case (<#) value last of
      1# -> []
      _ ->
        I# value : case (+#) value step of
          next -> case (>#) next value of
            1# -> []
            _ -> enumIndices descending (I# next) (I# step) (I# last)
    _ -> case (>#) value last of
      1# -> []
      _ ->
        I# value : case (+#) value step of
          next -> case (<#) next value of
            1# -> []
            _ -> enumIndices descending (I# next) (I# step) (I# last)

class Bounded a where
  minBound :: a
  maxBound :: a
