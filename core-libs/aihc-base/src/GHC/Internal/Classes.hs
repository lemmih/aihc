{-# LANGUAGE MagicHash #-}

module GHC.Internal.Classes
  ( Eq (..),
    Ord (..),
    Ordering (..),
  )
where

import Data.Bool (not, (&&))
import Data.Either (Either (..))
import GHC.Classes (Eq (..), Ord (..))
import GHC.Int (Int (..), Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Internal.Integer (Integer, compareInteger#, eqInteger#)
import GHC.Prim
  ( Int#,
    Word#,
    compareInt#,
    eqWord#,
    int16ToInt#,
    int32ToInt#,
    int64ToInt#,
    int8ToInt#,
    ltWord#,
    ord#,
    word16ToWord#,
    word32ToWord#,
    word64ToWord#,
    word8ToWord#,
    (==#),
  )
import GHC.Prim.Base (List (..), Maybe (..))
import GHC.Types (Bool (..), Char (..), Ordering (..))
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))

instance Eq Bool where
  False == False = True
  False == True = False
  True == False = False
  True == True = True

  left /= right = not (left == right)

instance Eq Int where
  I# left == I# right =
    case (==#) left right of
      0# -> False
      _ -> True

  left /= right = not (left == right)

instance Eq Integer where
  left == right =
    case eqInteger# left right of
      0# -> False
      _ -> True

  left /= right = not (left == right)

instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _ == _ = False

  left /= right = not (left == right)

instance Eq Word where
  W# left == W# right = wordEquals left right
  left /= right = not (left == right)

instance Eq Word8 where
  W8# left == W8# right = wordEquals (word8ToWord# left) (word8ToWord# right)
  left /= right = not (left == right)

instance Eq Word16 where
  W16# left == W16# right = wordEquals (word16ToWord# left) (word16ToWord# right)
  left /= right = not (left == right)

instance Eq Word32 where
  W32# left == W32# right = wordEquals (word32ToWord# left) (word32ToWord# right)
  left /= right = not (left == right)

instance Eq Word64 where
  W64# left == W64# right = wordEquals (word64ToWord# left) (word64ToWord# right)
  left /= right = not (left == right)

instance Eq Int8 where
  I8# left == I8# right = intEquals (int8ToInt# left) (int8ToInt# right)
  left /= right = not (left == right)

instance Eq Int16 where
  I16# left == I16# right = intEquals (int16ToInt# left) (int16ToInt# right)
  left /= right = not (left == right)

instance Eq Int32 where
  I32# left == I32# right = intEquals (int32ToInt# left) (int32ToInt# right)
  left /= right = not (left == right)

instance Eq Int64 where
  I64# left == I64# right = intEquals (int64ToInt# left) (int64ToInt# right)
  left /= right = not (left == right)

instance Ord Bool where
  compare = compareBool
  left < right = classesLessBy compareBool left right
  left <= right = classesLessOrEqualBy compareBool left right
  left > right = classesGreaterBy compareBool left right
  left >= right = classesGreaterOrEqualBy compareBool left right
  max = classesMaxBy compareBool
  min = classesMinBy compareBool

instance Ord Int where
  compare = compareInt
  left < right = classesLessBy compareInt left right
  left <= right = classesLessOrEqualBy compareInt left right
  left > right = classesGreaterBy compareInt left right
  left >= right = classesGreaterOrEqualBy compareInt left right
  max = classesMaxBy compareInt
  min = classesMinBy compareInt

instance Ord Integer where
  compare = compareInteger
  left < right = classesLessBy compareInteger left right
  left <= right = classesLessOrEqualBy compareInteger left right
  left > right = classesGreaterBy compareInteger left right
  left >= right = classesGreaterOrEqualBy compareInteger left right
  max = classesMaxBy compareInteger
  min = classesMinBy compareInteger

instance Ord Ordering where
  compare = compareOrdering
  left < right = classesLessBy compareOrdering left right
  left <= right = classesLessOrEqualBy compareOrdering left right
  left > right = classesGreaterBy compareOrdering left right
  left >= right = classesGreaterOrEqualBy compareOrdering left right
  max = classesMaxBy compareOrdering
  min = classesMinBy compareOrdering

instance Ord Word where
  compare = compareWord
  left < right = classesLessBy compareWord left right
  left <= right = classesLessOrEqualBy compareWord left right
  left > right = classesGreaterBy compareWord left right
  left >= right = classesGreaterOrEqualBy compareWord left right
  max = classesMaxBy compareWord
  min = classesMinBy compareWord

instance Ord Word8 where
  compare = compareWord8
  left < right = classesLessBy compareWord8 left right
  left <= right = classesLessOrEqualBy compareWord8 left right
  left > right = classesGreaterBy compareWord8 left right
  left >= right = classesGreaterOrEqualBy compareWord8 left right
  max = classesMaxBy compareWord8
  min = classesMinBy compareWord8

instance Ord Word16 where
  compare = compareWord16
  left < right = classesLessBy compareWord16 left right
  left <= right = classesLessOrEqualBy compareWord16 left right
  left > right = classesGreaterBy compareWord16 left right
  left >= right = classesGreaterOrEqualBy compareWord16 left right
  max = classesMaxBy compareWord16
  min = classesMinBy compareWord16

instance Ord Word32 where
  compare = compareWord32
  left < right = classesLessBy compareWord32 left right
  left <= right = classesLessOrEqualBy compareWord32 left right
  left > right = classesGreaterBy compareWord32 left right
  left >= right = classesGreaterOrEqualBy compareWord32 left right
  max = classesMaxBy compareWord32
  min = classesMinBy compareWord32

instance Ord Word64 where
  compare = compareWord64
  left < right = classesLessBy compareWord64 left right
  left <= right = classesLessOrEqualBy compareWord64 left right
  left > right = classesGreaterBy compareWord64 left right
  left >= right = classesGreaterOrEqualBy compareWord64 left right
  max = classesMaxBy compareWord64
  min = classesMinBy compareWord64

instance Ord Int8 where
  compare = compareInt8
  left < right = classesLessBy compareInt8 left right
  left <= right = classesLessOrEqualBy compareInt8 left right
  left > right = classesGreaterBy compareInt8 left right
  left >= right = classesGreaterOrEqualBy compareInt8 left right
  max = classesMaxBy compareInt8
  min = classesMinBy compareInt8

instance Ord Int16 where
  compare = compareInt16
  left < right = classesLessBy compareInt16 left right
  left <= right = classesLessOrEqualBy compareInt16 left right
  left > right = classesGreaterBy compareInt16 left right
  left >= right = classesGreaterOrEqualBy compareInt16 left right
  max = classesMaxBy compareInt16
  min = classesMinBy compareInt16

instance Ord Int32 where
  compare = compareInt32
  left < right = classesLessBy compareInt32 left right
  left <= right = classesLessOrEqualBy compareInt32 left right
  left > right = classesGreaterBy compareInt32 left right
  left >= right = classesGreaterOrEqualBy compareInt32 left right
  max = classesMaxBy compareInt32
  min = classesMinBy compareInt32

instance Ord Int64 where
  compare = compareInt64
  left < right = classesLessBy compareInt64 left right
  left <= right = classesLessOrEqualBy compareInt64 left right
  left > right = classesGreaterBy compareInt64 left right
  left >= right = classesGreaterOrEqualBy compareInt64 left right
  max = classesMaxBy compareInt64
  min = classesMinBy compareInt64

compareBool :: Bool -> Bool -> Ordering
compareBool False False = EQ
compareBool False True = LT
compareBool True False = GT
compareBool True True = EQ

compareInt :: Int -> Int -> Ordering
compareInt (I# left) (I# right) = orderingFromInt# (compareInt# left right)

compareInteger :: Integer -> Integer -> Ordering
compareInteger left right = orderingFromInt# (compareInteger# left right)

compareOrdering :: Ordering -> Ordering -> Ordering
compareOrdering LT LT = EQ
compareOrdering LT _ = LT
compareOrdering EQ LT = GT
compareOrdering EQ EQ = EQ
compareOrdering EQ GT = LT
compareOrdering GT GT = EQ
compareOrdering GT _ = GT

compareWord :: Word -> Word -> Ordering
compareWord (W# left) (W# right) = compareWord# left right

compareWord8 :: Word8 -> Word8 -> Ordering
compareWord8 (W8# left) (W8# right) = compareWord# (word8ToWord# left) (word8ToWord# right)

compareWord16 :: Word16 -> Word16 -> Ordering
compareWord16 (W16# left) (W16# right) = compareWord# (word16ToWord# left) (word16ToWord# right)

compareWord32 :: Word32 -> Word32 -> Ordering
compareWord32 (W32# left) (W32# right) = compareWord# (word32ToWord# left) (word32ToWord# right)

compareWord64 :: Word64 -> Word64 -> Ordering
compareWord64 (W64# left) (W64# right) = compareWord# (word64ToWord# left) (word64ToWord# right)

compareInt8 :: Int8 -> Int8 -> Ordering
compareInt8 (I8# left) (I8# right) = orderingFromInt# (compareInt# (int8ToInt# left) (int8ToInt# right))

compareInt16 :: Int16 -> Int16 -> Ordering
compareInt16 (I16# left) (I16# right) = orderingFromInt# (compareInt# (int16ToInt# left) (int16ToInt# right))

compareInt32 :: Int32 -> Int32 -> Ordering
compareInt32 (I32# left) (I32# right) = orderingFromInt# (compareInt# (int32ToInt# left) (int32ToInt# right))

compareInt64 :: Int64 -> Int64 -> Ordering
compareInt64 (I64# left) (I64# right) = orderingFromInt# (compareInt# (int64ToInt# left) (int64ToInt# right))

intEquals :: Int# -> Int# -> Bool
intEquals left right =
  case (==#) left right of
    0# -> False
    _ -> True

wordEquals :: Word# -> Word# -> Bool
wordEquals left right =
  case eqWord# left right of
    0# -> False
    _ -> True

compareWord# :: Word# -> Word# -> Ordering
compareWord# left right =
  case eqWord# left right of
    0# ->
      case ltWord# left right of
        0# -> GT
        _ -> LT
    _ -> EQ

orderingFromInt# :: Int# -> Ordering
orderingFromInt# value =
  case value of
    0# -> EQ
    1# -> GT
    _ -> LT

classesLessBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesLessBy comparison left right =
  case comparison left right of
    LT -> True
    _ -> False

classesLessOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesLessOrEqualBy comparison left right =
  case comparison left right of
    GT -> False
    _ -> True

classesGreaterBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesGreaterBy comparison left right =
  case comparison left right of
    GT -> True
    _ -> False

classesGreaterOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
classesGreaterOrEqualBy comparison left right =
  case comparison left right of
    LT -> False
    _ -> True

classesMaxBy :: (a -> a -> Ordering) -> a -> a -> a
classesMaxBy comparison left right =
  case comparison left right of
    GT -> left
    _ -> right

classesMinBy :: (a -> a -> Ordering) -> a -> a -> a
classesMinBy comparison left right =
  case comparison left right of
    GT -> right
    _ -> left

instance Eq Char where
  C# x == C# y =
    case (==#) (ord# x) (ord# y) of
      0# -> False
      _ -> True

  x /= y = not (x == y)

compareChar :: Char -> Char -> Ordering
compareChar (C# x) (C# y) = compareInt (I# (ord# x)) (I# (ord# y))

instance Ord Char where
  compare = compareChar
  left < right = classesLessBy compareChar left right
  left <= right = classesLessOrEqualBy compareChar left right
  left > right = classesGreaterBy compareChar left right
  left >= right = classesGreaterOrEqualBy compareChar left right
  max = classesMaxBy compareChar
  min = classesMinBy compareChar

instance Eq () where
  () == () = True
  () /= () = False

instance (Eq a) => Eq [a] where
  [] == [] = True
  [] == (_ : _) = False
  (_ : _) == [] = False
  (x : xs) == (y : ys) = x == y && xs == ys

  xs /= ys = not (xs == ys)

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Nothing == Just _ = False
  Just _ == Nothing = False
  Just x == Just y = x == y

  x /= y = not (x == y)

instance (Eq a, Eq b) => Eq (Either a b) where
  Left x == Left y = x == y
  Left _ == Right _ = False
  Right _ == Left _ = False
  Right x == Right y = x == y

  x /= y = not (x == y)

instance (Eq a, Eq b) => Eq (a, b) where
  (leftA, leftB) == (rightA, rightB) = leftA == rightA && leftB == rightB
  left /= right = not (left == right)

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (leftA, leftB, leftC) == (rightA, rightB, rightC) =
    leftA == rightA && leftB == rightB && leftC == rightC
  left /= right = not (left == right)
