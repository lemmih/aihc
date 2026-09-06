{-# LANGUAGE MagicHash #-}

module GHC.Enum
  ( Bounded (..),
    Enum (..),
    boundedEnumFrom,
    boundedEnumFromThen,
    fromEnumError,
    predError,
    succError,
    toEnumError,
  )
where

import Data.Bool (Bool (..))
import GHC.Int (Int (..), Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Internal.Char (Char)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Integer (Integer (..), integerToInt#)
import GHC.Num (Num (..))
import GHC.Prim
  ( Int#,
    int16ToInt#,
    int2Word#,
    int32ToInt#,
    int64ToInt#,
    int8ToInt#,
    intToInt16#,
    intToInt32#,
    intToInt64#,
    intToInt8#,
    not#,
    uncheckedShiftRL#,
    word16ToWord#,
    word2Int#,
    word32ToWord#,
    word64ToWord#,
    word8ToWord#,
    wordToWord16#,
    wordToWord32#,
    wordToWord64#,
    wordToWord8#,
    (+#),
    (-#),
    (<#),
  )
import GHC.Prim.Enum (Enum (..))
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))

class Bounded a where
  minBound :: a
  maxBound :: a

boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom value = enumFromTo value maxBound

boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen first second =
  case fromEnum second >= fromEnum first of
    True -> enumFromThenTo first second maxBound
    False -> enumFromThenTo first second minBound

toEnumError :: [Char] -> Int -> (a, a) -> b
toEnumError = toEnumError

fromEnumError :: [Char] -> a -> b
fromEnumError = fromEnumError

succError :: [Char] -> a
succError = succError

predError :: [Char] -> a
predError = predError

instance Bounded Bool where
  minBound = False
  maxBound = True

instance Enum Bool where
  succ False = True
  succ True = succError "Prelude.Enum.Bool.succ"

  pred True = False
  pred False = predError "Prelude.Enum.Bool.pred"

  toEnum (I# value) =
    case value of
      0# -> False
      1# -> True
      _ -> toEnumError "Bool" (I# value) (False, True)

  fromEnum False = I# 0#
  fromEnum True = I# 1#

  enumFrom False = [False, True]
  enumFrom True = [True]

  enumFromThen False True = [False, True]
  enumFromThen True False = [True, False]
  enumFromThen False False = [False]
  enumFromThen True True = [True]

  enumFromTo False False = [False]
  enumFromTo False True = [False, True]
  enumFromTo True True = [True]
  enumFromTo True False = []

  enumFromThenTo False True False = [False]
  enumFromThenTo False True True = [False, True]
  enumFromThenTo True False True = [True]
  enumFromThenTo True False False = [True, False]
  enumFromThenTo False False _ = [False]
  enumFromThenTo True True _ = [True]

instance Bounded Int where
  minBound =
    case maximumInt of
      I# value -> I# ((+#) value 1#)
  maxBound = maximumInt

instance Bounded Word where
  minBound = W# (int2Word# 0#)
  maxBound = W# (not# (int2Word# 0#))

instance Bounded Word8 where
  minBound = W8# (wordToWord8# (int2Word# 0#))
  maxBound = W8# (wordToWord8# (int2Word# 255#))

instance Bounded Word16 where
  minBound = W16# (wordToWord16# (int2Word# 0#))
  maxBound = W16# (wordToWord16# (int2Word# 65535#))

instance Bounded Word32 where
  minBound = W32# (wordToWord32# (int2Word# 0#))
  maxBound = W32# (wordToWord32# (int2Word# 4294967295#))

instance Bounded Word64 where
  minBound = W64# (wordToWord64# (int2Word# 0#))
  maxBound = W64# (wordToWord64# (not# (int2Word# 0#)))

instance Bounded Int8 where
  minBound = I8# (intToInt8# ((-#) 0# 128#))
  maxBound = I8# (intToInt8# 127#)

instance Bounded Int16 where
  minBound = I16# (intToInt16# ((-#) 0# 32768#))
  maxBound = I16# (intToInt16# 32767#)

instance Bounded Int32 where
  minBound = I32# (intToInt32# ((-#) 0# 2147483648#))
  maxBound = I32# (intToInt32# 2147483647#)

instance Bounded Int64 where
  minBound =
    case maximumInt of
      I# value -> I64# (intToInt64# ((+#) value 1#))
  maxBound =
    case maximumInt of
      I# value -> I64# (intToInt64# value)

instance Enum Int where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Int.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Int.pred"
      False -> value - 1

  toEnum value = value
  fromEnum value = value

  enumFrom value = enumFromTo value maxBound

  enumFromThen first second =
    case second >= first of
      True -> enumFromThenTo first second maxBound
      False -> enumFromThenTo first second minBound

  enumFromTo = enumIntFromTo
  enumFromThenTo = enumIntFromThenTo

instance Enum Integer where
  succ value = value + 1
  pred value = value - 1
  toEnum (I# value) = IS value
  fromEnum value = I# (integerToInt# value)
  enumFrom value = enumIntegerFromThen value (value + 1)
  enumFromThen = enumIntegerFromThen
  enumFromTo first = enumIntegerFromThenTo first (first + 1)
  enumFromThenTo = enumIntegerFromThenTo

instance Enum Word8 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Word8.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Word8.pred"
      False -> value - 1

  toEnum (I# value) =
    case inIntRange# 0# 255# value of
      True -> W8# (wordToWord8# (int2Word# value))
      False -> sizedToEnumError "Word8" (I# value)

  fromEnum (W8# value) = I# (word2Int# (word8ToWord# value))

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Word16 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Word16.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Word16.pred"
      False -> value - 1

  toEnum (I# value) =
    case inIntRange# 0# 65535# value of
      True -> W16# (wordToWord16# (int2Word# value))
      False -> sizedToEnumError "Word16" (I# value)

  fromEnum (W16# value) = I# (word2Int# (word16ToWord# value))

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Word32 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Word32.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Word32.pred"
      False -> value - 1

  toEnum (I# value) =
    case inIntRange# 0# 4294967295# value of
      True -> W32# (wordToWord32# (int2Word# value))
      False -> sizedToEnumError "Word32" (I# value)

  fromEnum (W32# value) = I# (word2Int# (word32ToWord# value))

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Word64 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Word64.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Word64.pred"
      False -> value - 1

  toEnum (I# value) =
    case nonNegativeInt# value of
      True -> W64# (wordToWord64# (int2Word# value))
      False -> sizedToEnumError "Word64" (I# value)

  fromEnum (W64# value) = I# (word2Int# (word64ToWord# value))

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Word where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Word.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Word.pred"
      False -> value - 1

  toEnum (I# value) =
    case nonNegativeInt# value of
      True -> W# (int2Word# value)
      False -> sizedToEnumError "Word" (I# value)

  fromEnum (W# value) = I# (word2Int# value)

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Int8 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Int8.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Int8.pred"
      False -> value - 1

  toEnum (I# value) =
    case inIntRange# ((-#) 0# 128#) 127# value of
      True -> I8# (intToInt8# value)
      False -> sizedToEnumError "Int8" (I# value)

  fromEnum (I8# value) = I# (int8ToInt# value)

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Int16 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Int16.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Int16.pred"
      False -> value - 1

  toEnum (I# value) =
    case inIntRange# ((-#) 0# 32768#) 32767# value of
      True -> I16# (intToInt16# value)
      False -> sizedToEnumError "Int16" (I# value)

  fromEnum (I16# value) = I# (int16ToInt# value)

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Int32 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Int32.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Int32.pred"
      False -> value - 1

  toEnum (I# value) =
    case inIntRange# ((-#) 0# 2147483648#) 2147483647# value of
      True -> I32# (intToInt32# value)
      False -> sizedToEnumError "Int32" (I# value)

  fromEnum (I32# value) = I# (int32ToInt# value)

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

instance Enum Int64 where
  succ value =
    case value == maxBound of
      True -> succError "Prelude.Enum.Int64.succ"
      False -> value + 1

  pred value =
    case value == minBound of
      True -> predError "Prelude.Enum.Int64.pred"
      False -> value - 1

  toEnum (I# value) = I64# (intToInt64# value)

  fromEnum (I64# value) = I# (int64ToInt# value)

  enumFrom value = enumFromTo value maxBound
  enumFromThen = boundedEnumFromThen
  enumFromTo = enumSizedFromTo
  enumFromThenTo = enumSizedFromThenTo

-- | Test that an 'Int' index is not less than zero.
nonNegativeInt# :: Int# -> Bool
nonNegativeInt# value =
  case (<#) value 0# of
    1# -> False
    _ -> True

inIntRange# :: Int# -> Int# -> Int# -> Bool
inIntRange# lower upper value =
  case (<#) value lower of
    1# -> False
    _ ->
      case (<#) upper value of
        1# -> False
        _ -> True

-- | Report an out-of-range 'toEnum' argument for a fixed-width type.
sizedToEnumError :: [Char] -> Int -> a
sizedToEnumError name value = toEnumError name value (sizedToEnumError name value, sizedToEnumError name value)

enumSizedFromTo :: (Ord a, Num a) => a -> a -> [a]
enumSizedFromTo value last =
  case value <= last of
    False -> []
    True ->
      value
        : case value == last of
          True -> []
          False -> enumSizedFromTo (value + 1) last

enumSizedFromThenTo :: (Ord a, Num a) => a -> a -> a -> [a]
enumSizedFromThenTo first second last =
  case second >= first of
    True -> enumSizedUp first (second - first) last
    False -> enumSizedDown first (second - first) last

-- | Count up and stop when the step makes the value wrap.
enumSizedUp :: (Ord a, Num a) => a -> a -> a -> [a]
enumSizedUp value step last =
  case value <= last of
    False -> []
    True ->
      value
        : case value + step of
          next ->
            case next < value of
              True -> []
              False -> enumSizedUp next step last

-- | Count down and stop when the step makes the value wrap.
enumSizedDown :: (Ord a, Num a) => a -> a -> a -> [a]
enumSizedDown value step last =
  case value >= last of
    False -> []
    True ->
      value
        : case value + step of
          next ->
            case next > value of
              True -> []
              False -> enumSizedDown next step last

maximumInt :: Int
maximumInt = I# (word2Int# (uncheckedShiftRL# (not# (int2Word# 0#)) 1#))

enumIntFromTo :: Int -> Int -> [Int]
enumIntFromTo value last =
  case value <= last of
    False -> []
    True ->
      value
        : case value == last of
          True -> []
          False -> enumIntFromTo (value + 1) last

enumIntFromThenTo :: Int -> Int -> Int -> [Int]
enumIntFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            False -> []
            True ->
              value
                : case value + step of
                  next ->
                    case next < value of
                      True -> []
                      False -> go next
        False ->
          case value >= last of
            False -> []
            True ->
              value
                : case value + step of
                  next ->
                    case next > value of
                      True -> []
                      False -> go next

enumIntegerFromThen :: Integer -> Integer -> [Integer]
enumIntegerFromThen first second = first : enumIntegerFromThen second (second + (second - first))

enumIntegerFromThenTo :: Integer -> Integer -> Integer -> [Integer]
enumIntegerFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            True -> value : go (value + step)
            False -> []
        False ->
          case value >= last of
            True -> value : go (value + step)
            False -> []
