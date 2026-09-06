{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The 'Show' class. This module does not import "Prelude" so that the
-- exception types can use it.
module GHC.Show
  ( Show (..),
    ShowS,
    showChar,
    showParen,
    shows,
    showString,
    showList__,
    showSpace,
    appPrec,
    appPrec1,
    showMultiLineString,
    intToDigit,
  )
where

import Data.Bool (Bool (..), (&&))
import GHC.Base (Maybe (..), String, (++), (.))
import GHC.Char (chr)
import GHC.Err (errorWithoutStackTrace)
import GHC.Int (Int (..), Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Internal.Classes (Ord (..), Ordering (..))
import GHC.Internal.Integer (Integer (..), compareInteger#, eqInteger#, integerAbs, integerQuotRemWord#)
import GHC.Num (Num (..))
import GHC.Prim (Word#, chr#, eqWord#, int16ToInt#, int2Word#, int32ToInt#, int64ToInt#, int8ToInt#, minusWord#, quotRemWord#, word16ToWord#, word2Int#, word32ToWord#, word64ToWord#, word8ToWord#, (+#), (<#))
import GHC.Types (Char (..))
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

  showsPrec _ value suffix = show value ++ suffix
  show value = showsPrec (I# 0#) value []
  showList = showListWith shows

shows :: (Show a) => a -> ShowS
shows = showsPrec (I# 0#)

showChar :: Char -> ShowS
showChar char suffix = char : suffix

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen condition output =
  case condition of
    False -> output
    True -> showChar '(' . output . showChar ')'

showSpace :: ShowS
showSpace = showChar ' '

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] = showString "[]"
showListWith showElement (value : values) =
  showChar '[' . showElement value . showListTail showElement values

showListTail :: (a -> ShowS) -> [a] -> ShowS
showListTail _ [] = showChar ']'
showListTail showElement (value : values) =
  showChar ',' . showElement value . showListTail showElement values

showList__ :: (a -> ShowS) -> [a] -> ShowS
showList__ = showListWith

appPrec :: Int
appPrec = 10

appPrec1 :: Int
appPrec1 = 11

-- | The string in double quotes on one line. The characters are not
-- escaped, because the escape code lives with the @Show Char@ instance.
showMultiLineString :: String -> [String]
showMultiLineString value = [showChar '"' (value ++ "\"")]

intToDigit :: Int -> Char
intToDigit digit =
  case (0 <= digit && digit < 10, 10 <= digit && digit < 16) of
    (True, _) -> chr (48 + digit)
    (_, True) -> chr (87 + digit)
    _ -> errorWithoutStackTrace "Char.intToDigit: not a digit"

instance Show Int where
  showsPrec precedence (I# value) =
    case (<#) value 0# of
      0# -> showsUnsignedInt (int2Word# value)
      _ -> showParen (precedence > 6) (showChar '-' . showsUnsignedInt (minusWord# (int2Word# 0#) (int2Word# value)))

instance Show Word where
  showsPrec _ (W# value) = showsUnsignedInt value

instance Show Word8 where
  showsPrec _ (W8# value) = showsUnsignedInt (word8ToWord# value)

instance Show Word16 where
  showsPrec _ (W16# value) = showsUnsignedInt (word16ToWord# value)

instance Show Word32 where
  showsPrec _ (W32# value) = showsUnsignedInt (word32ToWord# value)

instance Show Word64 where
  showsPrec _ (W64# value) = showsUnsignedInt (word64ToWord# value)

-- The sized signed types show like Int, because their values fit in Int.
instance Show Int8 where
  showsPrec precedence (I8# value) = showsPrec precedence (I# (int8ToInt# value))

instance Show Int16 where
  showsPrec precedence (I16# value) = showsPrec precedence (I# (int16ToInt# value))

instance Show Int32 where
  showsPrec precedence (I32# value) = showsPrec precedence (I# (int32ToInt# value))

instance Show Int64 where
  showsPrec precedence (I64# value) = showsPrec precedence (I# (int64ToInt# value))

instance Show Integer where
  showsPrec = showsSignedInteger

showsSignedInteger :: Int -> Integer -> ShowS
showsSignedInteger precedence value =
  case (<#) (compareInteger# value (IS 0#)) 0# of
    0# -> showsUnsignedInteger value
    _ -> showParen (precedence > 6) (showChar '-' . showsUnsignedInteger (integerAbs value))

showsUnsignedInteger :: Integer -> ShowS
showsUnsignedInteger value suffix =
  case integerQuotRemWord# value (int2Word# 10#) of
    (# quotient, remainder #) ->
      case eqInteger# quotient (IS 0#) of
        1# -> digitChar remainder : suffix
        _ -> showsUnsignedInteger quotient (digitChar remainder : suffix)

showsUnsignedInt :: Word# -> ShowS
showsUnsignedInt value suffix =
  case quotRemWord# value (int2Word# 10#) of
    (# quotient, remainder #) ->
      case eqWord# quotient (int2Word# 0#) of
        1# -> digitChar remainder : suffix
        _ -> showsUnsignedInt quotient (digitChar remainder : suffix)

digitChar :: Word# -> Char
digitChar digit = C# (chr# ((+#) (word2Int# digit) 48#))

instance Show Bool where
  showsPrec _ False = showString "False"
  showsPrec _ True = showString "True"

instance Show () where
  showsPrec _ () = showString "()"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance (Show a) => Show (Maybe a) where
  showsPrec _ Nothing = showString "Nothing"
  showsPrec precedence (Just value) =
    showParen (precedence > 10) (showString "Just " . showsPrec 11 value)
