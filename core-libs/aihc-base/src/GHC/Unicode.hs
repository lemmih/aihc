{-# LANGUAGE MagicHash #-}

module GHC.Unicode
  ( GeneralCategory (..),
    generalCategory,
    isAlpha,
    isAlphaNum,
    isAscii,
    isAsciiLower,
    isAsciiUpper,
    isControl,
    isDigit,
    isHexDigit,
    isLatin1,
    isLower,
    isLowerCase,
    isOctDigit,
    isPrint,
    isPunctuation,
    isSpace,
    isSymbol,
    isUpper,
    isUpperCase,
    toLower,
    toTitle,
    toUpper,
  )
where

import GHC.Base (ord)
import GHC.Prim (Int#)
import GHC.Prim.Unicode
  ( generalCategory#,
    isLowercase#,
    isUppercase#,
    unicodeToLower,
    unicodeToTitle,
    unicodeToUpper,
  )
import Prelude

data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned

generalCategory :: Char -> GeneralCategory
generalCategory (C# value) =
  case generalCategory# value of
    0# -> UppercaseLetter
    1# -> LowercaseLetter
    2# -> TitlecaseLetter
    3# -> ModifierLetter
    4# -> OtherLetter
    5# -> NonSpacingMark
    6# -> SpacingCombiningMark
    7# -> EnclosingMark
    8# -> DecimalNumber
    9# -> LetterNumber
    10# -> OtherNumber
    11# -> ConnectorPunctuation
    12# -> DashPunctuation
    13# -> OpenPunctuation
    14# -> ClosePunctuation
    15# -> InitialQuote
    16# -> FinalQuote
    17# -> OtherPunctuation
    18# -> MathSymbol
    19# -> CurrencySymbol
    20# -> ModifierSymbol
    21# -> OtherSymbol
    22# -> Space
    23# -> LineSeparator
    24# -> ParagraphSeparator
    25# -> Control
    26# -> Format
    27# -> Surrogate
    28# -> PrivateUse
    _ -> NotAssigned

isAscii :: Char -> Bool
isAscii value = ord value < 128

isLatin1 :: Char -> Bool
isLatin1 value = ord value <= 255

isAsciiLower :: Char -> Bool
isAsciiLower value =
  let codePoint = ord value
   in codePoint >= 97 && codePoint <= 122

isAsciiUpper :: Char -> Bool
isAsciiUpper value =
  let codePoint = ord value
   in codePoint >= 65 && codePoint <= 90

isControl :: Char -> Bool
isControl value =
  case generalCategory value of
    Control -> True
    _ -> False

isPrint :: Char -> Bool
isPrint value =
  case generalCategory value of
    LineSeparator -> False
    ParagraphSeparator -> False
    Control -> False
    Format -> False
    Surrogate -> False
    PrivateUse -> False
    NotAssigned -> False
    _ -> True

isSpace :: Char -> Bool
isSpace value =
  let codePoint = ord value
   in (codePoint >= 9 && codePoint <= 13)
        || case generalCategory value of
          Space -> True
          _ -> False

isUpper :: Char -> Bool
isUpper value =
  case generalCategory value of
    UppercaseLetter -> True
    TitlecaseLetter -> True
    _ -> False

isUpperCase :: Char -> Bool
isUpperCase (C# value) = intHashToBool (isUppercase# value)

isLower :: Char -> Bool
isLower value =
  case generalCategory value of
    LowercaseLetter -> True
    _ -> False

isLowerCase :: Char -> Bool
isLowerCase (C# value) = intHashToBool (isLowercase# value)

isAlpha :: Char -> Bool
isAlpha = isLetter

isDigit :: Char -> Bool
isDigit value =
  let codePoint = ord value
   in codePoint >= 48 && codePoint <= 57

isOctDigit :: Char -> Bool
isOctDigit value =
  let codePoint = ord value
   in codePoint >= 48 && codePoint <= 55

isHexDigit :: Char -> Bool
isHexDigit value =
  let codePoint = ord value
   in isDigit value
        || (codePoint >= 65 && codePoint <= 70)
        || (codePoint >= 97 && codePoint <= 102)

isAlphaNum :: Char -> Bool
isAlphaNum value = isAlpha value || isNumber value

isPunctuation :: Char -> Bool
isPunctuation value =
  case generalCategory value of
    ConnectorPunctuation -> True
    DashPunctuation -> True
    OpenPunctuation -> True
    ClosePunctuation -> True
    InitialQuote -> True
    FinalQuote -> True
    OtherPunctuation -> True
    _ -> False

isSymbol :: Char -> Bool
isSymbol value =
  case generalCategory value of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    _ -> False

toUpper :: Char -> Char
toUpper (C# value) = C# (unicodeToUpper value)

toLower :: Char -> Char
toLower (C# value) = C# (unicodeToLower value)

toTitle :: Char -> Char
toTitle (C# value) = C# (unicodeToTitle value)

intHashToBool :: Int# -> Bool
intHashToBool value =
  case value of
    0# -> False
    _ -> True

-- Data.Char exports these predicates; GHC.Unicode only uses them.
isLetter :: Char -> Bool
isLetter value =
  case generalCategory value of
    UppercaseLetter -> True
    LowercaseLetter -> True
    TitlecaseLetter -> True
    ModifierLetter -> True
    OtherLetter -> True
    _ -> False

isMark :: Char -> Bool
isMark value =
  case generalCategory value of
    NonSpacingMark -> True
    SpacingCombiningMark -> True
    EnclosingMark -> True
    _ -> False

isNumber :: Char -> Bool
isNumber value =
  case generalCategory value of
    DecimalNumber -> True
    LetterNumber -> True
    OtherNumber -> True
    _ -> False

isSeparator :: Char -> Bool
isSeparator value =
  case generalCategory value of
    Space -> True
    LineSeparator -> True
    ParagraphSeparator -> True
    _ -> False
