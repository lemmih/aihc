{-# LANGUAGE MagicHash #-}

module GHC.Read.Lex
  ( Lexeme (..),
    NumberToken (..),
    lex,
    lexDigits,
    lexLitChar,
    readLitChar,
    lexP,
    expectP,
    parseSignedInteger,
    stringEqual,
  )
where

import GHC.Int (Int (..))
import GHC.Internal.Integer (eqInteger#)
import GHC.Prim (chr#, ord#, (<#), (==#))
import Text.ParserCombinators.ReadPrec (ReadPrec, pfail, readS_to_Prec)
import Prelude

newtype NumberToken = NumberToken Integer

data Lexeme
  = Char Char
  | String String
  | Punc String
  | Ident String
  | Symbol String
  | Number NumberToken
  | EOF

lexDigits :: ReadS String
lexDigits input =
  case takeDigits input of
    ([], _) -> []
    (digits, rest) -> [(digits, rest)]

lexLitChar :: ReadS String
lexLitChar input =
  case readLitCharWithSpelling input of
    [] -> []
    (_, spelling, rest) : _ -> [(spelling, rest)]

readLitChar :: ReadS Char
readLitChar input =
  case readLitCharWithSpelling input of
    [] -> []
    (char, _, rest) : _ -> [(char, rest)]

lexP :: ReadPrec Lexeme
lexP =
  readS_to_Prec
    ( \_ input ->
        case lexLexeme input of
          [] -> []
          (token, rest) : _ -> [(token, rest)]
    )

expectP :: Lexeme -> ReadPrec ()
expectP expected = do
  actual <- lexP
  case sameLexeme expected actual of
    True -> return ()
    False -> pfail

sameLexeme :: Lexeme -> Lexeme -> Bool
sameLexeme (Char left) (Char right) = charEqual left right
sameLexeme (String left) (String right) = stringEqual left right
sameLexeme (Punc left) (Punc right) = stringEqual left right
sameLexeme (Ident left) (Ident right) = stringEqual left right
sameLexeme (Symbol left) (Symbol right) = stringEqual left right
sameLexeme (Number (NumberToken left)) (Number (NumberToken right)) = integerEqual left right
sameLexeme EOF EOF = True
sameLexeme _ _ = False

lexLexeme :: String -> [(Lexeme, String)]
lexLexeme input =
  case skipReadSpaces input of
    [] -> [(EOF, [])]
    rest ->
      case lexTokenWithSpelling rest of
        [] -> []
        (_, token, remaining) : _ -> [(token, remaining)]

lexTokenWithSpelling :: String -> [(String, Lexeme, String)]
lexTokenWithSpelling input =
  case input of
    '\'' : rest -> lexCharacterToken rest
    '"' : rest -> lexStringToken rest
    char : rest ->
      case isLexPunctuation char of
        True -> [([char], Punc [char], rest)]
        False ->
          case isIdentifierStart char of
            True ->
              case takeWhileRead isIdentifierContinue rest of
                (suffix, remaining) ->
                  let spelling = char : suffix
                   in [(spelling, Ident spelling, remaining)]
            False ->
              case isDecimalDigit char of
                True -> lexNumberToken input
                False ->
                  case isSymbolCharacter char of
                    True ->
                      case takeWhileRead isSymbolCharacter rest of
                        (suffix, remaining) ->
                          let spelling = char : suffix
                           in [(spelling, classifySymbol spelling, remaining)]
                    False -> []
    [] -> [([], EOF, [])]

lexCharacterToken :: String -> [(String, Lexeme, String)]
lexCharacterToken input =
  case readLitCharWithSpelling input of
    [] -> []
    (char, spelling, '\'' : rest) : _ -> [("'" ++ spelling ++ "'", Char char, rest)]
    _ -> []

lexStringToken :: String -> [(String, Lexeme, String)]
lexStringToken = lexStringCharacters [] []

lexStringCharacters :: String -> String -> String -> [(String, Lexeme, String)]
lexStringCharacters values spelling input =
  case input of
    '"' : rest -> [("\"" ++ reverseRead spelling ++ "\"", String (reverseRead values), rest)]
    '\\' : afterSlash ->
      case afterSlash of
        '&' : rest -> lexStringCharacters values ('&' : '\\' : spelling) rest
        _ -> continueStringCharacters values spelling input
    _ -> continueStringCharacters values spelling input

continueStringCharacters :: String -> String -> String -> [(String, Lexeme, String)]
continueStringCharacters values spelling input =
  case readLitCharWithSpelling input of
    [] -> []
    (char, consumed, rest) : _ ->
      lexStringCharacters (char : values) (reverseAppend consumed spelling) rest

lexNumberToken :: String -> [(String, Lexeme, String)]
lexNumberToken input =
  case input of
    '0' : afterZero -> lexZeroNumber afterZero
    _ ->
      lexBasedNumber 10 isDecimalDigit input []

lexZeroNumber :: String -> [(String, Lexeme, String)]
lexZeroNumber input =
  case input of
    'x' : rest -> lexBasedNumber 16 isHexDigitRead rest "0x"
    'X' : rest -> lexBasedNumber 16 isHexDigitRead rest "0X"
    'o' : rest -> lexBasedNumber 8 isOctalDigit rest "0o"
    'O' : rest -> lexBasedNumber 8 isOctalDigit rest "0O"
    'b' : rest -> lexBasedNumber 2 isBinaryDigit rest "0b"
    'B' : rest -> lexBasedNumber 2 isBinaryDigit rest "0B"
    _ -> lexBasedNumber 10 isDecimalDigit ('0' : input) []

lexBasedNumber :: Integer -> (Char -> Bool) -> String -> String -> [(String, Lexeme, String)]
lexBasedNumber base predicate chars prefix =
  case takeWhileRead predicate chars of
    ([], _) -> []
    (digits, rest) ->
      let spelling = prefix ++ digits
       in [(spelling, Number (NumberToken (digitsToInteger base digits)), rest)]

classifySymbol :: String -> Lexeme
classifySymbol symbol =
  case isReservedSymbol symbol of
    True -> Punc symbol
    False -> Symbol symbol

isReservedSymbol :: String -> Bool
isReservedSymbol symbol =
  stringEqual symbol ".."
    || stringEqual symbol ":"
    || stringEqual symbol "::"
    || stringEqual symbol "="
    || stringEqual symbol "\\"
    || stringEqual symbol "|"
    || stringEqual symbol "<-"
    || stringEqual symbol "->"
    || stringEqual symbol "@"
    || stringEqual symbol "~"
    || stringEqual symbol "=>"

skipReadSpaces :: String -> String
skipReadSpaces [] = []
skipReadSpaces (char : rest) =
  case isReadSpace char of
    True -> skipReadSpaces rest
    False -> char : rest

isReadSpace :: Char -> Bool
isReadSpace ' ' = True
isReadSpace '\t' = True
isReadSpace '\n' = True
isReadSpace '\r' = True
isReadSpace '\f' = True
isReadSpace '\v' = True
isReadSpace _ = False

isLexPunctuation :: Char -> Bool
isLexPunctuation ',' = True
isLexPunctuation ';' = True
isLexPunctuation '(' = True
isLexPunctuation ')' = True
isLexPunctuation '[' = True
isLexPunctuation ']' = True
isLexPunctuation '{' = True
isLexPunctuation '}' = True
isLexPunctuation '`' = True
isLexPunctuation _ = False

isIdentifierStart :: Char -> Bool
isIdentifierStart char = isAsciiLetter char || charEqual char '_'

isIdentifierContinue :: Char -> Bool
isIdentifierContinue char = isIdentifierStart char || isDecimalDigit char || charEqual char '\''

isAsciiLetter :: Char -> Bool
isAsciiLetter char = charBetween 'a' 'z' char || charBetween 'A' 'Z' char

isDecimalDigit :: Char -> Bool
isDecimalDigit = charBetween '0' '9'

isOctalDigit :: Char -> Bool
isOctalDigit = charBetween '0' '7'

isBinaryDigit :: Char -> Bool
isBinaryDigit char = charEqual char '0' || charEqual char '1'

isHexDigitRead :: Char -> Bool
isHexDigitRead char =
  isDecimalDigit char
    || charBetween 'a' 'f' char
    || charBetween 'A' 'F' char

isSymbolCharacter :: Char -> Bool
isSymbolCharacter '!' = True
isSymbolCharacter '#' = True
isSymbolCharacter '$' = True
isSymbolCharacter '%' = True
isSymbolCharacter '&' = True
isSymbolCharacter '*' = True
isSymbolCharacter '+' = True
isSymbolCharacter '.' = True
isSymbolCharacter '/' = True
isSymbolCharacter '<' = True
isSymbolCharacter '=' = True
isSymbolCharacter '>' = True
isSymbolCharacter '?' = True
isSymbolCharacter '@' = True
isSymbolCharacter '\\' = True
isSymbolCharacter '^' = True
isSymbolCharacter '|' = True
isSymbolCharacter '-' = True
isSymbolCharacter '~' = True
isSymbolCharacter ':' = True
isSymbolCharacter _ = False

takeWhileRead :: (Char -> Bool) -> String -> (String, String)
takeWhileRead _ [] = ([], [])
takeWhileRead predicate input@(char : rest) =
  case predicate char of
    False -> ([], input)
    True ->
      case takeWhileRead predicate rest of
        (matched, remaining) -> (char : matched, remaining)

takeDigits :: String -> (String, String)
takeDigits = takeWhileRead isDecimalDigit

digitsToInteger :: Integer -> String -> Integer
digitsToInteger base = go 0
  where
    go value [] = value
    go value (digit : digits) = go (value * base + fromIntegral (digitValue digit)) digits

digitValue :: Char -> Int
digitValue char =
  case charBetween '0' '9' char of
    True -> charCode char - charCode '0'
    False ->
      case charBetween 'a' 'f' char of
        True -> charCode char - charCode 'a' + 10
        False -> charCode char - charCode 'A' + 10

charEqual :: Char -> Char -> Bool
charEqual (C# left) (C# right) =
  case (==#) (ord# left) (ord# right) of
    0# -> False
    _ -> True

stringEqual :: String -> String -> Bool
stringEqual [] [] = True
stringEqual [] (_ : _) = False
stringEqual (_ : _) [] = False
stringEqual (left : lefts) (right : rights) =
  case charEqual left right of
    True -> stringEqual lefts rights
    False -> False

integerEqual :: Integer -> Integer -> Bool
integerEqual left right =
  case eqInteger# left right of
    0# -> False
    _ -> True

charBetween :: Char -> Char -> Char -> Bool
charBetween (C# lower) (C# upper) (C# value) =
  case (<#) (ord# value) (ord# lower) of
    1# -> False
    _ ->
      case (<#) (ord# upper) (ord# value) of
        1# -> False
        _ -> True

charCode :: Char -> Int
charCode (C# char) = I# (ord# char)

readLitCharWithSpelling :: String -> [(Char, String, String)]
readLitCharWithSpelling [] = []
readLitCharWithSpelling ('\\' : rest) = readEscape rest
readLitCharWithSpelling (char : rest) = [(char, [char], rest)]

readEscape :: String -> [(Char, String, String)]
readEscape [] = []
readEscape ('a' : rest) = [('\a', "\\a", rest)]
readEscape ('b' : rest) = [('\b', "\\b", rest)]
readEscape ('f' : rest) = [('\f', "\\f", rest)]
readEscape ('n' : rest) = [('\n', "\\n", rest)]
readEscape ('r' : rest) = [('\r', "\\r", rest)]
readEscape ('t' : rest) = [('\t', "\\t", rest)]
readEscape ('v' : rest) = [('\v', "\\v", rest)]
readEscape ('\\' : rest) = [('\\', "\\\\", rest)]
readEscape ('\'' : rest) = [('\'', "\\'", rest)]
readEscape ('"' : rest) = [('"', "\\\"", rest)]
readEscape input@(char : _) =
  case isDecimalDigit char of
    True ->
      case takeDigits input of
        (digits, rest) -> [(characterFromDigits 10 digits, '\\' : digits, rest)]
    False -> []
readEscape _ = []

characterFromDigits :: Int -> String -> Char
characterFromDigits base digits =
  case digitsToInt base digits of
    I# value -> C# (chr# value)

digitsToInt :: Int -> String -> Int
digitsToInt base = go 0
  where
    go value [] = value
    go value (digit : digits) = go (value * base + digitValue digit) digits

reverseRead :: [a] -> [a]
reverseRead values = reverseAppend values []

reverseAppend :: [a] -> [a] -> [a]
reverseAppend [] suffix = suffix
reverseAppend (value : values) suffix = reverseAppend values (value : suffix)

parseSignedInteger :: ReadS Integer
parseSignedInteger input =
  case lexLexeme input of
    (Symbol sign, afterSign) : _ ->
      case stringEqual sign "-" of
        True -> negateIntegerResults (parseUnsignedInteger afterSign)
        False -> []
    (Punc open, afterOpen) : _ ->
      case stringEqual open "(" of
        True -> closeIntegerResults (parseSignedInteger afterOpen)
        False -> []
    _ -> parseUnsignedInteger input

parseUnsignedInteger :: ReadS Integer
parseUnsignedInteger input =
  case lexLexeme input of
    (Number (NumberToken value), rest) : _ -> [(value, rest)]
    _ -> []

negateIntegerResults :: [(Integer, String)] -> [(Integer, String)]
negateIntegerResults [] = []
negateIntegerResults ((value, rest) : results) =
  (negate value, rest) : negateIntegerResults results

closeIntegerResults :: [(Integer, String)] -> [(Integer, String)]
closeIntegerResults [] = []
closeIntegerResults ((value, input) : results) =
  case lexLexeme input of
    (Punc close, rest) : _ ->
      case stringEqual close ")" of
        True -> (value, rest) : closeIntegerResults results
        False -> closeIntegerResults results
    _ -> closeIntegerResults results
