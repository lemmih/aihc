{-# LANGUAGE MagicHash #-}

-- The suggested replacements name values of the base library, which this
-- module must not use.
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Use foldr" -}
{- HLINT ignore "Use foldl" -}

-- | The reader that a derived @Read@ instance is built from.
--
-- A derived instance is ordinary source code, so its body names library
-- values. The type checker must not name the base library for this, because
-- a package can depend on the primitive package alone. Every value that
-- generated @Read@ code refers to therefore lives here: the precedence
-- parser, the lexer, and the four combinators that read a constructor.
--
-- The @Read@ class itself stays in @Prelude@. A derived instance finds its
-- methods through the class, so this module holds no class of its own.
--
-- The code below uses primitive comparisons instead of @Eq@ and @Ord@,
-- because the instances for @Char@ and @Int@ belong to the base library.
module GHC.Prim.Read
  ( -- * Precedence parser
    ReadS,
    Prec,
    ReadPrec,
    minPrec,
    readPrec_to_S,
    readS_to_Prec,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,

    -- * Lexemes
    Lexeme (..),
    NumberToken (..),
    lexP,
    expectP,
    lexDigits,
    lexLitChar,
    readLitChar,
    parseSignedInteger,
    stringEqual,

    -- * Readers of a derived instance
    paren,
    parens,
    list,
    readField,
    readSymField,
  )
where

import GHC.Prim (chr#, ord#, (+#), (<#), (==#))
import GHC.Prim.Base (Applicative (..), Functor (..), Monad (..), String)
import GHC.Prim.Integer (Integer (..), eqInteger#)
import GHC.Prim.Num (Num (..))
import GHC.Types (Bool (..), Char (..), Int (..))

-- | The synonym that @Prelude.ReadS@ and the reader of a derived @Read@
-- instance share.
type ReadS a = String -> [(a, String)]

type Prec = Int

minPrec :: Prec
minPrec = 0

newtype ReadPrec a = ReadPrec (Prec -> ReadS a)

instance Functor ReadPrec where
  fmap f (ReadPrec parser) =
    ReadPrec (\precedence input -> mapReadResults f (parser precedence input))

instance Applicative ReadPrec where
  pure value = ReadPrec (\_ input -> [(value, input)])

  ReadPrec functionParser <*> ReadPrec valueParser =
    ReadPrec
      ( \precedence input ->
          applyReadResults precedence valueParser (functionParser precedence input)
      )

instance Monad ReadPrec where
  ReadPrec parser >>= next =
    ReadPrec
      ( \precedence input ->
          bindReadResults precedence next (parser precedence input)
      )

  ReadPrec first >> ReadPrec second =
    ReadPrec
      ( \precedence input ->
          thenReadResults precedence second (first precedence input)
      )

  return = pure

mapReadResults :: (a -> b) -> [(a, String)] -> [(b, String)]
mapReadResults _ [] = []
mapReadResults f ((value, rest) : results) = (f value, rest) : mapReadResults f results

applyReadResults :: Prec -> (Prec -> ReadS a) -> [(a -> b, String)] -> [(b, String)]
applyReadResults _ _ [] = []
applyReadResults precedence parser ((f, rest) : results) =
  appendList (mapReadResults f (parser precedence rest)) (applyReadResults precedence parser results)

bindReadResults :: Prec -> (a -> ReadPrec b) -> [(a, String)] -> [(b, String)]
bindReadResults _ _ [] = []
bindReadResults precedence next ((value, rest) : results) =
  appendList (runReadPrec (next value) precedence rest) (bindReadResults precedence next results)

thenReadResults :: Prec -> (Prec -> ReadS b) -> [(a, String)] -> [(b, String)]
thenReadResults _ _ [] = []
thenReadResults precedence parser ((_, rest) : results) =
  appendList (parser precedence rest) (thenReadResults precedence parser results)

runReadPrec :: ReadPrec a -> Prec -> ReadS a
runReadPrec (ReadPrec parser) = parser

readPrec_to_S :: ReadPrec a -> Prec -> ReadS a
readPrec_to_S = runReadPrec

readS_to_Prec :: (Prec -> ReadS a) -> ReadPrec a
readS_to_Prec = ReadPrec

prec :: Prec -> ReadPrec a -> ReadPrec a
prec required parser =
  ReadPrec
    ( \context input ->
        case intAtMost context required of
          True -> runReadPrec parser required input
          False -> []
    )

step :: ReadPrec a -> ReadPrec a
step parser = ReadPrec (\context -> runReadPrec parser (intSucc context))

reset :: ReadPrec a -> ReadPrec a
reset parser = ReadPrec (\_ -> runReadPrec parser minPrec)

get :: ReadPrec Char
get =
  ReadPrec
    ( \_ input ->
        case input of
          [] -> []
          char : rest -> [(char, rest)]
    )

look :: ReadPrec String
look = ReadPrec (\_ input -> [(input, input)])

(+++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
ReadPrec left +++ ReadPrec right =
  ReadPrec (\precedence input -> appendList (left precedence input) (right precedence input))

(<++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
ReadPrec left <++ ReadPrec right =
  ReadPrec
    ( \precedence input ->
        case left precedence input of
          [] -> right precedence input
          results -> results
    )

pfail :: ReadPrec a
pfail = ReadPrec (\_ _ -> [])

choice :: [ReadPrec a] -> ReadPrec a
choice [] = pfail
choice (parser : parsers) = parser +++ choice parsers

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
    (char, spelling, '\'' : rest) : _ -> [(appendList "'" (appendList spelling "'"), Char char, rest)]
    _ -> []

lexStringToken :: String -> [(String, Lexeme, String)]
lexStringToken = lexStringCharacters [] []

lexStringCharacters :: String -> String -> String -> [(String, Lexeme, String)]
lexStringCharacters values spelling input =
  case input of
    '"' : rest -> [(appendList "\"" (appendList (reverseRead spelling) "\""), String (reverseRead values), rest)]
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
      let spelling = appendList prefix digits
       in [(spelling, Number (NumberToken (digitsToInteger base digits)), rest)]

classifySymbol :: String -> Lexeme
classifySymbol symbol =
  case isReservedSymbol symbol of
    True -> Punc symbol
    False -> Symbol symbol

isReservedSymbol :: String -> Bool
isReservedSymbol symbol =
  anyString
    symbol
    ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

-- | Whether the string equals one of the listed strings.
anyString :: String -> [String] -> Bool
anyString _ [] = False
anyString value (candidate : candidates) =
  case stringEqual value candidate of
    True -> True
    False -> anyString value candidates

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
isIdentifierStart char = orBool (isAsciiLetter char) (charEqual char '_')

isIdentifierContinue :: Char -> Bool
isIdentifierContinue char = orBool (isIdentifierStart char) (orBool (isDecimalDigit char) (charEqual char '\''))

isAsciiLetter :: Char -> Bool
isAsciiLetter char = orBool (charBetween 'a' 'z' char) (charBetween 'A' 'Z' char)

isDecimalDigit :: Char -> Bool
isDecimalDigit = charBetween '0' '9'

isOctalDigit :: Char -> Bool
isOctalDigit = charBetween '0' '7'

isBinaryDigit :: Char -> Bool
isBinaryDigit char = orBool (charEqual char '0') (charEqual char '1')

isHexDigitRead :: Char -> Bool
isHexDigitRead char =
  orBool
    (isDecimalDigit char)
    (orBool (charBetween 'a' 'f' char) (charBetween 'A' 'F' char))

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
    go value (digit : digits) = go (value * base + intToInteger (digitValue digit)) digits

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

-- * Readers of a derived instance

-- | Read a value between parentheses.
paren :: ReadPrec a -> ReadPrec a
paren parser = do
  expectP (Punc "(")
  value <- reset parser
  expectP (Punc ")")
  return value

-- | Read a value with any number of parentheses around it.
parens :: ReadPrec a -> ReadPrec a
parens parser = parser +++ paren (parens parser)

-- | Read a list of values.
list :: ReadPrec a -> ReadPrec [a]
list parser =
  parens
    ( do
        expectP (Punc "[")
        emptyList +++ nonEmptyList
    )
  where
    emptyList = do
      expectP (Punc "]")
      return []
    nonEmptyList = do
      value <- reset parser
      values <- listTail
      return (value : values)
    listTail =
      ( do
          expectP (Punc "]")
          return []
      )
        +++ do
          expectP (Punc ",")
          nonEmptyList

-- | Read a record field that has an ordinary label.
readField :: String -> ReadPrec a -> ReadPrec a
readField name parser = do
  expectP (Ident name)
  expectP (Punc "=")
  parser

-- | Read a record field that has a symbolic label.
readSymField :: String -> ReadPrec a -> ReadPrec a
readSymField name parser = do
  expectP (Punc "(")
  expectP (Symbol name)
  expectP (Punc ")")
  expectP (Punc "=")
  parser

-- * Primitive helpers

-- | Whether the left value is not greater than the right one.
intAtMost :: Int -> Int -> Bool
intAtMost (I# left) (I# right) =
  case (<#) right left of
    1# -> False
    _ -> True

intSucc :: Int -> Int
intSucc (I# value) = I# (value +# 1#)

intToInteger :: Int -> Integer
intToInteger (I# value) = IS value

-- | Disjunction. @(||)@ belongs to the base library.
orBool :: Bool -> Bool -> Bool
orBool True _ = True
orBool False value = value

appendList :: [a] -> [a] -> [a]
appendList [] suffix = suffix
appendList (value : values) suffix = value : appendList values suffix
