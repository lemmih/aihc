{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer
  ( LexToken (..),
    LexTokenKind (..),
    lexTokens,
  )
where

import Data.Char (isAlphaNum, isHexDigit, isOctDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric (readHex, readOct)
import Parser.Ast
import Text.Megaparsec
  ( Parsec,
    anySingle,
    eof,
    getSourcePos,
    many,
    runParser,
    satisfy,
    some,
    try,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (unPos)

data LexTokenKind
  = TkKeywordModule
  | TkKeywordWhere
  | TkKeywordData
  | TkKeywordImport
  | TkKeywordQualified
  | TkKeywordAs
  | TkKeywordHiding
  | TkKeywordCase
  | TkKeywordOf
  | TkKeywordIf
  | TkKeywordThen
  | TkKeywordElse
  | TkIdentifier Text
  | TkOperator Text
  | TkInteger Integer
  | TkIntegerBase Integer Text
  | TkFloat Double
  | TkChar Char
  | TkString Text
  | TkSymbol Text
  | TkQuasiQuote Text Text
  deriving (Eq, Ord, Show)

data LexToken = LexToken
  { lexTokenKind :: !LexTokenKind,
    lexTokenText :: !Text,
    lexTokenSpan :: !SourceSpan
  }
  deriving (Eq, Ord, Show)

type LParser = Parsec Void Text

lexTokens :: Text -> [LexToken]
lexTokens input =
  case runParser (spaceConsumer *> many (lexTokenParser <* spaceConsumer) <* eof) "<lexer>" input of
    Right toks -> toks
    Left _ -> []

spaceConsumer :: LParser ()
spaceConsumer = L.space C.space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexTokenParser :: LParser LexToken
lexTokenParser =
  lexWithSpan $
    try quasiQuoteToken
      <|> try floatToken
      <|> try intBaseToken
      <|> try intToken
      <|> try charToken
      <|> try stringToken
      <|> try symbolToken
      <|> try identifierToken
      <|> operatorToken

lexWithSpan :: LParser (Text, LexTokenKind) -> LParser LexToken
lexWithSpan parser = do
  start <- getSourcePos
  (tokTxt, kind) <- parser
  end <- getSourcePos
  pure
    LexToken
      { lexTokenKind = kind,
        lexTokenText = tokTxt,
        lexTokenSpan = mkSpan start end
      }

mkSpan :: MP.SourcePos -> MP.SourcePos -> SourceSpan
mkSpan start end =
  SourceSpan
    { sourceSpanStartLine = unPos (MP.sourceLine start),
      sourceSpanStartCol = unPos (MP.sourceColumn start),
      sourceSpanEndLine = unPos (MP.sourceLine end),
      sourceSpanEndCol = unPos (MP.sourceColumn end)
    }

identifierToken :: LParser (Text, LexTokenKind)
identifierToken = do
  first <- C.letterChar <|> C.char '_'
  rest <- many identTailChar
  more <- many (C.char '.' *> ((:) <$> C.letterChar <*> many identTailChar))
  let base = first : rest
      chunks = base : more
      ident = T.intercalate "." (map T.pack chunks)
      kind = fromMaybe (TkIdentifier ident) (keywordTokenKind ident)
  pure (ident, kind)

identTailChar :: LParser Char
identTailChar =
  C.alphaNumChar
    <|> C.char '_'
    <|> C.char '\''

operatorToken :: LParser (Text, LexTokenKind)
operatorToken = do
  op <- some (satisfy isSymbolicOpChar)
  let txt = T.pack op
  pure (txt, TkOperator txt)

symbolToken :: LParser (Text, LexTokenKind)
symbolToken =
  choice2
    [ ("..", TkSymbol ".."),
      ("(", TkSymbol "("),
      (")", TkSymbol ")"),
      ("[", TkSymbol "["),
      ("]", TkSymbol "]"),
      ("{", TkSymbol "{"),
      ("}", TkSymbol "}"),
      (",", TkSymbol ","),
      (";", TkSymbol ";")
    ]
  where
    choice2 xs =
      foldr1 (<|>) [try (C.string t >> pure (t, k)) | (t, k) <- xs]

intBaseToken :: LParser (Text, LexTokenKind)
intBaseToken = do
  _ <- C.char '0'
  base <- C.char 'x' <|> C.char 'X' <|> C.char 'o' <|> C.char 'O'
  digits <-
    if base `elem` ['x', 'X']
      then some (satisfy isHexDigit)
      else some (satisfy isOctDigit)
  let txt = T.pack ('0' : base : digits)
      n =
        if base `elem` ['x', 'X']
          then readHexLiteral txt
          else readOctLiteral txt
  pure (txt, TkIntegerBase n txt)

intToken :: LParser (Text, LexTokenKind)
intToken = do
  digits <- some C.digitChar
  let txt = T.pack digits
  pure (txt, TkInteger (read digits))

floatToken :: LParser (Text, LexTokenKind)
floatToken = do
  lhs <- some C.digitChar
  repr <-
    try
      ( do
          _ <- C.char '.'
          rhs <- some C.digitChar
          expo <- MP.optional exponentPart
          pure (lhs <> "." <> rhs <> fromMaybe "" expo)
      )
      <|> do
        expo <- exponentPart
        pure (lhs <> expo)
  let txt = T.pack repr
  pure (txt, TkFloat (read repr))

exponentPart :: LParser String
exponentPart = do
  marker <- C.char 'e' <|> C.char 'E'
  sign <- MP.optional (C.char '+' <|> C.char '-')
  ds <- some C.digitChar
  pure (marker : maybe [] pure sign <> ds)

charToken :: LParser (Text, LexTokenKind)
charToken = do
  _ <- C.char '\''
  body <- manyTillChar '\''
  let raw = "'" <> body <> "'"
  case readMaybeChar raw of
    Just c -> pure (T.pack raw, TkChar c)
    Nothing -> fail "char literal"

stringToken :: LParser (Text, LexTokenKind)
stringToken = do
  _ <- C.char '"'
  body <- manyTillChar '"'
  let raw = "\"" <> body <> "\""
      decoded =
        case reads raw of
          [(str, "")] -> T.pack str
          _ -> T.pack body
  pure (T.pack raw, TkString decoded)

quasiQuoteToken :: LParser (Text, LexTokenKind)
quasiQuoteToken = do
  _ <- C.char '['
  quoter <- takeQuoter
  _ <- C.char '|'
  body <- manyTillText "|]"
  _ <- C.string "|]"
  let raw = "[" <> quoter <> "|" <> body <> "|]"
      q = T.pack quoter
      b = T.pack body
  pure (T.pack raw, TkQuasiQuote q b)

takeQuoter :: LParser String
takeQuoter = do
  first <- C.letterChar <|> C.char '_'
  rest <- many (satisfy isIdentTailOrStart)
  more <- many (C.char '.' *> ((:) <$> C.letterChar <*> many (satisfy isIdentTailOrStart)))
  let base = first : rest
  pure (concat (base : map ('.' :) more))

manyTillChar :: Char -> LParser String
manyTillChar endCh = go []
  where
    go acc =
      (C.char endCh >> pure (reverse acc))
        <|> do
          ch <- anySingle
          go (ch : acc)

manyTillText :: Text -> LParser String
manyTillText end = go []
  where
    go acc =
      (try (C.string end) >> pure (reverse acc))
        <|> do
          ch <- anySingle
          go (ch : acc)

readMaybeChar :: String -> Maybe Char
readMaybeChar raw =
  case reads raw of
    [(c, "")] -> Just c
    _ -> Nothing

readHexLiteral :: Text -> Integer
readHexLiteral txt =
  case readHex (T.unpack (T.drop 2 txt)) of
    [(n, "")] -> n
    _ -> 0

readOctLiteral :: Text -> Integer
readOctLiteral txt =
  case readOct (T.unpack (T.drop 2 txt)) of
    [(n, "")] -> n
    _ -> 0

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c = c `elem` (":!#$%&*+./<=>?\\^|-~" :: String)

isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart c = isAlphaNum c || c == '_' || c == '\''

keywordTokenKind :: Text -> Maybe LexTokenKind
keywordTokenKind txt = case txt of
  "module" -> Just TkKeywordModule
  "where" -> Just TkKeywordWhere
  "data" -> Just TkKeywordData
  "import" -> Just TkKeywordImport
  "qualified" -> Just TkKeywordQualified
  "as" -> Just TkKeywordAs
  "hiding" -> Just TkKeywordHiding
  "case" -> Just TkKeywordCase
  "of" -> Just TkKeywordOf
  "if" -> Just TkKeywordIf
  "then" -> Just TkKeywordThen
  "else" -> Just TkKeywordElse
  _ -> Nothing
