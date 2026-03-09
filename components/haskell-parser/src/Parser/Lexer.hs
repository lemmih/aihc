{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer
  ( LexToken (..),
    LexTokenKind (..),
    lexTokens,
    parseImportDeclTokens,
    parseModuleHeaderTokens,
  )
where

import Data.Char (isAlphaNum, isHexDigit, isOctDigit, isUpper)
import Data.Maybe (isJust)
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
  = TkKeyword Text
  | TkIdentifier Text
  | TkOperator Text
  | TkInteger Integer
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

type TokParser = Parsec Void [LexToken]

lexTokens :: Text -> Either Text [LexToken]
lexTokens input =
  case runParser (many (spaceConsumer *> lexTokenParser) <* spaceConsumer <* eof) "<lexer>" input of
    Right toks -> Right toks
    Left _ -> Left "token stream"

parseModuleHeaderTokens :: Text -> Either Text (Text, Maybe [ExportSpec])
parseModuleHeaderTokens input = do
  toks <- lexTokens input
  case runParser (moduleHeaderTokParser <* eof) "<module-header>" toks of
    Right header -> Right header
    Left _ -> Left "module header"

parseImportDeclTokens :: Text -> Either Text ImportDecl
parseImportDeclTokens input = do
  toks <- lexTokens input
  case runParser (importDeclTokParser <* eof) "<import-decl>" toks of
    Right decl -> Right decl
    Left _ -> Left "import declaration"

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
      kind =
        if ident `elem` reservedKeywords
          then TkKeyword ident
          else TkIdentifier ident
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
  pure (txt, TkInteger n)

intToken :: LParser (Text, LexTokenKind)
intToken = do
  digits <- some C.digitChar
  let txt = T.pack digits
  pure (txt, TkInteger (read digits))

floatToken :: LParser (Text, LexTokenKind)
floatToken = do
  lhs <- some C.digitChar
  _ <- C.char '.'
  rhs <- some C.digitChar
  let txt = T.pack (lhs <> "." <> rhs)
  pure (txt, TkFloat (read (lhs <> "." <> rhs)))

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

moduleHeaderTokParser :: TokParser (Text, Maybe [ExportSpec])
moduleHeaderTokParser = do
  _ <- keywordTok "module"
  modName <- identifierTok
  exports <- MP.optional (try exportSpecListTokParser)
  _ <- keywordTok "where"
  pure (modName, exports)

importDeclTokParser :: TokParser ImportDecl
importDeclTokParser = do
  _ <- keywordTok "import"
  qualifiedFlag <- isJust <$> MP.optional (try (keywordTok "qualified"))
  modName <- identifierTok
  alias <- MP.optional (try (keywordTok "as" *> identifierTok))
  spec <- MP.optional (try importSpecTokParser)
  pure
    ImportDecl
      { importDeclSpan = noSourceSpan,
        importDeclQualified = qualifiedFlag,
        importDeclModule = modName,
        importDeclAs = alias,
        importDeclSpec = spec
      }

exportSpecListTokParser :: TokParser [ExportSpec]
exportSpecListTokParser = do
  _ <- symbolTok "("
  specs <- exportSpecTokParser `sepEndByTok` symbolTok ","
  _ <- symbolTok ")"
  pure specs

exportSpecTokParser :: TokParser ExportSpec
exportSpecTokParser =
  try moduleSpecTokParser <|> entitySpecTokParser
  where
    moduleSpecTokParser = do
      _ <- keywordTok "module"
      ExportModule noSourceSpan <$> identifierTok

    entitySpecTokParser = do
      name <- identifierOrOperatorTok
      members <- MP.optional (try exportMembersTokParser)
      pure $
        case members of
          Nothing
            | isTypeToken name -> ExportAbs noSourceSpan name
            | otherwise -> ExportVar noSourceSpan name
          Just Nothing -> ExportAll noSourceSpan name
          Just (Just xs) -> ExportWith noSourceSpan name xs

exportMembersTokParser :: TokParser (Maybe [Text])
exportMembersTokParser = do
  _ <- symbolTok "("
  allMembers <- MP.optional (try (symbolTok ".."))
  case allMembers of
    Just _ -> do
      _ <- symbolTok ")"
      pure Nothing
    Nothing -> do
      members <- identifierOrOperatorTok `sepEndByTok` symbolTok ","
      _ <- symbolTok ")"
      pure (Just members)

importSpecTokParser :: TokParser ImportSpec
importSpecTokParser = do
  hidingFlag <- isJust <$> MP.optional (try (keywordTok "hiding"))
  _ <- symbolTok "("
  items <- importItemTokParser `sepEndByTok` symbolTok ","
  _ <- symbolTok ")"
  pure
    ImportSpec
      { importSpecSpan = noSourceSpan,
        importSpecHiding = hidingFlag,
        importSpecItems = items
      }

importItemTokParser :: TokParser ImportItem
importItemTokParser = do
  name <- identifierOrOperatorTok
  members <- MP.optional (try exportMembersTokParser)
  pure $
    case members of
      Nothing
        | isTypeToken name -> ImportItemAbs noSourceSpan name
        | otherwise -> ImportItemVar noSourceSpan name
      Just Nothing -> ImportItemAll noSourceSpan name
      Just (Just xs) -> ImportItemWith noSourceSpan name xs

identifierOrOperatorTok :: TokParser Text
identifierOrOperatorTok =
  identifierTok
    <|> do
      _ <- symbolTok "("
      op <- operatorTokP
      _ <- symbolTok ")"
      pure op

identifierTok :: TokParser Text
identifierTok = tokenSatisfy $ \tok ->
  case lexTokenKind tok of
    TkIdentifier txt -> Just txt
    TkKeyword txt
      | txt `elem` ["as", "qualified", "hiding"] -> Just txt
    _ -> Nothing

keywordTok :: Text -> TokParser ()
keywordTok expected =
  tokenSatisfy_ $ \tok ->
    case lexTokenKind tok of
      TkKeyword txt -> txt == expected
      _ -> False

symbolTok :: Text -> TokParser ()
symbolTok expected =
  tokenSatisfy_ $ \tok ->
    case lexTokenKind tok of
      TkSymbol txt -> txt == expected
      _ -> False

operatorTokP :: TokParser Text
operatorTokP = tokenSatisfy $ \tok ->
  case lexTokenKind tok of
    TkOperator txt -> Just txt
    _ -> Nothing

tokenSatisfy :: (LexToken -> Maybe a) -> TokParser a
tokenSatisfy f = do
  tok <- anySingle
  case f tok of
    Just out -> pure out
    Nothing -> fail "token"

tokenSatisfy_ :: (LexToken -> Bool) -> TokParser ()
tokenSatisfy_ f = tokenSatisfy (\tok -> if f tok then Just () else Nothing)

sepEndByTok :: TokParser a -> TokParser sep -> TokParser [a]
sepEndByTok p sep = do
  first <- MP.optional p
  case first of
    Nothing -> pure []
    Just x -> go [x]
  where
    go acc = do
      hasSep <- isJust <$> MP.optional (try sep)
      if not hasSep
        then pure (reverse acc)
        else do
          mx <- MP.optional p
          case mx of
            Nothing -> pure (reverse acc)
            Just x -> go (x : acc)

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

isTypeToken :: Text -> Bool
isTypeToken token =
  case T.uncons (stripParens token) of
    Just (c, _) -> isUpper c
    Nothing -> False

stripParens :: Text -> Text
stripParens t =
  let trimmed = T.strip t
   in if T.length trimmed >= 2 && T.head trimmed == '(' && T.last trimmed == ')'
        then T.strip (T.init (T.tail trimmed))
        else trimmed

reservedKeywords :: [Text]
reservedKeywords =
  [ "module",
    "where",
    "import",
    "qualified",
    "as",
    "hiding"
  ]
