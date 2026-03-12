{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer
  ( LexToken (..),
    LexTokenKind (..),
    lexTokens,
    lexModuleTokens,
  )
where

import Control.Monad (void)
import Data.Char (digitToInt, isAlphaNum, isDigit, isHexDigit, isOctDigit)
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric (readHex, readInt, readOct)
import Parser.Ast
import Text.Megaparsec
  ( Parsec,
    anySingle,
    eof,
    getSourcePos,
    many,
    notFollowedBy,
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
  | TkKeywordDo
  | TkKeywordData
  | TkKeywordImport
  | TkKeywordQualified
  | TkKeywordAs
  | TkKeywordHiding
  | TkKeywordCase
  | TkKeywordOf
  | TkKeywordLet
  | TkKeywordIn
  | TkKeywordIf
  | TkKeywordThen
  | TkKeywordElse
  | TkPragmaLanguage [Text]
  | TkIdentifier Text
  | TkOperator Text
  | TkInteger Integer
  | TkIntegerBase Integer Text
  | TkFloat Double Text
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
  case runParser (triviaConsumer *> many (lexTokenParser <* triviaConsumer) <* eof) "<lexer>" input of
    Right toks -> applyLayoutTokens False toks
    Left _ -> []

lexModuleTokens :: Text -> [LexToken]
lexModuleTokens input =
  case runParser (triviaConsumer *> many (lexTokenParser <* triviaConsumer) <* eof) "<lexer>" input of
    Right toks -> applyLayoutTokens True toks
    Left _ -> []

data LayoutContext
  = LayoutExplicit
  | LayoutImplicit !Int
  deriving (Eq, Show)

data LayoutState = LayoutState
  { layoutContexts :: [LayoutContext],
    layoutPendingLayout :: !Bool,
    layoutPrevLine :: !(Maybe Int)
  }
  deriving (Eq, Show)

applyLayoutTokens :: Bool -> [LexToken] -> [LexToken]
applyLayoutTokens enableModuleLayout toks =
  go initialState 0 toks
    <> closeAllImplicit (layoutContexts finalState) eofAnchor
  where
    initialState = LayoutState [] False Nothing
    pendingOpenIndices =
      if enableModuleLayout
        then moduleLayoutOpenIndices toks
        else IntSet.empty
    finalState = foldl stepState initialState (zip [0 ..] toks)
    eofAnchor = eofAnchorSpan toks

    go _ _ [] = []
    go st idx (tok : rest) =
      let stWithPending =
            if IntSet.member idx pendingOpenIndices
              then st {layoutPendingLayout = True}
              else st
          (pendingInserted, stAfterPending, skipBOL) = openPendingLayout stWithPending tok
          (bolInserted, stAfterBOL) = if skipBOL then ([], stAfterPending) else bolLayout stAfterPending tok
          stAfterToken = stepTokenContext stAfterBOL tok
          stNext = stAfterToken {layoutPrevLine = Just (tokenStartLine tok)}
       in pendingInserted <> bolInserted <> (tok : go stNext (idx + 1) rest)

    stepState st (idx, tok) =
      let stWithPending =
            if IntSet.member idx pendingOpenIndices
              then st {layoutPendingLayout = True}
              else st
          (_, stAfterPending, skipBOL) = openPendingLayout stWithPending tok
          (_, stAfterBOL) = if skipBOL then ([], stAfterPending) else bolLayout stAfterPending tok
          stAfterToken = stepTokenContext stAfterBOL tok
       in stAfterToken {layoutPrevLine = Just (tokenStartLine tok)}

openPendingLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openPendingLayout st tok
  | not (layoutPendingLayout st) = ([], st, False)
  | otherwise =
      case lexTokenKind tok of
        TkSymbol "{" -> ([], st {layoutPendingLayout = False}, False)
        _ ->
          let col = tokenStartCol tok
              parentIndent = currentLayoutIndent (layoutContexts st)
              openTok = virtualSymbolToken "{" (lexTokenSpan tok)
              closeTok = virtualSymbolToken "}" (lexTokenSpan tok)
           in if col <= parentIndent
                then ([openTok, closeTok], st {layoutPendingLayout = False}, False)
                else
                  ( [openTok],
                    st
                      { layoutPendingLayout = False,
                        layoutContexts = LayoutImplicit col : layoutContexts st
                      },
                    True
                  )

bolLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState)
bolLayout st tok
  | not (isBOL st tok) = ([], st)
  | otherwise =
      let col = tokenStartCol tok
          (inserted, contexts') = closeForDedent col (lexTokenSpan tok) (layoutContexts st)
          eqSemi =
            case contexts' of
              LayoutImplicit indent : _
                | col == indent,
                  not (suppressesVirtualSemicolon tok) ->
                    [virtualSymbolToken ";" (lexTokenSpan tok)]
              _ -> []
       in (inserted <> eqSemi, st {layoutContexts = contexts'})

suppressesVirtualSemicolon :: LexToken -> Bool
suppressesVirtualSemicolon tok =
  case lexTokenKind tok of
    TkKeywordThen -> True
    TkKeywordElse -> True
    _ -> False

closeForDedent :: Int -> SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeForDedent col anchor = go []
  where
    go acc contexts =
      case contexts of
        LayoutImplicit indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        _ -> (reverse acc, contexts)

closeAllImplicit :: [LayoutContext] -> SourceSpan -> [LexToken]
closeAllImplicit contexts anchor =
  [virtualSymbolToken "}" anchor | LayoutImplicit _ <- contexts]

stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo -> st {layoutPendingLayout = True}
    TkKeywordOf -> st {layoutPendingLayout = True}
    TkSymbol "{" -> st {layoutContexts = LayoutExplicit : layoutContexts st}
    TkSymbol "}" -> st {layoutContexts = popOneContext (layoutContexts st)}
    _ -> st

moduleLayoutOpenIndices :: [LexToken] -> IntSet.IntSet
moduleLayoutOpenIndices toks =
  case firstNonPragma of
    Nothing -> IntSet.empty
    Just (startIx, startTok) ->
      case lexTokenKind startTok of
        TkKeywordModule ->
          case find (\(ix, tok) -> ix > startIx && lexTokenKind tok == TkKeywordWhere) indexedToks of
            Just (whereIx, _)
              | whereIx + 1 < length toks -> IntSet.singleton (whereIx + 1)
            _ -> IntSet.empty
        _ -> IntSet.singleton startIx
  where
    indexedToks = zip [0 ..] toks
    firstNonPragma =
      find
        ( \(_, tok) ->
            case lexTokenKind tok of
              TkPragmaLanguage _ -> False
              _ -> True
        )
        indexedToks

popOneContext :: [LayoutContext] -> [LayoutContext]
popOneContext contexts =
  case contexts of
    _ : rest -> rest
    [] -> []

currentLayoutIndent :: [LayoutContext] -> Int
currentLayoutIndent contexts =
  case contexts of
    LayoutImplicit indent : _ -> indent
    _ -> 0

isBOL :: LayoutState -> LexToken -> Bool
isBOL st tok =
  case layoutPrevLine st of
    Just prevLine -> tokenStartLine tok > prevLine
    Nothing -> False

tokenStartLine :: LexToken -> Int
tokenStartLine tok =
  case lexTokenSpan tok of
    SourceSpan line _ _ _ -> line
    NoSourceSpan -> 1

tokenStartCol :: LexToken -> Int
tokenStartCol tok =
  case lexTokenSpan tok of
    SourceSpan _ col _ _ -> col
    NoSourceSpan -> 1

eofAnchorSpan :: [LexToken] -> SourceSpan
eofAnchorSpan toks =
  case reverse toks of
    tok : _ ->
      case lexTokenSpan tok of
        SourceSpan _ _ endLine endCol -> SourceSpan endLine endCol endLine endCol
        NoSourceSpan -> NoSourceSpan
    [] -> NoSourceSpan

virtualSymbolToken :: Text -> SourceSpan -> LexToken
virtualSymbolToken sym span' =
  LexToken
    { lexTokenKind = TkSymbol sym,
      lexTokenText = sym,
      lexTokenSpan = span'
    }

triviaConsumer :: LParser ()
triviaConsumer = MP.skipMany (void C.spaceChar <|> lineCommentConsumer <|> try blockCommentConsumer)

lineCommentConsumer :: LParser ()
lineCommentConsumer = L.skipLineComment "--"

blockCommentConsumer :: LParser ()
blockCommentConsumer = do
  _ <- C.string "{-"
  notFollowedBy (C.char '#')
  skipNestedBlockCommentBody 1

skipNestedBlockCommentBody :: Int -> LParser ()
skipNestedBlockCommentBody depth
  | depth <= 0 = pure ()
  | otherwise =
      try (C.string "{-" *> skipNestedBlockCommentBody (depth + 1))
        <|> try (C.string "-}" *> skipNestedBlockCommentBody (depth - 1))
        <|> (anySingle *> skipNestedBlockCommentBody depth)

lexTokenParser :: LParser LexToken
lexTokenParser =
  lexWithSpan $
    try languagePragmaToken
      <|> try quasiQuoteToken
      <|> try hexFloatToken
      <|> try floatToken
      <|> try intBaseToken
      <|> try intToken
      <|> try charToken
      <|> try stringToken
      <|> try symbolToken
      <|> try identifierToken
      <|> operatorToken

languagePragmaToken :: LParser (Text, LexTokenKind)
languagePragmaToken = do
  _ <- C.string "{-#"
  _ <- many C.spaceChar
  _ <- C.string "LANGUAGE"
  _ <- many C.spaceChar
  body <- manyTillText "#-}"
  let names = parseLanguagePragmaNames (T.pack body)
      raw = "{-# LANGUAGE " <> T.intercalate ", " names <> " #-}"
  pure (raw, TkPragmaLanguage names)

parseLanguagePragmaNames :: Text -> [Text]
parseLanguagePragmaNames body =
  filter (not . T.null) (map (T.strip . T.takeWhile (/= '#')) (T.splitOn "," body))

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
  more <- many (try (C.char '.' *> ((:) <$> C.letterChar <*> many identTailChar)))
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
  base <- C.char 'x' <|> C.char 'X' <|> C.char 'o' <|> C.char 'O' <|> C.char 'b' <|> C.char 'B'
  digitsRaw <-
    if base `elem` ['x', 'X']
      then digitsWithUnderscores isHexDigit
      else
        if base `elem` ['o', 'O']
          then digitsWithUnderscores isOctDigit
          else digitsWithUnderscores (`elem` ("01" :: String))
  let txt = T.pack ('0' : base : digitsRaw)
      n
        | base `elem` ['x', 'X'] = readHexLiteral txt
        | base `elem` ['o', 'O'] = readOctLiteral txt
        | otherwise = readBinLiteral txt
  pure (txt, TkIntegerBase n txt)

intToken :: LParser (Text, LexTokenKind)
intToken = do
  digitsRaw <- digitsWithUnderscores isDigit
  let txt = T.pack digitsRaw
      digits = filter (/= '_') digitsRaw
  pure (txt, TkInteger (read digits))

floatToken :: LParser (Text, LexTokenKind)
floatToken = do
  lhsRaw <- digitsWithUnderscores isDigit
  repr <-
    try
      ( do
          _ <- C.char '.'
          rhsRaw <- digitsWithUnderscores isDigit
          expo <- MP.optional exponentPart
          pure (lhsRaw <> "." <> rhsRaw <> fromMaybe "" expo)
      )
      <|> do
        expo <- exponentPart
        pure (lhsRaw <> expo)
  let txt = T.pack repr
      normalized = filter (/= '_') repr
  pure (txt, TkFloat (read normalized) txt)

hexFloatToken :: LParser (Text, LexTokenKind)
hexFloatToken = do
  _ <- C.char '0'
  x <- C.char 'x' <|> C.char 'X'
  intDigits <- some (satisfy isHexDigit)
  mFracDigits <- MP.optional (C.char '.' *> many (satisfy isHexDigit))
  expo <- hexExponentPart
  let fracDigits = fromMaybe "" mFracDigits
      dotAndFrac =
        case mFracDigits of
          Just ds -> '.' : ds
          Nothing -> ""
      repr = '0' : x : intDigits <> dotAndFrac <> expo
      value = parseHexFloatLiteral intDigits fracDigits expo
  pure (T.pack repr, TkFloat value (T.pack repr))

hexExponentPart :: LParser String
hexExponentPart = do
  marker <- C.char 'p' <|> C.char 'P'
  sign <- MP.optional (C.char '+' <|> C.char '-')
  ds <- some C.digitChar
  pure (marker : maybe [] pure sign <> ds)

exponentPart :: LParser String
exponentPart = do
  marker <- C.char 'e' <|> C.char 'E'
  sign <- MP.optional (C.char '+' <|> C.char '-')
  ds <- digitsWithUnderscores isDigit
  pure (marker : maybe [] pure sign <> ds)

digitsWithUnderscores :: (Char -> Bool) -> LParser String
digitsWithUnderscores isDigitChar = do
  firstChunk <- some (satisfy isDigitChar)
  rest <- many $ do
    _ <- C.char '_'
    some (satisfy isDigitChar)
  pure (concat (firstChunk : map ('_' :) rest))

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
  case readHex (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

readOctLiteral :: Text -> Integer
readOctLiteral txt =
  case readOct (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

readBinLiteral :: Text -> Integer
readBinLiteral txt =
  case readInt 2 (`elem` ("01" :: String)) digitToInt (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

parseHexFloatLiteral :: String -> String -> String -> Double
parseHexFloatLiteral intDigits fracDigits expo =
  (parseHexDigits intDigits + parseHexFraction fracDigits) * (2 ^^ exponentValue expo)

parseHexDigits :: String -> Double
parseHexDigits = foldl (\acc d -> acc * 16 + fromIntegral (digitToInt d)) 0

parseHexFraction :: String -> Double
parseHexFraction ds =
  sum [fromIntegral (digitToInt d) / (16 ^^ i) | (d, i) <- zip ds [1 :: Int ..]]

exponentValue :: String -> Int
exponentValue expo =
  case expo of
    _ : '-' : ds -> negate (read ds)
    _ : '+' : ds -> read ds
    _ : ds -> read ds
    _ -> 0

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c = c `elem` (":!#$%&*+./<=>?\\^|-~" :: String)

isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart c = isAlphaNum c || c == '_' || c == '\''

keywordTokenKind :: Text -> Maybe LexTokenKind
keywordTokenKind txt = case txt of
  "module" -> Just TkKeywordModule
  "where" -> Just TkKeywordWhere
  "do" -> Just TkKeywordDo
  "data" -> Just TkKeywordData
  "import" -> Just TkKeywordImport
  "qualified" -> Just TkKeywordQualified
  "as" -> Just TkKeywordAs
  "hiding" -> Just TkKeywordHiding
  "case" -> Just TkKeywordCase
  "of" -> Just TkKeywordOf
  "let" -> Just TkKeywordLet
  "in" -> Just TkKeywordIn
  "if" -> Just TkKeywordIf
  "then" -> Just TkKeywordThen
  "else" -> Just TkKeywordElse
  _ -> Nothing
