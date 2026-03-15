{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Lexer
-- Description : Lex Haskell source into span-annotated tokens, then apply extensions and layout.
--
-- This module performs the pre-parse tokenization step for Haskell source code.
-- It turns raw text into 'LexToken's that preserve:
--
-- * a semantic token classification ('LexTokenKind')
-- * the original token text ('lexTokenText')
-- * source location information ('lexTokenSpan')
--
-- The lexer runs in three phases:
--
-- 1. /Raw tokenization/ with Megaparsec ('lexTokenParser') while skipping trivia
--    ('triviaConsumer').
-- 2. /Extension rewrites/ ('applyExtensions'), for example @NegativeLiterals@ which
--    folds @-@ plus an adjacent numeric literal into one literal token.
-- 3. /Layout insertion/ ('applyLayoutTokens') that inserts virtual @{@, @;@ and @}@
--    according to indentation (the offside rule), so the parser can treat implicit
--    layout like explicit braces and semicolons.
--
-- Layout-sensitive syntax is the tricky part. The implementation tracks a stack of
-- layout contexts and mirrors the @haskell-src-exts@ model summarized in
-- @docs/hse-indentation-layout.md@:
--
-- * after layout-introducing keywords (currently @do@ and @of@, plus optional module
--   body layout), mark a pending implicit block
-- * if the next token is an explicit @{@, disable implicit insertion for that block
-- * otherwise, open an implicit layout context at the next token column
-- * at beginning-of-line tokens, dedent emits virtual @}@, equal-indent emits virtual
--   @;@ (with a small suppression rule for @then@/@else@)
--
-- Keyword classification is intentionally lexical and exact. 'identifierToken'
-- produces a keyword token /only/ when the full identifier text exactly matches a
-- reserved word in 'keywordTokenKind'. That means:
--
-- * @where@ becomes 'TkKeywordWhere'
-- * @where'@, @_where@, and @M.where@ remain identifiers
--
-- In other words, use keyword tokens only for exact reserved lexemes; contextual
-- validity is left to the parser.
module Parser.Lexer
  ( LexToken (..),
    LexTokenKind (..),
    lexTokensWithExtensions,
    lexModuleTokensWithExtensions,
    lexTokens,
    lexModuleTokens,
  )
where

import Control.Monad (void)
import Data.Char (digitToInt, isAlphaNum, isDigit, isHexDigit, isOctDigit)
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
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
    lookAhead,
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
  | TkPragmaLanguage [Extension]
  | TkPragmaWarning Text
  | TkPragmaDeprecated Text
  | TkIdentifier Text
  | TkOperator Text
  | TkInteger Integer
  | TkIntegerBase Integer Text
  | TkFloat Double Text
  | TkChar Char
  | TkString Text
  | TkSymbol Text
  | TkQuasiQuote Text Text
  deriving (Eq, Ord, Show, Read)

-- | A token with both lexical meaning and precise source span.
data LexToken = LexToken
  { lexTokenKind :: !LexTokenKind,
    lexTokenText :: !Text,
    lexTokenSpan :: !SourceSpan
  }
  deriving (Eq, Ord, Show)

type LParser = Parsec Void Text

-- | Convenience lexer entrypoint: no extensions, parse as expression/declaration stream.
--
-- Returns @[]@ on lexing errors.
lexTokens :: Text -> [LexToken]
lexTokens input =
  case lexTokensWithExtensions [] input of
    Right toks -> toks
    Left _ -> []

-- | Convenience lexer entrypoint for full modules: no extensions, with module layout enabled.
--
-- Returns @[]@ on lexing errors.
lexModuleTokens :: Text -> [LexToken]
lexModuleTokens input =
  case lexModuleTokensWithExtensions [] input of
    Right toks -> toks
    Left _ -> []

-- | Lex source text using explicit lexer extensions.
--
-- This runs raw tokenization, extension rewrites, and implicit-layout insertion.
-- Module-top layout is /not/ enabled here.
lexTokensWithExtensions :: [Extension] -> Text -> Either String [LexToken]
lexTokensWithExtensions exts input =
  case runParser (triviaConsumer *> many (lexTokenParser <* triviaConsumer) <* eof) "<lexer>" input of
    Right toks -> Right (applyLayoutTokens False (applyExtensions exts toks))
    Left err -> Left (MP.errorBundlePretty err)

-- | Lex module source text using explicit lexer extensions.
--
-- Like 'lexTokensWithExtensions', but also enables top-level module-body layout:
-- when the source omits explicit braces, virtual layout tokens are inserted
-- after @module ... where@ (or from the first non-pragma token in module-less files).
lexModuleTokensWithExtensions :: [Extension] -> Text -> Either String [LexToken]
lexModuleTokensWithExtensions exts input =
  case runParser (triviaConsumer *> many (lexTokenParser <* triviaConsumer) <* eof) "<lexer>" input of
    Right toks -> Right (applyLayoutTokens True (applyExtensions exts toks))
    Left err -> Left (MP.errorBundlePretty err)

-- | Apply all extension-driven post-lexing rewrites in a deterministic order.
applyExtensions :: [Extension] -> [LexToken] -> [LexToken]
applyExtensions exts toks
  | NegativeLiterals `elem` exts = applyNegativeLiterals toks
  | otherwise = toks

-- | Implement @NegativeLiterals@ by merging @-@ and immediately adjacent numerics.
--
-- The merge only happens when there is no intervening whitespace/comments, and only
-- for integer/base-integer/float tokens.
applyNegativeLiterals :: [LexToken] -> [LexToken]
applyNegativeLiterals toks =
  case toks of
    minusTok : numTok : rest
      | lexTokenKind minusTok == TkOperator "-",
        tokensAdjacent minusTok numTok ->
          case lexTokenKind numTok of
            TkInteger n ->
              negativeIntegerToken minusTok numTok n : applyNegativeLiterals rest
            TkIntegerBase n repr ->
              negativeIntegerBaseToken minusTok numTok n repr : applyNegativeLiterals rest
            TkFloat n repr ->
              negativeFloatToken minusTok numTok n repr : applyNegativeLiterals rest
            _ -> minusTok : applyNegativeLiterals (numTok : rest)
    tok : rest -> tok : applyNegativeLiterals rest
    [] -> []

-- | True when the second token starts exactly where the first one ends.
tokensAdjacent :: LexToken -> LexToken -> Bool
tokensAdjacent first second =
  case (lexTokenSpan first, lexTokenSpan second) of
    (SourceSpan _ _ firstEndLine firstEndCol, SourceSpan secondStartLine secondStartCol _ _) ->
      firstEndLine == secondStartLine && firstEndCol == secondStartCol
    _ -> False

-- | Build a negative decimal integer token from @-@ and a positive literal token.
negativeIntegerToken :: LexToken -> LexToken -> Integer -> LexToken
negativeIntegerToken minusTok numTok n =
  LexToken
    { lexTokenKind = TkInteger (negate n),
      lexTokenText = lexTokenText minusTok <> lexTokenText numTok,
      lexTokenSpan = combinedSpan minusTok numTok
    }

-- | Build a negative non-decimal integer token from @-@ and a positive literal token.
negativeIntegerBaseToken :: LexToken -> LexToken -> Integer -> Text -> LexToken
negativeIntegerBaseToken minusTok numTok n repr =
  LexToken
    { lexTokenKind = TkIntegerBase (negate n) ("-" <> repr),
      lexTokenText = lexTokenText minusTok <> lexTokenText numTok,
      lexTokenSpan = combinedSpan minusTok numTok
    }

-- | Build a negative float token from @-@ and a positive literal token.
negativeFloatToken :: LexToken -> LexToken -> Double -> Text -> LexToken
negativeFloatToken minusTok numTok n repr =
  LexToken
    { lexTokenKind = TkFloat (negate n) ("-" <> repr),
      lexTokenText = lexTokenText minusTok <> lexTokenText numTok,
      lexTokenSpan = combinedSpan minusTok numTok
    }

-- | Span that starts at the first token and ends at the second token.
combinedSpan :: LexToken -> LexToken -> SourceSpan
combinedSpan first second =
  case (lexTokenSpan first, lexTokenSpan second) of
    (SourceSpan sl sc _ _, SourceSpan _ _ el ec) -> SourceSpan sl sc el ec
    _ -> NoSourceSpan

-- | Layout stack entries.
--
-- 'LayoutExplicit' means we are inside explicit braces, so indentation should not
-- create virtual punctuation for that level.
-- 'LayoutImplicit' stores the indentation column for an offside block.
data LayoutContext
  = LayoutExplicit
  | LayoutImplicit !Int
  deriving (Eq, Show)

-- | Mutable state threaded through the layout insertion pass.
data LayoutState = LayoutState
  { layoutContexts :: [LayoutContext],
    layoutPendingLayout :: !Bool,
    layoutPrevLine :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | Insert virtual layout tokens (@{@, @;@, @}@) according to indentation.
--
-- When @enableModuleLayout@ is True, a synthetic layout block is also considered for
-- the module body (after @module ... where@, or from the first non-pragma token when
-- the @module@ header is omitted).
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

-- | If a layout-introducing token was seen previously, decide how to open that block.
--
-- Returns:
--
-- * tokens inserted before @tok@
-- * updated state
-- * whether BOL processing should be skipped for this token
--
-- If @tok@ is explicit @{@, pending layout is cancelled. Otherwise, we insert a
-- virtual @{@ and either:
--
-- * immediately close it with virtual @}@ when indentation is not greater than parent
-- * push a new implicit indentation context
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

-- | Handle beginning-of-line layout rules for the current token.
--
-- Dedent closes implicit blocks with virtual @}@. Matching indentation inserts a
-- virtual @;@ unless suppressed by 'suppressesVirtualSemicolon'.
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

-- | Tokens that should not be preceded by an inserted virtual semicolon on EQ-indent.
--
-- This avoids producing a spurious statement separator before @then@/@else@ in common
-- conditional layouts.
suppressesVirtualSemicolon :: LexToken -> Bool
suppressesVirtualSemicolon tok =
  case lexTokenKind tok of
    TkKeywordThen -> True
    TkKeywordElse -> True
    _ -> False

-- | Pop implicit layout contexts while current column is less indented.
closeForDedent :: Int -> SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeForDedent col anchor = go []
  where
    go acc contexts =
      case contexts of
        LayoutImplicit indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        _ -> (reverse acc, contexts)

-- | Emit virtual closing braces for all still-open implicit layouts at EOF.
closeAllImplicit :: [LayoutContext] -> SourceSpan -> [LexToken]
closeAllImplicit contexts anchor =
  [virtualSymbolToken "}" anchor | LayoutImplicit _ <- contexts]

-- | Update layout state from a real token.
--
-- @do@ and @of@ start pending implicit layout, and explicit braces push/pop
-- 'LayoutExplicit'.
stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo -> st {layoutPendingLayout = True}
    TkKeywordOf -> st {layoutPendingLayout = True}
    TkSymbol "{" -> st {layoutContexts = LayoutExplicit : layoutContexts st}
    TkSymbol "}" -> st {layoutContexts = popOneContext (layoutContexts st)}
    _ -> st

-- | Determine token indices where module-level layout should begin.
--
-- Strategy:
--
-- * skip leading LANGUAGE/WARNING/DEPRECATED pragmas
-- * if first token is @module@, start after the corresponding @where@ (if body exists)
-- * otherwise, start from that first non-pragma token
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
              TkPragmaWarning _ -> False
              TkPragmaDeprecated _ -> False
              _ -> True
        )
        indexedToks

-- | Pop one layout context, if any.
popOneContext :: [LayoutContext] -> [LayoutContext]
popOneContext contexts =
  case contexts of
    _ : rest -> rest
    [] -> []

-- | Current indentation baseline used for pending implicit layout opening.
currentLayoutIndent :: [LayoutContext] -> Int
currentLayoutIndent contexts =
  case contexts of
    LayoutImplicit indent : _ -> indent
    _ -> 0

-- | True when @tok@ begins on a later line than the previously emitted token.
isBOL :: LayoutState -> LexToken -> Bool
isBOL st tok =
  case layoutPrevLine st of
    Just prevLine -> tokenStartLine tok > prevLine
    Nothing -> False

-- | Start line of a token (defaults to 1 for missing spans).
tokenStartLine :: LexToken -> Int
tokenStartLine tok =
  case lexTokenSpan tok of
    SourceSpan line _ _ _ -> line
    NoSourceSpan -> 1

-- | Start column of a token (defaults to 1 for missing spans).
tokenStartCol :: LexToken -> Int
tokenStartCol tok =
  case lexTokenSpan tok of
    SourceSpan _ col _ _ -> col
    NoSourceSpan -> 1

-- | Zero-width EOF anchor span at the end of the final token.
eofAnchorSpan :: [LexToken] -> SourceSpan
eofAnchorSpan toks =
  case reverse toks of
    tok : _ ->
      case lexTokenSpan tok of
        SourceSpan _ _ endLine endCol -> SourceSpan endLine endCol endLine endCol
        NoSourceSpan -> NoSourceSpan
    [] -> NoSourceSpan

-- | Construct a virtual punctuation token used by the layout pass.
virtualSymbolToken :: Text -> SourceSpan -> LexToken
virtualSymbolToken sym span' =
  LexToken
    { lexTokenKind = TkSymbol sym,
      lexTokenText = sym,
      lexTokenSpan = span'
    }

-- | Skip whitespace and non-pragma comments between tokens.
--
-- Pragmas are lexed as tokens, so @{-# ... #-}@ must not be consumed here.
triviaConsumer :: LParser ()
triviaConsumer = MP.skipMany (void C.spaceChar <|> lineCommentConsumer <|> try blockCommentConsumer)

-- | Consume a line comment introduced by @--@.
lineCommentConsumer :: LParser ()
lineCommentConsumer = L.skipLineComment "--"

-- | Consume a non-pragma nested block comment.
--
-- The initial @{-@ has been read, and @{-# ... #-}@ is excluded by caller.
blockCommentConsumer :: LParser ()
blockCommentConsumer = do
  _ <- C.string "{-"
  notFollowedBy (C.char '#')
  skipNestedBlockCommentBody 1

-- | Skip the remaining body of a nested block comment.
skipNestedBlockCommentBody :: Int -> LParser ()
skipNestedBlockCommentBody depth
  | depth <= 0 = pure ()
  | otherwise =
      try (C.string "{-" *> skipNestedBlockCommentBody (depth + 1))
        <|> try (C.string "-}" *> skipNestedBlockCommentBody (depth - 1))
        <|> (anySingle *> skipNestedBlockCommentBody depth)

-- | Parse one lexical token and attach its source span.
--
-- Order matters here: more specific token forms must appear before more general ones.
lexTokenParser :: LParser LexToken
lexTokenParser =
  lexWithSpan $
    try languagePragmaToken
      <|> try pragmaWarningToken
      <|> try pragmaDeprecatedToken
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

-- | Parse a @LANGUAGE@ pragma token.
--
-- The token kind stores parsed extension names, and token text is normalized to a
-- canonical comma-separated representation.
languagePragmaToken :: LParser (Text, LexTokenKind)
languagePragmaToken = do
  _ <- C.string "{-#"
  _ <- many C.spaceChar
  _ <- C.string "LANGUAGE"
  _ <- many C.spaceChar
  body <- manyTillText "#-}"
  let names = parseLanguagePragmaNames (T.pack body)
      raw = "{-# LANGUAGE " <> T.intercalate ", " (map extensionName names) <> " #-}"
  pure (raw, TkPragmaLanguage names)

-- | Parse extension names from the body of a LANGUAGE pragma.
parseLanguagePragmaNames :: Text -> [Extension]
parseLanguagePragmaNames body =
  mapMaybe (parseExtensionName . T.strip . T.takeWhile (/= '#')) (T.splitOn "," body)

-- | Parse a @WARNING@ pragma token.
--
-- Accepts both string-literal and raw-text message forms.
pragmaWarningToken :: LParser (Text, LexTokenKind)
pragmaWarningToken = do
  _ <- C.string "{-#"
  _ <- many C.spaceChar
  _ <- C.string "WARNING"
  _ <- many C.spaceChar
  (msg, rawMsg) <-
    try
      ( do
          (rawStr, TkString decoded) <- stringToken
          pure (decoded, rawStr)
      )
      <|> ( do
              body <- MP.manyTill anySingle (try (lookAhead (C.string "#-}")))
              let txt = T.strip (T.pack body)
              pure (txt, txt)
          )
  _ <- many C.spaceChar
  void (C.string "#-}")
  let raw = "{-# WARNING " <> rawMsg <> " #-}"
  pure (raw, TkPragmaWarning msg)

-- | Parse a @DEPRECATED@ pragma token.
--
-- Accepts both string-literal and raw-text message forms.
pragmaDeprecatedToken :: LParser (Text, LexTokenKind)
pragmaDeprecatedToken = do
  _ <- C.string "{-#"
  _ <- many C.spaceChar
  _ <- C.string "DEPRECATED"
  _ <- many C.spaceChar
  (msg, rawMsg) <-
    try
      ( do
          (rawStr, TkString decoded) <- stringToken
          pure (decoded, rawStr)
      )
      <|> ( do
              body <- MP.manyTill anySingle (try (lookAhead (C.string "#-}")))
              let txt = T.strip (T.pack body)
              pure (txt, txt)
          )
  _ <- many C.spaceChar
  void (C.string "#-}")
  let raw = "{-# DEPRECATED " <> rawMsg <> " #-}"
  pure (raw, TkPragmaDeprecated msg)

-- | Run a token parser while capturing start/end positions.
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

-- | Convert Megaparsec source positions into project 'SourceSpan'.
mkSpan :: MP.SourcePos -> MP.SourcePos -> SourceSpan
mkSpan start end =
  SourceSpan
    { sourceSpanStartLine = unPos (MP.sourceLine start),
      sourceSpanStartCol = unPos (MP.sourceColumn start),
      sourceSpanEndLine = unPos (MP.sourceLine end),
      sourceSpanEndCol = unPos (MP.sourceColumn end)
    }

-- | Parse identifiers (including dotted qualifiers) and classify exact keywords.
--
-- Keyword promotion is strict lexical equality against 'keywordTokenKind':
-- qualified names and suffixed variants are always identifiers.
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

-- | Identifier tail character after the first character.
identTailChar :: LParser Char
identTailChar =
  C.alphaNumChar
    <|> C.char '_'
    <|> C.char '\''

-- | Parse symbolic operators such as @->@, @::@, or @+@.
operatorToken :: LParser (Text, LexTokenKind)
operatorToken = do
  op <- some (satisfy isSymbolicOpChar)
  let txt = T.pack op
  pure (txt, TkOperator txt)

-- | Parse punctuation symbols handled as standalone tokens.
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

-- | Parse non-decimal integer literals (@0x@/@0o@/@0b@) with underscore separators.
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

-- | Parse decimal integer literals with underscore separators.
intToken :: LParser (Text, LexTokenKind)
intToken = do
  digitsRaw <- digitsWithUnderscores isDigit
  let txt = T.pack digitsRaw
      digits = filter (/= '_') digitsRaw
  pure (txt, TkInteger (read digits))

-- | Parse decimal float literals (fractional and/or exponent form).
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

-- | Parse hexadecimal floating-point literals with @p@/@P@ exponent.
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

-- | Parse hex-float exponent component (for example @p-3@).
hexExponentPart :: LParser String
hexExponentPart = do
  marker <- C.char 'p' <|> C.char 'P'
  sign <- MP.optional (C.char '+' <|> C.char '-')
  ds <- some C.digitChar
  pure (marker : maybe [] pure sign <> ds)

-- | Parse decimal-float exponent component (for example @e+12@).
exponentPart :: LParser String
exponentPart = do
  marker <- C.char 'e' <|> C.char 'E'
  sign <- MP.optional (C.char '+' <|> C.char '-')
  ds <- digitsWithUnderscores isDigit
  pure (marker : maybe [] pure sign <> ds)

-- | Parse one-or-more digits with optional underscore separators between chunks.
digitsWithUnderscores :: (Char -> Bool) -> LParser String
digitsWithUnderscores isDigitChar = do
  firstChunk <- some (satisfy isDigitChar)
  rest <- many $ do
    _ <- C.char '_'
    some (satisfy isDigitChar)
  pure (concat (firstChunk : map ('_' :) rest))

-- | Parse character literal and decode escapes via @reads@.
charToken :: LParser (Text, LexTokenKind)
charToken = do
  _ <- C.char '\''
  body <- manyTillChar '\''
  let raw = "'" <> body <> "'"
  case readMaybeChar raw of
    Just c -> pure (T.pack raw, TkChar c)
    Nothing -> fail "char literal"

-- | Parse string literal and decode escapes via @reads@.
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

-- | Parse quasi-quote literal of the form @[quoter|body|]@.
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

-- | Parse quasiquoter name (identifier with optional dotted qualifiers).
takeQuoter :: LParser String
takeQuoter = do
  first <- C.letterChar <|> C.char '_'
  rest <- many (satisfy isIdentTailOrStart)
  more <- many (C.char '.' *> ((:) <$> C.letterChar <*> many (satisfy isIdentTailOrStart)))
  let base = first : rest
  pure (concat (base : map ('.' :) more))

-- | Consume characters until @endCh@ (excluding terminator from result).
manyTillChar :: Char -> LParser String
manyTillChar endCh = go []
  where
    go acc =
      (C.char endCh >> pure (reverse acc))
        <|> do
          ch <- anySingle
          go (ch : acc)

-- | Consume characters until @end@ text marker (excluding terminator from result).
manyTillText :: Text -> LParser String
manyTillText end = go []
  where
    go acc =
      (try (C.string end) >> pure (reverse acc))
        <|> do
          ch <- anySingle
          go (ch : acc)

-- | Safe @reads@ helper for character literals.
readMaybeChar :: String -> Maybe Char
readMaybeChar raw =
  case reads raw of
    [(c, "")] -> Just c
    _ -> Nothing

-- | Parse a @0x@/@0X@ literal text into an 'Integer' (underscores ignored).
readHexLiteral :: Text -> Integer
readHexLiteral txt =
  case readHex (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

-- | Parse a @0o@/@0O@ literal text into an 'Integer' (underscores ignored).
readOctLiteral :: Text -> Integer
readOctLiteral txt =
  case readOct (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

-- | Parse a @0b@/@0B@ literal text into an 'Integer' (underscores ignored).
readBinLiteral :: Text -> Integer
readBinLiteral txt =
  case readInt 2 (`elem` ("01" :: String)) digitToInt (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

-- | Evaluate a hexadecimal floating-point literal from parsed components.
parseHexFloatLiteral :: String -> String -> String -> Double
parseHexFloatLiteral intDigits fracDigits expo =
  (parseHexDigits intDigits + parseHexFraction fracDigits) * (2 ^^ exponentValue expo)

-- | Parse hex integer digits into a numeric value.
parseHexDigits :: String -> Double
parseHexDigits = foldl (\acc d -> acc * 16 + fromIntegral (digitToInt d)) 0

-- | Parse hex fractional digits into a numeric value.
parseHexFraction :: String -> Double
parseHexFraction ds =
  sum [fromIntegral (digitToInt d) / (16 ^^ i) | (d, i) <- zip ds [1 :: Int ..]]

-- | Decode exponent sign and magnitude from @e@/@p@-style exponent text.
exponentValue :: String -> Int
exponentValue expo =
  case expo of
    _ : '-' : ds -> negate (read ds)
    _ : '+' : ds -> read ds
    _ : ds -> read ds
    _ -> 0

-- | Characters allowed in symbolic operator tokens.
isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c = c `elem` (":!#$%&*+./<=>?\\^|-~" :: String)

-- | Identifier character allowed after first position (and in dotted chunks).
isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart c = isAlphaNum c || c == '_' || c == '\''

-- | Reserved-word lookup used by 'identifierToken'.
--
-- Only exact lexemes are keywords. Any qualification or extra suffix means the
-- token stays an identifier.
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
