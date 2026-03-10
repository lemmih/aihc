{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
    errorBundlePretty,
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Parser.Ast (Decl (..), Expr (..), Match (..), Module (..), Rhs (..), SourceSpan (..), ValueDecl (..))
import Parser.Lexer (LexToken (..), LexTokenKind (..), lexTokens)
import Parser.Types
import Text.Megaparsec (Parsec, anySingle, lookAhead, runParser, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec Void TokStream

exprParser :: TokParser Expr
exprParser = ifExprParser <|> appExprParser

moduleParser :: TokParser Module
moduleParser = withSpan $ do
  mName <- MP.optional (moduleHeaderParser <* MP.many (symbolLikeTok ";"))
  decls <- MP.some (declParser <* MP.many (symbolLikeTok ";"))
  pure $ \span' ->
    Module
      { moduleSpan = span',
        moduleName = mName,
        moduleLanguagePragmas = [],
        moduleExports = Nothing,
        moduleImports = [],
        moduleDecls = decls
      }

moduleHeaderParser :: TokParser Text
moduleHeaderParser = do
  keywordTok TkKeywordModule
  name <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  keywordTok TkKeywordWhere
  pure name

declParser :: TokParser Decl
declParser = withSpan $ do
  name <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  operatorLikeTok "="
  rhsExpr <- exprParser
  pure $ \span' ->
    DeclValue
      span'
      ( FunctionBind
          span'
          name
          [ Match
              { matchSpan = span',
                matchPats = [],
                matchRhs = UnguardedRhs span' rhsExpr
              }
          ]
      )

defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { allowLineComments = True
    }

parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr _cfg input =
  case runParser (exprParser <* MP.eof) "" (TokStream (lexTokens input)) of
    Left bundle -> ParseErr bundle
    Right expr -> ParseOk expr

parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule _cfg input =
  case runParser (moduleParser <* MP.eof) "" (TokStream (lexTokens input)) of
    Left bundle -> ParseErr bundle
    Right m -> ParseOk m

-- | Render parse error bundles.
errorBundlePretty :: ParseErrorBundle -> String
errorBundlePretty = show

ifExprParser :: TokParser Expr
ifExprParser = withSpan $ do
  keywordTok TkKeywordIf
  cond <- exprParser
  keywordTok TkKeywordThen
  yes <- exprParser
  keywordTok TkKeywordElse
  no <- exprParser
  pure (\span' -> EIf span' cond yes no)

intExprParser :: TokParser Expr
intExprParser = withSpan $ do
  n <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just i
      _ -> Nothing
  pure (`EInt` n)

appExprParser :: TokParser Expr
appExprParser = withSpan $ do
  first <- atomExprParser
  let appLine =
        case exprSourceSpan first of
          SourceSpan line _ _ _ -> line
          NoSourceSpan -> 1
  rest <- MP.many (sameLineAtomExprParser appLine)
  pure $ \span' ->
    foldl (EApp span') first rest

atomExprParser :: TokParser Expr
atomExprParser = parenExprParser <|> intExprParser <|> varExprParser

sameLineAtomExprParser :: Int -> TokParser Expr
sameLineAtomExprParser expectedLine = do
  nextTok <- lookAhead anySingle
  case lexTokenSpan nextTok of
    SourceSpan line _ _ _ | line == expectedLine -> atomExprParser
    _ -> fail "line break"

parenExprParser :: TokParser Expr
parenExprParser = withSpan $ do
  symbolLikeTok "("
  inner <- exprParser
  symbolLikeTok ")"
  pure (`EParen` inner)

exprSourceSpan :: Expr -> SourceSpan
exprSourceSpan expr =
  case expr of
    EVar span' _ -> span'
    EInt span' _ -> span'
    EIntBase span' _ _ -> span'
    EFloat span' _ -> span'
    EChar span' _ -> span'
    EString span' _ -> span'
    EQuasiQuote span' _ _ -> span'
    EIf span' _ _ _ -> span'
    ELambdaPats span' _ _ -> span'
    EInfix span' _ _ _ -> span'
    ENegate span' _ -> span'
    ESectionL span' _ _ -> span'
    ESectionR span' _ _ -> span'
    ELetDecls span' _ _ -> span'
    ECase span' _ _ -> span'
    EDo span' _ -> span'
    EListComp span' _ _ -> span'
    EListCompParallel span' _ _ -> span'
    EArithSeq span' _ -> span'
    ERecordCon span' _ _ -> span'
    ERecordUpd span' _ _ -> span'
    ETypeSig span' _ _ -> span'
    EParen span' _ -> span'
    EWhereDecls span' _ _ -> span'
    EList span' _ -> span'
    ETuple span' _ -> span'
    ETupleCon span' _ -> span'
    ETypeApp span' _ _ -> span'
    EApp span' _ _ -> span'

varExprParser :: TokParser Expr
varExprParser = withSpan $ do
  name <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  pure (`EVar` name)

keywordTok :: LexTokenKind -> TokParser ()
keywordTok expected =
  tokenSatisfy $ \tok ->
    if lexTokenKind tok == expected then Just () else Nothing

symbolLikeTok :: Text -> TokParser ()
symbolLikeTok expected =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkSymbol sym
        | sym == expected -> Just ()
      _ -> Nothing

operatorLikeTok :: Text -> TokParser ()
operatorLikeTok expected =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkOperator op
        | op == expected -> Just ()
      _ -> Nothing

tokenSatisfy :: (LexToken -> Maybe a) -> TokParser a
tokenSatisfy f = do
  tok <- lookAhead anySingle
  case f tok of
    Just out -> out <$ anySingle
    Nothing -> fail "token"

withSpan :: TokParser (SourceSpan -> a) -> TokParser a
withSpan parser = do
  start <- MP.getSourcePos
  out <- parser
  out . sourceSpanFromPositions start <$> MP.getSourcePos

sourceSpanFromPositions :: SourcePos -> SourcePos -> SourceSpan
sourceSpanFromPositions start end =
  SourceSpan
    { sourceSpanStartLine = MP.unPos (sourceLine start),
      sourceSpanStartCol = MP.unPos (sourceColumn start),
      sourceSpanEndLine = MP.unPos (sourceLine end),
      sourceSpanEndCol = MP.unPos (sourceColumn end)
    }
