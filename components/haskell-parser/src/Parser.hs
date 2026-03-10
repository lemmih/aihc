{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
    errorBundlePretty,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Parser.Ast (DataConDecl (..), DataDecl (..), Decl (..), Expr (..), ImportDecl (..), Match (..), Module (..), Rhs (..), SourceSpan (..), ValueDecl (..))
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
  languagePragmas <- MP.many languagePragmaParser
  mName <- MP.optional (moduleHeaderParser <* MP.many (symbolLikeTok ";"))
  imports <- MP.many (importDeclParser <* MP.many (symbolLikeTok ";"))
  decls <- MP.some (declParser <* MP.many (symbolLikeTok ";"))
  pure $ \span' ->
    Module
      { moduleSpan = span',
        moduleName = mName,
        moduleLanguagePragmas = concat languagePragmas,
        moduleExports = Nothing,
        moduleImports = imports,
        moduleDecls = decls
      }

languagePragmaParser :: TokParser [Text]
languagePragmaParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkPragmaLanguage names -> Just names
      _ -> Nothing

moduleHeaderParser :: TokParser Text
moduleHeaderParser = do
  keywordTok TkKeywordModule
  name <- moduleNameParser
  keywordTok TkKeywordWhere
  pure name

importDeclParser :: TokParser ImportDecl
importDeclParser = withSpan $ do
  keywordTok TkKeywordImport
  importedModule <- moduleNameParser
  pure $ \span' ->
    ImportDecl
      { importDeclSpan = span',
        importDeclQualified = False,
        importDeclModule = importedModule,
        importDeclAs = Nothing,
        importDeclSpec = Nothing
      }

declParser :: TokParser Decl
declParser = dataDeclParser <|> valueDeclParser

dataDeclParser :: TokParser Decl
dataDeclParser = withSpan $ do
  keywordTok TkKeywordData
  typeName <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  typeParams <- MP.many $ tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  constructors <- MP.optional (operatorLikeTok "=" *> dataConDeclParser `MP.sepBy1` operatorLikeTok "|")
  pure $ \span' ->
    DeclData
      span'
      DataDecl
        { dataDeclSpan = span',
          dataDeclContext = [],
          dataDeclName = typeName,
          dataDeclParams = typeParams,
          dataDeclConstructors = fromMaybe [] constructors,
          dataDeclDeriving = Nothing
        }

dataConDeclParser :: TokParser DataConDecl
dataConDeclParser = withSpan $ do
  name <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  pure $ \span' -> PrefixCon span' name []

valueDeclParser :: TokParser Decl
valueDeclParser = withSpan $ do
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

intBaseExprParser :: TokParser Expr
intBaseExprParser = withSpan $ do
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (i, txt)
      _ -> Nothing
  pure (\span' -> EIntBase span' n repr)

floatExprParser :: TokParser Expr
floatExprParser = withSpan $ do
  n <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkFloat x -> Just x
      _ -> Nothing
  pure (`EFloat` n)

charExprParser :: TokParser Expr
charExprParser = withSpan $ do
  c <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just x
      _ -> Nothing
  pure (`EChar` c)

stringExprParser :: TokParser Expr
stringExprParser = withSpan $ do
  s <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just x
      _ -> Nothing
  pure (`EString` s)

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
atomExprParser =
  parenExprParser
    <|> listExprParser
    <|> intBaseExprParser
    <|> floatExprParser
    <|> intExprParser
    <|> charExprParser
    <|> stringExprParser
    <|> varExprParser

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

listExprParser :: TokParser Expr
listExprParser = withSpan $ do
  symbolLikeTok "["
  elems <- exprParser `MP.sepBy` symbolLikeTok ","
  symbolLikeTok "]"
  pure (`EList` elems)

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

moduleNameParser :: TokParser Text
moduleNameParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing

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
