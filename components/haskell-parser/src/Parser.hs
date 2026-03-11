{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
    errorBundlePretty,
  )
where

import Data.Char (isLower)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Parser.Ast
  ( CallConv (..),
    CaseAlt (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    ImportDecl (..),
    Match (..),
    Module (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    ValueDecl (..),
  )
import Parser.Lexer (LexToken (..), LexTokenKind (..), lexTokens)
import Parser.Types
import Text.Megaparsec (Parsec, anySingle, lookAhead, runParser, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec Void TokStream

exprParser :: TokParser Expr
exprParser = doExprParser <|> ifExprParser <|> caseExprParser <|> infixExprParser

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
declParser =
  MP.try foreignDeclParser
    <|> MP.try typeSigDeclParser
    <|> dataDeclParser
    <|> valueDeclParser

typeSigDeclParser :: TokParser Decl
typeSigDeclParser = withSpan $ do
  names <- identifierTextParser `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok "::"
  ty <- typeParser
  pure (\span' -> DeclTypeSig span' names ty)

foreignDeclParser :: TokParser Decl
foreignDeclParser = withSpan $ do
  identifierExact "foreign"
  direction <- foreignDirectionParser
  callConv <- callConvParser
  safety <-
    case direction of
      ForeignImport -> MP.optional foreignSafetyParser
      ForeignExport -> pure Nothing
  entity <- MP.optional foreignEntityParser
  name <- identifierTextParser
  operatorLikeTok "::"
  ty <- typeParser
  pure $ \span' ->
    DeclForeign
      span'
      ForeignDecl
        { foreignDeclSpan = span',
          foreignDirection = direction,
          foreignCallConv = callConv,
          foreignSafety = safety,
          foreignEntity = fromMaybe ForeignEntityOmitted entity,
          foreignName = name,
          foreignType = ty
        }

foreignDirectionParser :: TokParser ForeignDirection
foreignDirectionParser =
  (keywordTok TkKeywordImport $> ForeignImport)
    <|> (identifierExact "export" $> ForeignExport)

callConvParser :: TokParser CallConv
callConvParser =
  (identifierExact "ccall" $> CCall)
    <|> (identifierExact "stdcall" $> StdCall)

foreignSafetyParser :: TokParser ForeignSafety
foreignSafetyParser =
  (identifierExact "safe" $> Safe)
    <|> (identifierExact "unsafe" $> Unsafe)

foreignEntityParser :: TokParser ForeignEntitySpec
foreignEntityParser = do
  entityTxt <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkString txt -> Just txt
      _ -> Nothing
  pure (foreignEntityFromString entityTxt)

foreignEntityFromString :: Text -> ForeignEntitySpec
foreignEntityFromString txt
  | txt == "dynamic" = ForeignEntityDynamic
  | txt == "wrapper" = ForeignEntityWrapper
  | txt == "static" = ForeignEntityStatic Nothing
  | Just rest <- T.stripPrefix "static " txt = ForeignEntityStatic (Just rest)
  | txt == "&" = ForeignEntityAddress Nothing
  | Just rest <- T.stripPrefix "&" txt = ForeignEntityAddress (Just rest)
  | otherwise = ForeignEntityNamed txt

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
  name <- identifierTextParser
  pats <- MP.many simplePatternParser
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
                matchPats = pats,
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

doExprParser :: TokParser Expr
doExprParser = withSpan $ do
  keywordTok TkKeywordDo
  stmts <- bracedStmtListParser doStmtParser
  pure (`EDo` stmts)

bracedStmtListParser :: TokParser a -> TokParser [a]
bracedStmtListParser stmtParser = do
  symbolLikeTok "{"
  _ <- MP.many (symbolLikeTok ";")
  stmts <- stmtParser `MP.sepBy` symbolLikeTok ";"
  _ <- MP.many (symbolLikeTok ";")
  symbolLikeTok "}"
  pure stmts

doStmtParser :: TokParser DoStmt
doStmtParser = MP.try doBindStmtParser <|> doExprStmtParser

doBindStmtParser :: TokParser DoStmt
doBindStmtParser = withSpan $ do
  pat <- simplePatternParser
  operatorLikeTok "<-"
  expr <- exprParser
  pure (\span' -> DoBind span' pat expr)

doExprStmtParser :: TokParser DoStmt
doExprStmtParser = withSpan $ do
  expr <- exprParser
  pure (`DoExpr` expr)

infixExprParser :: TokParser Expr
infixExprParser = do
  lhs <- appExprParser
  rest <- MP.many ((,) <$> infixOperatorParser <*> appExprParser)
  pure (foldl buildInfix lhs rest)

buildInfix :: Expr -> (Text, Expr) -> Expr
buildInfix lhs (op, rhs) =
  EInfix (mergeSourceSpans (exprSourceSpan lhs) (exprSourceSpan rhs)) lhs op rhs

infixOperatorParser :: TokParser Text
infixOperatorParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkOperator op
        | op /= "=" && op /= "::" && op /= "->" -> Just op
      _ -> Nothing

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

patternParser :: TokParser Pattern
patternParser = withSpan $ do
  first <- identifierTextParser
  rest <- MP.many identifierTextParser
  pure $ \span' ->
    case rest of
      [] -> PVar span' first
      _ -> PCon span' first (map (PVar span') rest)

rhsParser :: TokParser Rhs
rhsParser = withSpan $ do
  operatorLikeTok "->"
  body <- exprParser
  pure (`UnguardedRhs` body)

caseAltParser :: TokParser CaseAlt
caseAltParser = withSpan $ do
  pat <- patternParser
  rhs <- rhsParser
  pure $ \span' ->
    CaseAlt
      { caseAltSpan = span',
        caseAltPattern = pat,
        caseAltRhs = rhs
      }

caseExprParser :: TokParser Expr
caseExprParser = withSpan $ do
  keywordTok TkKeywordCase
  scrutinee <- exprParser
  keywordTok TkKeywordOf
  alts <- bracedAlts <|> plainAlts
  pure $ \span' -> ECase span' scrutinee alts
  where
    plainAlts = MP.some (caseAltParser <* MP.many (symbolLikeTok ";"))
    bracedAlts = do
      symbolLikeTok "{"
      parsed <- plainAlts
      symbolLikeTok "}"
      pure parsed

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
  name <- identifierTextParser
  pure (`EVar` name)

simplePatternParser :: TokParser Pattern
simplePatternParser = withSpan $ do
  name <- identifierTextParser
  pure (`PVar` name)

typeParser :: TokParser Type
typeParser = typeFunParser

typeFunParser :: TokParser Type
typeFunParser = do
  lhs <- typeAppParser
  mRhs <- MP.optional (operatorLikeTok "->" *> typeFunParser)
  pure $
    case mRhs of
      Just rhs -> TFun (mergeSourceSpans (typeSourceSpan lhs) (typeSourceSpan rhs)) lhs rhs
      Nothing -> lhs

typeAppParser :: TokParser Type
typeAppParser = do
  first <- typeAtomParser
  let appLine =
        case typeSourceSpan first of
          SourceSpan line _ _ _ -> line
          NoSourceSpan -> 1
  rest <- MP.many (sameLineTypeAtomParser appLine)
  pure (foldl buildTypeApp first rest)

buildTypeApp :: Type -> Type -> Type
buildTypeApp lhs rhs =
  TApp (mergeSourceSpans (typeSourceSpan lhs) (typeSourceSpan rhs)) lhs rhs

typeAtomParser :: TokParser Type
typeAtomParser =
  typeListParser
    <|> typeParenOrTupleParser
    <|> typeIdentifierParser

typeIdentifierParser :: TokParser Type
typeIdentifierParser = withSpan $ do
  name <- identifierTextParser
  pure $ \span' ->
    case T.uncons name of
      Just (c, _) | isLower c || c == '_' -> TVar span' name
      _ -> TCon span' name

typeListParser :: TokParser Type
typeListParser = withSpan $ do
  symbolLikeTok "["
  inner <- typeParser
  symbolLikeTok "]"
  pure (`TList` inner)

typeParenOrTupleParser :: TokParser Type
typeParenOrTupleParser = withSpan $ do
  symbolLikeTok "("
  mClosed <- MP.optional (symbolLikeTok ")")
  case mClosed of
    Just () -> pure (`TTuple` [])
    Nothing -> do
      first <- typeParser
      mComma <- MP.optional (symbolLikeTok ",")
      case mComma of
        Nothing -> do
          symbolLikeTok ")"
          pure (`TParen` first)
        Just () -> do
          second <- typeParser
          more <- MP.many (symbolLikeTok "," *> typeParser)
          symbolLikeTok ")"
          pure (`TTuple` (first : second : more))

sameLineTypeAtomParser :: Int -> TokParser Type
sameLineTypeAtomParser expectedLine = do
  nextTok <- lookAhead anySingle
  case lexTokenSpan nextTok of
    SourceSpan line _ _ _ | line == expectedLine -> typeAtomParser
    _ -> fail "line break"

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
moduleNameParser = identifierTextParser

identifierTextParser :: TokParser Text
identifierTextParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing

identifierExact :: Text -> TokParser ()
identifierExact expected =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident
        | ident == expected -> Just ()
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

typeSourceSpan :: Type -> SourceSpan
typeSourceSpan ty =
  case ty of
    TVar span' _ -> span'
    TCon span' _ -> span'
    TQuasiQuote span' _ _ -> span'
    TApp span' _ _ -> span'
    TFun span' _ _ -> span'
    TTuple span' _ -> span'
    TList span' _ -> span'
    TParen span' _ -> span'
    TContext span' _ _ -> span'

mergeSourceSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSourceSpans left right =
  case (left, right) of
    (SourceSpan l1 c1 _ _, SourceSpan _ _ l2 c2) -> SourceSpan l1 c1 l2 c2
    (NoSourceSpan, span') -> span'
    (span', NoSourceSpan) -> span'
