{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Expr
  ( exprParser,
    simplePatternParser,
    patternParser,
    typeParser,
  )
where

import Data.Char (isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Parser.Internal.Common
import Parser.Lexer (LexTokenKind (..), lexTokenKind, lexTokenSpan)
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import qualified Text.Megaparsec as MP

exprParser :: TokParser Expr
exprParser = do
  core <- exprCoreParser
  mWhere <- MP.optional whereClauseParser
  pure $
    case mWhere of
      Just decls -> EWhereDecls (mergeSourceSpans (exprSourceSpan core) (declSpanEnd decls)) core decls
      Nothing -> core

exprCoreParser :: TokParser Expr
exprCoreParser = doExprParser <|> ifExprParser <|> caseExprParser <|> lambdaExprParser <|> letExprParser <|> infixExprParser

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
  pat <- patternParser
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
  rest <- MP.many ((,) <$> infixOperatorParserExcept [] <*> appExprParser)
  pure (foldl buildInfix lhs rest)

buildInfix :: Expr -> (Text, Expr) -> Expr
buildInfix lhs (op, rhs) =
  EInfix (mergeSourceSpans (exprSourceSpan lhs) (exprSourceSpan rhs)) lhs op rhs

infixOperatorParserExcept :: [Text] -> TokParser Text
infixOperatorParserExcept forbidden =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkOperator op
        | op /= "=" && op /= "::" && op /= "->" && op `notElem` forbidden -> Just op
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
  MP.try parenOperatorExprParser
    <|> lambdaExprParser
    <|> letExprParser
    <|> parenExprParser
    <|> listExprParser
    <|> intBaseExprParser
    <|> floatExprParser
    <|> intExprParser
    <|> charExprParser
    <|> stringExprParser
    <|> varExprParser

parenOperatorExprParser :: TokParser Expr
parenOperatorExprParser = withSpan $ do
  symbolLikeTok "("
  op <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkOperator sym -> Just sym
      _ -> Nothing
  symbolLikeTok ")"
  pure (`EVar` op)

patternParser :: TokParser Pattern
patternParser = asPatternParser

asPatternParser :: TokParser Pattern
asPatternParser =
  MP.try
    ( withSpan $ do
        name <- identifierTextParser
        operatorLikeTok "@"
        inner <- patternAtomParser
        pure (\span' -> PAs span' name inner)
    )
    <|> infixPatternParser

infixPatternParser :: TokParser Pattern
infixPatternParser = do
  lhs <- appPatternParser
  rest <- MP.many ((,) <$> conOperatorParser <*> appPatternParser)
  pure (foldl buildInfixPattern lhs rest)

buildInfixPattern :: Pattern -> (Text, Pattern) -> Pattern
buildInfixPattern lhs (op, rhs) =
  PInfix (mergeSourceSpans (patternSourceSpan lhs) (patternSourceSpan rhs)) lhs op rhs

conOperatorParser :: TokParser Text
conOperatorParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkOperator op
        | ":" `T.isPrefixOf` op -> Just op
      _ -> Nothing

appPatternParser :: TokParser Pattern
appPatternParser = do
  first <- patternAtomParser
  if isPatternAppHead first
    then do
      let appLine =
            case patternSourceSpan first of
              SourceSpan line _ _ _ -> line
              NoSourceSpan -> 1
      rest <- MP.many (sameLinePatternAtomParser appLine)
      pure (foldl buildPatternApp first rest)
    else pure first

buildPatternApp :: Pattern -> Pattern -> Pattern
buildPatternApp lhs rhs =
  case lhs of
    PCon lSpan name args -> PCon (mergeSourceSpans lSpan (patternSourceSpan rhs)) name (args <> [rhs])
    PVar lSpan name
      | isConLikeName name -> PCon (mergeSourceSpans lSpan (patternSourceSpan rhs)) name [rhs]
    _ -> lhs

patternAtomParser :: TokParser Pattern
patternAtomParser =
  MP.try irrefutablePatternParser
    <|> MP.try negativeLiteralPatternParser
    <|> wildcardPatternParser
    <|> literalPatternParser
    <|> listPatternParser
    <|> parenOrTuplePatternParser
    <|> varOrConPatternParser

irrefutablePatternParser :: TokParser Pattern
irrefutablePatternParser = withSpan $ do
  operatorLikeTok "~"
  inner <- patternAtomParser
  pure (`PIrrefutable` inner)

negativeLiteralPatternParser :: TokParser Pattern
negativeLiteralPatternParser = withSpan $ do
  operatorLikeTok "-"
  lit <- literalParser
  pure (`PNegLit` lit)

wildcardPatternParser :: TokParser Pattern
wildcardPatternParser = withSpan $ do
  identifierExact "_"
  pure PWildcard

literalPatternParser :: TokParser Pattern
literalPatternParser = withSpan $ do
  lit <- literalParser
  pure (`PLit` lit)

literalParser :: TokParser Literal
literalParser = intLiteralParser <|> intBaseLiteralParser <|> floatLiteralParser <|> charLiteralParser <|> stringLiteralParser

intLiteralParser :: TokParser Literal
intLiteralParser = withSpan $ do
  n <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just i
      _ -> Nothing
  pure (`LitInt` n)

intBaseLiteralParser :: TokParser Literal
intBaseLiteralParser = withSpan $ do
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (i, txt)
      _ -> Nothing
  pure (\span' -> LitIntBase span' n repr)

floatLiteralParser :: TokParser Literal
floatLiteralParser = withSpan $ do
  n <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkFloat x -> Just x
      _ -> Nothing
  pure (`LitFloat` n)

charLiteralParser :: TokParser Literal
charLiteralParser = withSpan $ do
  c <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just x
      _ -> Nothing
  pure (`LitChar` c)

stringLiteralParser :: TokParser Literal
stringLiteralParser = withSpan $ do
  s <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just x
      _ -> Nothing
  pure (`LitString` s)

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
      _ <- MP.many (symbolLikeTok ";")
      parsed <- caseAltParser `MP.sepEndBy` symbolLikeTok ";"
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
  mClosed <- MP.optional (symbolLikeTok ")")
  case mClosed of
    Just () -> pure (`ETuple` [])
    Nothing -> do
      first <- exprParser
      mComma <- MP.optional (symbolLikeTok ",")
      case mComma of
        Nothing -> do
          symbolLikeTok ")"
          pure (`EParen` first)
        Just () -> do
          second <- exprParser
          more <- MP.many (symbolLikeTok "," *> exprParser)
          symbolLikeTok ")"
          pure (`ETuple` (first : second : more))

listExprParser :: TokParser Expr
listExprParser = withSpan $ do
  symbolLikeTok "["
  mClose <- MP.optional (symbolLikeTok "]")
  case mClose of
    Just () -> pure (`EList` [])
    Nothing -> do
      first <- exprParser
      parseListTail first

parseListTail :: Expr -> TokParser (SourceSpan -> Expr)
parseListTail first =
  MP.try listCompTailParser
    <|> MP.try arithFromToTailParser
    <|> MP.try commaTailParser
    <|> singletonTailParser
  where
    listCompTailParser = do
      operatorLikeTok "|"
      quals <- compStmtParser `MP.sepBy1` symbolLikeTok ","
      symbolLikeTok "]"
      pure (\span' -> EListComp span' first quals)

    arithFromToTailParser = do
      symbolLikeTok ".."
      mTo <- MP.optional exprParser
      symbolLikeTok "]"
      pure $ \span' ->
        EArithSeq span' $
          case mTo of
            Nothing -> ArithSeqFrom span' first
            Just toExpr -> ArithSeqFromTo span' first toExpr

    commaTailParser = do
      symbolLikeTok ","
      second <- exprParser
      MP.try (arithFromThenTailParser second) <|> listTailParser second

    arithFromThenTailParser second = do
      symbolLikeTok ".."
      mTo <- MP.optional exprParser
      symbolLikeTok "]"
      pure $ \span' ->
        EArithSeq span' $
          case mTo of
            Nothing -> ArithSeqFromThen span' first second
            Just toExpr -> ArithSeqFromThenTo span' first second toExpr

    listTailParser second = do
      rest <- MP.many (symbolLikeTok "," *> exprParser)
      symbolLikeTok "]"
      pure (\span' -> EList span' (first : second : rest))

    singletonTailParser = do
      symbolLikeTok "]"
      pure (\span' -> EList span' [first])

compStmtParser :: TokParser CompStmt
compStmtParser = MP.try compGenStmtParser <|> compGuardStmtParser

compGenStmtParser :: TokParser CompStmt
compGenStmtParser = withSpan $ do
  pat <- patternParser
  operatorLikeTok "<-"
  expr <- exprParser
  pure (\span' -> CompGen span' pat expr)

lambdaExprParser :: TokParser Expr
lambdaExprParser = withSpan $ do
  operatorLikeTok "\\"
  pats <- MP.some patternParser
  operatorLikeTok "->"
  body <- exprParser
  pure (\span' -> ELambdaPats span' pats body)

letExprParser :: TokParser Expr
letExprParser = withSpan $ do
  keywordTok TkKeywordLet
  decls <- bracedDeclsParser <|> plainDeclsParser
  keywordTok TkKeywordIn
  body <- exprParser
  pure (\span' -> ELetDecls span' decls body)
  where
    plainDeclsParser = MP.some (localDeclParser <* MP.many (symbolLikeTok ";"))
    bracedDeclsParser = do
      symbolLikeTok "{"
      parsed <- plainDeclsParser
      symbolLikeTok "}"
      pure parsed

whereClauseParser :: TokParser [Decl]
whereClauseParser = do
  keywordTok TkKeywordWhere
  bracedDeclsParser <|> plainDeclsParser
  where
    plainDeclsParser = MP.some (localDeclParser <* MP.many (symbolLikeTok ";"))
    bracedDeclsParser = do
      symbolLikeTok "{"
      parsed <- plainDeclsParser
      symbolLikeTok "}"
      pure parsed

localDeclParser :: TokParser Decl
localDeclParser = MP.try localFunctionDeclParser <|> localPatternDeclParser

localFunctionDeclParser :: TokParser Decl
localFunctionDeclParser = withSpan $ do
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

localPatternDeclParser :: TokParser Decl
localPatternDeclParser = withSpan $ do
  pat <- patternParser
  operatorLikeTok "="
  rhsExpr <- exprParser
  pure (\span' -> DeclValue span' (PatternBind span' pat (UnguardedRhs span' rhsExpr)))

varOrConPatternParser :: TokParser Pattern
varOrConPatternParser = MP.try recordPatternParser <|> bareVarOrConPatternParser

bareVarOrConPatternParser :: TokParser Pattern
bareVarOrConPatternParser = withSpan $ do
  name <- identifierTextParser
  pure $ \span' ->
    if isConLikeName name
      then PCon span' name []
      else PVar span' name

recordPatternParser :: TokParser Pattern
recordPatternParser = withSpan $ do
  con <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident
        | isConLikeName ident -> Just ident
      _ -> Nothing
  symbolLikeTok "{"
  mClose <- MP.optional (symbolLikeTok "}")
  case mClose of
    Just () -> pure (\span' -> PRecord span' con [])
    Nothing -> do
      fields <- recordFieldPatternParser `MP.sepBy` symbolLikeTok ","
      symbolLikeTok "}"
      pure (\span' -> PRecord span' con fields)

recordFieldPatternParser :: TokParser (Text, Pattern)
recordFieldPatternParser = do
  field <- identifierTextParser
  operatorLikeTok "="
  pat <- patternParser
  pure (field, pat)

listPatternParser :: TokParser Pattern
listPatternParser = withSpan $ do
  symbolLikeTok "["
  elems <- patternParser `MP.sepBy` symbolLikeTok ","
  symbolLikeTok "]"
  pure (`PList` elems)

parenOrTuplePatternParser :: TokParser Pattern
parenOrTuplePatternParser = withSpan $ do
  symbolLikeTok "("
  first <- patternParser
  mComma <- MP.optional (symbolLikeTok ",")
  case mComma of
    Nothing -> do
      symbolLikeTok ")"
      pure (`PParen` first)
    Just () -> do
      second <- patternParser
      more <- MP.many (symbolLikeTok "," *> patternParser)
      symbolLikeTok ")"
      pure (`PTuple` (first : second : more))

sameLinePatternAtomParser :: Int -> TokParser Pattern
sameLinePatternAtomParser expectedLine = do
  nextTok <- lookAhead anySingle
  case lexTokenSpan nextTok of
    SourceSpan line _ _ _ | line == expectedLine -> patternAtomParser
    _ -> fail "line break"

isConLikeName :: Text -> Bool
isConLikeName name =
  case T.uncons name of
    Just (c, _) -> isUpper c
    Nothing -> False

isPatternAppHead :: Pattern -> Bool
isPatternAppHead pat =
  case pat of
    PCon {} -> True
    PVar _ name -> isConLikeName name
    _ -> False

patternSourceSpan :: Pattern -> SourceSpan
patternSourceSpan pat =
  case pat of
    PVar span' _ -> span'
    PWildcard span' -> span'
    PLit span' _ -> span'
    PQuasiQuote span' _ _ -> span'
    PTuple span' _ -> span'
    PList span' _ -> span'
    PCon span' _ _ -> span'
    PInfix span' _ _ _ -> span'
    PView span' _ _ -> span'
    PAs span' _ _ -> span'
    PIrrefutable span' _ -> span'
    PNegLit span' _ -> span'
    PParen span' _ -> span'
    PRecord span' _ _ -> span'

declSpanEnd :: [Decl] -> SourceSpan
declSpanEnd decls =
  case reverse decls of
    [] -> NoSourceSpan
    d : _ -> declSourceSpan d

declSourceSpan :: Decl -> SourceSpan
declSourceSpan decl =
  case decl of
    DeclValue span' _ -> span'
    DeclTypeSig span' _ _ -> span'
    DeclFixity span' _ _ _ -> span'
    DeclTypeSyn span' _ -> span'
    DeclData span' _ -> span'
    DeclNewtype span' _ -> span'
    DeclClass span' _ -> span'
    DeclInstance span' _ -> span'
    DeclDefault span' _ -> span'
    DeclForeign span' _ -> span'

compGuardStmtParser :: TokParser CompStmt
compGuardStmtParser = withSpan $ do
  expr <- exprParser
  pure (`CompGuard` expr)

varExprParser :: TokParser Expr
varExprParser = withSpan $ do
  name <- identifierTextParser
  pure (`EVar` name)

simplePatternParser :: TokParser Pattern
simplePatternParser =
  MP.try
    ( withSpan $ do
        name <- identifierTextParser
        operatorLikeTok "@"
        inner <- patternAtomParser
        pure (\span' -> PAs span' name inner)
    )
    <|> patternAtomParser

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
