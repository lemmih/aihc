{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Expr
  ( exprParser,
    simplePatternParser,
    patternParser,
    typeParser,
    typeAtomParser,
  )
where

import Control.Monad (guard)
import Data.Char (isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Parser.Internal.Common
import Parser.Lexer (LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
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
exprCoreParser = MP.try negateExprParser <|> doExprParser <|> ifExprParser <|> caseExprParser <|> lambdaExprParser <|> letExprParser <|> infixExprParser

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
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (i, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> EInt span' n repr)

intBaseExprParser :: TokParser Expr
intBaseExprParser = withSpan $ do
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (i, txt)
      _ -> Nothing
  pure (\span' -> EIntBase span' n repr)

floatExprParser :: TokParser Expr
floatExprParser = withSpan $ do
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (x, txt)
      _ -> Nothing
  pure (\span' -> EFloat span' n repr)

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

negateExprParser :: TokParser Expr
negateExprParser = withSpan $ do
  operatorLikeTok "-"
  inner <- atomExprParser
  pure (`ENegate` inner)

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
  MP.try strictPatternParser
    <|> MP.try irrefutablePatternParser
    <|> MP.try negativeLiteralPatternParser
    <|> wildcardPatternParser
    <|> literalPatternParser
    <|> listPatternParser
    <|> parenOrTuplePatternParser
    <|> varOrConPatternParser

strictPatternParser :: TokParser Pattern
strictPatternParser = withSpan $ do
  operatorLikeTok "!"
  inner <- patternAtomParser
  pure (`PStrict` inner)

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
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (i, lexTokenText tok)
      _ -> Nothing
  pure (\span' -> LitInt span' n repr)

intBaseLiteralParser :: TokParser Literal
intBaseLiteralParser = withSpan $ do
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (i, txt)
      _ -> Nothing
  pure (\span' -> LitIntBase span' n repr)

floatLiteralParser :: TokParser Literal
floatLiteralParser = withSpan $ do
  (n, repr) <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (x, txt)
      _ -> Nothing
  pure (\span' -> LitFloat span' n repr)

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
rhsParser = MP.try guardedRhssParser <|> unguardedRhsParser

unguardedRhsParser :: TokParser Rhs
unguardedRhsParser = withSpan $ do
  operatorLikeTok "->"
  body <- exprParser
  pure (`UnguardedRhs` body)

guardedRhssParser :: TokParser Rhs
guardedRhssParser = withSpan $ do
  grhss <- MP.some guardedRhsParser
  pure (`GuardedRhss` grhss)

guardedRhsParser :: TokParser GuardedRhs
guardedRhsParser = withSpan $ do
  operatorLikeTok "|"
  guards <- exprParser `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok "->"
  body <- exprParser
  pure $ \span' ->
    GuardedRhs
      { guardedRhsSpan = span',
        guardedRhsGuards = guards,
        guardedRhsBody = body
      }

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
      -- Try to parse as tuple section first (e.g., "(,1)" or "(1,)")
      -- If that fails, fall back to regular tuple/paren parsing
      (MP.try parseTupleSection >>= \values -> pure (`ETupleSection` values))
        MP.<|> do
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

parseTupleSection :: TokParser [Maybe Expr]
parseTupleSection = do
  first <- MP.optional exprParser
  _ <- symbolLikeTok ","
  rest <- parseTupleSectionTail
  MP.try $
    do
      symbolLikeTok ")"
      let vals = first : rest
      let hasMissing = any isNothing vals
      if hasMissing && length vals > 1
        then pure vals
        else fail "not a tuple section"

parseTupleSectionTail :: TokParser [Maybe Expr]
parseTupleSectionTail = do
  mComma <- MP.optional (symbolLikeTok ",")
  case mComma of
    Nothing -> pure []
    Just () -> do
      mExpr <- MP.optional exprParser
      rest <- parseTupleSectionTail
      pure (mExpr : rest)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

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
  lambdaCaseParser <|> lambdaPatsParser
  where
    lambdaCaseParser = do
      keywordTok TkKeywordCase
      alts <- bracedAlts <|> plainAlts
      pure (`ELambdaCase` alts)

    lambdaPatsParser = do
      pats <- MP.some patternParser
      operatorLikeTok "->"
      body <- exprParser
      pure (\span' -> ELambdaPats span' pats body)

    plainAlts = MP.some (caseAltParser <* MP.many (symbolLikeTok ";"))
    bracedAlts = do
      symbolLikeTok "{"
      parsed <- plainAlts
      symbolLikeTok "}"
      pure parsed

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
localDeclParser = MP.try localTypeSigDeclParser <|> MP.try localFunctionDeclParser <|> localPatternDeclParser

localTypeSigDeclParser :: TokParser Decl
localTypeSigDeclParser = withSpan $ do
  names <- identifierTextParser `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok "::"
  ty <- typeParser
  guard (hasExplicitForall ty)
  pure (\span' -> DeclTypeSig span' names ty)

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
    PStrict span' _ -> span'
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
typeParser = MP.try forallTypeParser <|> typeFunParser

forallTypeParser :: TokParser Type
forallTypeParser = withSpan $ do
  identifierExact "forall"
  binders <- MP.some identifierTextParser
  operatorLikeTok "."
  inner <- MP.try contextTypeParser <|> typeFunParser
  pure (\span' -> TForall span' binders inner)

contextTypeParser :: TokParser Type
contextTypeParser = do
  constraints <- constraintsParser
  operatorLikeTok "=>"
  inner <- typeFunParser
  pure (TContext (mergeSourceSpans (constraintSpanHead constraints) (typeSourceSpan inner)) constraints inner)

constraintSpanHead :: [Constraint] -> SourceSpan
constraintSpanHead constraints =
  case constraints of
    c : _ -> constraintSpan c
    [] -> NoSourceSpan

constraintsParser :: TokParser [Constraint]
constraintsParser =
  MP.try parenthesizedConstraintsParser <|> fmap pure constraintParser

parenthesizedConstraintsParser :: TokParser [Constraint]
parenthesizedConstraintsParser = do
  symbolLikeTok "("
  cs <- constraintParser `MP.sepBy` symbolLikeTok ","
  symbolLikeTok ")"
  pure cs

constraintParser :: TokParser Constraint
constraintParser = withSpan $ do
  cls <- identifierTextParser
  args <- MP.many typeAtomParser
  pure $ \span' ->
    Constraint
      { constraintSpan = span',
        constraintClass = cls,
        constraintArgs = args,
        constraintParen = False
      }

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

hasExplicitForall :: Type -> Bool
hasExplicitForall ty =
  case ty of
    TForall {} -> True
    TApp _ f x -> hasExplicitForall f || hasExplicitForall x
    TFun _ a b -> hasExplicitForall a || hasExplicitForall b
    TTuple _ elems -> any hasExplicitForall elems
    TList _ inner -> hasExplicitForall inner
    TParen _ inner -> hasExplicitForall inner
    TContext _ constraints inner -> any constraintHasForall constraints || hasExplicitForall inner
    _ -> False

constraintHasForall :: Constraint -> Bool
constraintHasForall constraint = any hasExplicitForall (constraintArgs constraint)
