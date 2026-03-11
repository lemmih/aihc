{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Expr
  ( exprParser,
    simplePatternParser,
    patternParser,
    typeParser,
  )
where

import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Parser.Internal.Common
import Parser.Lexer (LexTokenKind (..), lexTokenKind, lexTokenSpan)
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import qualified Text.Megaparsec as MP

exprParser :: TokParser Expr
exprParser = doExprParser <|> ifExprParser <|> caseExprParser <|> infixExprParser

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
