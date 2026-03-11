{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Common
  ( TokParser,
    keywordTok,
    symbolLikeTok,
    operatorLikeTok,
    tokenSatisfy,
    moduleNameParser,
    identifierTextParser,
    identifierExact,
    withSpan,
    sourceSpanFromPositions,
    exprSourceSpan,
    typeSourceSpan,
    mergeSourceSpans,
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Parser.Ast
import Parser.Lexer (LexToken (..), LexTokenKind (..))
import Parser.Types (TokStream)
import Text.Megaparsec (Parsec, anySingle, lookAhead)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec Void TokStream

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
    ELambdaCase span' _ -> span'
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
