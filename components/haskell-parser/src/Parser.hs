{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
    errorBundlePretty,
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Parser.Ast (Expr (..), Module, SourceSpan (..))
import Parser.Lexer (LexToken (..), LexTokenKind (..), lexTokens)
import Parser.Types
import Text.Megaparsec (Parsec, anySingle, fancyFailure, lookAhead, runParser, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec Void TokStream

exprParser :: TokParser Expr
exprParser = ifExprParser <|> varExprParser

moduleParser :: TokParser Module
moduleParser = fancyFailure (Set.singleton (ErrorFail ("not implemented" :: String)))

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
  case runParser moduleParser "" (TokStream (lexTokens input)) of
    Left bundle -> ParseErr bundle
    Right m -> ParseOk m

-- | Render parse error bundles.
errorBundlePretty :: ParseErrorBundle -> String
errorBundlePretty = show

ifExprParser :: TokParser Expr
ifExprParser = withSpan $ do
  keywordLikeTok "if"
  cond <- exprParser
  keywordLikeTok "then"
  yes <- exprParser
  keywordLikeTok "else"
  no <- exprParser
  pure (\span' -> EIf span' cond yes no)

varExprParser :: TokParser Expr
varExprParser = withSpan $ do
  name <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident | ident `notElem` exprReservedKeywords -> Just ident
      _ -> Nothing
  pure (`EVar` name)

keywordLikeTok :: Text -> TokParser ()
keywordLikeTok expected =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkKeyword kw
        | kw == expected -> Just ()
      TkIdentifier kw
        | kw == expected -> Just ()
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

exprReservedKeywords :: [Text]
exprReservedKeywords = ["if", "then", "else"] <> lexerReservedKeywords

lexerReservedKeywords :: [Text]
lexerReservedKeywords =
  [ "module",
    "where",
    "import",
    "qualified",
    "as",
    "hiding"
  ]
