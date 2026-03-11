{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
    errorBundlePretty,
  )
where

import Data.Text (Text)
import Parser.Ast (Expr, Module (..))
import Parser.Internal.Common (TokParser, symbolLikeTok, withSpan)
import Parser.Internal.Decl (declParser, importDeclParser, languagePragmaParser, moduleHeaderParser)
import Parser.Internal.Expr (exprParser)
import Parser.Lexer (lexTokens)
import Parser.Types
import Text.Megaparsec (runParser)
import qualified Text.Megaparsec as MP

moduleParser :: TokParser Module
moduleParser = withSpan $ do
  languagePragmas <- MP.many languagePragmaParser
  mHeader <- MP.optional (moduleHeaderParser <* MP.many (symbolLikeTok ";"))
  imports <- MP.many (importDeclParser <* MP.many (symbolLikeTok ";"))
  decls <- MP.some (declParser <* MP.many (symbolLikeTok ";"))
  let (mName, mExports) =
        case mHeader of
          Nothing -> (Nothing, Nothing)
          Just (name, exports) -> (Just name, exports)
  pure $ \span' ->
    Module
      { moduleSpan = span',
        moduleName = mName,
        moduleLanguagePragmas = concat languagePragmas,
        moduleExports = mExports,
        moduleImports = imports,
        moduleDecls = decls
      }

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

errorBundlePretty :: ParseErrorBundle -> String
errorBundlePretty = show
