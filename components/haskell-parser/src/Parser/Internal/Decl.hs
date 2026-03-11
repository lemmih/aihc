{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Decl
  ( declParser,
    importDeclParser,
    moduleHeaderParser,
    languagePragmaParser,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Parser.Internal.Common
import Parser.Internal.Expr (exprParser, simplePatternParser, typeParser)
import Parser.Lexer (LexTokenKind (..), lexTokenKind)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

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
  isQualified <-
    MP.option False (keywordTok TkKeywordQualified >> pure True)
  importedModule <- moduleNameParser
  importAlias <- MP.optional (keywordTok TkKeywordAs *> moduleNameParser)
  importSpec <- MP.optional importSpecParser
  pure $ \span' ->
    ImportDecl
      { importDeclSpan = span',
        importDeclQualified = isQualified,
        importDeclModule = importedModule,
        importDeclAs = importAlias,
        importDeclSpec = importSpec
      }

importSpecParser :: TokParser ImportSpec
importSpecParser = withSpan $ do
  isHiding <-
    MP.option False (keywordTok TkKeywordHiding >> pure True)
  symbolLikeTok "("
  items <- importItemParser `MP.sepBy` symbolLikeTok ","
  symbolLikeTok ")"
  pure $ \span' ->
    ImportSpec
      { importSpecSpan = span',
        importSpecHiding = isHiding,
        importSpecItems = items
      }

importItemParser :: TokParser ImportItem
importItemParser = withSpan $ do
  itemName <- identifierTextParser
  pure (`ImportItemVar` itemName)

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
  (keywordTok TkKeywordImport >> pure ForeignImport)
    <|> (identifierExact "export" >> pure ForeignExport)

callConvParser :: TokParser CallConv
callConvParser =
  (identifierExact "ccall" >> pure CCall)
    <|> (identifierExact "stdcall" >> pure StdCall)

foreignSafetyParser :: TokParser ForeignSafety
foreignSafetyParser =
  (identifierExact "safe" >> pure Safe)
    <|> (identifierExact "unsafe" >> pure Unsafe)

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
