{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Decl
  ( declParser,
    importDeclParser,
    moduleHeaderParser,
    languagePragmaParser,
  )
where

import Control.Monad (when)
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Parser.Internal.Common
import Parser.Internal.Expr (exprParser, simplePatternParser, typeAtomParser, typeParser)
import Parser.Lexer (LexTokenKind (..), lexTokenKind)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP

languagePragmaParser :: TokParser [ExtensionSetting]
languagePragmaParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkPragmaLanguage names -> Just names
      _ -> Nothing

moduleHeaderParser :: TokParser (Text, Maybe WarningText, Maybe [ExportSpec])
moduleHeaderParser = do
  keywordTok TkKeywordModule
  name <- moduleNameParser
  mWarning <- MP.optional warningTextParser
  exports <- MP.optional exportSpecListParser
  keywordTok TkKeywordWhere
  pure (name, mWarning, exports)

warningTextParser :: TokParser WarningText
warningTextParser =
  withSpan $
    tokenSatisfy $ \tok ->
      case lexTokenKind tok of
        TkPragmaWarning msg -> Just (`WarnText` msg)
        TkPragmaDeprecated msg -> Just (`DeprText` msg)
        _ -> Nothing

exportSpecListParser :: TokParser [ExportSpec]
exportSpecListParser = do
  symbolLikeTok "("
  specs <- exportSpecParser `MP.sepBy` symbolLikeTok ","
  symbolLikeTok ")"
  pure specs

exportSpecParser :: TokParser ExportSpec
exportSpecParser =
  withSpan $
    MP.try exportModuleParser <|> exportNameParser

exportModuleParser :: TokParser (SourceSpan -> ExportSpec)
exportModuleParser = do
  keywordTok TkKeywordModule
  modName <- moduleNameParser
  pure (`ExportModule` modName)

exportNameParser :: TokParser (SourceSpan -> ExportSpec)
exportNameParser = do
  name <- identifierTextParser
  members <- MP.optional exportMembersParser
  pure $ \span' ->
    case members of
      Just Nothing -> ExportAll span' name
      Just (Just names) -> ExportWith span' name names
      Nothing
        | isTypeName name -> ExportAbs span' name
        | otherwise -> ExportVar span' name

exportMembersParser :: TokParser (Maybe [Text])
exportMembersParser = do
  symbolLikeTok "("
  members <-
    (symbolLikeTok ".." >> pure Nothing)
      <|> (Just <$> (identifierTextParser `MP.sepBy` symbolLikeTok ","))
  symbolLikeTok ")"
  pure members

isTypeName :: Text -> Bool
isTypeName txt =
  case T.uncons txt of
    Just (c, _) -> isUpper c
    Nothing -> False

importDeclParser :: TokParser ImportDecl
importDeclParser = withSpan $ do
  keywordTok TkKeywordImport
  preQualified <-
    MP.option False (keywordTok TkKeywordQualified >> pure True)
  importedLevel <- MP.optional importLevelParser
  importedPackage <- MP.optional packageNameParser
  importedModule <- moduleNameParser
  postQualified <-
    MP.option False (keywordTok TkKeywordQualified >> pure True)
  when (preQualified && postQualified) $
    fail "import declaration cannot contain 'qualified' both before and after the module name"
  importAlias <- MP.optional (keywordTok TkKeywordAs *> moduleNameParser)
  importSpec <- MP.optional importSpecParser
  let isQualified = preQualified || postQualified
  pure $ \span' ->
    ImportDecl
      { importDeclSpan = span',
        importDeclLevel = importedLevel,
        importDeclPackage = importedPackage,
        importDeclQualified = isQualified,
        importDeclQualifiedPost = postQualified,
        importDeclModule = importedModule,
        importDeclAs = importAlias,
        importDeclSpec = importSpec
      }

importLevelParser :: TokParser ImportLevel
importLevelParser =
  (identifierExact "quote" >> pure ImportLevelQuote)
    <|> (identifierExact "splice" >> pure ImportLevelSplice)

packageNameParser :: TokParser Text
packageNameParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkString txt -> Just txt
      _ -> Nothing

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
    <|> MP.try newtypeDeclParser
    <|> MP.try classDeclParser
    <|> MP.try instanceDeclParser
    <|> dataDeclParser
    <|> valueDeclParser

typeSigDeclParser :: TokParser Decl
typeSigDeclParser = withSpan $ do
  names <- identifierTextParser `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok "::"
  ty <- typeParser
  pure (\span' -> DeclTypeSig span' names ty)

classDeclParser :: TokParser Decl
classDeclParser = withSpan $ do
  identifierExact "class"
  context <- MP.optional (MP.try (declContextParser <* operatorLikeTok "=>"))
  className <- identifierTextParser
  classParams <- MP.some typeParamParser
  keywordTok TkKeywordWhere
  items <- classItemsBracedParser <|> classItemsPlainParser
  pure $ \span' ->
    DeclClass
      span'
      ClassDecl
        { classDeclSpan = span',
          classDeclContext = fromMaybe [] context,
          classDeclName = className,
          classDeclParams = classParams,
          classDeclItems = items
        }

classItemsPlainParser :: TokParser [ClassDeclItem]
classItemsPlainParser = MP.some (MP.try (classDeclItemParser <* MP.many (symbolLikeTok ";")))

classItemsBracedParser :: TokParser [ClassDeclItem]
classItemsBracedParser = do
  symbolLikeTok "{"
  _ <- MP.many (symbolLikeTok ";")
  items <- classDeclItemParser `MP.sepBy1` symbolLikeTok ";"
  _ <- MP.many (symbolLikeTok ";")
  symbolLikeTok "}"
  pure items

classDeclItemParser :: TokParser ClassDeclItem
classDeclItemParser = classTypeSigItemParser

classTypeSigItemParser :: TokParser ClassDeclItem
classTypeSigItemParser = withSpan $ do
  names <- identifierTextParser `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok "::"
  ty <- typeParser
  pure (\span' -> ClassItemTypeSig span' names ty)

instanceDeclParser :: TokParser Decl
instanceDeclParser = withSpan $ do
  identifierExact "instance"
  context <- MP.optional (MP.try (declContextParser <* operatorLikeTok "=>"))
  className <- identifierTextParser
  instanceTypes <- MP.some constraintTypeParser
  keywordTok TkKeywordWhere
  items <- instanceItemsBracedParser <|> instanceItemsPlainParser
  pure $ \span' ->
    DeclInstance
      span'
      InstanceDecl
        { instanceDeclSpan = span',
          instanceDeclContext = fromMaybe [] context,
          instanceDeclClassName = className,
          instanceDeclTypes = instanceTypes,
          instanceDeclItems = items
        }

instanceItemsPlainParser :: TokParser [InstanceDeclItem]
instanceItemsPlainParser = MP.some (MP.try (instanceDeclItemParser <* MP.many (symbolLikeTok ";")))

instanceItemsBracedParser :: TokParser [InstanceDeclItem]
instanceItemsBracedParser = do
  symbolLikeTok "{"
  _ <- MP.many (symbolLikeTok ";")
  items <- instanceDeclItemParser `MP.sepBy1` symbolLikeTok ";"
  _ <- MP.many (symbolLikeTok ";")
  symbolLikeTok "}"
  pure items

instanceDeclItemParser :: TokParser InstanceDeclItem
instanceDeclItemParser = MP.try instanceTypeSigItemParser <|> instanceValueItemParser

instanceTypeSigItemParser :: TokParser InstanceDeclItem
instanceTypeSigItemParser = withSpan $ do
  names <- identifierTextParser `MP.sepBy1` symbolLikeTok ","
  operatorLikeTok "::"
  ty <- typeParser
  pure (\span' -> InstanceItemTypeSig span' names ty)

instanceValueItemParser :: TokParser InstanceDeclItem
instanceValueItemParser = withSpan $ do
  name <- identifierTextParser
  pats <- MP.many simplePatternParser
  operatorLikeTok "="
  rhsExpr <- exprParser
  pure $ \span' ->
    InstanceItemBind
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
  context <- MP.optional (MP.try (declContextParser <* operatorLikeTok "=>"))
  typeName <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  typeParams <- MP.many typeParamParser
  constructors <- MP.optional (operatorLikeTok "=" *> dataConDeclParser `MP.sepBy1` operatorLikeTok "|")
  derivingClauses <- MP.many derivingClauseParser
  pure $ \span' ->
    DeclData
      span'
      DataDecl
        { dataDeclSpan = span',
          dataDeclContext = fromMaybe [] context,
          dataDeclName = typeName,
          dataDeclParams = typeParams,
          dataDeclConstructors = fromMaybe [] constructors,
          dataDeclDeriving = derivingClauses
        }

newtypeDeclParser :: TokParser Decl
newtypeDeclParser = withSpan $ do
  identifierExact "newtype"
  context <- MP.optional (MP.try (declContextParser <* operatorLikeTok "=>"))
  typeName <- tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing
  typeParams <- MP.many typeParamParser
  constructor <- MP.optional (operatorLikeTok "=" *> newtypeConDeclParser)
  derivingClauses <- MP.many derivingClauseParser
  pure $ \span' ->
    DeclNewtype
      span'
      NewtypeDecl
        { newtypeDeclSpan = span',
          newtypeDeclContext = fromMaybe [] context,
          newtypeDeclName = typeName,
          newtypeDeclParams = typeParams,
          newtypeDeclConstructor = constructor,
          newtypeDeclDeriving = derivingClauses
        }

declContextParser :: TokParser [Constraint]
declContextParser =
  MP.try parenContextParser <|> ((: []) <$> constraintParser)

parenContextParser :: TokParser [Constraint]
parenContextParser = do
  symbolLikeTok "("
  constraints <- constraintParser `MP.sepBy` symbolLikeTok ","
  symbolLikeTok ")"
  pure constraints

constraintParser :: TokParser Constraint
constraintParser = withSpan $ do
  className <- identifierTextParser
  args <- MP.many constraintTypeParser
  pure $ \span' ->
    Constraint
      { constraintSpan = span',
        constraintClass = className,
        constraintArgs = args,
        constraintParen = False
      }

constraintTypeParser :: TokParser Type
constraintTypeParser = withSpan $ do
  ident <- identifierTextParser
  pure $ \span' ->
    case T.uncons ident of
      Just (first, _)
        | isUpper first -> TCon span' ident
      _ -> TVar span' ident

typeParamParser :: TokParser Text
typeParamParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident
        | ident /= "deriving" -> Just ident
      _ -> Nothing

derivingClauseParser :: TokParser DerivingClause
derivingClauseParser = do
  identifierExact "deriving"
  strategy <- MP.optional derivingStrategyParser
  classes <- parenClasses <|> singleClass
  pure (DerivingClause strategy classes)
  where
    singleClass = (: []) <$> identifierTextParser
    parenClasses = do
      symbolLikeTok "("
      classes <- identifierTextParser `MP.sepBy` symbolLikeTok ","
      symbolLikeTok ")"
      pure classes

derivingStrategyParser :: TokParser DerivingStrategy
derivingStrategyParser =
  (identifierExact "stock" >> pure DerivingStock)
    <|> (identifierExact "newtype" >> pure DerivingNewtype)
    <|> (identifierExact "anyclass" >> pure DerivingAnyclass)

dataConDeclParser :: TokParser DataConDecl
dataConDeclParser = withSpan $ do
  name <- constructorNameParser
  fields <- MP.many constructorArgParser
  mRecordFields <- MP.optional (symbolLikeTok "{" *> symbolLikeTok "}")
  pure $ \span' ->
    case mRecordFields of
      Just () -> RecordCon span' name []
      Nothing -> PrefixCon span' name fields

newtypeConDeclParser :: TokParser DataConDecl
newtypeConDeclParser = newtypePrefixConDeclParser

newtypePrefixConDeclParser :: TokParser DataConDecl
newtypePrefixConDeclParser = withSpan $ do
  name <- constructorNameParser
  fields <- MP.many constructorArgParser
  pure (\span' -> PrefixCon span' name fields)

constructorArgParser :: TokParser BangType
constructorArgParser = MP.try $ do
  MP.notFollowedBy derivingKeywordParser
  bangTypeParser

derivingKeywordParser :: TokParser ()
derivingKeywordParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident
        | ident == "deriving" -> Just ()
      _ -> Nothing

bangTypeParser :: TokParser BangType
bangTypeParser = withSpan $ do
  strict <- MP.option False (operatorLikeTok "!" >> pure True)
  ty <- typeAtomParser
  pure $ \span' ->
    BangType
      { bangSpan = span',
        bangStrict = strict,
        bangType = ty
      }

constructorNameParser :: TokParser Text
constructorNameParser =
  tokenSatisfy $ \tok ->
    case lexTokenKind tok of
      TkIdentifier ident -> Just ident
      _ -> Nothing

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
