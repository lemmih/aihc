{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isLower, isSpace, isUpper)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Numeric (readHex, readOct)
import Parser.Ast
import Parser.Types
import Text.Megaparsec
  ( Parsec,
    eof,
    errorOffset,
    many,
    notFollowedBy,
    runParser,
    sepEndBy,
    some,
    try,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type MParser = Parsec Void Text

defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { allowLineComments = True
    }

parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr _cfg input =
  case parseExprText input of
    Right ast -> ParseOk ast
    Left msg ->
      ParseErr
        ParseError
          { offset = 0,
            line = 1,
            col = 1,
            expected = [msg],
            found = if T.null (T.strip input) then Nothing else Just (T.strip input)
          }

parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule cfg input =
  case parseModuleLines cfg input of
    Right ast -> ParseOk ast
    Left err -> ParseErr err

parseModuleLines :: ParserConfig -> Text -> Either ParseError Module
parseModuleLines cfg input = do
  let strippedComments = stripComments cfg input
      sourceLines = zip [1 ..] (T.lines strippedComments)
      noPragmas = filter (not . isLanguagePragma . T.strip . snd) sourceLines
      nonEmpty = filter (not . T.null . T.strip . snd) noPragmas
      compactText = T.strip (T.unlines (map snd noPragmas))
  case nonEmpty of
    [] ->
      Right
        Module
          { moduleName = Nothing,
            moduleExports = Nothing,
            moduleImports = [],
            moduleDecls = []
          }
    ((firstLineNo, firstLine) : rest) ->
      case parseModuleBodyBraces cfg firstLineNo compactText of
        Right modu -> Right modu
        Left _ ->
          case parseModuleHeader (T.strip firstLine) of
            Right (modName, exports) -> do
              (imports, decls) <- parseTopLevelChunks cfg (groupDeclarationChunks rest)
              Right
                Module
                  { moduleName = Just modName,
                    moduleExports = exports,
                    moduleImports = imports,
                    moduleDecls = mergeAdjacentFunctions decls
                  }
            Left _ -> do
              (imports, decls) <- parseTopLevelChunks cfg (groupDeclarationChunks ((firstLineNo, firstLine) : rest))
              Right
                Module
                  { moduleName = Nothing,
                    moduleExports = Nothing,
                    moduleImports = imports,
                    moduleDecls = mergeAdjacentFunctions decls
                  }

parseModuleBodyBraces :: ParserConfig -> Int -> Text -> Either ParseError Module
parseModuleBodyBraces cfg lineNo txt
  | hasOuterBraces txt =
      case splitOuterBraces txt of
        Right (before, inside)
          | T.null (T.strip before) -> do
              let chunks = map (lineNo,) (splitDeclItems inside)
              (imports, decls) <- parseTopLevelChunks cfg chunks
              Right
                Module
                  { moduleName = Nothing,
                    moduleExports = Nothing,
                    moduleImports = imports,
                    moduleDecls = mergeAdjacentFunctions decls
                  }
        _ -> Left (mkModuleParseErr lineNo txt)
  | otherwise = Left (mkModuleParseErr lineNo txt)
  where
    mkModuleParseErr ln raw =
      ParseError
        { offset = 0,
          line = ln,
          col = 1,
          expected = ["module body"],
          found = if T.null (T.strip raw) then Nothing else Just (T.strip raw)
        }

parseModuleHeader :: Text -> Either ParseError (Text, Maybe [ExportSpec])
parseModuleHeader = parseLineWith headerParser
  where
    headerParser = do
      _ <- keyword "module"
      modName <- identifier
      exports <- MP.optional (try exportSpecListParser)
      _ <- keyword "where"
      eof
      pure (modName, exports)

exportSpecListParser :: MParser [ExportSpec]
exportSpecListParser = do
  _ <- symbol "("
  specs <- exportSpecParser `sepEndBy` symbol ","
  _ <- symbol ")"
  pure specs

exportSpecParser :: MParser ExportSpec
exportSpecParser =
  try moduleSpecParser <|> entitySpecParser
  where
    moduleSpecParser = do
      _ <- keyword "module"
      ExportModule <$> identifier

    entitySpecParser = do
      name <- identifierOrOperator
      members <- MP.optional (try exportMembersParser)
      pure $
        case members of
          Nothing
            | isTypeToken name -> ExportAbs name
            | otherwise -> ExportVar name
          Just Nothing -> ExportAll name
          Just (Just xs) -> ExportWith name xs

exportMembersParser :: MParser (Maybe [Text])
exportMembersParser = do
  _ <- symbol "("
  allMembers <- MP.optional (try (symbol ".."))
  case allMembers of
    Just _ -> do
      _ <- symbol ")"
      pure Nothing
    Nothing -> do
      members <- identifierOrOperator `sepEndBy` symbol ","
      _ <- symbol ")"
      pure (Just members)

parseTopLevelChunks :: ParserConfig -> [(Int, Text)] -> Either ParseError ([ImportDecl], [Decl])
parseTopLevelChunks cfg = go [] [] False
  where
    go imports decls seenDecl rows =
      case rows of
        [] -> Right (reverse imports, reverse decls)
        (lineNo, raw) : rest ->
          let txt = T.strip raw
           in if T.null txt
                then go imports decls seenDecl rest
                else
                  if "import " `T.isPrefixOf` txt
                    then
                      if seenDecl
                        then Left (mkTopLevelErr lineNo txt "declaration")
                        else case parseImportDeclText txt of
                          Right imp -> go (imp : imports) decls seenDecl rest
                          Left _ -> Left (mkTopLevelErr lineNo txt "import declaration")
                    else case parseDeclText cfg txt of
                      Right decl -> go imports (decl : decls) True rest
                      Left _ -> Left (mkTopLevelErr lineNo txt "declaration")

    mkTopLevelErr lineNo txt expectedText =
      ParseError
        { offset = 0,
          line = lineNo,
          col = 1,
          expected = [expectedText],
          found = if T.null txt then Nothing else Just txt
        }

parseImportDeclText :: Text -> Either Text ImportDecl
parseImportDeclText txt =
  case parseLineWith importDeclParser txt of
    Right decl -> Right decl
    Left _ -> Left "import declaration"

importDeclParser :: MParser ImportDecl
importDeclParser = do
  _ <- keyword "import"
  qualifiedFlag <- isJust <$> MP.optional (try (keyword "qualified"))
  modName <- identifier
  alias <- MP.optional (try (keyword "as" *> identifier))
  spec <- MP.optional (try importSpecParser)
  eof
  pure
    ImportDecl
      { importDeclQualified = qualifiedFlag,
        importDeclModule = modName,
        importDeclAs = alias,
        importDeclSpec = spec
      }

importSpecParser :: MParser ImportSpec
importSpecParser = do
  hidingFlag <- isJust <$> MP.optional (try (keyword "hiding"))
  _ <- symbol "("
  items <- importItemParser `sepEndBy` symbol ","
  _ <- symbol ")"
  pure
    ImportSpec
      { importSpecHiding = hidingFlag,
        importSpecItems = items
      }

importItemParser :: MParser ImportItem
importItemParser = do
  name <- identifierOrOperator
  members <- MP.optional (try exportMembersParser)
  pure $
    case members of
      Nothing
        | isTypeToken name -> ImportItemAbs name
        | otherwise -> ImportItemVar name
      Just Nothing -> ImportItemAll name
      Just (Just xs) -> ImportItemWith name xs

parseDeclarationChunk :: ParserConfig -> Int -> Text -> Either ParseError Decl
parseDeclarationChunk cfg lineNo raw =
  let txt = T.strip raw
   in case parseDeclText cfg txt of
        Right decl -> Right decl
        Left expectedText ->
          Left
            ParseError
              { offset = 0,
                line = lineNo,
                col = 1,
                expected = [expectedText],
                found = if T.null txt then Nothing else Just txt
              }

parseDeclText :: ParserConfig -> Text -> Either Text Decl
parseDeclText cfg txt
  | "foreign import" `T.isPrefixOf` txt = parseForeignDeclText ForeignImport txt
  | "foreign export" `T.isPrefixOf` txt = parseForeignDeclText ForeignExport txt
  | "data " `T.isPrefixOf` txt = parseDataDeclText txt
  | "newtype " `T.isPrefixOf` txt = parseNewtypeDeclText txt
  | "type " `T.isPrefixOf` txt = parseTypeSynonymDecl txt
  | "class " `T.isPrefixOf` txt = parseClassDeclText cfg txt
  | "instance " `T.isPrefixOf` txt = parseInstanceDeclText cfg txt
  | "default " `T.isPrefixOf` txt = parseDefaultDeclText txt
  | isFixityDecl txt = parseFixityDeclText txt
  | hasTopLevelEquals txt = parseEquationDecl cfg txt
  | hasTopLevelTypeSig txt = parseTypeSignatureDeclText txt
  | otherwise = Left "declaration"

parseTypeSignatureDeclText :: Text -> Either Text Decl
parseTypeSignatureDeclText txt = do
  (lhs, rhs) <- splitTopLevelOnce "::" txt
  let names = filter (not . T.null) (map (stripParens . T.strip) (splitTopLevel ',' lhs))
  if null names || not (all isValueName names)
    then Left "type signature"
    else do
      ty <- parseTypeText (T.strip rhs)
      Right (DeclTypeSig names ty)

parseFixityDeclText :: Text -> Either Text Decl
parseFixityDeclText txt =
  case T.words txt of
    assocTxt : rest -> do
      assoc <- parseAssoc assocTxt
      case rest of
        [] -> Left "fixity declaration"
        (x : xs)
          | T.all isDigit x ->
              case xs of
                [] -> Left "fixity declaration"
                _ -> Right (DeclFixity assoc (Just (read (T.unpack x))) (map stripParens xs))
          | otherwise -> Right (DeclFixity assoc Nothing (map stripParens rest))
    _ -> Left "fixity declaration"
  where
    parseAssoc token =
      case token of
        "infix" -> Right Infix
        "infixl" -> Right InfixL
        "infixr" -> Right InfixR
        _ -> Left "fixity declaration"

parseTypeSynonymDecl :: Text -> Either Text Decl
parseTypeSynonymDecl txt = do
  raw <- maybe (Left "type declaration") Right (T.stripPrefix "type" (T.stripStart txt))
  (lhs, rhs) <- splitTopLevelOnce "=" raw
  let lhsToks = splitTopLevelWords lhs
  case lhsToks of
    [] -> Left "type declaration"
    (name : params)
      | isTypeToken name -> do
          ty <- parseTypeText rhs
          Right
            ( DeclTypeSyn
                TypeSynDecl
                  { typeSynName = name,
                    typeSynParams = params,
                    typeSynBody = ty
                  }
            )
      | otherwise -> Left "type declaration"

parseDataDeclText :: Text -> Either Text Decl
parseDataDeclText txt = do
  raw <- maybe (Left "data declaration") Right (T.stripPrefix "data" (T.stripStart txt))
  parseDataLike False (T.strip raw)

parseNewtypeDeclText :: Text -> Either Text Decl
parseNewtypeDeclText txt = do
  raw <- maybe (Left "newtype declaration") Right (T.stripPrefix "newtype" (T.stripStart txt))
  parseDataLike True (T.strip raw)

parseDataLike :: Bool -> Text -> Either Text Decl
parseDataLike isNewtype raw = do
  let (noDeriving, derivingClauseText) = splitDerivingClause raw
  (lhs, rhsMaybe) <-
    case splitTopLevelMaybe "=" noDeriving of
      Just (a, b) -> Right (a, Just b)
      Nothing -> Right (noDeriving, Nothing)
  (ctx, headText) <- parseContextPrefix lhs
  let toks = splitTopLevelWords headText
  case toks of
    [] -> Left "data declaration"
    (typeName : params)
      | isTypeToken typeName -> do
          ctors <-
            case rhsMaybe of
              Nothing -> Right []
              Just rhsText -> parseConstructorsText rhsText
          derivingClause <- traverse parseDerivingClauseText derivingClauseText
          if isNewtype
            then
              Right
                ( DeclNewtype
                    NewtypeDecl
                      { newtypeDeclContext = ctx,
                        newtypeDeclName = typeName,
                        newtypeDeclParams = params,
                        newtypeDeclConstructor =
                          case ctors of
                            [] -> Nothing
                            (firstCtor : _) -> Just firstCtor,
                        newtypeDeclDeriving = derivingClause
                      }
                )
            else
              Right
                ( DeclData
                    DataDecl
                      { dataDeclContext = ctx,
                        dataDeclName = typeName,
                        dataDeclParams = params,
                        dataDeclConstructors = ctors,
                        dataDeclDeriving = derivingClause
                      }
                )
      | otherwise -> Left "data declaration"

splitDerivingClause :: Text -> (Text, Maybe Text)
splitDerivingClause txt =
  case splitTopLevelMaybe " deriving" txt of
    Nothing -> (T.strip txt, Nothing)
    Just (before, after) -> (T.strip before, Just (T.strip after))

parseDerivingClauseText :: Text -> Either Text DerivingClause
parseDerivingClauseText txt
  | T.null txt = Right (DerivingClause [])
  | otherwise =
      let stripped = T.strip txt
       in if stripped == "()"
            then Right (DerivingClause [])
            else
              let inner =
                    if hasOuterParens stripped
                      then T.drop 1 (T.dropEnd 1 stripped)
                      else stripped
                  classes = filter (not . T.null) (map (stripParens . T.strip) (splitTopLevel ',' inner))
               in Right (DerivingClause classes)

parseConstructorsText :: Text -> Either Text [DataConDecl]
parseConstructorsText rhs =
  traverse parseConstructorText (filter (not . T.null) (map T.strip (splitTopLevel '|' rhs)))

parseConstructorText :: Text -> Either Text DataConDecl
parseConstructorText txt
  | T.null txt = Left "constructor"
  | "{" `T.isInfixOf` txt = parseRecordConstructor txt
  | otherwise =
      case parseInfixConstructor txt of
        Just ctor -> Right ctor
        Nothing -> parsePrefixConstructor txt

parsePrefixConstructor :: Text -> Either Text DataConDecl
parsePrefixConstructor txt =
  case splitTopLevelWords txt of
    [] -> Left "constructor"
    (name : args)
      | isTypeToken name || isOperatorToken name -> do
          bangTypes <- traverse parseBangTypeText args
          Right (PrefixCon (stripParens name) bangTypes)
      | otherwise -> Left "constructor"

parseInfixConstructor :: Text -> Maybe DataConDecl
parseInfixConstructor txt =
  case splitTopLevelWords txt of
    [lhs, op, rhs]
      | isOperatorToken op -> do
          lhsTy <- either (const Nothing) Just (parseBangTypeText lhs)
          rhsTy <- either (const Nothing) Just (parseBangTypeText rhs)
          Just (InfixCon lhsTy op rhsTy)
    _ -> Nothing

parseRecordConstructor :: Text -> Either Text DataConDecl
parseRecordConstructor txt = do
  (beforeBrace, insideBrace) <- splitOuterBraces txt
  let ctorName = T.strip beforeBrace
  if T.null ctorName
    then Left "record constructor"
    else do
      fields <-
        if T.null (T.strip insideBrace)
          then Right []
          else
            let rawChunks = filter (not . T.null) (map T.strip (splitTopLevel ',' insideBrace))
                merged = mergeFieldChunks rawChunks
             in traverse parseFieldDeclText merged
      Right (RecordCon ctorName fields)

parseFieldDeclText :: Text -> Either Text FieldDecl
parseFieldDeclText txt = do
  (lhs, rhs) <- splitTopLevelOnce "::" txt
  let names = filter (not . T.null) (map (stripParens . T.strip) (splitTopLevel ',' lhs))
  if null names
    then Left "field declaration"
    else do
      bt <- parseBangTypeText rhs
      Right FieldDecl {fieldNames = names, fieldType = bt}

mergeFieldChunks :: [Text] -> [Text]
mergeFieldChunks chunks = go chunks []
  where
    go remaining acc =
      case remaining of
        [] -> reverse acc
        (c : cs)
          | "::" `T.isInfixOf` c -> go cs (c : acc)
          | otherwise ->
              case cs of
                [] -> reverse (c : acc)
                (next : rest) ->
                  let merged = c <> ", " <> next
                   in go (merged : rest) acc

parseBangTypeText :: Text -> Either Text BangType
parseBangTypeText txt =
  let stripped = T.strip txt
   in if T.null stripped
        then Left "type"
        else
          let (strict, rest) =
                case T.uncons stripped of
                  Just ('!', xs) -> (True, T.strip xs)
                  _ -> (False, stripped)
           in do
                ty <- parseTypeText rest
                Right BangType {bangStrict = strict, bangType = ty}

parseClassDeclText :: ParserConfig -> Text -> Either Text Decl
parseClassDeclText cfg txt = do
  raw <- maybe (Left "class declaration") Right (T.stripPrefix "class" (T.stripStart txt))
  let (headText, bodyText) =
        case splitTopLevelMaybe "where" raw of
          Just (a, b) -> (a, Just b)
          Nothing -> (raw, Nothing)
  (ctx, clsHead) <- parseContextPrefix headText
  let toks = splitTopLevelWords clsHead
  case toks of
    [clsName, param]
      | isTypeToken clsName -> do
          items <- maybe (Right []) (parseClassItems cfg) bodyText
          Right
            ( DeclClass
                ClassDecl
                  { classDeclContext = ctx,
                    classDeclName = clsName,
                    classDeclParam = param,
                    classDeclItems = items
                  }
            )
    _ -> Left "class declaration"

parseClassItems :: ParserConfig -> Text -> Either Text [ClassDeclItem]
parseClassItems cfg txt =
  let body = stripBracesIfAny (T.strip txt)
      itemTexts = splitDeclItems body
   in traverse (parseClassItem cfg) itemTexts

parseClassItem :: ParserConfig -> Text -> Either Text ClassDeclItem
parseClassItem cfg txt
  | T.null (T.strip txt) = Left "class item"
  | hasTopLevelTypeSig txt = do
      decl <- parseTypeSignatureDeclText txt
      case decl of
        DeclTypeSig names ty -> Right (ClassItemTypeSig names ty)
        _ -> Left "class item"
  | isFixityDecl txt = do
      decl <- parseFixityDeclText txt
      case decl of
        DeclFixity assoc prec ops -> Right (ClassItemFixity assoc prec ops)
        _ -> Left "class item"
  | hasTopLevelEquals txt = do
      decl <- parseEquationDecl cfg txt
      case decl of
        DeclValue v -> Right (ClassItemDefault v)
        _ -> Left "class item"
  | otherwise = Left "class item"

parseInstanceDeclText :: ParserConfig -> Text -> Either Text Decl
parseInstanceDeclText cfg txt = do
  raw <- maybe (Left "instance declaration") Right (T.stripPrefix "instance" (T.stripStart txt))
  let (headText, bodyText) =
        case splitTopLevelMaybe "where" raw of
          Just (a, b) -> (a, Just b)
          Nothing -> (raw, Nothing)
  (ctx, instHead) <- parseContextPrefix headText
  let toks = splitTopLevelWords instHead
  case toks of
    [] -> Left "instance declaration"
    (clsName : typeToks)
      | isTypeToken clsName -> do
          tys <- traverse parseTypeText typeToks
          items <- maybe (Right []) (parseInstanceItems cfg) bodyText
          Right
            ( DeclInstance
                InstanceDecl
                  { instanceDeclContext = ctx,
                    instanceDeclClassName = clsName,
                    instanceDeclTypes = tys,
                    instanceDeclItems = items
                  }
            )
      | otherwise -> Left "instance declaration"

parseInstanceItems :: ParserConfig -> Text -> Either Text [InstanceDeclItem]
parseInstanceItems cfg txt =
  let body = stripBracesIfAny (T.strip txt)
      itemTexts = splitDeclItems body
   in traverse (parseInstanceItem cfg) itemTexts

parseInstanceItem :: ParserConfig -> Text -> Either Text InstanceDeclItem
parseInstanceItem cfg txt
  | T.null (T.strip txt) = Left "instance item"
  | hasTopLevelTypeSig txt = do
      decl <- parseTypeSignatureDeclText txt
      case decl of
        DeclTypeSig names ty -> Right (InstanceItemTypeSig names ty)
        _ -> Left "instance item"
  | isFixityDecl txt = do
      decl <- parseFixityDeclText txt
      case decl of
        DeclFixity assoc prec ops -> Right (InstanceItemFixity assoc prec ops)
        _ -> Left "instance item"
  | hasTopLevelEquals txt = do
      decl <- parseEquationDecl cfg txt
      case decl of
        DeclValue v -> Right (InstanceItemBind v)
        _ -> Left "instance item"
  | otherwise = Left "instance item"

parseDefaultDeclText :: Text -> Either Text Decl
parseDefaultDeclText txt = do
  raw <- maybe (Left "default declaration") Right (T.stripPrefix "default" (T.stripStart txt))
  let inner = stripParens (T.strip raw)
  tys <- traverse parseTypeText (filter (not . T.null) (map T.strip (splitTopLevel ',' inner)))
  if null tys
    then Left "default declaration"
    else Right (DeclDefault tys)

parseForeignDeclText :: ForeignDirection -> Text -> Either Text Decl
parseForeignDeclText direction txt =
  case parseLineWith (foreignDeclParser direction) txt of
    Right decl -> Right decl
    Left _ -> Left "foreign declaration"

foreignDeclParser :: ForeignDirection -> MParser Decl
foreignDeclParser direction = do
  _ <- keyword "foreign"
  _ <-
    case direction of
      ForeignImport -> keyword "import"
      ForeignExport -> keyword "export"
  callConv <- callConvParser
  safety <-
    case direction of
      ForeignImport -> MP.optional (try safetyParser)
      ForeignExport -> pure Nothing
  entity <- MP.optional (try foreignEntityParser)
  name <- identifierOrOperator
  _ <- symbol "::"
  typeTxt <- T.strip <$> MP.takeRest
  if T.null typeTxt
    then fail "expected foreign type"
    else do
      ty <-
        case parseTypeText typeTxt of
          Right t -> pure t
          Left _ -> fail "foreign type"
      pure
        ( DeclForeign
            ForeignDecl
              { foreignDirection = direction,
                foreignCallConv = callConv,
                foreignSafety = safety,
                foreignEntity = classifyForeignEntitySpec entity,
                foreignName = name,
                foreignType = ty
              }
        )

classifyForeignEntitySpec :: Maybe Text -> ForeignEntitySpec
classifyForeignEntitySpec mEntity =
  case fmap T.strip mEntity of
    Nothing -> ForeignEntityOmitted
    Just "dynamic" -> ForeignEntityDynamic
    Just "wrapper" -> ForeignEntityWrapper
    Just raw
      | "static" `T.isPrefixOf` raw ->
          let namePart = T.strip (fromMaybeText "" (T.stripPrefix "static" raw))
           in if T.null namePart
                then ForeignEntityStatic Nothing
                else ForeignEntityStatic (Just namePart)
      | "&" `T.isPrefixOf` raw ->
          let namePart = T.strip (T.drop 1 raw)
           in if T.null namePart
                then ForeignEntityAddress Nothing
                else ForeignEntityAddress (Just namePart)
      | otherwise -> ForeignEntityNamed raw

parseEquationDecl :: ParserConfig -> Text -> Either Text Decl
parseEquationDecl cfg txt = do
  case parseGuardedEquationDecl cfg txt of
    Right guardedDecl -> Right guardedDecl
    Left _ -> do
      (lhsRaw, rhsRaw) <- splitTopLevelOnce "=" txt
      let lhs = T.strip lhsRaw
          rhs0 = T.strip rhsRaw
      if T.null lhs || T.null rhs0
        then Left "equation declaration"
        else do
          rhsExpr <- parseRhsExpr cfg rhs0
          case parseFunctionLhs lhs of
            Just (name, pats) ->
              Right
                ( DeclValue
                    ( FunctionBind
                        name
                        [ Match
                            { matchPats = pats,
                              matchRhs = UnguardedRhs rhsExpr
                            }
                        ]
                    )
                )
            Nothing -> do
              pat <- parsePatternText lhs
              Right (DeclValue (PatternBind pat (UnguardedRhs rhsExpr)))

parseGuardedEquationDecl :: ParserConfig -> Text -> Either Text Decl
parseGuardedEquationDecl cfg txt = do
  let rows = filter (not . T.null) (map T.strip (T.lines txt))
  case rows of
    [] -> Left "equation declaration"
    (headRow : guardRows)
      | null guardRows -> Left "equation declaration"
      | hasTopLevelEquals headRow -> Left "equation declaration"
      | otherwise ->
          case parseFunctionLhs headRow of
            Nothing -> Left "equation declaration"
            Just (name, pats) -> do
              grhss <- traverse parseGuardRow guardRows
              Right
                ( DeclValue
                    ( FunctionBind
                        name
                        [ Match
                            { matchPats = pats,
                              matchRhs = GuardedRhss grhss
                            }
                        ]
                    )
                )
  where
    parseGuardRow row = do
      guardBody <- maybe (Left "guarded equation") Right (T.stripPrefix "|" row)
      (guardTxt, exprTxt) <- splitTopLevelOnce "=" guardBody
      guardExpr <-
        case parseExpr cfg guardTxt of
          ParseOk expr -> Right expr
          ParseErr _ -> Left "guarded equation"
      bodyExpr <- parseRhsExpr cfg exprTxt
      Right GuardedRhs {guardedRhsGuards = [guardExpr], guardedRhsBody = bodyExpr}

parseRhsExpr :: ParserConfig -> Text -> Either Text Expr
parseRhsExpr cfg rhs0 =
  case splitTopLevelMaybe "where" rhs0 of
    Nothing ->
      case parseExpr cfg rhs0 of
        ParseOk expr -> Right expr
        ParseErr _ -> Left "expression"
    Just (bodyTxt, whereTxt) -> do
      bodyExpr <-
        case parseExpr cfg bodyTxt of
          ParseOk expr -> Right expr
          ParseErr _ -> Left "expression"
      decls <- parseLocalDecls cfg whereTxt
      case localDeclsToSimpleBindings decls of
        Just binds -> Right (EWhere bodyExpr binds)
        Nothing -> Right (EWhereDecls bodyExpr decls)

parseLocalDecls :: ParserConfig -> Text -> Either Text [Decl]
parseLocalDecls cfg txt =
  let body = stripBracesIfAny (T.strip txt)
      entries = splitDeclItems body
   in traverse (parseLocalDecl cfg) entries

parseLocalDecl :: ParserConfig -> Text -> Either Text Decl
parseLocalDecl cfg row
  | T.null (T.strip row) = Left "local declaration"
  | hasTopLevelTypeSig row = parseTypeSignatureDeclText row
  | isFixityDecl row = parseFixityDeclText row
  | hasTopLevelEquals row = parseEquationDecl cfg row
  | otherwise = Left "local declaration"

localDeclsToSimpleBindings :: [Decl] -> Maybe [(Text, Expr)]
localDeclsToSimpleBindings = traverse toSimpleBinding
  where
    toSimpleBinding decl =
      case decl of
        DeclValue (PatternBind (PVar name) (UnguardedRhs expr)) -> Just (name, expr)
        _ -> Nothing

parseFunctionLhs :: Text -> Maybe (Text, [Pattern])
parseFunctionLhs lhs =
  case splitTopLevelWords lhs of
    [] -> Nothing
    toks ->
      case parseInfixFunctionLhs toks of
        Just parsed -> Just parsed
        Nothing -> parsePrefixFunctionLhs toks

parsePrefixFunctionLhs :: [Text] -> Maybe (Text, [Pattern])
parsePrefixFunctionLhs toks =
  case toks of
    [] -> Nothing
    (firstTok : rest)
      | isVarToken firstTok -> do
          pats <- mapM (either (const Nothing) Just . parsePatternText) rest
          Just (firstTok, pats)
      | isParenthesizedOperator firstTok ->
          let op = stripParens firstTok
           in do
                pats <- mapM (either (const Nothing) Just . parsePatternText) rest
                Just (op, pats)
      | otherwise -> Nothing

parseInfixFunctionLhs :: [Text] -> Maybe (Text, [Pattern])
parseInfixFunctionLhs toks =
  case break isOperatorToken toks of
    ([], _) -> Nothing
    (_lhsToks, []) -> Nothing
    (lhsToks, opTok : rhsToks)
      | null lhsToks || null rhsToks -> Nothing
      | otherwise -> do
          lhsPat <- either (const Nothing) Just (parsePatternText (T.unwords lhsToks))
          rhsPat <- either (const Nothing) Just (parsePatternText (T.unwords rhsToks))
          Just (opTok, [lhsPat, rhsPat])

mergeAdjacentFunctions :: [Decl] -> [Decl]
mergeAdjacentFunctions = reverse . foldl' merge []
  where
    merge acc decl =
      case (acc, decl) of
        (DeclValue (FunctionBind prevName prevMatches) : rest, DeclValue (FunctionBind currName currMatches))
          | prevName == currName && shouldMerge prevMatches currMatches ->
              DeclValue (FunctionBind prevName (prevMatches <> currMatches)) : rest
        _ -> decl : acc

    shouldMerge prevMatches currMatches =
      not (all (null . matchPats) (prevMatches <> currMatches))

parseTypeText :: Text -> Either Text Type
parseTypeText input =
  parseTypeContext (T.strip input)
  where
    parseTypeContext txt =
      case splitTopLevelMaybe "=>" txt of
        Just (ctxTxt, tailTxt) -> do
          constraints <- parseConstraints ctxTxt
          rhsTy <- parseFunType (T.strip tailTxt)
          Right (TContext constraints rhsTy)
        Nothing -> parseFunType txt

    parseFunType txt =
      let parts = map T.strip (splitTopLevelToken "->" txt)
       in case parts of
            [] -> Left "type"
            [single] -> parseTypeApp single
            manyParts -> do
              tys <- traverse parseTypeApp manyParts
              pure (foldr1 TFun tys)

    parseTypeApp txt =
      let atoms = splitTopLevelWords txt
       in case atoms of
            [] -> Left "type"
            (firstAtom : restAtoms) -> do
              firstTy <- parseTypeAtom firstAtom
              foldl' applyTy (Right firstTy) restAtoms

    applyTy acc atomTxt = do
      fn <- acc
      arg <- parseTypeAtom atomTxt
      Right (TApp fn arg)

    parseTypeAtom atomTxt =
      let stripped = T.strip atomTxt
       in if T.null stripped
            then Left "type"
            else case T.uncons stripped of
              Just ('[', _) | T.last stripped == ']' -> do
                inner <- parseTypeText (T.init (T.tail stripped))
                Right (TList inner)
              Just ('(', _)
                | T.last stripped == ')' ->
                    let inner = T.strip (T.init (T.tail stripped))
                        tupleParts = splitTopLevel ',' inner
                        tupleCtorLike = not (T.null inner) && T.all (== ',') inner
                     in if inner == "->"
                          then Right (TCon "(->)")
                          else
                            if tupleCtorLike
                              then Right (TCon ("(" <> inner <> ")"))
                              else
                                if T.null inner
                                  then Right (TTuple [])
                                  else
                                    if length tupleParts > 1
                                      then TTuple <$> traverse parseTypeText tupleParts
                                      else TParen <$> parseTypeText inner
              _
                | isTypeToken stripped -> Right (TCon stripped)
                | otherwise -> Right (TVar stripped)

parseConstraints :: Text -> Either Text [Constraint]
parseConstraints txt =
  let stripped = T.strip txt
   in if stripped == "()"
        then Right [Constraint {constraintClass = "()", constraintArgs = [], constraintParen = False}]
        else
          let inner = if hasOuterParens stripped then T.drop 1 (T.dropEnd 1 stripped) else stripped
              parts = filter (not . T.null) (map T.strip (splitTopLevel ',' inner))
              parenthesizedSingle = hasOuterParens stripped && length parts == 1
           in do
                parsed <- traverse parseConstraint parts
                if parenthesizedSingle
                  then case parsed of
                    [single] -> Right [single {constraintParen = True}]
                    _ -> Right parsed
                  else Right parsed

parseConstraint :: Text -> Either Text Constraint
parseConstraint txt =
  case splitTopLevelWords txt of
    [] -> Left "constraint"
    (cls : args)
      | isTypeToken cls -> do
          argTypes <- traverse parseTypeText args
          Right Constraint {constraintClass = cls, constraintArgs = argTypes, constraintParen = False}
      | otherwise -> Left "constraint"

parsePatternText :: Text -> Either Text Pattern
parsePatternText input =
  let txt = T.strip input
   in if T.null txt
        then Left "pattern"
        else case T.uncons txt of
          Just ('~', rest) -> PIrrefutable <$> parsePatternText rest
          Just ('-', rest) ->
            case parseLiteralText (T.strip rest) of
              Just lit
                | isNumericLiteral lit -> Right (PNegLit lit)
              _ -> parsePatternCore txt
          _ -> parsePatternCore txt

parsePatternCore :: Text -> Either Text Pattern
parsePatternCore txt
  | txt == "_" = Right PWildcard
  | otherwise =
      case parseLiteralText txt of
        Just lit -> Right (PLit lit)
        Nothing ->
          case parseAsPattern txt of
            Just pat -> Right pat
            Nothing ->
              case parseParenedPattern txt of
                Just pat -> Right pat
                Nothing ->
                  case parseListPattern txt of
                    Just pat -> Right pat
                    Nothing ->
                      case parseRecordPattern txt of
                        Just pat -> Right pat
                        Nothing ->
                          case parseInfixPattern txt of
                            Just pat -> Right pat
                            Nothing -> parseConOrVarPattern txt

parseAsPattern :: Text -> Maybe Pattern
parseAsPattern txt = do
  (name, rest) <- splitTopLevelMaybe "@" txt
  if isVarToken (T.strip name)
    then case parsePatternText rest of
      Right pat -> Just (PAs (T.strip name) pat)
      Left _ -> Nothing
    else Nothing

parseParenedPattern :: Text -> Maybe Pattern
parseParenedPattern txt
  | hasOuterParens txt =
      let inner = T.strip (T.drop 1 (T.dropEnd 1 txt))
          parts = splitTopLevel ',' inner
       in if T.null inner
            then Just (PTuple [])
            else
              if length parts > 1
                then case traverse parsePatternText parts of
                  Right pats -> Just (PTuple pats)
                  Left _ -> Nothing
                else case parsePatternText inner of
                  Right pat -> Just (PParen pat)
                  Left _ -> Nothing
  | otherwise = Nothing

parseListPattern :: Text -> Maybe Pattern
parseListPattern txt
  | T.length txt >= 2 && T.head txt == '[' && T.last txt == ']' =
      let inner = T.strip (T.init (T.tail txt))
       in if T.null inner
            then Just (PList [])
            else case traverse parsePatternText (splitTopLevel ',' inner) of
              Right pats -> Just (PList pats)
              Left _ -> Nothing
  | otherwise = Nothing

parseRecordPattern :: Text -> Maybe Pattern
parseRecordPattern txt = do
  (ctor, fieldsTxt) <- either (const Nothing) Just (splitOuterBraces txt)
  let ctorName = T.strip ctor
  if T.null ctorName
    then Nothing
    else
      let fields = filter (not . T.null) (map T.strip (splitTopLevel ',' fieldsTxt))
       in case traverse parseField fields of
            Right parsed -> Just (PRecord ctorName parsed)
            Left _ -> Nothing
  where
    parseField fieldTxt =
      case splitTopLevelMaybe "=" fieldTxt of
        Just (nameTxt, patTxt) -> do
          pat <- parsePatternText patTxt
          Right (T.strip nameTxt, pat)
        Nothing -> do
          pat <- parsePatternText fieldTxt
          case pat of
            PVar fieldName -> Right (fieldName, PVar fieldName)
            _ -> Left "field pattern"

parseInfixPattern :: Text -> Maybe Pattern
parseInfixPattern txt = do
  (lhs, op, rhs) <- findTopLevelOperatorTriple txt
  lhsPat <- either (const Nothing) Just (parsePatternText lhs)
  rhsPat <- either (const Nothing) Just (parsePatternText rhs)
  Just (PInfix lhsPat op rhsPat)

parseConOrVarPattern :: Text -> Either Text Pattern
parseConOrVarPattern txt =
  case splitTopLevelWords txt of
    [] -> Left "pattern"
    (firstTok : rest)
      | isTypeToken firstTok -> do
          args <- traverse parsePatternText rest
          Right (PCon firstTok args)
      | isVarToken firstTok && null rest -> Right (PVar firstTok)
      | otherwise -> Left "pattern"

parseLiteralText :: Text -> Maybe Literal
parseLiteralText txt
  | T.length txt >= 2 && T.head txt == '\'' && T.last txt == '\'' =
      case T.unpack txt of
        ['\'', c, '\''] -> Just (LitChar c)
        _ -> Nothing
  | T.length txt >= 2 && T.head txt == '"' && T.last txt == '"' =
      Just (LitString (readStringLiteral txt))
  | isHexLiteral txt =
      Just (LitIntBase (readHexLiteral txt) txt)
  | isOctLiteral txt =
      Just (LitIntBase (readOctLiteral txt) txt)
  | T.all isDigit txt = Just (LitInt (read (T.unpack txt)))
  | T.count "." txt == 1 && T.all (\c -> isDigit c || c == '.') txt =
      Just (LitFloat (read (T.unpack txt)))
  | otherwise = Nothing

isNumericLiteral :: Literal -> Bool
isNumericLiteral lit =
  case lit of
    LitInt _ -> True
    LitIntBase _ _ -> True
    LitFloat _ -> True
    _ -> False

isHexLiteral :: Text -> Bool
isHexLiteral txt =
  case T.stripPrefix "0x" txt <|> T.stripPrefix "0X" txt of
    Just rest -> not (T.null rest) && T.all isHexDigit rest
    Nothing -> False

isOctLiteral :: Text -> Bool
isOctLiteral txt =
  case T.stripPrefix "0o" txt <|> T.stripPrefix "0O" txt of
    Just rest -> not (T.null rest) && T.all isOctDigit rest
    Nothing -> False

isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

readHexLiteral :: Text -> Integer
readHexLiteral txt =
  case readHex (T.unpack (T.drop 2 txt)) of
    [(n, "")] -> n
    _ -> 0

readOctLiteral :: Text -> Integer
readOctLiteral txt =
  case readOct (T.unpack (T.drop 2 txt)) of
    [(n, "")] -> n
    _ -> 0

readStringLiteral :: Text -> Text
readStringLiteral txt =
  case reads (T.unpack txt) of
    [(str, "")] -> T.pack str
    _ -> T.init (T.tail txt)

parseExprText :: Text -> Either Text Expr
parseExprText input =
  let txt = T.strip input
   in if T.null txt
        then Left "expression"
        else parseExprCore txt

parseExprCore :: Text -> Either Text Expr
parseExprCore txt
  | "if " `T.isPrefixOf` txt = parseIfExpr txt
  | "\\" `T.isPrefixOf` txt = parseLambdaExpr txt
  | "let " `T.isPrefixOf` txt = parseLetExpr txt
  | "case " `T.isPrefixOf` txt = parseCaseExpr txt
  | hasLeadingDoKeyword txt = parseDoExpr txt
  | T.length txt >= 2 && T.head txt == '[' && T.last txt == ']' = parseListExpr txt
  | otherwise =
      case splitTopLevelMaybe "::" txt of
        Just (lhs, rhs) -> do
          lhsExpr <- parseExprCore lhs
          rhsTy <- parseTypeText rhs
          Right (ETypeSig lhsExpr rhsTy)
        Nothing ->
          case parseRecordExpr txt of
            Right expr -> Right expr
            Left _ -> parseInfixAndAppExpr txt

hasLeadingDoKeyword :: Text -> Bool
hasLeadingDoKeyword txt
  | not ("do" `T.isPrefixOf` txt) = False
  | otherwise =
      case T.uncons (T.drop 2 txt) of
        Nothing -> True
        Just (c, _) -> isSpace c || c == '{'

parseIfExpr :: Text -> Either Text Expr
parseIfExpr txt = do
  rest <- maybe (Left "if expression") Right (T.stripPrefix "if" (T.stripStart txt))
  (condTxt, afterThen) <- splitTopLevelOnce "then" rest
  (thenTxt, elseTxt) <- splitTopLevelOnce "else" afterThen
  condExpr <- parseExprCore (stripOptionalSemi condTxt)
  thenExpr <- parseExprCore (stripOptionalSemi thenTxt)
  elseExpr <- parseExprCore elseTxt
  Right (EIf condExpr thenExpr elseExpr)

stripOptionalSemi :: Text -> Text
stripOptionalSemi txt =
  let stripped = T.strip txt
   in if ";" `T.isSuffixOf` stripped
        then T.strip (T.dropEnd 1 stripped)
        else stripped

parseLambdaExpr :: Text -> Either Text Expr
parseLambdaExpr txt = do
  rest <- maybe (Left "lambda") Right (T.stripPrefix "\\" txt)
  (paramsTxt, bodyTxt) <- splitTopLevelOnce "->" rest
  let paramTokens = filter (not . T.null) (splitTopLevelWords paramsTxt)
  if null paramTokens
    then Left "lambda"
    else do
      pats <- traverse parsePatternText paramTokens
      body <- parseExprCore bodyTxt
      pure $
        case traverse simpleVarName pats of
          Right names -> ELambda names body
          Left _ -> ELambdaPats pats body
  where
    simpleVarName pat =
      case pat of
        PVar name -> Right name
        _ -> Left ()

parseLetExpr :: Text -> Either Text Expr
parseLetExpr txt = do
  rest <- maybe (Left "let expression") Right (T.stripPrefix "let" (T.stripStart txt))
  (bindsTxt, bodyTxt) <- splitTopLevelOnce "in" rest
  decls <- parseLocalDecls defaultConfig bindsTxt
  body <- parseExprCore bodyTxt
  case localDeclsToSimpleBindings decls of
    Just binds -> Right (ELet binds body)
    Nothing -> Right (ELetDecls decls body)

parseCaseExpr :: Text -> Either Text Expr
parseCaseExpr txt = do
  rest <- maybe (Left "case expression") Right (T.stripPrefix "case" (T.stripStart txt))
  (scrutTxt, altsTxt) <- splitTopLevelOnce "of" rest
  scrut <- parseExprCore scrutTxt
  alts <- parseCaseAlts altsTxt
  Right (ECase scrut alts)

parseCaseAlts :: Text -> Either Text [CaseAlt]
parseCaseAlts txt =
  let stripped = T.strip txt
      altRows =
        if hasOuterBraces stripped
          then splitDeclItems (T.drop 1 (T.dropEnd 1 stripped))
          else filter (not . T.null) (map T.strip (T.lines stripped))
   in traverse parseCaseAlt altRows

parseCaseAlt :: Text -> Either Text CaseAlt
parseCaseAlt txt =
  case splitTopLevelMaybe "|" txt of
    Just (patTxt, guardTail) -> parseGuardedCaseAlt patTxt guardTail
    Nothing -> do
      (patTxt, bodyTxt) <- splitTopLevelOnce "->" txt
      pat <- parsePatternText patTxt
      body <- parseExprCore bodyTxt
      Right (CaseAlt pat (UnguardedRhs body))

parseGuardedCaseAlt :: Text -> Text -> Either Text CaseAlt
parseGuardedCaseAlt patTxt guardTail = do
  pat <- parsePatternText patTxt
  grhss <- traverse parseOne (splitDeclItems guardTail)
  Right (CaseAlt pat (GuardedRhss grhss))
  where
    parseOne entry = do
      row <- maybe (Right entry) Right (T.stripPrefix "|" (T.stripStart entry))
      (guardTxt, bodyTxt) <- splitTopLevelOnce "->" row
      guardExpr <- parseExprCore guardTxt
      bodyExpr <- parseExprCore bodyTxt
      Right GuardedRhs {guardedRhsGuards = [guardExpr], guardedRhsBody = bodyExpr}

parseDoExpr :: Text -> Either Text Expr
parseDoExpr txt = do
  rest <- maybe (Left "do expression") Right (T.stripPrefix "do" (T.stripStart txt))
  let body = T.strip rest
      rows =
        if hasOuterBraces body
          then splitDeclItems (T.drop 1 (T.dropEnd 1 body))
          else filter (not . T.null) (map T.strip (T.lines body))
  stmts <- traverse parseDoStmtText rows
  Right (EDo stmts)

parseDoStmtText :: Text -> Either Text DoStmt
parseDoStmtText txt
  | "let " `T.isPrefixOf` T.strip txt =
      let rest = T.strip (fromMaybeText txt (T.stripPrefix "let" (T.stripStart txt)))
       in do
            decls <- parseLocalDecls defaultConfig rest
            case localDeclsToSimpleBindings decls of
              Just binds -> Right (DoLet binds)
              Nothing -> Right (DoLetDecls decls)
  | otherwise =
      case splitTopLevelMaybe "<-" txt of
        Just (lhs, rhs) -> do
          pat <- parsePatternText lhs
          expr <- parseExprCore rhs
          Right (DoBind pat expr)
        Nothing -> DoExpr <$> parseExprCore txt

parseListExpr :: Text -> Either Text Expr
parseListExpr txt = do
  let inner = T.strip (T.init (T.tail txt))
  if T.null inner
    then Right (EList [])
    else case splitTopLevelMaybe "|" inner of
      Just (bodyTxt, qualsTxt) -> do
        body <- parseExprCore bodyTxt
        qualifiers <- traverse parseCompStmtText (splitTopLevel ',' qualsTxt)
        Right (EListComp body qualifiers)
      Nothing ->
        case splitTopLevelMaybe ".." inner of
          Just _ -> parseArithSeq inner
          Nothing -> EList <$> traverse parseExprCore (splitTopLevel ',' inner)

parseCompStmtText :: Text -> Either Text CompStmt
parseCompStmtText txt
  | "let " `T.isPrefixOf` T.strip txt =
      let rest = T.strip (fromMaybeText txt (T.stripPrefix "let" (T.stripStart txt)))
       in do
            decls <- parseLocalDecls defaultConfig rest
            case localDeclsToSimpleBindings decls of
              Just binds -> Right (CompLet binds)
              Nothing -> Right (CompLetDecls decls)
  | otherwise =
      case splitTopLevelMaybe "<-" txt of
        Just (patTxt, exprTxt) -> do
          pat <- parsePatternText patTxt
          expr <- parseExprCore exprTxt
          Right (CompGen pat expr)
        Nothing -> CompGuard <$> parseExprCore txt

parseArithSeq :: Text -> Either Text Expr
parseArithSeq inner =
  case splitTopLevelMaybe ".." inner of
    Nothing -> Left "arithmetic sequence"
    Just (lhs, rhs) ->
      case splitTopLevel ',' lhs of
        [fromTxt] -> do
          fromExpr <- parseExprCore fromTxt
          if T.null (T.strip rhs)
            then Right (EArithSeq (ArithSeqFrom fromExpr))
            else do
              toExpr <- parseExprCore rhs
              Right (EArithSeq (ArithSeqFromTo fromExpr toExpr))
        [fromTxt, thenTxt] -> do
          fromExpr <- parseExprCore fromTxt
          thenExpr <- parseExprCore thenTxt
          if T.null (T.strip rhs)
            then Right (EArithSeq (ArithSeqFromThen fromExpr thenExpr))
            else do
              toExpr <- parseExprCore rhs
              Right (EArithSeq (ArithSeqFromThenTo fromExpr thenExpr toExpr))
        _ -> Left "arithmetic sequence"

parseRecordExpr :: Text -> Either Text Expr
parseRecordExpr txt =
  case splitOuterBraces txt of
    Left _ -> Left "record expression"
    Right (baseTxt, fieldsTxt) -> do
      let fieldsRows =
            if T.null (T.strip fieldsTxt)
              then []
              else splitTopLevel ',' fieldsTxt
      fields <- traverse parseRecordField fieldsRows
      let base = T.strip baseTxt
      if isTypeToken base
        then Right (ERecordCon base fields)
        else do
          baseExpr <- parseExprCore base
          Right (ERecordUpd baseExpr fields)

parseRecordField :: Text -> Either Text (Text, Expr)
parseRecordField txt = do
  (nameTxt, exprTxt) <- splitTopLevelOnce "=" txt
  let name = T.strip nameTxt
  expr <- parseExprCore exprTxt
  Right (name, expr)

parseInfixAndAppExpr :: Text -> Either Text Expr
parseInfixAndAppExpr txt = do
  tokens <- tokenizeExpr txt
  buildExprFromTokens tokens

buildExprFromTokens :: [ExprToken] -> Either Text Expr
buildExprFromTokens tokens =
  case tokens of
    [] -> Left "expression"
    (TokOp "-" : rest) -> ENegate <$> buildExprFromTokens rest
    _ -> do
      (firstSeg, rest) <- takeSegment tokens
      firstExpr <- buildApp firstSeg
      foldSegments firstExpr rest
  where
    foldSegments acc ts =
      case ts of
        [] -> Right acc
        (TokOp op : remain) -> do
          (seg, tailTs) <- takeSegment remain
          rhs <- buildApp seg
          foldSegments (EInfix acc op rhs) tailTs
        _ -> Left "expression"

    buildApp seg =
      case seg of
        [] -> Left "expression"
        _ -> do
          atoms <- traverse parseExprAtom seg
          Right (foldl1 EApp atoms)

    parseExprAtom tok =
      case tok of
        TokAtom atomTxt -> parseAtomicExpression atomTxt
        TokOp op -> Right (EVar op)

    takeSegment ts =
      let (seg, restSeg) = break isOp ts
       in if null seg then Left "expression" else Right (seg, restSeg)

    isOp t =
      case t of
        TokOp _ -> True
        _ -> False

parseAtomicExpression :: Text -> Either Text Expr
parseAtomicExpression atomTxt =
  let txt = T.strip atomTxt
   in if T.null txt
        then Left "expression"
        else case parseLiteralText txt of
          Just lit ->
            case lit of
              LitInt n -> Right (EInt n)
              LitIntBase n repr -> Right (EIntBase n repr)
              LitFloat n -> Right (EFloat n)
              LitChar c -> Right (EChar c)
              LitString s -> Right (EString s)
          Nothing ->
            if hasOuterParens txt
              then parseParenExpr txt
              else
                if T.length txt >= 2 && T.head txt == '[' && T.last txt == ']'
                  then parseListExpr txt
                  else Right (EVar txt)

parseParenExpr :: Text -> Either Text Expr
parseParenExpr txt =
  let inner = T.strip (T.drop 1 (T.dropEnd 1 txt))
   in if T.null inner
        then Right (ETuple [])
        else
          if isOperatorToken inner || isVarToken inner
            then Right (EVar inner)
            else case parseTupleConstructor inner of
              Just arity -> Right (ETupleCon arity)
              Nothing ->
                case parseSection inner of
                  Just expr -> Right expr
                  Nothing ->
                    let parts = splitTopLevel ',' inner
                     in if length parts > 1
                          then ETuple <$> traverse parseExprCore parts
                          else EParen <$> parseExprCore inner

parseTupleConstructor :: Text -> Maybe Int
parseTupleConstructor inner =
  if T.all (== ',') inner && not (T.null inner)
    then Just (T.length inner + 1)
    else Nothing

parseSection :: Text -> Maybe Expr
parseSection inner =
  case tokenizeExpr inner of
    Right [TokOp op, TokAtom rhs] ->
      case parseExprCore rhs of
        Right rhsExpr -> Just (ESectionR op rhsExpr)
        Left _ -> Nothing
    Right [TokAtom lhs, TokOp op] ->
      case parseExprCore lhs of
        Right lhsExpr -> Just (ESectionL lhsExpr op)
        Left _ -> Nothing
    _ -> Nothing

data ExprToken
  = TokAtom Text
  | TokOp Text
  deriving (Eq, Show)

tokenizeExpr :: Text -> Either Text [ExprToken]
tokenizeExpr input = go (T.strip input) []
  where
    go txt acc
      | T.null txt = Right (reverse acc)
      | otherwise =
          case T.uncons txt of
            Nothing -> Right (reverse acc)
            Just (c, rest)
              | isSpace c -> go (T.dropWhile isSpace rest) acc
              | isDigit c ->
                  let (numTok, tailTxt) = consumeNumber txt
                   in go tailTxt (TokAtom numTok : acc)
              | c == '`' ->
                  let (name, tailTxt) = T.breakOn "`" rest
                   in if T.null tailTxt
                        then Left "expression"
                        else go (T.drop 1 tailTxt) (TokOp (T.strip name) : acc)
              | isSymbolicOpChar c ->
                  let (opTxt, tailTxt) = T.span isSymbolicOpChar txt
                   in if opTxt `elem` ["=", "->", "<-", "=>", "::", "|"]
                        then Left "expression"
                        else go tailTxt (TokOp opTxt : acc)
              | c == '@' -> Left "expression"
              | otherwise -> do
                  (atom, tailTxt) <- consumeAtom txt
                  go tailTxt (TokAtom atom : acc)

    consumeAtom txt =
      case T.uncons txt of
        Nothing -> Left "expression"
        Just (c, _)
          | c == '(' -> consumeBalanced '(' ')' txt
          | c == '[' -> consumeBalanced '[' ']' txt
          | c == '{' -> consumeBalanced '{' '}' txt
          | c == '"' -> consumeQuoted '"' txt
          | c == '\'' -> consumeQuoted '\'' txt
          | otherwise ->
              let (atom, tailTxt) = T.break isAtomStop txt
               in Right (atom, tailTxt)

    isAtomStop ch = isSpace ch || ch == '`' || (isSymbolicOpChar ch && ch /= '.')

    consumeNumber txt =
      case T.stripPrefix "0x" txt <|> T.stripPrefix "0X" txt of
        Just afterHex ->
          let (digits, tailTxt) = T.span isHexDigit afterHex
           in if T.null digits
                then consumeDecimal txt
                else (T.take (2 + T.length digits) txt, tailTxt)
        Nothing ->
          case T.stripPrefix "0o" txt <|> T.stripPrefix "0O" txt of
            Just afterOct ->
              let (digits, tailTxt) = T.span isOctDigit afterOct
               in if T.null digits
                    then consumeDecimal txt
                    else (T.take (2 + T.length digits) txt, tailTxt)
            Nothing -> consumeDecimal txt

    consumeDecimal txt =
      let (whole, rest) = T.span isDigit txt
       in case T.uncons rest of
            Just ('.', afterDot)
              | not (T.null afterDot) && isDigit (T.head afterDot) ->
                  let (frac, tailTxt) = T.span isDigit afterDot
                   in (whole <> "." <> frac, tailTxt)
            _ -> (whole, rest)

consumeBalanced :: Char -> Char -> Text -> Either Text (Text, Text)
consumeBalanced open close txt =
  let (chunk, rest, ok) = scan 0 False False T.empty txt
   in if ok then Right (chunk, rest) else Left "expression"
  where
    scan depth inStr inChr acc remaining =
      case T.uncons remaining of
        Nothing -> (acc, T.empty, False)
        Just (c, cs)
          | inStr ->
              if c == '"'
                then scan depth False inChr (T.snoc acc c) cs
                else scan depth True inChr (T.snoc acc c) cs
          | inChr ->
              if c == '\''
                then scan depth inStr False (T.snoc acc c) cs
                else scan depth inStr True (T.snoc acc c) cs
          | c == '"' -> scan depth True inChr (T.snoc acc c) cs
          | c == '\'' -> scan depth inStr inChr (T.snoc acc c) cs
          | c == open -> scan (depth + 1) inStr inChr (T.snoc acc c) cs
          | c == close ->
              let depth' = depth - 1
                  acc' = T.snoc acc c
               in if depth' == 0
                    then (acc', cs, True)
                    else scan depth' inStr inChr acc' cs
          | otherwise -> scan depth inStr inChr (T.snoc acc c) cs

consumeQuoted :: Char -> Text -> Either Text (Text, Text)
consumeQuoted quote txt =
  let (chunk, rest, ok) = scan False T.empty txt
   in if ok then Right (chunk, rest) else Left "expression"
  where
    scan escaped acc remaining =
      case T.uncons remaining of
        Nothing -> (acc, T.empty, False)
        Just (c, cs) ->
          let acc' = T.snoc acc c
           in if escaped
                then scan False acc' cs
                else
                  if c == '\\'
                    then scan True acc' cs
                    else
                      if c == quote && T.length acc > 0
                        then (acc', cs, True)
                        else scan False acc' cs

parseContextPrefix :: Text -> Either Text ([Constraint], Text)
parseContextPrefix txt =
  case splitTopLevelMaybe "=>" txt of
    Nothing -> Right ([], T.strip txt)
    Just (ctxTxt, restTxt) -> do
      constraints <- parseConstraints ctxTxt
      Right (constraints, T.strip restTxt)

splitDeclItems :: Text -> [Text]
splitDeclItems txt =
  let semicolonSplit = filter (not . T.null) (map T.strip (splitTopLevel ';' txt))
   in if length semicolonSplit > 1
        then semicolonSplit
        else filter (not . T.null) (map T.strip (T.lines txt))

stripBracesIfAny :: Text -> Text
stripBracesIfAny txt
  | hasOuterBraces txt = T.strip (T.drop 1 (T.dropEnd 1 txt))
  | otherwise = txt

splitOuterBraces :: Text -> Either Text (Text, Text)
splitOuterBraces txt =
  case findTopLevelChar '{' txt of
    Nothing -> Left "braces"
    Just openIx ->
      let before = T.strip (T.take openIx txt)
          afterOpen = T.drop (openIx + 1) txt
       in case findMatchingBrace afterOpen of
            Nothing -> Left "braces"
            Just closeIx ->
              let inside = T.take closeIx afterOpen
                  rest = T.strip (T.drop (closeIx + 1) afterOpen)
               in if T.null rest then Right (before, inside) else Left "braces"

findMatchingBrace :: Text -> Maybe Int
findMatchingBrace = go 0 0 False False
  where
    go ix depth inStr inChr remaining =
      case T.uncons remaining of
        Nothing -> Nothing
        Just (c, cs)
          | inStr ->
              if c == '"' then go (ix + 1) depth False inChr cs else go (ix + 1) depth True inChr cs
          | inChr ->
              if c == '\'' then go (ix + 1) depth inStr False cs else go (ix + 1) depth inStr True cs
          | c == '"' -> go (ix + 1) depth True inChr cs
          | c == '\'' -> go (ix + 1) depth inStr True cs
          | c == '{' -> go (ix + 1) (depth + 1) inStr inChr cs
          | c == '}' ->
              if depth == 0
                then Just ix
                else go (ix + 1) (depth - 1) inStr inChr cs
          | otherwise -> go (ix + 1) depth inStr inChr cs

hasTopLevelTypeSig :: Text -> Bool
hasTopLevelTypeSig = hasTopLevelToken "::"

hasTopLevelEquals :: Text -> Bool
hasTopLevelEquals = hasTopLevelToken "="

hasTopLevelToken :: Text -> Text -> Bool
hasTopLevelToken token txt =
  case splitTopLevelMaybe token txt of
    Just _ -> True
    Nothing -> False

splitTopLevelOnce :: Text -> Text -> Either Text (Text, Text)
splitTopLevelOnce token txt =
  case splitTopLevelMaybe token txt of
    Just (lhs, rhs) -> Right (T.strip lhs, T.strip rhs)
    Nothing -> Left token

splitTopLevelMaybe :: Text -> Text -> Maybe (Text, Text)
splitTopLevelMaybe token txt =
  case finder token txt of
    Nothing -> Nothing
    Just ix ->
      let lhs = T.take ix txt
          rhs = T.drop (ix + T.length token) txt
       in Just (lhs, rhs)
  where
    finder tok
      | tok == "=" = findTopLevelEqualsIndex
      | T.all isAlpha tok = findTopLevelKeyword tok
      | otherwise = findTopLevelToken tok

findTopLevelKeyword :: Text -> Text -> Maybe Int
findTopLevelKeyword token txt
  | T.null token = Nothing
  | otherwise = go 0 0 0 False False 0 txt
  where
    tokLen = T.length token

    go parenN braceN bracketN inStr inChr ix remaining =
      case T.uncons remaining of
        Nothing -> Nothing
        Just (c, cs)
          | inStr ->
              if c == '"'
                then go parenN braceN bracketN False inChr (ix + 1) cs
                else go parenN braceN bracketN True inChr (ix + 1) cs
          | inChr ->
              if c == '\''
                then go parenN braceN bracketN inStr False (ix + 1) cs
                else go parenN braceN bracketN inStr True (ix + 1) cs
          | parenN == 0 && braceN == 0 && bracketN == 0 && T.isPrefixOf token remaining ->
              if matchesBoundary ix
                then Just ix
                else go parenN braceN bracketN inStr inChr (ix + 1) cs
          | c == '"' -> go parenN braceN bracketN True inChr (ix + 1) cs
          | c == '\'' -> go parenN braceN bracketN inStr True (ix + 1) cs
          | c == '(' -> go (parenN + 1) braceN bracketN inStr inChr (ix + 1) cs
          | c == ')' -> go (max 0 (parenN - 1)) braceN bracketN inStr inChr (ix + 1) cs
          | c == '{' -> go parenN (braceN + 1) bracketN inStr inChr (ix + 1) cs
          | c == '}' -> go parenN (max 0 (braceN - 1)) bracketN inStr inChr (ix + 1) cs
          | c == '[' -> go parenN braceN (bracketN + 1) inStr inChr (ix + 1) cs
          | c == ']' -> go parenN braceN (max 0 (bracketN - 1)) inStr inChr (ix + 1) cs
          | otherwise -> go parenN braceN bracketN inStr inChr (ix + 1) cs

    matchesBoundary ix =
      let prevChar = if ix <= 0 then Nothing else Just (T.index txt (ix - 1))
          nextIx = ix + tokLen
          nextChar = if nextIx < T.length txt then Just (T.index txt nextIx) else Nothing
       in maybe True (not . isIdentTailOrStart) prevChar
            && maybe True (not . isIdentTailOrStart) nextChar

splitTopLevelToken :: Text -> Text -> [Text]
splitTopLevelToken token txt =
  case splitTopLevelMaybe token txt of
    Nothing -> [T.strip txt]
    Just (lhs, rhs) -> T.strip lhs : splitTopLevelToken token rhs

splitTopLevel :: Char -> Text -> [Text]
splitTopLevel delim input = reverse (go 0 0 0 False False T.empty input [])
  where
    go parenN braceN bracketN inStr inChr current remaining acc =
      case T.uncons remaining of
        Nothing -> T.strip current : acc
        Just (c, cs)
          | inStr ->
              if c == '"'
                then go parenN braceN bracketN False inChr (T.snoc current c) cs acc
                else go parenN braceN bracketN True inChr (T.snoc current c) cs acc
          | inChr ->
              if c == '\''
                then go parenN braceN bracketN inStr False (T.snoc current c) cs acc
                else go parenN braceN bracketN inStr True (T.snoc current c) cs acc
          | c == '"' -> go parenN braceN bracketN True inChr (T.snoc current c) cs acc
          | c == '\'' -> go parenN braceN bracketN inStr True (T.snoc current c) cs acc
          | c == '(' -> go (parenN + 1) braceN bracketN inStr inChr (T.snoc current c) cs acc
          | c == ')' -> go (max 0 (parenN - 1)) braceN bracketN inStr inChr (T.snoc current c) cs acc
          | c == '{' -> go parenN (braceN + 1) bracketN inStr inChr (T.snoc current c) cs acc
          | c == '}' -> go parenN (max 0 (braceN - 1)) bracketN inStr inChr (T.snoc current c) cs acc
          | c == '[' -> go parenN braceN (bracketN + 1) inStr inChr (T.snoc current c) cs acc
          | c == ']' -> go parenN braceN (max 0 (bracketN - 1)) inStr inChr (T.snoc current c) cs acc
          | c == delim && parenN == 0 && braceN == 0 && bracketN == 0 ->
              go parenN braceN bracketN inStr inChr T.empty cs (T.strip current : acc)
          | otherwise -> go parenN braceN bracketN inStr inChr (T.snoc current c) cs acc

splitTopLevelWords :: Text -> [Text]
splitTopLevelWords txt = reverse (go (T.strip txt) [] T.empty 0 0 0 False False)
  where
    flushToken token acc =
      if T.null (T.strip token)
        then acc
        else T.strip token : acc

    go remaining acc token parenN braceN bracketN inStr inChr =
      case T.uncons remaining of
        Nothing -> flushToken token acc
        Just (c, cs)
          | inStr ->
              if c == '"'
                then go cs acc (T.snoc token c) parenN braceN bracketN False inChr
                else go cs acc (T.snoc token c) parenN braceN bracketN True inChr
          | inChr ->
              if c == '\''
                then go cs acc (T.snoc token c) parenN braceN bracketN inStr False
                else go cs acc (T.snoc token c) parenN braceN bracketN inStr True
          | c == '"' -> go cs acc (T.snoc token c) parenN braceN bracketN True inChr
          | c == '\'' -> go cs acc (T.snoc token c) parenN braceN bracketN inStr True
          | c == '(' -> go cs acc (T.snoc token c) (parenN + 1) braceN bracketN inStr inChr
          | c == ')' -> go cs acc (T.snoc token c) (max 0 (parenN - 1)) braceN bracketN inStr inChr
          | c == '{' -> go cs acc (T.snoc token c) parenN (braceN + 1) bracketN inStr inChr
          | c == '}' -> go cs acc (T.snoc token c) parenN (max 0 (braceN - 1)) bracketN inStr inChr
          | c == '[' -> go cs acc (T.snoc token c) parenN braceN (bracketN + 1) inStr inChr
          | c == ']' -> go cs acc (T.snoc token c) parenN braceN (max 0 (bracketN - 1)) inStr inChr
          | isSpace c && parenN == 0 && braceN == 0 && bracketN == 0 ->
              go (T.dropWhile isSpace cs) (flushToken token acc) T.empty parenN braceN bracketN inStr inChr
          | otherwise -> go cs acc (T.snoc token c) parenN braceN bracketN inStr inChr

findTopLevelToken :: Text -> Text -> Maybe Int
findTopLevelToken token txt
  | T.null token = Nothing
  | otherwise = go 0 0 0 False False 0 txt
  where
    go parenN braceN bracketN inStr inChr ix remaining =
      case T.uncons remaining of
        Nothing -> Nothing
        Just (c, cs)
          | inStr ->
              if c == '"'
                then go parenN braceN bracketN False inChr (ix + 1) cs
                else go parenN braceN bracketN True inChr (ix + 1) cs
          | inChr ->
              if c == '\''
                then go parenN braceN bracketN inStr False (ix + 1) cs
                else go parenN braceN bracketN inStr True (ix + 1) cs
          | parenN == 0 && braceN == 0 && bracketN == 0 && T.isPrefixOf token remaining -> Just ix
          | c == '"' -> go parenN braceN bracketN True inChr (ix + 1) cs
          | c == '\'' -> go parenN braceN bracketN inStr True (ix + 1) cs
          | c == '(' -> go (parenN + 1) braceN bracketN inStr inChr (ix + 1) cs
          | c == ')' -> go (max 0 (parenN - 1)) braceN bracketN inStr inChr (ix + 1) cs
          | c == '{' -> go parenN (braceN + 1) bracketN inStr inChr (ix + 1) cs
          | c == '}' -> go parenN (max 0 (braceN - 1)) bracketN inStr inChr (ix + 1) cs
          | c == '[' -> go parenN braceN (bracketN + 1) inStr inChr (ix + 1) cs
          | c == ']' -> go parenN braceN (max 0 (bracketN - 1)) inStr inChr (ix + 1) cs
          | otherwise -> go parenN braceN bracketN inStr inChr (ix + 1) cs

findTopLevelEqualsIndex :: Text -> Maybe Int
findTopLevelEqualsIndex txt = go 0 0 0 False 0 txt
  where
    go parenN braceN bracketN inStr ix remaining =
      case T.uncons remaining of
        Nothing -> Nothing
        Just (c, cs)
          | inStr ->
              if c == '"'
                then go parenN braceN bracketN False (ix + 1) cs
                else go parenN braceN bracketN True (ix + 1) cs
          | c == '"' -> go parenN braceN bracketN True (ix + 1) cs
          | c == '(' -> go (parenN + 1) braceN bracketN inStr (ix + 1) cs
          | c == ')' -> go (max 0 (parenN - 1)) braceN bracketN inStr (ix + 1) cs
          | c == '{' -> go parenN (braceN + 1) bracketN inStr (ix + 1) cs
          | c == '}' -> go parenN (max 0 (braceN - 1)) bracketN inStr (ix + 1) cs
          | c == '[' -> go parenN braceN (bracketN + 1) inStr (ix + 1) cs
          | c == ']' -> go parenN braceN (max 0 (bracketN - 1)) inStr (ix + 1) cs
          | parenN == 0 && braceN == 0 && bracketN == 0 && isAssignment remaining ix txt -> Just ix
          | otherwise -> go parenN braceN bracketN inStr (ix + 1) cs

    isAssignment remaining ix whole =
      case T.uncons remaining of
        Just ('=', afterEq) ->
          let prev = if ix <= 0 then Nothing else Just (T.index whole (ix - 1))
              next = fmap fst (T.uncons afterEq)
           in next /= Just '>'
                && next /= Just '='
                && prev /= Just '<'
                && prev /= Just ':'
                && prev /= Just '='
        _ -> False

findTopLevelChar :: Char -> Text -> Maybe Int
findTopLevelChar ch = findTopLevelToken (T.singleton ch)

findTopLevelOperatorTriple :: Text -> Maybe (Text, Text, Text)
findTopLevelOperatorTriple txt =
  case tokenizeExpr txt of
    Right toks ->
      case break isOp toks of
        (_, []) -> Nothing
        (lhsSeg, TokOp op : rhsSeg)
          | null lhsSeg || null rhsSeg -> Nothing
          | otherwise ->
              let lhsTxt = T.unwords [a | TokAtom a <- lhsSeg]
                  rhsTxt = T.unwords [a | TokAtom a <- rhsSeg]
               in if T.null lhsTxt || T.null rhsTxt
                    then Nothing
                    else Just (lhsTxt, op, rhsTxt)
    Left _ -> Nothing
  where
    isOp token =
      case token of
        TokOp _ -> True
        _ -> False

parseLineWith :: MParser a -> Text -> Either ParseError a
parseLineWith parser input =
  case runParser parser "<line>" input of
    Right value -> Right value
    Left bundle -> Left (bundleToError input bundle)

callConvParser :: MParser CallConv
callConvParser =
  (keyword "ccall" >> pure CCall)
    <|> (keyword "stdcall" >> pure StdCall)

safetyParser :: MParser ForeignSafety
safetyParser =
  (keyword "safe" >> pure Safe)
    <|> (keyword "unsafe" >> pure Unsafe)

foreignEntityParser :: MParser Text
foreignEntityParser = lexeme scLine $ do
  _ <- C.char '"'
  txt <- manyTillChar '"'
  pure (T.pack txt)

manyTillChar :: Char -> MParser String
manyTillChar endCh = go []
  where
    go acc =
      (C.char endCh >> pure (reverse acc))
        <|> do
          ch <- C.printChar
          go (ch : acc)

identifier :: MParser Text
identifier = identifierLexeme scLine

identifierOrOperator :: MParser Text
identifierOrOperator =
  identifier
    <|> do
      _ <- symbol "("
      op <- operatorTokenLexeme scLine
      _ <- symbol ")"
      pure op

identifierLexeme :: MParser () -> MParser Text
identifierLexeme sc = lexeme sc $ do
  notFollowedBy reservedWord
  first <- C.letterChar <|> C.char '_'
  rest <- many identTailChar
  more <- many (C.char '.' *> ((:) <$> C.letterChar <*> many identTailChar))
  let base = first : rest
      chunks = base : more
  pure (T.intercalate "." (map T.pack chunks))

operatorTokenLexeme :: MParser () -> MParser Text
operatorTokenLexeme sc =
  lexeme sc $ do
    tok <- some (MP.satisfy isSymbolicOpChar)
    let t = T.pack tok
    if t `elem` ["=", "->", "<-", "=>", "::", "|"]
      then fail "operator"
      else pure t

identTailChar :: MParser Char
identTailChar =
  C.alphaNumChar
    <|> C.char '_'
    <|> C.char '\''

symbol :: Text -> MParser Text
symbol = L.symbol scLine

keyword :: Text -> MParser Text
keyword kw = lexeme scLine (C.string kw <* notFollowedBy identTailOrStartChar)

identTailOrStartChar :: MParser Char
identTailOrStartChar = MP.satisfy isIdentTailOrStart

isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart c = isAlphaNum c || c == '_' || c == '\''

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c = c `elem` (":!#$%&*+./<=>?\\^|-~" :: String)

lexeme :: MParser () -> MParser a -> MParser a
lexeme = L.lexeme

scLine :: MParser ()
scLine = L.space C.space1 MP.empty MP.empty

stripComments :: ParserConfig -> Text -> Text
stripComments cfg = go 0 False False False T.empty
  where
    go blockDepth inStr inChr escaped acc remaining =
      case T.uncons remaining of
        Nothing -> acc
        Just (c, cs)
          | blockDepth > 0 ->
              case T.stripPrefix "{-" remaining of
                Just rest -> go (blockDepth + 1) False False False acc rest
                Nothing ->
                  case T.stripPrefix "-}" remaining of
                    Just rest ->
                      go (max 0 (blockDepth - 1)) False False False acc rest
                    Nothing ->
                      if c == '\n'
                        then go blockDepth False False False (T.snoc acc c) cs
                        else go blockDepth False False False acc cs
          | inStr ->
              if escaped
                then go blockDepth True False False (T.snoc acc c) cs
                else
                  if c == '\\'
                    then go blockDepth True False True (T.snoc acc c) cs
                    else
                      if c == '"'
                        then go blockDepth False False False (T.snoc acc c) cs
                        else go blockDepth True False False (T.snoc acc c) cs
          | inChr ->
              if escaped
                then go blockDepth False True False (T.snoc acc c) cs
                else
                  if c == '\\'
                    then go blockDepth False True True (T.snoc acc c) cs
                    else
                      if c == '\''
                        then go blockDepth False False False (T.snoc acc c) cs
                        else go blockDepth False True False (T.snoc acc c) cs
          | otherwise ->
              case T.stripPrefix "{-" remaining of
                Just rest -> go (blockDepth + 1) False False False acc rest
                Nothing ->
                  if allowLineComments cfg && "--" `T.isPrefixOf` remaining
                    then
                      let afterComment = T.drop 2 remaining
                          (_, newlineAndRest) = T.break (== '\n') afterComment
                       in case T.uncons newlineAndRest of
                            Just ('\n', rest) -> go blockDepth False False False (T.snoc acc '\n') rest
                            _ -> acc
                    else
                      if c == '"'
                        then go blockDepth True False False (T.snoc acc c) cs
                        else
                          if c == '\''
                            then go blockDepth False True False (T.snoc acc c) cs
                            else go blockDepth False False False (T.snoc acc c) cs

isLanguagePragma :: Text -> Bool
isLanguagePragma txt =
  "{-#" `T.isPrefixOf` txt && "#-}" `T.isSuffixOf` txt

isFixityDecl :: Text -> Bool
isFixityDecl txt =
  case T.words txt of
    (kw : _) -> kw `elem` ["infix", "infixl", "infixr"]
    _ -> False

stripParens :: Text -> Text
stripParens t =
  let trimmed = T.strip t
   in if T.length trimmed >= 2 && T.head trimmed == '(' && T.last trimmed == ')'
        then T.strip (T.init (T.tail trimmed))
        else trimmed

isTypeToken :: Text -> Bool
isTypeToken token =
  case T.uncons (stripParens token) of
    Just (c, _) -> isUpper c
    Nothing -> False

isVarToken :: Text -> Bool
isVarToken token =
  case T.uncons token of
    Just (c, rest) ->
      (isLower c || c == '_')
        && T.all isIdentTailOrStart rest
    Nothing -> False

isValueName :: Text -> Bool
isValueName tok = isVarToken tok || isOperatorToken tok

isOperatorToken :: Text -> Bool
isOperatorToken tok =
  let inner = stripParens tok
   in not (T.null inner) && T.all isSymbolicOpChar inner

isParenthesizedOperator :: Text -> Bool
isParenthesizedOperator tok =
  let trimmed = T.strip tok
   in T.length trimmed >= 3
        && T.head trimmed == '('
        && T.last trimmed == ')'
        && isOperatorToken (stripParens trimmed)

groupDeclarationChunks :: [(Int, Text)] -> [(Int, Text)]
groupDeclarationChunks = go Nothing []
  where
    go current acc rows =
      case rows of
        [] ->
          case current of
            Nothing -> reverse acc
            Just (ln, pieces) -> reverse ((ln, T.intercalate "\n" (reverse pieces)) : acc)
        (ln, rawLine) : rest ->
          let trimmed = T.strip rawLine
              ind = indentation rawLine
           in if T.null trimmed
                then go current acc rest
                else case current of
                  Nothing -> go (Just (ln, [trimmed])) acc rest
                  Just (startLn, pieces)
                    | ind == 0 ->
                        go (Just (ln, [trimmed])) ((startLn, T.intercalate "\n" (reverse pieces)) : acc) rest
                    | otherwise ->
                        go (Just (startLn, trimmed : pieces)) acc rest

indentation :: Text -> Int
indentation = T.length . T.takeWhile (\c -> c == ' ' || c == '\t')

hasOuterParens :: Text -> Bool
hasOuterParens txt =
  T.length txt >= 2
    && T.head txt == '('
    && T.last txt == ')'
    && outerWraps txt '(' ')'

hasOuterBraces :: Text -> Bool
hasOuterBraces txt =
  T.length txt >= 2
    && T.head txt == '{'
    && T.last txt == '}'
    && outerWraps txt '{' '}'

outerWraps :: Text -> Char -> Char -> Bool
outerWraps txt open close =
  go 0 False False (T.unpack txt)
  where
    go _ _ _ [] = False
    go depth inStr inChr (c : cs)
      | inStr =
          if c == '"'
            then go depth False inChr cs
            else go depth True inChr cs
      | inChr =
          if c == '\''
            then go depth inStr False cs
            else go depth inStr True cs
      | c == '"' = go depth True inChr cs
      | c == '\'' = go depth inStr inChr cs
      | c == open = go (depth + 1) inStr inChr cs
      | c == close =
          let depth' = depth - 1
           in if depth' == 0
                then null cs
                else go depth' inStr inChr cs
      | otherwise = go depth inStr inChr cs

renderPatternText :: Pattern -> Text
renderPatternText pat =
  case pat of
    PVar name -> name
    PWildcard -> "_"
    PLit lit ->
      case lit of
        LitInt n -> T.pack (show n)
        LitIntBase _ repr -> repr
        LitFloat n -> T.pack (show n)
        LitChar c -> T.pack (show c)
        LitString s -> T.pack (show (T.unpack s))
    PTuple pats -> "(" <> T.intercalate ", " (map renderPatternText pats) <> ")"
    PList pats -> "[" <> T.intercalate ", " (map renderPatternText pats) <> "]"
    PCon con args -> T.unwords (con : map renderPatternText args)
    PInfix lhs op rhs -> renderPatternText lhs <> " " <> op <> " " <> renderPatternText rhs
    PAs name inner -> name <> "@" <> renderPatternText inner
    PIrrefutable inner -> "~" <> renderPatternText inner
    PNegLit lit -> "-" <> renderPatternText (PLit lit)
    PParen inner -> "(" <> renderPatternText inner <> ")"
    PRecord con fields ->
      con
        <> " { "
        <> T.intercalate ", " [name <> " = " <> renderPatternText p | (name, p) <- fields]
        <> " }"

fromMaybeText :: Text -> Maybe Text -> Text
fromMaybeText = fromMaybe

bundleToError :: Text -> MP.ParseErrorBundle Text Void -> ParseError
bundleToError input bundle =
  case MP.bundleErrors bundle of
    firstErr :| _ ->
      let off = errorOffset firstErr
          (ln, cl) = offsetToLineCol input off
          foundTok = tokenAt input off
          expectedItems = toExpectations firstErr
       in ParseError
            { offset = off,
              line = ln,
              col = cl,
              expected =
                if null expectedItems
                  then ["valid syntax"]
                  else expectedItems,
              found = foundTok
            }

toExpectations :: MP.ParseError Text Void -> [Text]
toExpectations parseErr =
  case parseErr of
    MP.TrivialError _ _ expectedItems ->
      map renderErrorItem (Set.toList expectedItems)
    MP.FancyError _ _ -> []

renderErrorItem :: MP.ErrorItem Char -> Text
renderErrorItem item =
  case item of
    MP.Tokens chars -> T.pack (NE.toList chars)
    MP.Label labelChars -> T.pack (NE.toList labelChars)
    MP.EndOfInput -> "<eof>"

offsetToLineCol :: Text -> Int -> (Int, Int)
offsetToLineCol input rawOffset =
  let len = T.length input
      off
        | rawOffset < 0 = 0
        | rawOffset > len = len
        | otherwise = rawOffset
      prefix = T.take off input
      lineNo = 1 + T.count "\n" prefix
      colNo = T.length (T.takeWhileEnd (/= '\n') prefix) + 1
   in (lineNo, colNo)

tokenAt :: Text -> Int -> Maybe Text
tokenAt input off
  | off < 0 = Nothing
  | off >= T.length input = Just "<eof>"
  | otherwise = Just (T.singleton (T.index input off))

reservedWords :: [Text]
reservedWords =
  [ "module",
    "where",
    "data",
    "class",
    "instance",
    "type",
    "newtype",
    "default",
    "foreign",
    "import",
    "export",
    "if",
    "then",
    "else",
    "let",
    "in",
    "case",
    "of",
    "do"
  ]

reservedWord :: MParser ()
reservedWord =
  foldr1 (<|>) (map oneReservedWord reservedWords)
  where
    oneReservedWord kw = try (C.string kw *> notFollowedBy identTailOrStartChar)
