{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseModule,
    defaultConfig,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Parser.Ast
import Parser.Types
import Text.Megaparsec
  ( Parsec,
    choice,
    eof,
    errorOffset,
    many,
    manyTill,
    notFollowedBy,
    optional,
    parse,
    runParser,
    sepBy,
    sepBy1,
    some,
    takeRest,
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
parseExpr cfg input =
  case parse (scExpr cfg *> expression cfg <* eof) "<expr>" input of
    Right ast -> ParseOk ast
    Left bundle -> ParseErr (bundleToError input bundle)

parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule cfg input =
  case parseModuleLines cfg input of
    Right ast -> ParseOk ast
    Left err -> ParseErr err

parseModuleLines :: ParserConfig -> Text -> Either ParseError Module
parseModuleLines cfg input = do
  let sourceLines = zip [1 ..] (T.lines input)
      cleaned = [(ln, stripComment cfg txtLine) | (ln, txtLine) <- sourceLines]
      nonEmpty = filter (not . T.null . T.strip . snd) cleaned
      meaningful = filter (not . isLanguagePragma . T.strip . snd) nonEmpty
      allowFfiFallback = any (isForeignDeclarationLine . T.strip . snd) meaningful
  case meaningful of
    [] -> Right Module {moduleName = Nothing, moduleDecls = [], moduleDeclChunks = Nothing}
    ((firstLineNo, firstLine) : rest) ->
      case parseModuleHeader (T.strip firstLine) of
        Right modName -> do
          let chunks = groupDeclarationChunks rest
          decls <- traverse (uncurry (parseDeclarationChunk cfg allowFfiFallback)) chunks
          let mergedDecls = mergeAdjacentFunctions decls
          Right
            Module
              { moduleName = Just modName,
                moduleDecls = mergedDecls,
                moduleDeclChunks =
                  if shouldPreserveDeclChunks mergedDecls
                    then Just (map snd chunks)
                    else Nothing
              }
        Left _ -> do
          let chunks = groupDeclarationChunks ((firstLineNo, firstLine) : rest)
          parsed <- traverse (uncurry (parseDeclarationChunk cfg allowFfiFallback)) chunks
          let merged = mergeAdjacentFunctions parsed
          Right
            Module
              { moduleName = Nothing,
                moduleDecls = merged,
                moduleDeclChunks =
                  if shouldPreserveDeclChunks merged
                    then Just (map snd chunks)
                    else Nothing
              }

parseModuleHeader :: Text -> Either ParseError Text
parseModuleHeader =
  parseLineWith headerParser
  where
    headerParser = do
      _ <- keyword "module"
      modName <- identifier
      _ <- keyword "where"
      eof
      pure modName

parseDeclarationChunk :: ParserConfig -> Bool -> Int -> Text -> Either ParseError Decl
parseDeclarationChunk cfg allowFfiFallback lineNo raw =
  let txt = T.strip raw
      parseWith parser =
        case parseLineWith parser txt of
          Right decl -> Right decl
          Left err ->
            Left
              err
                { line = lineNo,
                  offset = 0
                }
   in if "foreign import" `T.isPrefixOf` txt || "foreign export" `T.isPrefixOf` txt
        then parseWith (declarationParser allowFfiFallback)
        else case parseDeclText cfg txt of
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

parseLineWith :: MParser a -> Text -> Either ParseError a
parseLineWith parser input =
  case runParser parser "<line>" input of
    Right value -> Right value
    Left bundle -> Left (bundleToError input bundle)

declarationParser :: Bool -> MParser Decl
declarationParser allowFfiFallback
  | allowFfiFallback =
      choice
        [ try foreignImportDeclaration,
          try foreignExportDeclaration,
          try dataDeclaration,
          try typeSignatureDeclaration,
          try functionDeclaration,
          valueDeclaration
        ]
  | otherwise = try dataDeclaration <|> valueDeclaration

parseDeclText :: ParserConfig -> Text -> Either Text Decl
parseDeclText cfg txt
  | "data " `T.isPrefixOf` txt = parseDataLikeDecl "data" txt
  | "newtype " `T.isPrefixOf` txt = parseDataLikeDecl "newtype" txt
  | "type " `T.isPrefixOf` txt = parseTypeSynonymDecl txt
  | "class " `T.isPrefixOf` txt = parseClassDecl txt
  | "instance " `T.isPrefixOf` txt = parseInstanceDecl txt
  | "default " `T.isPrefixOf` txt = parseDefaultDecl txt
  | isFixityDecl txt = parseFixityDeclText txt
  | "::" `T.isInfixOf` txt = parseTypeSignatureDeclText txt
  | "=" `T.isInfixOf` txt = parseEquationDecl cfg txt
  | otherwise = Left "declaration"

parseTypeSynonymDecl :: Text -> Either Text Decl
parseTypeSynonymDecl txt =
  case typeNameAfterKeyword "type" txt of
    Just name -> Right TypeDecl {typeName = name}
    Nothing -> Left "type declaration"

parseDataLikeDecl :: Text -> Text -> Either Text Decl
parseDataLikeDecl keywordName txt = do
  typeName <- maybe (Left (keywordName <> " declaration")) Right (typeNameAfterKeyword keywordName txt)
  let (_, afterEq) = T.breakOn "=" txt
      constructors = if T.null afterEq then [] else parseConstructors (T.drop 1 afterEq)
   in case keywordName of
        "data" ->
          Right
            DataDecl
              { dataTypeName = typeName,
                dataConstructors = constructors
              }
        "newtype" ->
          Right
            NewtypeDecl
              { newtypeName = typeName,
                newtypeConstructor = case constructors of
                  [] -> Nothing
                  (firstCtor : _) -> Just firstCtor
              }
        _ -> Left "data declaration"

parseClassDecl :: Text -> Either Text Decl
parseClassDecl txt =
  case classOrInstanceName "class" txt of
    Just name -> Right ClassDecl {className = name}
    Nothing -> Left "class declaration"

parseInstanceDecl :: Text -> Either Text Decl
parseInstanceDecl txt =
  case classOrInstanceName "instance" txt of
    Just name -> Right InstanceDecl {instanceClassName = name}
    Nothing -> Left "instance declaration"

parseFixityDeclText :: Text -> Either Text Decl
parseFixityDeclText txt =
  case T.words txt of
    assocTxt : rest
      | assocTxt `elem` ["infix", "infixl", "infixr"] ->
          case rest of
            [] -> Left "fixity declaration"
            [op] ->
              Right
                FixityDecl
                  { fixityAssoc = assocTxt,
                    fixityPrecedence = Nothing,
                    fixityOperator = stripParens op
                  }
            precTxt : op : _
              | T.all isDigit precTxt ->
                  Right
                    FixityDecl
                      { fixityAssoc = assocTxt,
                        fixityPrecedence = Just (read (T.unpack precTxt)),
                        fixityOperator = stripParens op
                      }
            _ -> Left "fixity declaration"
    _ -> Left "fixity declaration"

parseDefaultDecl :: Text -> Either Text Decl
parseDefaultDecl txt =
  let inside = textInsideParens txt
      types = filter isTypeToken (map (T.strip . stripPunctuation) (T.splitOn "," inside))
   in if null types
        then Left "default declaration"
        else Right DefaultDecl {defaultTypes = types}

parseTypeSignatureDeclText :: Text -> Either Text Decl
parseTypeSignatureDeclText txt =
  let (lhs, rhs) = T.breakOn "::" txt
      names = map (stripParens . T.strip) (T.splitOn "," lhs)
      firstName = case names of
        [] -> Nothing
        (n : _) -> if isValueName n then Just n else Nothing
   in if T.null rhs
        then Left "type signature"
        else case firstName of
          Just name -> Right TypeSigDecl {typeSigName = name}
          Nothing -> Left "type signature"

parseEquationDecl :: ParserConfig -> Text -> Either Text Decl
parseEquationDecl cfg txt =
  let (lhsRaw, rhsRaw) = T.breakOn "=" txt
      lhs = T.strip lhsRaw
      rhs = T.strip (T.drop 1 rhsRaw)
   in if T.null rhsRaw || T.null lhs || T.null rhs
        then Left "equation declaration"
        else case extractFunctionName lhs of
          Just (name, hasArgs, isOperatorBinderName) ->
            if isOperatorBinderName
              then Right FunctionDecl {functionName = name}
              else case parseExpr cfg rhs of
                ParseOk expr -> Right Decl {declName = name, declExpr = expr}
                ParseErr _
                  | hasArgs -> Right FunctionDecl {functionName = name}
                  | otherwise -> Left "equation declaration"
          Nothing
            | isPatternBindingLhs lhs ->
                Right PatternDecl {patternLhs = lhs}
            | otherwise -> Left "equation declaration"

parseConstructors :: Text -> [Text]
parseConstructors rhs =
  let rhsCore = stripDerivingClause rhs
      parts = filter (not . T.null) (map T.strip (splitTopLevelPipes rhsCore))
   in mapMaybe constructorNameFromPart parts

splitTopLevelPipes :: Text -> [Text]
splitTopLevelPipes input = go 0 0 0 input T.empty []
  where
    go parenN braceN bracketN remaining current acc =
      case T.uncons remaining of
        Nothing -> reverse (current : acc)
        Just (c, cs)
          | c == '(' -> go (parenN + 1) braceN bracketN cs (T.snoc current c) acc
          | c == ')' -> go (max 0 (parenN - 1)) braceN bracketN cs (T.snoc current c) acc
          | c == '{' -> go parenN (braceN + 1) bracketN cs (T.snoc current c) acc
          | c == '}' -> go parenN (max 0 (braceN - 1)) bracketN cs (T.snoc current c) acc
          | c == '[' -> go parenN braceN (bracketN + 1) cs (T.snoc current c) acc
          | c == ']' -> go parenN braceN (max 0 (bracketN - 1)) cs (T.snoc current c) acc
          | c == '|' && parenN == 0 && braceN == 0 && bracketN == 0 ->
              go parenN braceN bracketN cs T.empty (T.strip current : acc)
          | otherwise ->
              go parenN braceN bracketN cs (T.snoc current c) acc

constructorNameFromPart :: Text -> Maybe Text
constructorNameFromPart part =
  let noBang = T.replace "!" "" part
      recordHead = T.strip (fst (T.breakOn "{" noBang))
      tokens = map stripParens (T.words recordHead)
      symbolic = filter isSymbolicToken tokens
   in case tokens of
        [] -> Nothing
        (firstTok : _)
          | isTypeToken firstTok -> Just firstTok
          | otherwise ->
              case symbolic of
                (op : _) -> Just op
                [] -> Nothing

typeNameAfterKeyword :: Text -> Text -> Maybe Text
typeNameAfterKeyword kw txt = do
  afterKw <- T.stripPrefix (kw <> " ") txt
  let beforeEq = T.strip (fst (T.breakOn "=" afterKw))
      beforeDeriving = T.strip (fst (T.breakOn " deriving" beforeEq))
      afterCtx = case T.breakOn "=>" beforeDeriving of
        (lhs, rhs)
          | T.null rhs -> lhs
          | otherwise -> T.strip (T.drop 2 rhs)
      tokens = map stripParens (T.words (T.strip afterCtx))
  findTypeToken tokens

classOrInstanceName :: Text -> Text -> Maybe Text
classOrInstanceName kw txt = do
  afterKw <- T.stripPrefix (kw <> " ") txt
  let noWhere = T.strip (fst (T.breakOn " where" afterKw))
      afterCtx = case T.breakOn "=>" noWhere of
        (lhs, rhs)
          | T.null rhs -> lhs
          | otherwise -> T.strip (T.drop 2 rhs)
      tokens = map stripParens (T.words (T.strip afterCtx))
  findTypeToken tokens

findTypeToken :: [Text] -> Maybe Text
findTypeToken = find isTypeToken

extractFunctionName :: Text -> Maybe (Text, Bool, Bool)
extractFunctionName lhs =
  case T.words lhs of
    [] -> Nothing
    (firstTok : rest) ->
      let normalized = stripParens firstTok
          isOperatorBinderName = isOperatorToken normalized && T.isPrefixOf "(" (T.strip firstTok)
       in if isVarToken normalized || isOperatorBinderName
            then Just (normalized, not (null rest), isOperatorBinderName)
            else Nothing

stripDerivingClause :: Text -> Text
stripDerivingClause txt =
  let (before, after) = T.breakOn " deriving" txt
   in if T.null after then txt else before

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

stripPunctuation :: Text -> Text
stripPunctuation = T.dropAround (\c -> c == '(' || c == ')' || c == ',' || isSpace c)

textInsideParens :: Text -> Text
textInsideParens txt =
  let (_, afterOpen) = T.breakOn "(" txt
      insideWithClose = T.drop 1 afterOpen
   in fst (T.breakOn ")" insideWithClose)

isTypeToken :: Text -> Bool
isTypeToken token =
  case T.uncons (stripPunctuation token) of
    Just (c, _) -> isUpper c
    Nothing -> False

isVarToken :: Text -> Bool
isVarToken token =
  case T.uncons token of
    Just (c, rest) ->
      (isLower c || c == '_')
        && T.all isIdentTailOrStart rest
    Nothing -> False

isSymbolicToken :: Text -> Bool
isSymbolicToken tok =
  not (T.null tok) && T.all (`elem` (":!#$%&*+./<=>?@\\^|-~" :: String)) tok

isOperatorToken :: Text -> Bool
isOperatorToken tok = isSymbolicToken tok && not (T.null tok)

isValueName :: Text -> Bool
isValueName tok = isVarToken tok || isOperatorToken tok

isPatternBindingLhs :: Text -> Bool
isPatternBindingLhs lhs =
  case T.uncons (T.strip lhs) of
    Just ('(', _) -> True
    _ -> False

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
                  Nothing -> go (Just (ln, [rawLine])) acc rest
                  Just (startLn, pieces@(firstPiece : _))
                    | ind == 0 && not (continuesPrevious firstPiece trimmed) ->
                        go (Just (ln, [rawLine])) ((startLn, T.intercalate "\n" (reverse pieces)) : acc) rest
                    | otherwise ->
                        go (Just (startLn, rawLine : pieces)) acc rest
                  Just _ -> go current acc rest

continuesPrevious :: Text -> Text -> Bool
continuesPrevious previousLine nextLine =
  case (equationBinderInfo previousLine, equationBinderInfo nextLine) of
    (Just (lhsA, mergeableA), Just (lhsB, mergeableB)) -> mergeableA && mergeableB && lhsA == lhsB
    _ -> False

equationBinderInfo :: Text -> Maybe (Text, Bool)
equationBinderInfo lineTxt = do
  let (lhs, rhs) = T.breakOn "=" lineTxt
  if T.null rhs
    then Nothing
    else do
      (name, hasArgs, isOperatorBinderName) <- extractFunctionName (T.strip lhs)
      pure (name, hasArgs || isOperatorBinderName)

indentation :: Text -> Int
indentation = T.length . T.takeWhile (\c -> c == ' ' || c == '\t')

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f =
  foldr
    ( \x acc ->
        case f x of
          Just y -> y : acc
          Nothing -> acc
    )
    []

find :: (a -> Bool) -> [a] -> Maybe a
find predicate =
  foldr
    ( \x acc ->
        if predicate x
          then Just x
          else acc
    )
    Nothing

mergeAdjacentFunctions :: [Decl] -> [Decl]
mergeAdjacentFunctions =
  reverse . foldl merge []
  where
    merge acc decl =
      case (acc, decl) of
        (FunctionDecl {functionName = prev} : rest, FunctionDecl {functionName = curr})
          | prev == curr -> acc
        _ -> decl : acc

shouldPreserveDeclChunks :: [Decl] -> Bool
shouldPreserveDeclChunks decls =
  any isStructuralDecl decls && not (any isValueDecl decls)
  where
    isStructuralDecl decl =
      case decl of
        DataDecl {} -> True
        NewtypeDecl {} -> True
        TypeDecl {} -> True
        ClassDecl {} -> True
        InstanceDecl {} -> True
        FixityDecl {} -> True
        DefaultDecl {} -> True
        _ -> False
    isValueDecl decl =
      case decl of
        Decl {} -> True
        _ -> False

valueDeclaration :: MParser Decl
valueDeclaration = do
  name <- identifier
  _ <- symbol "="
  rhs <- expressionLine
  eof
  pure Decl {declName = name, declExpr = rhs}

dataDeclaration :: MParser Decl
dataDeclaration = do
  _ <- keyword "data"
  typeName <- typeConstructor
  _ <- symbol "="
  constructors <- sepBy1 typeConstructor (symbol "|")
  eof
  pure DataDecl {dataTypeName = typeName, dataConstructors = constructors}

typeSignatureDeclaration :: MParser Decl
typeSignatureDeclaration = do
  name <- identifier
  _ <- symbol "::"
  sigTail <- T.strip <$> takeRest
  if T.null sigTail
    then fail "expected type signature"
    else pure TypeSigDecl {typeSigName = name}

functionDeclaration :: MParser Decl
functionDeclaration = do
  name <- identifier
  _ <- some identifier
  _ <- symbol "="
  rhs <- T.strip <$> takeRest
  if T.null rhs
    then fail "expected function body"
    else pure FunctionDecl {functionName = name}

foreignImportDeclaration :: MParser Decl
foreignImportDeclaration = do
  _ <- keyword "foreign"
  _ <- keyword "import"
  callConv <- callConvParser
  safety <- optional (try safetyParser)
  entity <- optional (try foreignEntityParser)
  name <- identifier
  _ <- symbol "::"
  ftype <- T.strip <$> takeRest
  if T.null ftype
    then fail "expected foreign import type"
    else
      pure
        ForeignDecl
          { foreignDirection = ForeignImport,
            foreignCallConv = callConv,
            foreignSafety = safety,
            foreignEntity = entity,
            foreignName = name
          }

foreignExportDeclaration :: MParser Decl
foreignExportDeclaration = do
  _ <- keyword "foreign"
  _ <- keyword "export"
  callConv <- callConvParser
  entity <- optional (try foreignEntityParser)
  name <- identifier
  _ <- symbol "::"
  etype <- T.strip <$> takeRest
  if T.null etype
    then fail "expected foreign export type"
    else
      pure
        ForeignDecl
          { foreignDirection = ForeignExport,
            foreignCallConv = callConv,
            foreignSafety = Nothing,
            foreignEntity = entity,
            foreignName = name
          }

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
  txt <- manyTill C.printChar (C.char '"')
  pure (T.pack txt)

expression :: ParserConfig -> MParser Expr
expression cfg = expressionWith (scExpr cfg)

expressionLine :: MParser Expr
expressionLine = expressionWith scLine

expressionWith :: MParser () -> MParser Expr
expressionWith sc = do
  atoms <- some (atomWith sc)
  pure (foldl1 EApp atoms)

atomWith :: MParser () -> MParser Expr
atomWith sc =
  listLiteral sc
    <|> parenExpression sc
    <|> try (EFloat <$> floating sc)
    <|> (EInt <$> integer sc)
    <|> (EChar <$> charLiteral sc)
    <|> (EString <$> stringLiteral sc)
    <|> (EVar <$> identifierLexeme sc)

listLiteral :: MParser () -> MParser Expr
listLiteral sc = do
  _ <- symbolWith sc "["
  elems <- expressionWith sc `sepBy` symbolWith sc ","
  _ <- symbolWith sc "]"
  pure (EList elems)

parenExpression :: MParser () -> MParser Expr
parenExpression sc =
  try (tupleConstructor sc)
    <|> do
      _ <- symbolWith sc "("
      ( do
          _ <- symbolWith sc ")"
          pure (ETuple [])
        )
        <|> do
          firstExpr <- expressionWith sc
          ( do
              _ <- symbolWith sc ","
              rest <- expressionWith sc `sepBy1` symbolWith sc ","
              _ <- symbolWith sc ")"
              pure (ETuple (firstExpr : rest))
            )
            <|> do
              _ <- symbolWith sc ")"
              pure firstExpr

tupleConstructor :: MParser () -> MParser Expr
tupleConstructor sc = do
  _ <- symbolWith sc "("
  commas <- some (lexeme sc (C.char ','))
  _ <- symbolWith sc ")"
  pure (ETupleCon (length commas + 1))

identifier :: MParser Text
identifier = identifierLexeme scLine

typeConstructor :: MParser Text
typeConstructor = lexeme scLine $ do
  first <- C.upperChar
  rest <- many identTailChar
  pure (T.pack (first : rest))

identifierLexeme :: MParser () -> MParser Text
identifierLexeme sc = lexeme sc $ do
  notFollowedBy reservedWord
  first <- C.letterChar <|> C.char '_'
  rest <- many identTailChar
  pure (T.pack (first : rest))

identTailChar :: MParser Char
identTailChar =
  C.alphaNumChar
    <|> C.char '_'
    <|> C.char '\''

integer :: MParser () -> MParser Integer
integer sc = lexeme sc (try hexadecimal <|> L.decimal)
  where
    hexadecimal = do
      _ <- C.string "0x" <|> C.string "0X"
      L.hexadecimal

floating :: MParser () -> MParser Double
floating sc = lexeme sc $ do
  whole <- some C.digitChar
  _ <- C.char '.'
  frac <- some C.digitChar
  pure (read (whole <> "." <> frac))

charLiteral :: MParser () -> MParser Char
charLiteral sc = lexeme sc (C.char '\'' *> L.charLiteral <* C.char '\'')

stringLiteral :: MParser () -> MParser Text
stringLiteral sc = lexeme sc $ do
  _ <- C.char '"'
  chars <- manyTill L.charLiteral (C.char '"')
  pure (T.pack chars)

symbol :: Text -> MParser Text
symbol = L.symbol scLine

keyword :: Text -> MParser Text
keyword kw = lexeme scLine (C.string kw <* notFollowedBy identTailOrStartChar)

symbolWith :: MParser () -> Text -> MParser Text
symbolWith = L.symbol

identTailOrStartChar :: MParser Char
identTailOrStartChar = MP.satisfy isIdentTailOrStart

isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart c = isAlphaNum c || c == '_' || c == '\''

lexeme :: MParser () -> MParser a -> MParser a
lexeme = L.lexeme

scLine :: MParser ()
scLine = L.space C.space1 MP.empty MP.empty

scExpr :: ParserConfig -> MParser ()
scExpr cfg
  | allowLineComments cfg = L.space C.space1 (L.skipLineComment "--") MP.empty
  | otherwise = L.space C.space1 MP.empty MP.empty

stripComment :: ParserConfig -> Text -> Text
stripComment cfg txtLine
  | not (allowLineComments cfg) = txtLine
  | otherwise =
      case T.breakOn "--" txtLine of
        (before, after)
          | T.null after -> txtLine
          | otherwise -> T.stripEnd before

isLanguagePragma :: Text -> Bool
isLanguagePragma txt =
  "{-#" `T.isPrefixOf` txt && "#-}" `T.isSuffixOf` txt

isForeignDeclarationLine :: Text -> Bool
isForeignDeclarationLine txt =
  "foreign import" `T.isPrefixOf` txt || "foreign export" `T.isPrefixOf` txt

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
reservedWords = ["module", "where", "data", "foreign", "import", "export"]

reservedWord :: MParser ()
reservedWord =
  choice (map oneReservedWord reservedWords)
  where
    oneReservedWord kw = try (C.string kw *> notFollowedBy identTailOrStartChar)
