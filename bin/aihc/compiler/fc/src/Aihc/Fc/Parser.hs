{-# LANGUAGE OverloadedStrings #-}

-- | Parse System FC text.
module Aihc.Fc.Parser
  ( FcParseError,
    parseProgram,
    renderParseError,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isHexDigit, ord)
import Data.Either (isLeft, lefts, partitionEithers)
import Data.Functor (($>))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = ReaderT ScopeTable (Parsec Void Text)

type FcParseError = ParseErrorBundle Text Void

parseProgram :: Text -> Either FcParseError Program
parseProgram input = do
  (scopes, body) <- parseScopeHeader input
  parseWith scopes (space *> program <* MP.eof) "<system-fc>" body

renderParseError :: FcParseError -> String
renderParseError = MP.errorBundlePretty

parseWith :: ScopeTable -> Parser value -> String -> Text -> Either FcParseError value
parseWith scopes parser = MP.parse (runReaderT parser scopes)

parseScopeHeader :: Text -> Either FcParseError (ScopeTable, Text)
parseScopeHeader = MP.parse parser "<system-fc-scope>"
  where
    parser = ((,) . toScopeTable <$> runReaderT (MP.many scopeDeclaration) emptyScopeTable) <*> MP.takeRest
    toScopeTable = foldr (\(scopeId, package, moduleName) -> insertScope scopeId package moduleName) emptyScopeTable

scopeDeclaration :: Parser (Int, PackageId, Text)
scopeDeclaration =
  (,,)
    <$> (keyword "scope" *> int)
    <*> (PackageId <$> (symbol "=" *> stringLiteral))
    <*> qualifiedModuleName

program :: Parser Program
program = do
  scopes <- ask
  imports <- foldr ($) emptyImports . concat <$> MP.many importGroup
  Program scopes imports <$> MP.many declaration
  where
    emptyImports = Imports Map.empty Map.empty Map.empty Map.empty

importGroup :: Parser [Imports -> Imports]
importGroup = do
  _ <- keyword "import"
  MP.choice
    [ keyword "headers" *> importEntries importedHeader,
      keyword "synonyms" *> importEntries importedSynonym,
      keyword "axioms" *> importEntries importedAxiom,
      keyword "type-binders" *> importEntries (importedBinder SortTypeVariable),
      keyword "value-binders" *> importEntries (importedBinder SortValue)
    ]

importEntries :: Parser (Imports -> Imports) -> Parser [Imports -> Imports]
importEntries entry = entry `MP.sepBy1` symbol ";"

importedHeader :: Parser (Imports -> Imports)
importedHeader = do
  name <- importedHeaderName
  ty <- symbol "::" *> fcType
  pure (\imports -> imports {importHeaders = Map.insert name ty (importHeaders imports)})

importedSynonym :: Parser (Imports -> Imports)
importedSynonym = do
  name <- topName SortSynonym
  ty <- symbol "=" *> fcType
  pure (\imports -> imports {importSynonyms = Map.insert name ty (importSynonyms imports)})

importedBinder :: Sort -> Parser (Imports -> Imports)
importedBinder sort = do
  name <- localNameWithSort sort
  ty <- symbol "::" *> fcType
  pure (\imports -> imports {importBinders = Map.insert name ty (importBinders imports)})

importedAxiom :: Parser (Imports -> Imports)
importedAxiom = do
  name <- topName SortAxiom
  binders <- MP.many openPiBinder
  _ <- symbol ":"
  left <- fcType
  role <- parseAxiomRole
  right <- fcType
  let axiom = AxiomDecl Private name binders role left right
  pure (\imports -> imports {importAxioms = Map.insert name axiom (importAxioms imports)})

importedHeaderName :: Parser Name
importedHeaderName =
  MP.choice
    [ MP.try (topName SortTypeConstructor),
      MP.try (topName SortDataConstructor),
      MP.try (topName SortSynonym),
      topName SortValue
    ]

declaration :: Parser Decl
declaration =
  MP.choice
    [ MP.try typeOrSynonym,
      MP.try axiomDeclaration,
      valDeclaration
    ]

typeOrSynonym :: Parser Decl
typeOrSynonym = do
  vis <- optionalPub
  _ <- keyword "type"
  name <- topName SortTypeConstructor
  binders <- MP.many openPiBinder
  _ <- symbol "::"
  result <- fcType
  roles <- MP.option [] roleList
  MP.choice
    [ do
        _ <- symbol "="
        DeclSynonym
          . SynonymDecl vis name {nameSort = SortSynonym} binders result
          <$> fcType,
      DeclType
        . TypeDecl vis name binders result (defaultRoles binders roles)
        <$> constructorBlock
    ]

constructorBlock :: Parser [ConDecl]
constructorBlock = braces (MP.many (constructorDecl <* MP.optional (symbol ";")))

constructorDecl :: Parser ConDecl
constructorDecl = ConDecl <$> optionalPub <*> topName SortDataConstructor <*> (symbol "::" *> fcType)

axiomDeclaration :: Parser Decl
axiomDeclaration = do
  vis <- optionalPub
  _ <- keyword "axiom"
  name <- topName SortAxiom
  binders <- MP.option [] (MP.try (MP.some openPiBinder))
  _ <- symbol ":"
  left <- fcType
  role <- parseAxiomRole
  DeclAxiom . AxiomDecl vis name binders role left <$> fcType

-- | A foreign call: @foreign {convention deps name :: type} @ty... arg...@.
-- Every type argument comes before the value arguments.
foreignCallExpr :: Parser Expr
foreignCallExpr = do
  _ <- keyword "foreign"
  call <- MP.between (symbol "{") (symbol "}") foreignCall
  arguments <- MP.many appArgument
  let (types, rest) = span isLeft arguments
  case partitionEithers rest of
    ([], values) -> pure (ExForeignCall call (lefts types) values)
    _ -> fail "a foreign call takes its type arguments before its value arguments"

foreignCall :: Parser ForeignCall
foreignCall = do
  convention <- callingConvention
  dependencies <- MP.option [] parseForeignImportDependencies
  name <- topName SortValue
  _ <- symbol "::"
  ForeignCall name convention dependencies <$> fcType

parseForeignImportDependencies :: Parser [ForeignImportDependency]
parseForeignImportDependencies =
  keyword "using"
    *> MP.between
      (symbol "[")
      (symbol "]")
      (foreignImportDependency `MP.sepBy1` symbol ",")

foreignImportDependency :: Parser ForeignImportDependency
foreignImportDependency =
  MP.choice
    [ ForeignAxiom <$> (keyword "axiom" *> topName SortAxiom),
      ForeignConstructor <$> (keyword "constructor" *> topName SortDataConstructor)
    ]

callingConvention :: Parser CallingConvention
callingConvention =
  MP.choice
    [ keyword "prim" $> Prim,
      makeCCall
        <$> (keyword "ccall" *> callTarget)
        <*> foreignSafety
        <*> stringLiteral
        <*> MP.between (symbol "[") (symbol "]") foreignSignature
    ]
  where
    makeCCall target safety foreignSymbol (arguments, result, effect) =
      CCall
        CCallSpec
          { ccallSymbol = foreignSymbol,
            ccallTarget = target,
            ccallSafety = safety,
            ccallArgumentTypes = arguments,
            ccallResultType = result,
            ccallEffect = effect
          }

callTarget :: Parser CCallTarget
callTarget = MP.option CCallFunction (keyword "address" $> CCallAddress)

foreignSafety :: Parser ForeignSafety
foreignSafety = (keyword "unsafe" $> ForeignUnsafe) <|> (keyword "safe" $> ForeignSafe)

foreignSignature :: Parser ([CAbiType], CAbiType, ForeignEffect)
foreignSignature =
  (,,)
    <$> cAbiType `MP.sepBy` symbol ","
    <*> (symbol "→" *> cAbiType)
    <*> (symbol ";" *> foreignEffect)

foreignEffect :: Parser ForeignEffect
foreignEffect = (keyword "pure" $> ForeignPure) <|> (keyword "real-world" $> ForeignRealWorld)

cAbiType :: Parser CAbiType
cAbiType =
  MP.choice
    [ keyword "Int8" $> CAbiInt8,
      keyword "Int16" $> CAbiInt16,
      keyword "Int32" $> CAbiInt32,
      keyword "Int64" $> CAbiInt64,
      keyword "Int" $> CAbiInt,
      keyword "Word8" $> CAbiWord8,
      keyword "Word16" $> CAbiWord16,
      keyword "Word32" $> CAbiWord32,
      keyword "Word64" $> CAbiWord64,
      keyword "Word" $> CAbiWord,
      keyword "Addr" $> CAbiAddr,
      keyword "Void" $> CAbiVoid
    ]

valDeclaration :: Parser Decl
valDeclaration = do
  vis <- optionalPub
  _ <- keyword "val"
  name <- topName SortValue
  _ <- symbol "::"
  ty <- fcType
  _ <- symbol "="
  DeclVal . ValDecl vis name ty <$> expression

optionalPub :: Parser Vis
optionalPub = MP.option Private (keyword "pub" $> Pub)

roleList :: Parser [Role]
roleList = MP.some (symbol "@" *> roleTag)

roleTag :: Parser Role
roleTag =
  MP.choice
    [ symbol "N" $> Nominal,
      symbol "R" $> Representational,
      symbol "P" $> Phantom
    ]

parseAxiomRole :: Parser Role
parseAxiomRole =
  MP.choice
    [ symbol "~N" $> Nominal,
      symbol "~R" $> Representational,
      symbol "~P" $> Phantom
    ]

fcType :: Parser Type
fcType = forallType

forallType :: Parser Type
forallType =
  MP.choice
    [ flip (foldr TyForAll)
        <$> (symbol "∀" *> MP.some openPiBinder <* symbol ".")
        <*> forallType,
      funType
    ]

funType :: Parser Type
funType = do
  left <- eqType
  MP.option left $ do
    representation <- scopedArrow
    TyFun representation representation left <$> funType

scopedArrow :: Parser Type
scopedArrow = lexeme $ do
  (package, moduleName) <- scopeReference
  _ <- MPC.string "→" <|> MPC.string "->"
  pure (TyCon (Name "LiftedRep" SortSynonym (OriginTop package moduleName)))

eqType :: Parser Type
eqType = do
  left <- appType
  MP.option left $ do
    _ <- MP.try (symbol "~" <* MP.notFollowedBy axiomRoleLetter)
    TyEq left <$> appType

axiomRoleLetter :: Parser Char
axiomRoleLetter = do
  letter <- MP.satisfy (`elem` ("NRP" :: String))
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just character
      | identContinue character -> fail "role prefix"
    _ -> pure letter

appType :: Parser Type
appType = MP.try explicitFun <|> (foldl TyApp <$> typeAtom <*> MP.many (MP.try typeAtom))

explicitFun :: Parser Type
explicitFun =
  TyFun
    <$> (keyword "FUN" *> symbol "@" *> typeAtom)
    <*> (symbol "@" *> typeAtom)
    <*> typeAtom
    <*> typeAtom

typeAtom :: Parser Type
typeAtom =
  MP.choice
    [ parens fcType,
      TyVar <$> MP.try typeLocalName,
      TyCon <$> topNameWithSort
    ]

reservedWords :: [Text]
reservedWords =
  [ "pub",
    "val",
    "type",
    "axiom",
    "foreign",
    "import",
    "prim",
    "module",
    "where",
    "let",
    "rec",
    "in",
    "case",
    "as",
    "of",
    "FUN",
    "refl",
    "sym",
    "trans",
    "tycon-co",
    "axiom-co"
  ]

openPiBinder :: Parser Binder
openPiBinder = parens (Binder <$> localBinderName SortTypeVariable <*> (symbol ":" *> fcType))

expression :: Parser Expr
expression =
  MP.choice
    [ lambdaExpr,
      typeLambdaExpr,
      letExpr,
      recExpr,
      caseExpr,
      castOrApp
    ]

lambdaExpr :: Parser Expr
lambdaExpr = ExLam <$> (symbol "λ" *> openTermBinder SortValue) <*> (symbol "." *> expression)

typeLambdaExpr :: Parser Expr
typeLambdaExpr = ExTyLam <$> (symbol "Λ" *> openTermBinder SortTypeVariable) <*> (symbol "." *> expression)

openTermBinder :: Sort -> Parser Binder
openTermBinder sort = parens (Binder <$> localBinderName sort <*> (symbol ":" *> fcType))

letExpr :: Parser Expr
letExpr = ExLet <$> (keyword "let" *> braces openBind) <*> (keyword "in" *> expression)

recExpr :: Parser Expr
recExpr = ExRec <$> (keyword "rec" *> braces (MP.sepBy openBind (symbol ";"))) <*> (keyword "in" *> expression)

openBind :: Parser Bind
openBind = makeBind <$> localBinderName SortValue <*> (symbol ":" *> fcType) <*> (symbol "=" *> expression)
  where
    makeBind name ty = Bind (Binder name ty)

caseExpr :: Parser Expr
caseExpr =
  ExCase
    <$> (keyword "case" *> expression)
    <*> (keyword "as" *> openTermBinder SortValue)
    <*> (keyword "return" *> parens fcType)
    <*> (keyword "of" *> braces (MP.sepBy caseAlt (symbol ";")))

caseAlt :: Parser Alt
caseAlt =
  MP.choice
    [ Alt AltDefault [] [] <$> (symbol "_" *> caseArrow *> expression),
      Alt
        <$> (MP.try (AltLit <$> literal) <|> (AltData <$> topNameWithSort))
        <*> MP.many (symbol "@" *> openTermBinder SortTypeVariable)
        <*> MP.many (openTermBinder SortValue)
        <*> (caseArrow *> expression)
    ]

caseArrow :: Parser Text
caseArrow = symbol "→" <|> symbol "->"

castOrApp :: Parser Expr
castOrApp = do
  function <- appExpr
  MP.option function $ do
    _ <- symbol "▷"
    ExCast function <$> coercion

appExpr :: Parser Expr
appExpr = foreignCallExpr <|> (foldl applyArg <$> exprAtom <*> MP.many appArgument)
  where
    applyArg function argument =
      case argument of
        Left ty -> ExTyApp function ty
        Right expr -> ExApp function expr

appArgument :: Parser (Either Type Expr)
appArgument =
  MP.choice
    [ Left <$> (symbol "@" *> typeAtom),
      Right <$> exprAtom
    ]

exprAtom :: Parser Expr
exprAtom =
  MP.choice
    [ parens expression,
      ExLit <$> MP.try literal,
      ExVar <$> MP.try localName,
      ExVar <$> topNameWithSort
    ]

coercion :: Parser Coercion
coercion =
  MP.choice
    [ CoRefl <$> (keyword "refl" *> typeAtom),
      CoSym <$> (keyword "sym" *> parens coercion),
      CoTrans <$> (keyword "trans" *> parens coercion) <*> parens coercion,
      CoTyConApp <$> (keyword "tycon-co" *> topNameWithSort) <*> MP.many (parens coercion),
      CoAxiom <$> (keyword "axiom-co" *> topNameWithSort) <*> MP.many (symbol "@" *> typeAtom),
      CoVar <$> localName
    ]

literal :: Parser Literal
literal =
  MP.choice
    [ flip LitInt <$> integerLiteral <*> (MPC.char '#' *> representationType),
      flip LitChar <$> charLiteral <*> (MPC.char '#' *> representationType),
      flip LitAddr <$> addrLiteral <*> (MPC.char '#' *> representationType)
    ]

representationType :: Parser Type
representationType = TyCon <$> (MP.try localName <|> topNameWithSort)

topNameWithSort :: Parser Name
topNameWithSort = topName SortValue

topName :: Sort -> Parser Name
topName defaultSort = lexeme (makeName <$> scopeReference <*> printedName defaultSort)
  where
    makeName (package, moduleName) (printed, sort) = Name printed sort (OriginTop package moduleName)

scopeReference :: Parser (PackageId, Text)
scopeReference = do
  scopeId <- L.decimal <* MPC.char '.'
  scopes <- ask
  case lookupScope scopeId scopes of
    Just scope -> pure scope
    Nothing -> fail ("unknown scope " <> show scopeId)

printedName :: Sort -> Parser (Text, Sort)
printedName defaultSort =
  MP.choice
    [ do
        prefix <- MP.optional (MP.satisfy (\character -> character == 't' || character == 'v'))
        raw <- rawName
        let printedClass =
              case prefix of
                Just 't' -> Just NameClassType
                Just 'v' -> Just NameClassValue
                _ | "$ax$" `T.isPrefixOf` raw -> Just NameClassAxiom
                _ -> Nothing
            sort =
              case printedClass of
                Just class'
                  | class' == nameClass defaultSort -> defaultSort
                  | class' == NameClassType -> SortTypeConstructor
                  | class' == NameClassValue -> SortValue
                  | class' == NameClassAxiom -> SortAxiom
                _ -> defaultSort
        pure (raw, sort)
    ]

rawName :: Parser Text
rawName =
  MP.choice
    [ "[]" <$ MPC.string "[]",
      MP.try tupleName,
      MP.try identName,
      operatorName
    ]

tupleName :: Parser Text
tupleName = unboxedTupleName <|> boxedTupleName

unboxedTupleName :: Parser Text
unboxedTupleName = makeTuple <$> MP.between (MPC.string "(#") (MPC.string "#)") (MP.many (MPC.char ','))
  where
    makeTuple commas = T.pack ("(#" <> commas <> "#)")

boxedTupleName :: Parser Text
boxedTupleName = makeTuple <$> MP.between (MPC.char '(') (MPC.char ')') (MP.many (MPC.char ','))
  where
    makeTuple commas = T.pack ('(' : commas <> ")")

identName :: Parser Text
identName = do
  first <- MP.satisfy identStart
  rest <- MP.many (MP.satisfy identContinue)
  listSuffix <- MP.option "" (MPC.string "[]")
  let value = T.pack (first : rest) <> listSuffix
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just next
      | first == '$' && next `elem` operatorNameCharacters -> fail "operator"
    _
      | value `elem` reservedWords -> fail "reserved word"
      | otherwise -> pure value

operatorName :: Parser Text
operatorName = do
  value <- T.pack <$> MP.some (MP.satisfy (`elem` operatorNameCharacters))
  if value `elem` reservedOperators
    then fail "reserved operator"
    else pure value

reservedOperators :: [Text]
reservedOperators = ["=", "::", "→", "->", "~", "@", "▷", "|"]

localName :: Parser Name
localName = localNameWithSort SortValue

typeLocalName :: Parser Name
typeLocalName = localNameWithSort SortTypeVariable

localBinderName :: Sort -> Parser Name
localBinderName = localNameWithSort

localNameWithSort :: Sort -> Parser Name
localNameWithSort sort = lexeme (makeName <$> rawName <*> MP.option 0 bracesInt)
  where
    makeName text unique = Name text sort (OriginLocal (Unique unique))

bracesInt :: Parser Int
bracesInt = MP.between (MPC.char '{') (MPC.char '}') L.decimal

defaultRoles :: [Binder] -> [Role] -> [Role]
defaultRoles binders roles
  | null roles && not (null binders) = replicate (length binders) Representational
  | otherwise = roles

-- Lexer

space :: Parser ()
space = lift (L.space MPC.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}"))

lexeme :: Parser a -> Parser a
lexeme parser = parser <* space

symbol :: Text -> Parser Text
symbol value = lexeme (MPC.string value)

keyword :: Text -> Parser Text
keyword value = lexeme $ do
  _ <- MPC.string value
  following <- MP.optional (MP.lookAhead MP.anySingle)
  case following of
    Just character
      | identContinue character -> fail "keyword prefix"
    _ -> pure value

int :: Parser Int
int = lexeme L.decimal

integerLiteral :: Parser Integer
integerLiteral = lexeme L.decimal

stringLiteral :: Parser Text
stringLiteral = lexeme (T.pack <$> MP.between (MPC.char '"') (MPC.char '"') (MP.many stringChar))

addrLiteral :: Parser BS.ByteString
addrLiteral = lexeme (BS.pack <$> MP.between (MPC.char '"') (MPC.char '"') (MP.many addrByte))

charLiteral :: Parser Char
charLiteral = lexeme (MP.between (MPC.char '\'') (MPC.char '\'') charLiteralValue)

charLiteralValue :: Parser Char
charLiteralValue =
  MP.choice
    [ bracedHexChar,
      MP.satisfy (\character -> character /= '\'' && character /= '\\'),
      MPC.string "\\\\" $> '\\',
      MPC.string "\\'" $> '\'',
      MPC.string "\\n" $> '\n'
    ]

bracedHexChar :: Parser Char
bracedHexChar = do
  _ <- MPC.string "\\x{"
  value <- L.hexadecimal
  _ <- MPC.char '}'
  if value <= fromEnum (maxBound :: Char)
    then pure (chr value)
    else fail "character literal is outside the character range"

stringChar :: Parser Char
stringChar =
  MP.choice
    [ hexChar,
      MP.satisfy (\character -> character /= '"' && character /= '\\'),
      MPC.string "\\\\" $> '\\',
      MPC.string "\\\"" $> '"',
      MPC.string "\\'" $> '\'',
      MPC.string "\\n" $> '\n'
    ]

addrByte :: Parser Word8
addrByte =
  MP.choice
    [ hexByte,
      fromIntegral . ord <$> MP.satisfy (\character -> character /= '"' && character /= '\\'),
      MPC.string "\\\\" $> 92,
      MPC.string "\\\"" $> 34,
      MPC.string "\\n" $> 10
    ]

hexChar :: Parser Char
hexChar = chr . fromIntegral <$> hexByte

hexByte :: Parser Word8
hexByte = makeByte <$> (MPC.string "\\x" *> MP.satisfy isHexDigit) <*> MP.satisfy isHexDigit
  where
    makeByte high low = fromIntegral (digitToInt high * 16 + digitToInt low)

qualifiedModuleName :: Parser Text
qualifiedModuleName = lexeme (T.intercalate "." <$> ((:) <$> identName <*> MP.many (MPC.char '.' *> identName)))

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = MP.between (symbol "{") (symbol "}")

identStart :: Char -> Bool
identStart character = isAlpha character || character == '_' || character == '$'

identContinue :: Char -> Bool
identContinue character = isAlphaNum character || character `elem` ("_$#'" :: String)

operatorNameCharacters :: [Char]
operatorNameCharacters = "!#$%&*+./<=>?@\\^|-~:"
