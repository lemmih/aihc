{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the human-readable GRIN syntax emitted by
-- "Aihc.Grin.Pretty".
module Aihc.Grin.Parser
  ( GrinParseError,
    parseProgram,
    parseExpr,
    renderParseError,
  )
where

import Aihc.Grin.Syntax
import Control.Applicative (optional, (<|>))
import Control.Monad (guard, void, when)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isSpace, ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

-- | The scope declarations of the program give the package and the module of
-- each numbered scope. A name that has no number needs no scope.
type Scopes = Map Int GrinScope

type Parser = ReaderT Scopes (Parsec Void Text)

type GrinParseError = ParseErrorBundle Text Void

parseProgram :: Text -> Either GrinParseError GrinProgram
parseProgram = MP.parse programParser "<grin>"

parseExpr :: Text -> Either GrinParseError GrinExpr
parseExpr = MP.parse (runReaderT (exprAfterIndent 0 <* MP.eof) Map.empty) "<grin-expression>"

renderParseError :: GrinParseError -> String
renderParseError = MP.errorBundlePretty

data TopDeclaration
  = TopConstructor (Text, [[GrinRep]])
  | TopPrimitive (GrinVar, Int)
  | TopForeign GrinForeignCall
  | TopGlobal (Text, GrinNode)
  | TopFunction GrinFunction

programParser :: Parsec Void Text GrinProgram
programParser = do
  scopes <- runReaderT (blankLines *> MP.many (scopeDeclaration <* blankLines)) Map.empty
  runReaderT programBody (Map.fromList scopes)

programBody :: Parser GrinProgram
programBody = do
  declarations <- MP.many (topDeclaration <* blankLines)
  MP.eof
  pure (foldl' addDeclaration emptyProgram declarations)

-- | @scope 1 = "package" Module.Name@ names one scope. Every later name of
-- that scope prints as @1.name@.
scopeDeclaration :: Parser (Int, GrinScope)
scopeDeclaration = do
  exactIndent 0
  keyword "scope"
  horizontal1
  number <- natural
  horizontal1
  _ <- MPC.char '='
  horizontal1
  packageName <- stringText
  horizontal1
  moduleName <- name
  lineEnd
  pure (number, GrinScope packageName moduleName)

emptyProgram :: GrinProgram
emptyProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions = []
    }

addDeclaration :: GrinProgram -> TopDeclaration -> GrinProgram
addDeclaration program declaration =
  case declaration of
    TopConstructor value -> program {grinConstructors = grinConstructors program <> [value]}
    TopPrimitive value -> program {grinPrimitives = grinPrimitives program <> [value]}
    TopForeign value -> program {grinForeignCalls = grinForeignCalls program <> [value]}
    TopGlobal value -> program {grinGlobals = grinGlobals program <> [value]}
    TopFunction value -> program {grinFunctions = grinFunctions program <> [value]}

topDeclaration :: Parser TopDeclaration
topDeclaration = do
  exactIndent 0
  MP.choice
    [ MP.try constructorDeclaration,
      MP.try primitiveDeclaration,
      MP.try foreignDeclaration,
      MP.try (TopGlobal <$> globalDeclaration),
      TopFunction <$> functionDeclaration
    ]

constructorDeclaration :: Parser TopDeclaration
constructorDeclaration = do
  keyword "constructor"
  horizontal1
  constructorName <- scopedName
  legacyArity <- optional (MPC.char '/' *> natural)
  horizontal1
  fieldLayouts <- constructorLayouts
  lineEnd
  when (maybe False (/= length fieldLayouts) legacyArity) $ fail "constructor arity does not match its layout"
  pure (TopConstructor (constructorName, fieldLayouts))

primitiveDeclaration :: Parser TopDeclaration
primitiveDeclaration = do
  keyword "primitive"
  horizontal1
  variable <- bareVar
  _ <- MPC.char '/'
  arity <- signedInt
  lineEnd
  pure (TopPrimitive (variable, arity))

foreignDeclaration :: Parser TopDeclaration
foreignDeclaration = do
  keyword "foreign"
  horizontal1
  foreignCall <- foreignCallDefinition
  lineEnd
  pure (TopForeign foreignCall)

globalDeclaration :: Parser (Text, GrinNode)
globalDeclaration = do
  keyword "global"
  horizontal1
  globalName <- scopedName
  horizontal1
  _ <- MPC.char '='
  horizontal1
  node <- grinNode
  lineEnd
  pure (globalName, node)

functionDeclaration :: Parser GrinFunction
functionDeclaration = do
  functionName <- FunctionName <$> name
  parameters <- MP.many (MP.try (horizontal1 *> varAtom))
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  resultRep <- runtimeRep
  horizontal1
  _ <- MPC.char '='
  lineEnd
  body <- nestedExpr 0
  pure
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = parameters,
        grinFunctionResultRep = resultRep,
        grinFunctionBody = body
      }

exprAt :: Int -> Parser GrinExpr
exprAt indentation = exactIndent indentation *> exprAfterIndent indentation

nestedExpr :: Int -> Parser GrinExpr
nestedExpr parentIndentation = do
  indentation <- indentationLevel
  when (indentation <= parentIndentation) $ fail "expected an indented GRIN expression"
  exprAfterIndent indentation

exprAfterIndent :: Int -> Parser GrinExpr
exprAfterIndent indentation =
  MP.choice
    [ bindExpr indentation,
      caseExpr indentation,
      MP.try (storeRecExpr indentation "store-rec-unchecked" GrinStoreRecUnchecked),
      MP.try (storeRecExpr indentation "store-rec" GrinStoreRec),
      atomicExpr
    ]

bindExpr :: Int -> Parser GrinExpr
bindExpr indentation = do
  binders <- MP.try (binderList <* horizontal1 <* MPC.string "<-")
  valueExpression <-
    MP.try (lineEnd *> nestedExpr indentation)
      <|> (horizontal1 *> atomicExpr)
  blankLines
  body <- exprAt indentation
  pure (GrinBind binders valueExpression body)

binderList :: Parser [GrinVar]
binderList =
  [] <$ MPC.string "()"
    <|> binderVar `MP.sepBy1` commaSeparator

binderVar :: Parser GrinVar
binderVar = MP.try varAtom <|> bareVar

caseExpr :: Int -> Parser GrinExpr
caseExpr indentation = do
  keyword "case"
  horizontal1
  scrutinee <- grinValue
  horizontal1
  keyword "as"
  horizontal1
  binder <- bareVar
  horizontal1
  keyword "of"
  lineEnd
  alternatives <- grinAlternatives indentation binder
  pure (GrinCase scrutinee binder alternatives)

grinAlternatives :: Int -> GrinVar -> Parser [GrinAlt]
grinAlternatives parentIndentation binder = do
  next <- optional (MP.try (MP.lookAhead deeperIndentation))
  case next of
    Nothing -> pure []
    Just indentation -> alternativesAt indentation
  where
    deeperIndentation = do
      indentation <- indentationLevel
      guard (indentation > parentIndentation)
      pure indentation
    alternativesAt indentation = do
      blankLines
      hasAlternative <- optional (MP.try (MP.lookAhead (exactIndent indentation)))
      case hasAlternative of
        Nothing -> pure []
        Just () -> (:) <$> alternativeAt indentation binder <*> alternativesAt indentation

alternativeAt :: Int -> GrinVar -> Parser GrinAlt
alternativeAt indentation binder = do
  exactIndent indentation
  constructor <- altConstructor binder
  binders <- MP.many (MP.try (horizontal1 *> varAtom))
  horizontal1
  _ <- MPC.string "->"
  lineEnd
  rhs <- nestedExpr indentation
  pure
    GrinAlt
      { grinAltCon = constructor,
        grinAltBinders = binders,
        grinAltRhs = rhs
      }

altConstructor :: GrinVar -> Parser GrinAltCon
altConstructor binder =
  MP.try (GrinDataAlt <$> (keyword "data" *> horizontal1 *> scopedName))
    <|> GrinDefaultAlt <$ MPC.char '_'
    <|> MP.try (GrinLitAlt <$> grinLiteral)
    <|> MP.try (GrinLitAlt . GrinLitInt (grinVarRuntimeRep binder) <$> signedInteger)
    <|> GrinDataAlt <$> scopedName

storeRecExpr :: Int -> Text -> ([(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr) -> Parser GrinExpr
storeRecExpr indentation expressionName constructor = do
  keyword expressionName
  lineEnd
  blankLines
  next <- optional (MP.try (MP.lookAhead deeperIndentation))
  bindings <-
    case next of
      Nothing -> pure []
      Just bindingIndentation -> bindingsAt bindingIndentation
  body <- exprAt indentation
  pure (constructor bindings body)
  where
    deeperIndentation = do
      bindingIndentation <- indentationLevel
      guard (bindingIndentation > indentation)
      pure bindingIndentation
    bindingsAt bindingIndentation = do
      hasBinding <- optional (MP.try (MP.lookAhead (exactIndent bindingIndentation)))
      case hasBinding of
        Nothing -> pure []
        Just () -> (:) <$> storeBindingAt bindingIndentation <*> bindingsAt bindingIndentation

storeBindingAt :: Int -> Parser (GrinVar, GrinNode)
storeBindingAt indentation = do
  exactIndent indentation
  variable <- binderVar
  horizontal1
  _ <- MPC.char '='
  horizontal1
  node <- grinNode
  lineEnd
  pure (variable, node)

atomicExpr :: Parser GrinExpr
atomicExpr =
  MP.choice
    [ unaryValuesExpr "constant" GrinConstant,
      MP.try (unaryNodeExpr "store-unchecked" GrinStoreUnchecked),
      unaryNodeExpr "store" GrinStore,
      ensureHeapExpr,
      twoValuesExpr "update-blackhole" GrinUpdateBlackhole,
      twoValuesExpr "update" GrinUpdate,
      runtimeRepValueExpr "eval" GrinEval,
      cpsEvalExpr,
      namedCallExpr "call" GrinCall,
      primitiveCallExpr,
      cpsPrimitiveCallExpr,
      applyExpr,
      cpsApplyExpr,
      continueExpr,
      twoValuesExpr "raise-cps" GrinCpsRaise,
      unaryValuesExpr "halt" GrinHalt,
      unaryValueExpr "exit" GrinExit,
      unaryValueExpr "throw" GrinThrow,
      catchExpr,
      foreignCallExpr
    ]

unaryValuesExpr :: Text -> ([GrinValue] -> GrinExpr) -> Parser GrinExpr
unaryValuesExpr expressionName constructor = do
  keyword expressionName
  values <- grinValues
  lineEnd
  pure (constructor values)

unaryNodeExpr :: Text -> (GrinNode -> GrinExpr) -> Parser GrinExpr
unaryNodeExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  node <- grinNode
  lineEnd
  pure (constructor node)

unaryValueExpr :: Text -> (GrinValue -> GrinExpr) -> Parser GrinExpr
unaryValueExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  value <- grinValue
  lineEnd
  pure (constructor value)

ensureHeapExpr :: Parser GrinExpr
ensureHeapExpr = do
  keyword "ensure-heap"
  horizontal1
  requiredWords <- grinValue
  roots <- grinValues
  lineEnd
  pure (GrinEnsureHeap requiredWords roots)

runtimeRepValueExpr :: Text -> (GrinRep -> GrinValue -> GrinExpr) -> Parser GrinExpr
runtimeRepValueExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  value <- grinValue
  lineEnd
  pure (constructor representation value)

twoValuesExpr :: Text -> (GrinValue -> GrinValue -> GrinExpr) -> Parser GrinExpr
twoValuesExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  first <- grinValue
  horizontal1
  second <- grinValue
  lineEnd
  pure (constructor first second)

cpsEvalExpr :: Parser GrinExpr
cpsEvalExpr = do
  keyword "cps-eval"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  value <- grinValue
  horizontal1
  continuation <- grinValue
  horizontal1
  updateContinuation <- grinValue
  lineEnd
  pure (GrinCpsEval representation value continuation updateContinuation)

namedCallExpr :: Text -> (GrinRep -> FunctionName -> [GrinValue] -> GrinExpr) -> Parser GrinExpr
namedCallExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  functionName <- FunctionName <$> name
  arguments <- grinValues
  lineEnd
  pure (constructor representation functionName arguments)

primitiveCallExpr :: Parser GrinExpr
primitiveCallExpr = do
  keyword "primitive-call"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  primitiveName <- name
  arguments <- grinValues
  lineEnd
  pure (GrinPrimitiveCall representation primitiveName arguments)

cpsPrimitiveCallExpr :: Parser GrinExpr
cpsPrimitiveCallExpr = do
  keyword "cps-primitive-call"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  primitiveName <- name
  arguments <- MP.many (MP.try (horizontal1 *> grinValue))
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  continuation <- grinValue
  lineEnd
  pure (GrinCpsPrimitiveCall representation primitiveName arguments continuation)

applyExpr :: Parser GrinExpr
applyExpr = do
  keyword "apply"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  function <- grinValue
  horizontal1
  arguments <- grinArgument
  lineEnd
  pure (GrinApply representation function arguments)

cpsApplyExpr :: Parser GrinExpr
cpsApplyExpr = do
  keyword "cps-apply"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  function <- grinValue
  horizontal1
  arguments <- grinArgument
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  continuation <- grinValue
  lineEnd
  pure (GrinCpsApply representation function arguments continuation)

continueExpr :: Parser GrinExpr
continueExpr = do
  keyword "continue"
  horizontal1
  continuation <- grinValue
  horizontal1
  values <- grinArgument
  lineEnd
  pure (GrinContinue continuation values)

catchExpr :: Parser GrinExpr
catchExpr = do
  keyword "catch"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  action <- grinValue
  horizontal1
  handler <- grinValue
  state <- grinValues
  lineEnd
  pure (GrinCatch representation action handler state)

foreignCallExpr :: Parser GrinExpr
foreignCallExpr = do
  keyword "foreign-call"
  horizontal1
  foreignCall <- foreignCallDefinition
  horizontal1
  keyword "with"
  arguments <- grinValues
  lineEnd
  pure (GrinForeignCallExpr foreignCall arguments)

grinValues :: Parser [GrinValue]
grinValues = MP.many (MP.try (horizontal1 *> grinValue))

grinArgument :: Parser [GrinValue]
grinArgument =
  MP.try ((: []) <$> grinValue)
    <|> betweenHorizontal '(' ')' (MP.many (grinValue <* horizontal))

grinNode :: Parser GrinNode
grinNode = betweenHorizontal '(' ')' $ do
  tag <- nodeTag
  GrinNode tag <$> grinValues

nodeTag :: Parser GrinNodeTag
nodeTag =
  MP.choice
    [ do
        _ <- MPC.char 'C'
        constructorName <- scopedName
        remaining <- optional (MPC.char '/' *> signedInt)
        pure (GrinConstructor constructorName (fromMaybe 0 remaining)),
      do
        _ <- MPC.char 'P'
        functionName <- FunctionName <$> name
        _ <- MPC.char '/'
        argumentLayouts <-
          MP.try layouts <|> do
            arity <- natural
            exactLayouts <- optional layouts
            let parsedLayouts = fromMaybe (replicate arity [liftedGrinRep]) exactLayouts
            when (arity /= length parsedLayouts) $ fail "closure arity does not match its layouts"
            pure parsedLayouts
        pure (GrinClosure functionName argumentLayouts),
      GrinThunk . FunctionName <$> (MPC.char 'F' *> name)
    ]

grinValue :: Parser GrinValue
grinValue =
  MP.try (GrinGlobalValue <$> (keyword "global-ref" *> horizontal1 *> scopedName))
    <|> MP.try (GrinVarValue <$> varAtom)
    <|> GrinLitValue <$> grinLiteral

grinLiteral :: Parser GrinLiteral
grinLiteral =
  MP.choice
    [ MP.try typedIntegerLiteral,
      MP.try typedCharLiteral,
      MP.try addressLiteral,
      GrinLitString <$> stringText
    ]

typedIntegerLiteral :: Parser GrinLiteral
typedIntegerLiteral = betweenHorizontal '(' ')' $ do
  value <- signedInteger
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  GrinLitInt <$> runtimeRep <*> pure value

typedCharLiteral :: Parser GrinLiteral
typedCharLiteral = betweenHorizontal '(' ')' $ do
  value <- haskellChar
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  GrinLitChar <$> runtimeRep <*> pure value

addressLiteral :: Parser GrinLiteral
addressLiteral = do
  value <- stringText
  _ <- MPC.char '#'
  pure (GrinLitAddr (BS.pack (map (fromIntegral . ord) (T.unpack value))))

varAtom :: Parser GrinVar
varAtom = betweenHorizontal '(' ')' bareVar

-- | The printer omits a zero number, except for names that would then be read
-- back as a literal. Rejecting those here lets @(0 :: IntRep)@ fall through to
-- the literal parsers.
variableNumber :: Text -> Parser Int
variableNumber variableName = do
  explicit <- optional (MPC.char '%' *> signedInt)
  case explicit of
    Just value -> pure value
    Nothing
      | grinVarNameNeedsNumber variableName -> fail "variable name requires an explicit number"
      | otherwise -> pure 0

bareVar :: Parser GrinVar
bareVar = do
  variableName <- name
  uniqueValue <- variableNumber variableName
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  GrinVar variableName uniqueValue <$> runtimeRep

foreignCallDefinition :: Parser GrinForeignCall
foreignCallDefinition = do
  callName <- scopedName
  horizontal1
  _ <- MPC.char '='
  horizontal1
  target <- MP.option GrinForeignFunction (GrinForeignAddress <$ (keyword "address" <* horizontal1))
  symbolName <- stringText
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  signature <- foreignSignature
  pure
    GrinForeignCall
      { grinForeignCallName = callName,
        grinForeignCallSymbol = symbolName,
        grinForeignCallTarget = target,
        grinForeignCallSignature = signature
      }

foreignSignature :: Parser GrinForeignSignature
foreignSignature = do
  argumentTypes <- betweenHorizontal '(' ')' (foreignType `MP.sepBy` commaSeparator)
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  resultType <- foreignType
  horizontal1
  _ <- MPC.char '!'
  horizontal1
  effect <-
    GrinForeignPure <$ keyword "pure"
      <|> GrinForeignRealWorld <$ keyword "real-world"
  pure
    GrinForeignSignature
      { grinForeignArgumentTypes = argumentTypes,
        grinForeignResultType = resultType,
        grinForeignEffect = effect
      }

foreignType :: Parser GrinForeignType
foreignType =
  MP.choice
    [ GrinForeignInt16 <$ keyword "int16",
      GrinForeignInt32 <$ keyword "int32",
      GrinForeignInt64 <$ keyword "int64",
      GrinForeignInt8 <$ keyword "int8",
      GrinForeignInt <$ keyword "int",
      GrinForeignWord16 <$ keyword "word16",
      GrinForeignWord32 <$ keyword "word32",
      GrinForeignWord64 <$ keyword "word64",
      GrinForeignWord8 <$ keyword "word8",
      GrinForeignWord <$ keyword "word",
      GrinForeignFloat <$ keyword "float",
      GrinForeignDouble <$ keyword "double",
      GrinForeignAddr <$ keyword "addr",
      GrinForeignVoid <$ keyword "void"
    ]

runtimeRepArgument :: Parser GrinRep
runtimeRepArgument = MPC.char '@' *> runtimeRep

runtimeRep :: Parser GrinRep
runtimeRep =
  MP.try (betweenHorizontal '(' ')' runtimeRep)
    <|> MP.choice
      [ VecRep <$ keyword "VecRep" <* horizontal1 <*> vecCount <* horizontal1 <*> vecElem,
        TupleRep <$ keyword "TupleRep" <* horizontal1 <*> runtimeRepList,
        SumRep <$ keyword "SumRep" <* horizontal1 <*> runtimeRepList,
        BoxedRep <$ keyword "BoxedRep" <* horizontal1 <*> levity,
        Int8Rep <$ keyword "Int8Rep",
        Int16Rep <$ keyword "Int16Rep",
        Int32Rep <$ keyword "Int32Rep",
        Int64Rep <$ keyword "Int64Rep",
        IntRep <$ keyword "IntRep",
        Word8Rep <$ keyword "Word8Rep",
        Word16Rep <$ keyword "Word16Rep",
        Word32Rep <$ keyword "Word32Rep",
        Word64Rep <$ keyword "Word64Rep",
        WordRep <$ keyword "WordRep",
        AddrRep <$ keyword "AddrRep",
        FloatRep <$ keyword "FloatRep",
        DoubleRep <$ keyword "DoubleRep"
      ]

runtimeRepList :: Parser [GrinRep]
runtimeRepList = betweenHorizontal '[' ']' (runtimeRep `MP.sepBy` commaSeparator)

constructorLayouts :: Parser [[GrinRep]]
constructorLayouts =
  betweenHorizontal '[' ']' (constructorLayout `MP.sepBy` commaSeparator)
  where
    constructorLayout = runtimeRepList <|> ((: []) <$> runtimeRep)

layouts :: Parser [[GrinRep]]
layouts = betweenHorizontal '[' ']' (runtimeRepList `MP.sepBy` commaSeparator)

levity :: Parser GrinLevity
levity = Lifted <$ keyword "Lifted" <|> Unlifted <$ keyword "Unlifted"

vecCount :: Parser GrinVecCount
vecCount =
  MP.choice
    [ Vec2 <$ keyword "Vec2",
      Vec4 <$ keyword "Vec4",
      Vec8 <$ keyword "Vec8",
      Vec16 <$ keyword "Vec16",
      Vec32 <$ keyword "Vec32",
      Vec64 <$ keyword "Vec64"
    ]

vecElem :: Parser GrinVecElem
vecElem =
  MP.choice
    [ Int8ElemRep <$ keyword "Int8ElemRep",
      Int16ElemRep <$ keyword "Int16ElemRep",
      Int32ElemRep <$ keyword "Int32ElemRep",
      Int64ElemRep <$ keyword "Int64ElemRep",
      Word8ElemRep <$ keyword "Word8ElemRep",
      Word16ElemRep <$ keyword "Word16ElemRep",
      Word32ElemRep <$ keyword "Word32ElemRep",
      Word64ElemRep <$ keyword "Word64ElemRep",
      FloatElemRep <$ keyword "FloatElemRep",
      DoubleElemRep <$ keyword "DoubleElemRep"
    ]

-- | A top-level name. @1.name@ takes its package and its module from scope 1.
scopedName :: Parser Text
scopedName = MP.try qualified <|> name
  where
    qualified = do
      number <- natural
      _ <- MPC.char '.'
      scopes <- ask
      case Map.lookup number scopes of
        Nothing -> fail "unknown scope number"
        Just scope -> grinScopedName (grinScopePackage scope) (grinScopeModule scope) <$> name

name :: Parser Text
name = stringText <|> MP.takeWhile1P (Just "name") isBareNameCharacter
  where
    isBareNameCharacter character =
      not (isSpace character)
        && character `notElem` ['"', '(', ')', '[', ']', ',', '=', '/', '%']

stringText :: Parser Text
stringText = T.pack <$> (MPC.char '"' *> MP.manyTill L.charLiteral (MPC.char '"'))

haskellChar :: Parser Char
haskellChar = MPC.char '\'' *> L.charLiteral <* MPC.char '\''

signedInt :: Parser Int
signedInt = L.signed (pure ()) L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed (pure ()) L.decimal

natural :: Parser Int
natural = L.decimal

keyword :: Text -> Parser ()
keyword text = void (MPC.string text <* MP.notFollowedBy (MP.satisfy isKeywordContinuation))
  where
    isKeywordContinuation character = isAlphaNum character || character == '_'

betweenHorizontal :: Char -> Char -> Parser value -> Parser value
betweenHorizontal opening closing parser = do
  _ <- MPC.char opening
  horizontal
  value <- parser
  horizontal
  _ <- MPC.char closing
  pure value

indentationLevel :: Parser Int
indentationLevel = T.length <$> MP.takeWhileP (Just "indentation") (== ' ')

exactIndent :: Int -> Parser ()
exactIndent expected = do
  actual <- indentationLevel
  when (actual /= expected) $ fail ("expected " <> show expected <> " spaces of indentation")

horizontal :: Parser ()
horizontal = void (MP.takeWhileP (Just "horizontal whitespace") (== ' '))

horizontal1 :: Parser ()
horizontal1 = void (MP.takeWhile1P (Just "horizontal whitespace") (== ' '))

commaSeparator :: Parser Char
commaSeparator = MP.try (horizontal *> MPC.char ',' <* horizontal)

blankLines :: Parser ()
blankLines = void (MP.many (MP.try (horizontal *> MPC.eol)))

lineEnd :: Parser ()
lineEnd = horizontal *> (void MPC.eol <|> MP.eof)
