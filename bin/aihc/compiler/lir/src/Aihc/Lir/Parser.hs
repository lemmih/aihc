-- | Parser for the human-readable Lir syntax emitted by "Aihc.Lir.Pretty".
module Aihc.Lir.Parser
  ( LirParseError,
    parseModule,
    renderParseError,
  )
where

import Aihc.Lir.Pretty (binaryOpName, compareOpName, convertOpName, floatBinaryOpName, floatUnaryOpName, unaryOpName, wideOpName)
import Aihc.Lir.Syntax
import Control.Applicative (empty, optional, (<|>))
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Char (chr, isAlphaNum, isHexDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import Data.Word (Word8)
import Numeric (readHex)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

type LirParseError = ParseErrorBundle Text Void

parseModule :: Text -> Either LirParseError Module
parseModule = MP.parse (spaceConsumer *> moduleParser <* MP.eof) "<lir>"

renderParseError :: LirParseError -> String
renderParseError = MP.errorBundlePretty

-- Lexical structure

spaceConsumer :: Parser ()
spaceConsumer = L.space MPC.space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

token :: Text -> Parser ()
token text = void (L.symbol spaceConsumer text)

keyword :: Text -> Parser ()
keyword text = lexeme (MP.try (MPC.string text *> MP.notFollowedBy (MP.satisfy isBareNameCharacter)))

isBareNameCharacter :: Char -> Bool
isBareNameCharacter character =
  isAlphaNum character && character < '\x80' || character `elem` ['_', '.', '$']

bareName :: Parser Text
bareName = MP.takeWhile1P (Just "name") isBareNameCharacter

-- | A string literal decoded to characters. The escape @\\xHH@ gives the
-- character with that code.
quotedText :: Parser Text
quotedText = T.pack . map (either (chr . fromIntegral) id) <$> stringUnits

-- | A string literal decoded to bytes. Characters are encoded as UTF-8 and the
-- escape @\\xHH@ gives one byte.
quotedBytes :: Parser BS.ByteString
quotedBytes = BS.concat . map (either BS.singleton (TE.encodeUtf8 . T.singleton)) <$> stringUnits

stringUnits :: Parser [Either Word8 Char]
stringUnits = MPC.char '"' *> MP.manyTill stringUnit (MPC.char '"')

-- | A character literal, for example @\'K\'@ or @\'\\n\'@. Its value is the code
-- point, so it stands wherever an integer literal does. The escape @\\xHH@
-- gives the byte value @HH@.
characterLiteral :: Parser Integer
characterLiteral = do
  unit <- MPC.char '\'' *> stringUnit <* MPC.char '\''
  pure (either fromIntegral (fromIntegral . fromEnum) unit)

-- | One character of a string or a character literal. A byte escape is
-- @Left@; every other character, escaped or not, is @Right@.
stringUnit :: Parser (Either Word8 Char)
stringUnit = (MPC.char '\\' *> escape) <|> (Right <$> MP.anySingle)
  where
    escape =
      MP.choice
        [ Right '\\' <$ MPC.char '\\',
          Right '"' <$ MPC.char '"',
          Right '\'' <$ MPC.char '\'',
          Right '\n' <$ MPC.char 'n',
          Right '\r' <$ MPC.char 'r',
          Right '\t' <$ MPC.char 't',
          Right '\0' <$ MPC.char '0',
          Left <$> (MPC.char 'x' *> byteEscape),
          Right <$> (MPC.char 'u' *> MPC.char '{' *> codePointEscape <* MPC.char '}')
        ]
    byteEscape = do
      digits <- MP.count 2 (MP.satisfy isHexDigit)
      pure (fromIntegral (hexValue digits))
    codePointEscape = do
      digits <- MP.takeWhile1P (Just "hexadecimal digit") isHexDigit
      let value = hexValue (T.unpack digits)
      if T.length digits > 6 || value > 0x10FFFF
        then fail "code point escape out of range"
        else pure (chr (fromIntegral value))
    hexValue :: String -> Integer
    hexValue digits =
      case readHex digits of
        [(value, "")] -> value
        _ -> 0

nameAfterSigil :: Parser Text
nameAfterSigil = bareName <|> quotedText

variable :: Parser Var
variable = lexeme (Var <$> (MPC.char '%' *> nameAfterSigil))

symbolName :: Parser Symbol
symbolName = lexeme (Symbol <$> (MPC.char '@' *> nameAfterSigil))

label :: Parser Label
label = lexeme (Label <$> (bareName <|> quotedText))

-- | An integer literal: a signed decimal or a character literal.
integer :: Parser Integer
integer = lexeme (characterLiteral <|> L.signed (pure ()) L.decimal)

natural :: Parser Integer
natural = lexeme L.decimal

typeParser :: Parser Type
typeParser =
  MP.choice
    [ I1 <$ keyword "i1",
      I8 <$ keyword "i8",
      I16 <$ keyword "i16",
      I32 <$ keyword "i32",
      I64 <$ keyword "i64",
      F32 <$ keyword "f32",
      F64 <$ keyword "f64",
      Ptr <$ keyword "ptr",
      Code <$ keyword "code"
    ]

literal :: Parser Literal
literal =
  MP.choice
    [ LitNull <$ keyword "null",
      LitSymbol <$> symbolName,
      LitInt <$> lexeme characterLiteral,
      lexeme number
    ]
  where
    number = do
      negative <- isJust <$> optional (MPC.char '-')
      let sign :: (Num a) => a -> a
          sign value = if negative then negate value else value
      MP.choice
        [ LitFloat (sign (1 / 0)) <$ keyword "inf",
          LitFloat (0 / 0) <$ keyword "nan",
          MP.try (LitFloat . sign <$> L.float),
          LitInt . sign <$> L.decimal
        ]

operand :: Parser Operand
operand = OperandVar <$> variable <|> OperandLiteral <$> literal

operands :: Parser [Operand]
operands = operand `MP.sepBy` token ","

parenthesized :: Parser a -> Parser a
parenthesized = MP.between (token "(") (token ")")

argumentList :: Parser [Operand]
argumentList = parenthesized operands

-- Items

moduleParser :: Parser Module
moduleParser = Module <$> MP.many item

item :: Parser Item
item =
  MP.choice
    [ externItem,
      ItemGlobal <$> globalItem,
      exportedItem,
      ItemFunction <$> functionItem Internal,
      ItemData <$> dataItem Internal,
      ItemConstant <$> constantItem,
      ItemInclude <$> (keyword "include" *> lexeme quotedText)
    ]

externItem :: Parser Item
externItem = do
  keyword "extern"
  MP.choice
    [ keyword "func" *> (ItemExternFunction <$> (ExternFunction <$> symbolName <*> signature)),
      keyword "data" *> (ItemExternData <$> symbolName)
    ]

exportedItem :: Parser Item
exportedItem = do
  keyword "export"
  ItemFunction <$> functionItem Export <|> ItemData <$> dataItem Export

globalItem :: Parser Global
globalItem = do
  keyword "global"
  name <- symbolName
  token ":"
  ty <- typeParser
  pinned <- isJust <$> optional (keyword "pinned")
  pure Global {globalName = name, globalType = ty, globalPinned = pinned}

constantItem :: Parser Constant
constantItem = do
  keyword "const"
  name <- symbolName
  token "="
  Constant name <$> integer

dataItem :: Linkage -> Parser DataItem
dataItem linkage = do
  keyword "data"
  mutable <- isJust <$> optional (keyword "mut")
  name <- symbolName
  keyword "align"
  dataAlign <- natural
  token "="
  fields <- MP.between (token "{") (token "}") (dataField `MP.sepBy` token ",")
  pure
    DataItem
      { dataName = name,
        dataLinkage = linkage,
        dataMutable = mutable,
        dataAlignment = dataAlign,
        dataFields = fields
      }

dataField :: Parser DataField
dataField =
  MP.choice
    [ DataBytes <$> (keyword "bytes" *> lexeme quotedBytes),
      DataZero <$> (keyword "zero" *> natural),
      keyword "word" *> (DataWordConstant <$> symbolName <|> DataWord <$> integer),
      keyword "ptr" *> (DataNull <$ keyword "null" <|> DataSymbol <$> symbolName <*> addend),
      keyword "code" *> (DataCode Nothing <$ keyword "null" <|> DataCode . Just <$> symbolName),
      typedField
    ]
  where
    typedField = do
      ty <- typeParser
      if isFloatType ty
        then DataFloat ty <$> floatLiteral
        else DataIntConstant ty <$> symbolName <|> DataInt ty <$> integer
    floatLiteral = do
      value <- literal
      case value of
        LitFloat number -> pure number
        LitInt number -> pure (fromInteger number)
        _ -> fail "expected a float literal"

addend :: Parser Integer
addend =
  MP.option 0 (MP.choice [token "+" *> natural, negate <$> (token "-" *> natural)])

signature :: Parser Signature
signature = do
  parameters <- parenthesized (typeParser `MP.sepBy` token ",")
  results <- resultTypes
  convention <- callingConvention
  pure
    Signature
      { signatureParameters = parameters,
        signatureResults = results,
        signatureConvention = convention
      }

resultTypes :: Parser [Type]
resultTypes =
  MP.option [] (token "->" *> (parenthesized (typeParser `MP.sepBy` token ",") <|> (: []) <$> typeParser))

callingConvention :: Parser CallingConvention
callingConvention =
  MP.option AihcConvention (keyword "cc" *> (AihcConvention <$ keyword "aihc" <|> CConvention <$ keyword "c"))

functionItem :: Linkage -> Parser Function
functionItem linkage = do
  keyword "func"
  name <- symbolName
  parameters <- parameterList
  results <- resultTypes
  convention <- callingConvention
  token "{"
  blocks <- MP.some block
  token "}"
  pure
    Function
      { functionName = name,
        functionLinkage = linkage,
        functionParameters = parameters,
        functionResults = results,
        functionConvention = convention,
        functionBlocks = blocks
      }

parameterList :: Parser [(Var, Type)]
parameterList = parenthesized (parameter `MP.sepBy` token ",")
  where
    parameter = (,) <$> variable <* token ":" <*> typeParser

-- Blocks

block :: Parser Block
block = do
  blockName <- label
  parameters <- MP.option [] parameterList
  token ":"
  instructions <- MP.many (MP.try instruction)
  terminator <- terminatorParser
  pure
    Block
      { blockLabel = blockName,
        blockParameters = parameters,
        blockInstructions = instructions,
        blockTerminator = terminator
      }

instruction :: Parser Instruction
instruction = do
  results <- MP.option [] (MP.try (variable `MP.sepBy1` token "," <* token "="))
  opcode <- lexeme bareName
  case Map.lookup opcode operations of
    Nothing -> fail ("unknown operation " <> T.unpack opcode)
    Just parser -> Instruction results <$> parser

operations :: Map Text (Parser Operation)
operations =
  Map.fromList
    ( [(binaryOpName op, binary (Binary op)) | op <- [minBound .. maxBound]]
        <> [(unaryOpName op, Unary op <$> typeParser <*> operand) | op <- [minBound .. maxBound]]
        <> [(wideOpName op, binary (Wide op)) | op <- [minBound .. maxBound]]
        <> [(compareOpName op, binary (Compare op)) | op <- [minBound .. maxBound]]
        <> [(floatBinaryOpName op, binary (FloatBinary op)) | op <- [minBound .. maxBound]]
        <> [(floatUnaryOpName op, FloatUnary op <$> typeParser <*> operand) | op <- [minBound .. maxBound]]
        <> [(convertOpName op, Convert op <$> typeParser <*> operand <* keyword "to" <*> typeParser) | op <- [minBound .. maxBound]]
        <> [ ("ptr.to_int", PtrToInt <$> operand),
             ("ptr.from_int", PtrFromInt <$> operand),
             ("select", Select <$> typeParser <*> operand <* token "," <*> operand <* token "," <*> operand),
             ("load", Load <$> typeParser <*> address <*> alignment),
             ("store", Store <$> typeParser <*> operand <* token "," <*> address <*> alignment),
             ("ptr.add", PtrAdd <$> operand <* token "," <*> operand),
             ("stack.alloc", StackAlloc <$> natural <*> alignment),
             ("global.get", GlobalGet <$> symbolName),
             ("global.set", GlobalSet <$> symbolName <* token "," <*> operand),
             ("call", Call <$> symbolName <*> argumentList),
             ("call.indirect", CallIndirect <$> operand <*> argumentList <* token ":" <*> signature)
           ]
    )
  where
    binary construct = construct <$> typeParser <*> operand <* token "," <*> operand

address :: Parser Address
address = MP.between (token "[") (token "]") (Address <$> operand <*> addend)

alignment :: Parser Integer
alignment = keyword "align" *> natural

terminatorParser :: Parser Terminator
terminatorParser = do
  opcode <- lexeme bareName
  case opcode of
    "jump" -> Jump <$> target
    "br" -> Branch <$> operand <* token "," <*> target <* token "," <*> target
    "switch" -> switchTerminator
    "return" -> Return <$> operands
    "tailcall" -> TailCall <$> symbolName <*> argumentList
    "tailcall.indirect" -> TailCallIndirect <$> operand <*> argumentList <* token ":" <*> signature
    "trap" -> Trap <$> lexeme quotedText
    _ -> fail ("unknown terminator " <> T.unpack opcode)

switchTerminator :: Parser Terminator
switchTerminator = do
  ty <- typeParser
  scrutinee <- operand
  token "{"
  cases <- concat <$> MP.many switchCase
  fallback <- optional (keyword "default" *> token "->" *> target)
  token "}"
  pure (Switch ty scrutinee cases fallback)
  where
    -- A case lists one or more literals. The list is sugar for one case per
    -- literal, so the module holds them separately.
    switchCase = do
      values <- integer `MP.sepBy1` token ","
      token "->"
      chosen <- target
      pure [SwitchCase value chosen | value <- values]

target :: Parser Target
target = Target <$> label <*> MP.option [] argumentList
