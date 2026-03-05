{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr
  , parseModule
  , defaultConfig
  ) where

import Data.Char (isAlphaNum)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Parser.Ast
import Parser.Types
import Text.Megaparsec
  ( Parsec
  , choice
  , errorOffset
  , eof
  , many
  , notFollowedBy
  , parse
  , runParser
  , sepBy1
  , some
  , try
  , (<|>)
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
      cleaned = [(ln, stripComment cfg (T.strip txtLine)) | (ln, txtLine) <- sourceLines]
      nonEmpty = filter (not . T.null . snd) cleaned
  case nonEmpty of
    [] -> Right Module {moduleName = Nothing, moduleDecls = []}
    ((firstLineNo, firstLine) : rest) ->
      case parseModuleHeader firstLine of
        Right modName -> do
          decls <- traverse (\(lineNo, lineText) -> parseDeclarationLine lineNo lineText) rest
          Right Module {moduleName = Just modName, moduleDecls = decls}
        Left _ -> do
          firstDecl <- parseDeclarationLine firstLineNo firstLine
          otherDecls <- traverse (\(lineNo, lineText) -> parseDeclarationLine lineNo lineText) rest
          Right Module {moduleName = Nothing, moduleDecls = firstDecl : otherDecls}

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

parseDeclarationLine :: Int -> Text -> Either ParseError Decl
parseDeclarationLine lineNo raw =
  case parseLineWith declarationParser raw of
    Right decl -> Right decl
    Left err ->
      Left
        err
          { line = lineNo
          , offset = 0
          }

parseLineWith :: MParser a -> Text -> Either ParseError a
parseLineWith parser input =
  case runParser parser "<line>" input of
    Right value -> Right value
    Left bundle -> Left (bundleToError input bundle)

declarationParser :: MParser Decl
declarationParser =
  try dataDeclaration <|> valueDeclaration

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

expression :: ParserConfig -> MParser Expr
expression cfg = do
  atoms <- some (atom cfg)
  pure (foldl1 EApp atoms)

expressionLine :: MParser Expr
expressionLine = do
  atoms <- some atomLine
  pure (foldl1 EApp atoms)

atom :: ParserConfig -> MParser Expr
atom cfg =
  parens (expression cfg)
    <|> (EInt <$> integer (scExpr cfg))
    <|> (EVar <$> identifierLexeme (scExpr cfg))

atomLine :: MParser Expr
atomLine =
  parens expressionLine
    <|> (EInt <$> integer scLine)
    <|> (EVar <$> identifierLexeme scLine)

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
integer sc = lexeme sc L.decimal

symbol :: Text -> MParser Text
symbol = L.symbol scLine

parens :: MParser a -> MParser a
parens parser = do
  _ <- symbol "("
  value <- parser
  _ <- symbol ")"
  pure value

keyword :: Text -> MParser Text
keyword kw = lexeme scLine (C.string kw <* notFollowedBy identTailOrStartChar)

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

bundleToError :: Text -> MP.ParseErrorBundle Text Void -> ParseError
bundleToError input bundle =
  case MP.bundleErrors bundle of
    firstErr :| _ ->
      let off = errorOffset firstErr
          (ln, cl) = offsetToLineCol input off
          foundTok = tokenAt input off
          expectedItems = toExpectations firstErr
       in ParseError
            { offset = off
            , line = ln
            , col = cl
            , expected =
                if null expectedItems
                  then ["valid syntax"]
                  else expectedItems
            , found = foundTok
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
reservedWords = ["module", "where", "data"]

reservedWord :: MParser ()
reservedWord =
  choice (map oneReservedWord reservedWords)
  where
    oneReservedWord kw = try (C.string kw *> notFollowedBy identTailOrStartChar)
