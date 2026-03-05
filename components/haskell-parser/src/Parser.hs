{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr
  , parseModule
  , defaultConfig
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast
import Parser.Types

defaultConfig :: ParserConfig
defaultConfig = ParserConfig {allowLineComments = True}

data Token
  = TkIdent Text
  | TkInt Integer
  | TkEquals
  | TkLParen
  | TkRParen
  | TkModule
  | TkWhere
  | TkEOF
  deriving (Eq, Show)

data PositionedToken = PositionedToken
  { ptToken :: !Token
  , ptOffset :: !Int
  , ptLine :: !Int
  , ptCol :: !Int
  }
  deriving (Eq, Show)

data PState = PState
  { inputTokens :: [PositionedToken]
  }

data DataParseResult
  = NotADataDecl
  | InvalidDataDecl Text
  | ParsedDataDecl Decl

newtype P a = P
  { unP :: PState -> Either ParseError (a, PState)
  }

instance Functor P where
  fmap f (P parser) = P $ \st -> do
    (a, st') <- parser st
    Right (f a, st')

instance Applicative P where
  pure a = P (\st -> Right (a, st))
  P pf <*> P pa = P $ \st -> do
    (f, st') <- pf st
    (a, st'') <- pa st'
    Right (f a, st'')

instance Monad P where
  P pa >>= f = P $ \st -> do
    (a, st') <- pa st
    unP (f a) st'

parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr cfg input =
  case lexInput cfg input of
    Left err -> ParseErr err
    Right toks ->
      case runParser expression toks of
        Left err -> ParseErr err
        Right ast -> ParseOk ast

parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule cfg input =
  case parseModuleLines cfg input of
    Left err -> ParseErr err
    Right ast -> ParseOk ast

runParser :: P a -> [PositionedToken] -> Either ParseError a
runParser parser toks = do
  (ast, st) <- unP parser (PState toks)
  case inputTokens st of
    (tok : _)
      | ptToken tok /= TkEOF -> Left (unexpectedToken ["<eof>"] tok)
    _ -> Right ast

expression :: P Expr
expression = do
  firstAtom <- atom
  rest <- atomList
  pure (foldl EApp firstAtom rest)

atom :: P Expr
atom = do
  token <- peekToken
  case ptToken token of
    TkIdent name -> advance *> pure (EVar name)
    TkInt value -> advance *> pure (EInt value)
    TkLParen -> do
      _ <- advance
      inner <- expression
      _ <- satisfyToken (== TkRParen) [")"]
      pure inner
    _ -> failAt ["identifier", "integer", "("] token

atomList :: P [Expr]
atomList = do
  token <- peekToken
  case ptToken token of
    TkIdent _ -> do
      x <- atom
      xs <- atomList
      pure (x : xs)
    TkInt _ -> do
      x <- atom
      xs <- atomList
      pure (x : xs)
    TkLParen -> do
      x <- atom
      xs <- atomList
      pure (x : xs)
    _ -> pure []

peekToken :: P PositionedToken
peekToken = P $ \st ->
  case inputTokens st of
    [] -> Left (ParseError 0 1 1 ["token"] Nothing)
    (tok : _) -> Right (tok, st)

advance :: P PositionedToken
advance = P $ \st ->
  case inputTokens st of
    [] -> Left (ParseError 0 1 1 ["token"] Nothing)
    (tok : toks) -> Right (tok, st {inputTokens = toks})

satisfyToken :: (Token -> Bool) -> [Expectation] -> P PositionedToken
satisfyToken predicate exps = do
  token <- peekToken
  if predicate (ptToken token)
    then advance
    else failAt exps token

failAt :: [Expectation] -> PositionedToken -> P a
failAt exps token = P $ \_ -> Left (unexpectedToken exps token)

unexpectedToken :: [Expectation] -> PositionedToken -> ParseError
unexpectedToken exps token =
  ParseError
    { offset = ptOffset token
    , line = ptLine token
    , col = ptCol token
    , expected = exps
    , found = Just (renderToken (ptToken token))
    }

renderToken :: Token -> Text
renderToken token =
  case token of
    TkIdent t -> T.append "identifier:" t
    TkInt n -> T.pack (show n)
    TkEquals -> "="
    TkLParen -> "("
    TkRParen -> ")"
    TkModule -> "module"
    TkWhere -> "where"
    TkEOF -> "<eof>"

lexInput :: ParserConfig -> Text -> Either ParseError [PositionedToken]
lexInput cfg input = go 0 1 1 (T.unpack input)
  where
    go off ln cl chars =
      case chars of
        [] -> Right [PositionedToken TkEOF off ln cl]
        c : cs
          | isSpace c ->
              let (ln', cl') =
                    if c == '\n' then (ln + 1, 1) else (ln, cl + 1)
               in go (off + 1) ln' cl' cs
          | allowLineComments cfg && c == '-' ->
              case cs of
                '-' : tailChars ->
                  let rest = dropWhile (/= '\n') tailChars
                      consumed = length tailChars - length rest + 2
                      off' = off + consumed
                   in case rest of
                        [] -> Right [PositionedToken TkEOF off' ln cl]
                        (_ : rs) -> go (off' + 1) (ln + 1) 1 rs
                _ -> Left
                  ParseError
                    { offset = off
                    , line = ln
                    , col = cl
                    , expected = ["valid token"]
                    , found = Just "-"
                    }
          | c == '=' -> consToken TkEquals off ln cl cs
          | c == '(' -> consToken TkLParen off ln cl cs
          | c == ')' -> consToken TkRParen off ln cl cs
          | isDigit c ->
              let (ds, rest) = span isDigit cs
                  raw = c : ds
                  val = read raw
               in consTokenLen (TkInt val) off ln cl (length raw) rest
          | isIdentStart c ->
              let (tailChars, rest) = span isIdentChar cs
                  raw = c : tailChars
                  tok = keywordOrIdent (T.pack raw)
               in consTokenLen tok off ln cl (length raw) rest
          | otherwise ->
              Left
                ParseError
                  { offset = off
                  , line = ln
                  , col = cl
                  , expected = ["valid token"]
                  , found = Just (T.singleton c)
                  }

    consToken token off ln cl rest = consTokenLen token off ln cl 1 rest

    consTokenLen token off ln cl len rest = do
      toks <- go (off + len) ln (cl + len) rest
      Right (PositionedToken token off ln cl : toks)

    isIdentStart c = isAlpha c || c == '_'
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

    keywordOrIdent :: Text -> Token
    keywordOrIdent name
      | name == "module" = TkModule
      | name == "where" = TkWhere
      | otherwise = TkIdent name

parseModuleLines :: ParserConfig -> Text -> Either ParseError Module
parseModuleLines cfg input = do
  let sourceLines = zip [1 ..] (T.lines input)
      cleaned = [(ln, stripComment cfg (T.strip txtLine)) | (ln, txtLine) <- sourceLines]
      nonEmpty = filter (not . T.null . snd) cleaned
  case nonEmpty of
    [] -> Right Module {moduleName = Nothing, moduleDecls = []}
    ((firstLineNo, firstLine) : rest) ->
      case parseModuleHeader firstLine of
        Just modName -> do
          decls <- traverse (\(lineNo, lineText) -> parseDeclarationLine cfg lineNo lineText) rest
          Right Module {moduleName = Just modName, moduleDecls = decls}
        Nothing -> do
          firstDecl <- parseDeclarationLine cfg firstLineNo firstLine
          otherDecls <- traverse (\(lineNo, lineText) -> parseDeclarationLine cfg lineNo lineText) rest
          Right Module {moduleName = Nothing, moduleDecls = firstDecl : otherDecls}

parseModuleHeader :: Text -> Maybe Text
parseModuleHeader headerLine =
  case T.words headerLine of
    ["module", modName, "where"] | isValidIdent modName -> Just modName
    _ -> Nothing

parseDeclarationLine :: ParserConfig -> Int -> Text -> Either ParseError Decl
parseDeclarationLine cfg lineNo raw =
  case parseDataDeclaration raw of
    ParsedDataDecl decl -> Right decl
    NotADataDecl ->
      let (lhs, rhsRaw) = T.breakOn "=" raw
       in if T.null rhsRaw
            then Left (lineError lineNo 1 ["declaration (<name> = <expr>)"] raw)
            else
              let name = T.strip lhs
                  rhs = T.strip (T.drop 1 rhsRaw)
                  rhsOffset = T.length lhs + 2
               in if not (isValidIdent name)
                    then Left (lineError lineNo 1 ["identifier"] raw)
                    else
                      case parseExpr cfg rhs of
                        ParseOk expr -> Right Decl {declName = name, declExpr = expr}
                        ParseErr err ->
                          Left
                            ParseError
                              { offset = offset err
                              , line = lineNo
                              , col = rhsOffset + col err
                              , expected = expected err
                              , found = found err
                              }
    InvalidDataDecl reason ->
      Left (lineError lineNo 1 [T.append "data declaration (" (T.append reason ")")] raw)

parseDataDeclaration :: Text -> DataParseResult
parseDataDeclaration raw =
  case T.stripPrefix "data" raw of
    Nothing -> NotADataDecl
    Just afterData ->
      case T.uncons afterData of
        Just (nextChar, _) | not (isSpace nextChar) -> NotADataDecl
        _ ->
          let trimmed = T.strip afterData
              (typeChunk, constructorsChunkRaw) = T.breakOn "=" trimmed
              typeWords = T.words (T.strip typeChunk)
           in case (typeWords, T.stripPrefix "=" constructorsChunkRaw) of
                ([typeName], Just constructorsChunk)
                  | isValidTypeCtor typeName ->
                      let constructors = map T.strip (T.splitOn "|" constructorsChunk)
                       in if null constructors || any T.null constructors
                            then InvalidDataDecl "missing constructor"
                            else
                              case traverse parseNullaryCtor constructors of
                                Right ctorNames ->
                                  ParsedDataDecl DataDecl {dataTypeName = typeName, dataConstructors = ctorNames}
                                Left () -> InvalidDataDecl "constructors must be nullary type constructors"
                _ -> InvalidDataDecl "expected: data <Type> = <Ctor> | <Ctor>"
  where
    parseNullaryCtor ctorText =
      case T.words ctorText of
        [ctorName] | isValidTypeCtor ctorName -> Right ctorName
        _ -> Left ()

lineError :: Int -> Int -> [Expectation] -> Text -> ParseError
lineError lineNo colNo exps foundText =
  ParseError
    { offset = 0
    , line = lineNo
    , col = colNo
    , expected = exps
    , found = if T.null foundText then Nothing else Just foundText
    }

stripComment :: ParserConfig -> Text -> Text
stripComment cfg txtLine
  | not (allowLineComments cfg) = txtLine
  | otherwise =
      case T.breakOn "--" txtLine of
        (before, after)
          | T.null after -> txtLine
          | otherwise -> T.stripEnd before

isValidIdent :: Text -> Bool
isValidIdent ident =
  case T.uncons ident of
    Nothing -> False
    Just (firstChar, rest) ->
      validStart firstChar
        && T.all validTail rest
        && ident /= "module"
        && ident /= "where"
        && ident /= "data"
  where
    validStart c = isAlpha c || c == '_'
    validTail c = isAlphaNum c || c == '_' || c == '\''

isValidTypeCtor :: Text -> Bool
isValidTypeCtor ident =
  case T.uncons ident of
    Nothing -> False
    Just (firstChar, rest) ->
      isAlpha firstChar
        && firstChar == toUpper firstChar
        && T.all validTail rest
        && ident /= "module"
        && ident /= "where"
        && ident /= "data"
  where
    validTail c = isAlphaNum c || c == '_' || c == '\''
