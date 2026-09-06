module GHC.Read
  ( Read (..),
    ReadS,
    lex,
    lexLitChar,
    readLitChar,
    lexDigits,
    lexP,
    expectP,
    paren,
    parens,
    list,
    choose,
    readListDefault,
    readListPrecDefault,
    readNumber,
    readField,
    readFieldHash,
    readSymField,
    readParen,
  )
where

import GHC.Read.Lex
  ( Lexeme (..),
    NumberToken (..),
    expectP,
    lex,
    lexDigits,
    lexLitChar,
    lexP,
    parseSignedInteger,
    readLitChar,
    stringEqual,
  )
import Text.ParserCombinators.ReadPrec (ReadPrec, minPrec, pfail, prec, readPrec_to_S, readS_to_Prec, reset, step, (+++))
import Prelude hiding (lex)

readListDefault :: (Read a) => ReadS [a]
readListDefault = readPrec_to_S readListPrec minPrec

readListPrecDefault :: (Read a) => ReadPrec [a]
readListPrecDefault = list readPrec

paren :: ReadPrec a -> ReadPrec a
paren parser = do
  expectP (Punc "(")
  value <- reset parser
  expectP (Punc ")")
  return value

parens :: ReadPrec a -> ReadPrec a
parens parser = parser +++ paren (parens parser)

list :: ReadPrec a -> ReadPrec [a]
list parser =
  parens
    ( do
        expectP (Punc "[")
        emptyList +++ nonEmptyList
    )
  where
    emptyList = do
      expectP (Punc "]")
      return []
    nonEmptyList = do
      value <- reset parser
      values <- listTail
      return (value : values)
    listTail =
      ( do
          expectP (Punc "]")
          return []
      )
        +++ do
          expectP (Punc ",")
          nonEmptyList

choose :: [(String, ReadPrec a)] -> ReadPrec a
choose [] = pfail
choose ((name, parser) : alternatives) = chooseOne +++ choose alternatives
  where
    chooseOne = do
      token <- lexP
      case token of
        Ident actual -> ifStringsEqual actual
        Symbol actual -> ifStringsEqual actual
        _ -> pfail
    ifStringsEqual actual =
      case stringEqual actual name of
        True -> parser
        False -> pfail

readNumber :: (Num a) => (Lexeme -> ReadPrec a) -> ReadPrec a
readNumber convert =
  parens
    ( do
        token <- lexP
        case token of
          Symbol sign ->
            case stringEqual sign "-" of
              True -> do
                magnitude <- lexP
                value <- convert magnitude
                return (negate value)
              False -> convert token
          _ -> convert token
    )

readField :: String -> ReadPrec a -> ReadPrec a
readField name parser = do
  expectP (Ident name)
  expectP (Punc "=")
  parser

readFieldHash :: String -> ReadPrec a -> ReadPrec a
readFieldHash name parser = do
  expectP (Ident name)
  expectP (Symbol "#")
  expectP (Punc "=")
  parser

readSymField :: String -> ReadPrec a -> ReadPrec a
readSymField name parser = do
  expectP (Punc "(")
  expectP (Symbol name)
  expectP (Punc ")")
  expectP (Punc "=")
  parser

readIntegralPrec :: (Num a) => ReadPrec a
readIntegralPrec =
  readS_to_Prec (\_ input -> convertIntegralResults (parseSignedInteger input))

convertIntegralResults :: (Num a) => [(Integer, String)] -> [(a, String)]
convertIntegralResults [] = []
convertIntegralResults ((value, rest) : results) =
  (fromInteger value, rest) : convertIntegralResults results

instance Read Bool where
  readPrec = parens (choose [("False", return False), ("True", return True)])
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Ordering where
  readPrec = parens (choose [("LT", return LT), ("EQ", return EQ), ("GT", return GT)])
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Int where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Integer where
  readPrec = readIntegralPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Integral a, Read a) => Read (Ratio a) where
  readPrec =
    parens
      ( prec
          7
          ( do
              numeratorValue <- step readPrec
              expectP (Symbol "%")
              denominatorValue <- step readPrec
              return (numeratorValue % denominatorValue)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read () where
  readPrec = parens (paren (return ()))
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Read Char where
  readPrec =
    parens
      ( do
          token <- lexP
          case token of
            Char value -> return value
            _ -> pfail
      )
  readListPrec =
    parens
      ( ( do
            token <- lexP
            case token of
              String value -> return value
              _ -> pfail
        )
          +++ readListPrecDefault
      )
  readList = readListDefault

instance (Read a) => Read [a] where
  readPrec = readListPrec
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a) => Read (Maybe a) where
  readPrec =
    parens
      ( choose [("Nothing", return Nothing)]
          +++ prec 10 (do expectP (Ident "Just"); value <- step readPrec; return (Just value))
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b) => Read (Either a b) where
  readPrec =
    parens
      ( prec 10 (do expectP (Ident "Left"); value <- step readPrec; return (Left value))
          +++ prec 10 (do expectP (Ident "Right"); value <- step readPrec; return (Right value))
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b) => Read (a, b) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              return (first, second)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault

instance (Read a, Read b, Read c) => Read (a, b, c) where
  readPrec =
    parens
      ( paren
          ( do
              first <- readPrec
              expectP (Punc ",")
              second <- readPrec
              expectP (Punc ",")
              third <- readPrec
              return (first, second, third)
          )
      )
  readListPrec = readListPrecDefault
  readList = readListDefault
