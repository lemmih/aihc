-- | The helpers of the 'Read' class and the instances of the types that the
-- primitive package and this module define.
--
-- The class itself lives in "GHC.Prim.Read", and 'Prelude' re-exports it.
-- The class cannot live in 'Prelude', because the instances need 'Prelude'
-- names such as 'Ratio'; a module that imported only 'Prelude' would then
-- see the class without its instances.
module GHC.Internal.Read
  ( Read (..),
    readListDefault,
    readListPrecDefault,
    choose,
    readNumber,
    readFieldHash,
  )
where

import Data.Either (Either (..))
import GHC.Base (Applicative (..), Functor (..), List (..), Maybe (..), Monad (..), String)
import GHC.Int (Int)
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import GHC.Internal.Integer (Integer)
import GHC.Num (Num (..))
import GHC.Prim.Read
  ( Lexeme (..),
    Read (..),
    ReadPrec,
    ReadS,
    expectP,
    lexP,
    list,
    minPrec,
    paren,
    parens,
    parseSignedInteger,
    pfail,
    prec,
    readPrec_to_S,
    readS_to_Prec,
    reset,
    step,
    stringEqual,
    (+++),
  )
import GHC.Real (Integral, Ratio, (%))
import GHC.Types (Bool (..), Char, Ordering (..))

readListDefault :: (Read a) => ReadS [a]
readListDefault = readPrec_to_S readListPrec minPrec

readListPrecDefault :: (Read a) => ReadPrec [a]
readListPrecDefault = list readPrec

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

readFieldHash :: String -> ReadPrec a -> ReadPrec a
readFieldHash name parser = do
  expectP (Ident name)
  expectP (Symbol "#")
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

instance (Read a) => Read (NonEmpty a) where
  readPrec =
    parens
      ( prec
          5
          ( do
              value <- step readPrec
              expectP (Symbol ":|")
              values <- step readPrec
              return (value :| values)
          )
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
