module Text.Read
  ( Read (..),
    ReadS,
    read,
    reads,
    readParen,
    lex,
    readMaybe,
    readEither,
    Lexeme (..),
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
    ReadPrec,
    Prec,
    minPrec,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import Data.Char (isSpace)
import GHC.Read
  ( Read (..),
    ReadS,
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
    readParen,
  )
import GHC.Read.Lex (Lexeme (..))
import Text.ParserCombinators.ReadPrec
  ( Prec,
    ReadPrec,
    choice,
    get,
    look,
    minPrec,
    pfail,
    prec,
    readPrec_to_S,
    readS_to_Prec,
    reset,
    step,
    (+++),
    (<++),
  )
import Prelude (Either (..), Maybe (..), String, all, lex, read, reads)

-- | Parse a whole string. Trailing white space is permitted.
readEither :: (Read a) => String -> Either String a
readEither input =
  case [value | (value, rest) <- reads input, all isSpace rest] of
    [value] -> Right value
    [] -> Left "Prelude.read: no parse"
    _ -> Left "Prelude.read: ambiguous parse"

readMaybe :: (Read a) => String -> Maybe a
readMaybe input =
  case readEither input of
    Left _ -> Nothing
    Right value -> Just value
