module Text.Read
  ( Read (..),
    ReadS,
    read,
    reads,
    readParen,
    lex,
    readMaybe,
    readEither,
    module Text.ParserCombinators.ReadPrec,
    Lexeme (..),
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
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
  )
import GHC.Read.Lex (Lexeme (..))
import Text.ParserCombinators.ReadPrec
import Prelude (Either (..), Maybe (..), String, all, lex, read, readParen, reads)

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
