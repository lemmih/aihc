-- | The lexer of the 'Read' class.
--
-- The lexer itself lives in "GHC.Prim.Read", so that a derived 'Read'
-- instance needs the primitive package only.
module GHC.Read.Lex
  ( Lexeme (..),
    NumberToken (..),
    lex,
    lexDigits,
    lexLitChar,
    readLitChar,
    lexP,
    expectP,
    parseSignedInteger,
    stringEqual,
  )
where

import GHC.Prim.Read
  ( Lexeme (..),
    NumberToken (..),
    expectP,
    lexDigits,
    lexLitChar,
    lexP,
    parseSignedInteger,
    readLitChar,
    stringEqual,
  )
import Prelude (lex)
