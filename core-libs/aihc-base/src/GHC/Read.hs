-- | The 'Read' class and the readers that its instances share.
--
-- The class, its helpers, and its instances live in "GHC.Internal.Read".
-- The readers that a derived instance needs live in "GHC.Prim.Read".
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

import GHC.Internal.Read
  ( Read (..),
    choose,
    readFieldHash,
    readListDefault,
    readListPrecDefault,
    readNumber,
  )
import GHC.Prim.Read
  ( expectP,
    lexDigits,
    lexLitChar,
    lexP,
    list,
    paren,
    parens,
    readField,
    readLitChar,
    readSymField,
  )
import Prelude (ReadS, lex, readParen)
