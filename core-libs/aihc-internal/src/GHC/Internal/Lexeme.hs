-- SPDX-License-Identifier: BSD-3-Clause

-----------------------------------------------------------------------------

-- |
-- Module      :  GHC.Internal.Lexeme
-- Copyright   :  (c) The GHC Team
--
-- Maintainer  :  ghc-devs@haskell.org
-- Portability :  portable
--
-- Functions to evaluate whether or not a string is a valid identifier.
module GHC.Internal.Lexeme
  ( -- * Lexical characteristics of Haskell names
    startsVarSym,
    startsVarId,
    startsConSym,
    startsConId,
    startsVarSymASCII,
    isVarSymChar,
    okSymChar,
  )
where

-- See note [Why do we import Prelude here?]
import Data.Char
import Prelude

-- | Is this character acceptable in a symbol (after the first char)?
-- See alexGetByte in GHC.Parser.Lexer
okSymChar :: Char -> Bool
okSymChar c =
  not (isReservedSymbolCharacter c)
    && case generalCategory c of
      ConnectorPunctuation -> True
      DashPunctuation -> True
      OtherPunctuation -> True
      MathSymbol -> True
      CurrencySymbol -> True
      ModifierSymbol -> True
      OtherSymbol -> True
      _ -> False

isReservedSymbolCharacter :: Char -> Bool
isReservedSymbolCharacter character = character `elem` "(),;[]`{}_\"'"

startsVarSym, startsVarId, startsConSym, startsConId :: Char -> Bool
startsVarSym c = okSymChar c && c /= ':' -- Infix Ids
startsConSym c = c == ':' -- Infix data constructors
startsVarId c =
  c == '_' || case generalCategory c of -- Ordinary Ids
    LowercaseLetter -> True
    OtherLetter -> True -- See #1103
    _ -> False
startsConId c = isUpper c || c == '(' -- Ordinary type constructors and data constructors

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"

isVarSymChar :: Char -> Bool
isVarSymChar c = c == ':' || startsVarSym c
