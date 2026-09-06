-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DerivingStrategies #-}

module GHC.Internal.ForeignSrcLang
  ( ForeignSrcLang (..),
  )
where

-- See note [Why do we import Prelude here?]
import Prelude

-- | Foreign formats supported by GHC via TH
data ForeignSrcLang
  = -- | C
    LangC
  | -- | C++
    LangCxx
  | -- | Objective C
    LangObjc
  | -- | Objective C++
    LangObjcxx
  | -- | Assembly language (.s)
    LangAsm
  | -- | JavaScript
    LangJs
  | -- | Object (.o)
    RawObject
  deriving stock (Eq, Show)
