{-# LANGUAGE DeriveFunctor #-}

module Resolver.Types
  ( Diagnostic (..),
    DiagnosticCode (..),
    DiagnosticSeverity (..),
    ExtensionTag (..),
    NameClass (..),
    NameId (..),
    PreludeMode (..),
    ResolveConfig (..),
    ResolveResult (..),
    Span (..),
  )
where

import Data.Set (Set)
import Data.Text (Text)

newtype NameId = NameId Int
  deriving (Eq, Ord, Show)

data NameClass
  = LocalBinder
  | TopLevelBinder
  | ImportedBinder
  | PreludeBinder
  | Unresolved
  deriving (Eq, Ord, Show)

data DiagnosticSeverity
  = Error
  | Warning
  deriving (Eq, Ord, Show)

data DiagnosticCode
  = EUnboundVariable
  | EDuplicateBinding
  | EUnsupportedSyntax
  | ENotInScopeImplicit
  deriving (Eq, Ord, Show, Enum, Bounded)

data Span
  = NoSpan
  | Span
      { startLine :: !Int,
        startCol :: !Int,
        endLine :: !Int,
        endCol :: !Int
      }
  deriving (Eq, Ord, Show)

data Diagnostic = Diagnostic
  { diagCode :: !DiagnosticCode,
    diagSeverity :: !DiagnosticSeverity,
    diagMessage :: !Text,
    diagSpan :: !Span
  }
  deriving (Eq, Show)

data ExtensionTag
  = ExtDoNotation
  | ExtRebindableSyntax
  deriving (Eq, Ord, Show, Enum, Bounded)

data PreludeMode
  = ImplicitPreludeOn
  | ImplicitPreludeOff
  deriving (Eq, Ord, Show)

data ResolveConfig = ResolveConfig
  { preludeMode :: !PreludeMode,
    enabledExtensions :: !(Set ExtensionTag)
  }
  deriving (Eq, Show)

data ResolveResult a = ResolveResult
  { resolved :: a,
    diagnostics :: [Diagnostic]
  }
  deriving (Eq, Show, Functor)
