module Resolver.Ast
  ( ResolvedDecl (..)
  , ResolvedExpr (..)
  , ResolvedModule (..)
  , ResolvedName (..)
  ) where

import Data.Text (Text)
import Resolver.Types

data ResolvedModule = ResolvedModule
  { resolvedModuleName :: Maybe Text
  , resolvedDecls :: [ResolvedDecl]
  }
  deriving (Eq, Show)

data ResolvedDecl = ResolvedDecl
  { resolvedDeclName :: Text
  , resolvedDeclId :: NameId
  , resolvedDeclExpr :: ResolvedExpr
  }
  deriving (Eq, Show)

data ResolvedName = ResolvedName
  { rnText :: Text
  , rnId :: Maybe NameId
  , rnClass :: NameClass
  }
  deriving (Eq, Show)

data ResolvedExpr
  = RVar ResolvedName
  | RInt Integer
  | RApp ResolvedExpr ResolvedExpr
  deriving (Eq, Show)
