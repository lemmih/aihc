module Parser.Canonical
  ( CanonicalDecl (..)
  , CanonicalExpr (..)
  , CanonicalModule (..)
  , normalizeDecl
  , normalizeExpr
  , normalizeModule
  ) where

import Data.Text (Text)
import Parser.Ast

data CanonicalModule = CanonicalModule
  { canonicalModuleName :: Maybe Text
  , canonicalDecls :: [CanonicalDecl]
  }
  deriving (Eq, Show)

data CanonicalDecl = CanonicalDecl
  { canonicalDeclName :: Text
  , canonicalDeclExpr :: CanonicalExpr
  }
  deriving (Eq, Show)

data CanonicalExpr
  = CVar Text
  | CInt Integer
  | CApp CanonicalExpr CanonicalExpr
  deriving (Eq, Show)

normalizeModule :: Module -> CanonicalModule
normalizeModule m =
  CanonicalModule
    { canonicalModuleName = moduleName m
    , canonicalDecls = fmap normalizeDecl (moduleDecls m)
    }

normalizeDecl :: Decl -> CanonicalDecl
normalizeDecl d =
  CanonicalDecl
    { canonicalDeclName = declName d
    , canonicalDeclExpr = normalizeExpr (declExpr d)
    }

normalizeExpr :: Expr -> CanonicalExpr
normalizeExpr expr =
  case expr of
    EVar name -> CVar name
    EInt value -> CInt value
    EApp fn arg -> CApp (normalizeExpr fn) (normalizeExpr arg)
