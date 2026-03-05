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

data CanonicalDecl
  = CanonicalValueDecl
      { canonicalDeclName :: Text
      , canonicalDeclExpr :: CanonicalExpr
      }
  | CanonicalDataDecl
      { canonicalTypeName :: Text
      , canonicalConstructors :: [Text]
      }
  deriving (Eq, Show)

data CanonicalExpr
  = CVar Text
  | CInt Integer
  | CFloat Double
  | CChar Char
  | CString Text
  | CList [CanonicalExpr]
  | CTuple [CanonicalExpr]
  | CTupleCon Int
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
  case d of
    Decl {declName = name, declExpr = expr} ->
      CanonicalValueDecl
        { canonicalDeclName = name
        , canonicalDeclExpr = normalizeExpr expr
        }
    DataDecl {dataTypeName = typeName, dataConstructors = ctors} ->
      CanonicalDataDecl
        { canonicalTypeName = typeName
        , canonicalConstructors = ctors
        }

normalizeExpr :: Expr -> CanonicalExpr
normalizeExpr expr =
  case expr of
    EVar name -> CVar name
    EInt value -> CInt value
    EFloat value -> CFloat value
    EChar value -> CChar value
    EString value -> CString value
    EList values -> CList (fmap normalizeExpr values)
    ETuple values -> CTuple (fmap normalizeExpr values)
    ETupleCon arity -> CTupleCon arity
    EApp fn arg -> CApp (normalizeExpr fn) (normalizeExpr arg)
