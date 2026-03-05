module Parser.Ast
  ( Decl (..),
    Expr (..),
    Module (..),
  )
where

import Data.Text (Text)

data Module = Module
  { moduleName :: Maybe Text,
    moduleDecls :: [Decl]
  }
  deriving (Eq, Show)

data Decl
  = Decl
      { declName :: Text,
        declExpr :: Expr
      }
  | DataDecl
      { dataTypeName :: Text,
        dataConstructors :: [Text]
      }
  deriving (Eq, Show)

data Expr
  = EVar Text
  | EInt Integer
  | EApp Expr Expr
  deriving (Eq, Show)
