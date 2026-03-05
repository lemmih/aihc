module Parser.Ast
  ( CallConv (..),
    Decl (..),
    Expr (..),
    ForeignDirection (..),
    ForeignSafety (..),
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
  | PatternDecl
      { patternLhs :: Text
      }
  | TypeSigDecl
      { typeSigName :: Text
      }
  | FunctionDecl
      { functionName :: Text
      }
  | TypeDecl
      { typeName :: Text
      }
  | DataDecl
      { dataTypeName :: Text,
        dataConstructors :: [Text]
      }
  | NewtypeDecl
      { newtypeName :: Text,
        newtypeConstructor :: Maybe Text
      }
  | ClassDecl
      { className :: Text
      }
  | InstanceDecl
      { instanceClassName :: Text
      }
  | FixityDecl
      { fixityAssoc :: Text,
        fixityPrecedence :: Maybe Int,
        fixityOperator :: Text
      }
  | DefaultDecl
      { defaultTypes :: [Text]
      }
  | ForeignDecl
      { foreignDirection :: ForeignDirection,
        foreignCallConv :: CallConv,
        foreignSafety :: Maybe ForeignSafety,
        foreignEntity :: Maybe Text,
        foreignName :: Text
      }
  deriving (Eq, Show)

data ForeignDirection
  = ForeignImport
  | ForeignExport
  deriving (Eq, Show)

data CallConv
  = CCall
  | StdCall
  deriving (Eq, Show)

data ForeignSafety
  = Safe
  | Unsafe
  deriving (Eq, Show)

data Expr
  = EVar Text
  | EInt Integer
  | EFloat Double
  | EChar Char
  | EString Text
  | EList [Expr]
  | ETuple [Expr]
  | ETupleCon Int
  | EApp Expr Expr
  deriving (Eq, Show)
