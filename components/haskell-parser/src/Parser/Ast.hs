module Parser.Ast
  ( ArithSeq (..),
    CallConv (..),
    CaseAlt (..),
    CompStmt (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    ForeignDirection (..),
    ForeignSafety (..),
    Module (..),
  )
where

import Data.Text (Text)

data Module = Module
  { moduleName :: Maybe Text,
    moduleDecls :: [Decl],
    moduleDeclChunks :: Maybe [Text]
  }
  deriving (Show)

instance Eq Module where
  lhs == rhs =
    moduleName lhs == moduleName rhs
      && moduleDecls lhs == moduleDecls rhs

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
  | EIf Expr Expr Expr
  | ELambda [Text] Expr
  | EInfix Expr Text Expr
  | ENegate Expr
  | ESectionL Expr Text
  | ESectionR Text Expr
  | ELet [(Text, Expr)] Expr
  | ECase Expr [CaseAlt]
  | EDo [DoStmt]
  | EListComp Expr [CompStmt]
  | EArithSeq ArithSeq
  | ERecordCon Text [(Text, Expr)]
  | ERecordUpd Expr [(Text, Expr)]
  | ETypeSig Expr Text
  | EList [Expr]
  | ETuple [Expr]
  | ETupleCon Int
  | EApp Expr Expr
  deriving (Eq, Show)

data CaseAlt = CaseAlt
  { caseAltPattern :: Text,
    caseAltExpr :: Expr
  }
  deriving (Eq, Show)

data DoStmt
  = DoBind Text Expr
  | DoLet [(Text, Expr)]
  | DoExpr Expr
  deriving (Eq, Show)

data CompStmt
  = CompGen Text Expr
  | CompGuard Expr
  | CompLet [(Text, Expr)]
  deriving (Eq, Show)

data ArithSeq
  = ArithSeqFrom Expr
  | ArithSeqFromThen Expr Expr
  | ArithSeqFromTo Expr Expr
  | ArithSeqFromThenTo Expr Expr Expr
  deriving (Eq, Show)
