{-# LANGUAGE OverloadedStrings #-}

module Parser.Canonical
  ( CanonicalCallConv (..),
    CanonicalDecl (..),
    CanonicalExpr (..),
    CanonicalForeignDirection (..),
    CanonicalForeignSafety (..),
    CanonicalModule (..),
    normalizeDecl,
    normalizeExpr,
    normalizeModule,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast

data CanonicalModule = CanonicalModule
  { canonicalModuleName :: Maybe Text,
    canonicalDecls :: [CanonicalDecl]
  }
  deriving (Eq, Show)

data CanonicalDecl
  = CanonicalValueDecl
      { canonicalDeclName :: Text,
        canonicalDeclExpr :: CanonicalExpr
      }
  | CanonicalPatternDecl
      { canonicalPatternLhs :: Text
      }
  | CanonicalTypeSigDecl
      { canonicalTypeSigName :: Text
      }
  | CanonicalFunctionDecl
      { canonicalFunctionName :: Text
      }
  | CanonicalTypeDecl
      { canonicalTypeDeclName :: Text
      }
  | CanonicalDataDecl
      { canonicalTypeName :: Text,
        canonicalConstructors :: [Text]
      }
  | CanonicalNewtypeDecl
      { canonicalNewtypeName :: Text,
        canonicalNewtypeConstructor :: Maybe Text
      }
  | CanonicalClassDecl
      { canonicalClassName :: Text
      }
  | CanonicalInstanceDecl
      { canonicalInstanceClassName :: Text
      }
  | CanonicalFixityDecl
      { canonicalFixityAssoc :: Text,
        canonicalFixityPrecedence :: Maybe Int,
        canonicalFixityOperator :: Text
      }
  | CanonicalDefaultDecl
      { canonicalDefaultTypes :: [Text]
      }
  | CanonicalForeignDecl
      { canonicalForeignDirection :: CanonicalForeignDirection,
        canonicalForeignCallConv :: CanonicalCallConv,
        canonicalForeignSafety :: Maybe CanonicalForeignSafety,
        canonicalForeignEntity :: Maybe Text,
        canonicalForeignName :: Text
      }
  deriving (Eq, Show)

data CanonicalForeignDirection
  = CanonicalForeignImport
  | CanonicalForeignExport
  deriving (Eq, Show)

data CanonicalCallConv
  = CanonicalCCall
  | CanonicalStdCall
  deriving (Eq, Show)

data CanonicalForeignSafety
  = CanonicalSafe
  | CanonicalUnsafe
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
    { canonicalModuleName = moduleName m,
      canonicalDecls = fmap normalizeDecl (moduleDecls m)
    }

normalizeDecl :: Decl -> CanonicalDecl
normalizeDecl d =
  case d of
    Decl {declName = name, declExpr = expr} ->
      CanonicalValueDecl
        { canonicalDeclName = name,
          canonicalDeclExpr = normalizeExpr expr
        }
    PatternDecl {patternLhs = lhs} ->
      CanonicalPatternDecl
        { canonicalPatternLhs = lhs
        }
    TypeSigDecl {typeSigName = name} ->
      CanonicalTypeSigDecl
        { canonicalTypeSigName = name
        }
    FunctionDecl {functionName = name} ->
      CanonicalFunctionDecl
        { canonicalFunctionName = name
        }
    TypeDecl {typeName = name} ->
      CanonicalTypeDecl
        { canonicalTypeDeclName = name
        }
    DataDecl {dataTypeName = typeName, dataConstructors = ctors} ->
      CanonicalDataDecl
        { canonicalTypeName = typeName,
          canonicalConstructors = ctors
        }
    NewtypeDecl {newtypeName = name, newtypeConstructor = ctor} ->
      CanonicalNewtypeDecl
        { canonicalNewtypeName = name,
          canonicalNewtypeConstructor = ctor
        }
    ClassDecl {className = name} ->
      CanonicalClassDecl
        { canonicalClassName = name
        }
    InstanceDecl {instanceClassName = name} ->
      CanonicalInstanceDecl
        { canonicalInstanceClassName = name
        }
    FixityDecl {fixityAssoc = assoc, fixityPrecedence = prec, fixityOperator = op} ->
      CanonicalFixityDecl
        { canonicalFixityAssoc = assoc,
          canonicalFixityPrecedence = prec,
          canonicalFixityOperator = op
        }
    DefaultDecl {defaultTypes = tys} ->
      CanonicalDefaultDecl
        { canonicalDefaultTypes = tys
        }
    ForeignDecl
      { foreignDirection = direction,
        foreignCallConv = callConv,
        foreignSafety = safety,
        foreignEntity = entity,
        foreignName = name
      } ->
        CanonicalForeignDecl
          { canonicalForeignDirection = normalizeDirection direction,
            canonicalForeignCallConv = normalizeCallConv callConv,
            canonicalForeignSafety = fmap normalizeSafety safety,
            canonicalForeignEntity = fmap classifyForeignEntity entity,
            canonicalForeignName = name
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

normalizeDirection :: ForeignDirection -> CanonicalForeignDirection
normalizeDirection direction =
  case direction of
    ForeignImport -> CanonicalForeignImport
    ForeignExport -> CanonicalForeignExport

normalizeCallConv :: CallConv -> CanonicalCallConv
normalizeCallConv callConv =
  case callConv of
    CCall -> CanonicalCCall
    StdCall -> CanonicalStdCall

normalizeSafety :: ForeignSafety -> CanonicalForeignSafety
normalizeSafety safety =
  case safety of
    Safe -> CanonicalSafe
    Unsafe -> CanonicalUnsafe

classifyForeignEntity :: Text -> Text
classifyForeignEntity entity
  | entity == "dynamic" = "dynamic"
  | entity == "wrapper" = "wrapper"
  | "static " `T.isPrefixOf` entity = "static"
  | "&" `T.isPrefixOf` entity = "address"
  | otherwise = "named"
