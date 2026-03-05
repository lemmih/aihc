{-# LANGUAGE OverloadedStrings #-}

module Parser.Canonical
  ( CanonicalArithSeq (..),
    CanonicalCallConv (..),
    CanonicalCaseAlt (..),
    CanonicalCompStmt (..),
    CanonicalDecl (..),
    CanonicalDoStmt (..),
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
  | CIf CanonicalExpr CanonicalExpr CanonicalExpr
  | CLambda [Text] CanonicalExpr
  | CInfix CanonicalExpr Text CanonicalExpr
  | CNegate CanonicalExpr
  | CSectionL CanonicalExpr Text
  | CSectionR Text CanonicalExpr
  | CLet [(Text, CanonicalExpr)] CanonicalExpr
  | CCase CanonicalExpr [CanonicalCaseAlt]
  | CDo [CanonicalDoStmt]
  | CListComp CanonicalExpr [CanonicalCompStmt]
  | CArithSeq CanonicalArithSeq
  | CRecordCon Text [(Text, CanonicalExpr)]
  | CRecordUpd CanonicalExpr [(Text, CanonicalExpr)]
  | CTypeSig CanonicalExpr Text
  | CList [CanonicalExpr]
  | CTuple [CanonicalExpr]
  | CTupleCon Int
  | CApp CanonicalExpr CanonicalExpr
  deriving (Eq, Show)

data CanonicalCaseAlt = CanonicalCaseAlt
  { canonicalCaseAltPattern :: Text,
    canonicalCaseAltExpr :: CanonicalExpr
  }
  deriving (Eq, Show)

data CanonicalDoStmt
  = CDoBind Text CanonicalExpr
  | CDoLet [(Text, CanonicalExpr)]
  | CDoExpr CanonicalExpr
  deriving (Eq, Show)

data CanonicalCompStmt
  = CCompGen Text CanonicalExpr
  | CCompGuard CanonicalExpr
  | CCompLet [(Text, CanonicalExpr)]
  deriving (Eq, Show)

data CanonicalArithSeq
  = CArithSeqFrom CanonicalExpr
  | CArithSeqFromThen CanonicalExpr CanonicalExpr
  | CArithSeqFromTo CanonicalExpr CanonicalExpr
  | CArithSeqFromThenTo CanonicalExpr CanonicalExpr CanonicalExpr
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
    StructuredDecl {structuredTokens = toks} ->
      CanonicalPatternDecl
        { canonicalPatternLhs = renderDeclTokens toks
        }

normalizeExpr :: Expr -> CanonicalExpr
normalizeExpr expr =
  case expr of
    EVar name -> CVar name
    EInt value -> CInt value
    EFloat value -> CFloat value
    EChar value -> CChar value
    EString value -> CString value
    EIf cond yes no -> CIf (normalizeExpr cond) (normalizeExpr yes) (normalizeExpr no)
    ELambda params body -> CLambda params (normalizeExpr body)
    EInfix lhs op rhs -> CInfix (normalizeExpr lhs) op (normalizeExpr rhs)
    ENegate inner -> CNegate (normalizeExpr inner)
    ESectionL lhs op -> CSectionL (normalizeExpr lhs) op
    ESectionR op rhs -> CSectionR op (normalizeExpr rhs)
    ELet binds body ->
      CLet [(name, normalizeExpr value) | (name, value) <- binds] (normalizeExpr body)
    ECase scrutinee alts ->
      CCase (normalizeExpr scrutinee) [normalizeCaseAlt alt | alt <- alts]
    EDo stmts -> CDo [normalizeDoStmt stmt | stmt <- stmts]
    EListComp body qualifiers ->
      CListComp (normalizeExpr body) [normalizeCompStmt stmt | stmt <- qualifiers]
    EArithSeq seqInfo -> CArithSeq (normalizeArithSeq seqInfo)
    ERecordCon name fields ->
      CRecordCon name [(fieldName, normalizeExpr fieldExpr) | (fieldName, fieldExpr) <- fields]
    ERecordUpd base fields ->
      CRecordUpd (normalizeExpr base) [(fieldName, normalizeExpr fieldExpr) | (fieldName, fieldExpr) <- fields]
    ETypeSig inner sigText -> CTypeSig (normalizeExpr inner) sigText
    EList values -> CList (fmap normalizeExpr values)
    ETuple values -> CTuple (fmap normalizeExpr values)
    ETupleCon arity -> CTupleCon arity
    EApp fn arg -> CApp (normalizeExpr fn) (normalizeExpr arg)

normalizeCaseAlt :: CaseAlt -> CanonicalCaseAlt
normalizeCaseAlt alt =
  CanonicalCaseAlt
    { canonicalCaseAltPattern = caseAltPattern alt,
      canonicalCaseAltExpr = normalizeExpr (caseAltExpr alt)
    }

normalizeDoStmt :: DoStmt -> CanonicalDoStmt
normalizeDoStmt stmt =
  case stmt of
    DoBind pat expr -> CDoBind pat (normalizeExpr expr)
    DoLet binds -> CDoLet [(name, normalizeExpr value) | (name, value) <- binds]
    DoExpr expr -> CDoExpr (normalizeExpr expr)

normalizeCompStmt :: CompStmt -> CanonicalCompStmt
normalizeCompStmt stmt =
  case stmt of
    CompGen pat expr -> CCompGen pat (normalizeExpr expr)
    CompGuard expr -> CCompGuard (normalizeExpr expr)
    CompLet binds -> CCompLet [(name, normalizeExpr value) | (name, value) <- binds]

normalizeArithSeq :: ArithSeq -> CanonicalArithSeq
normalizeArithSeq seqInfo =
  case seqInfo of
    ArithSeqFrom start -> CArithSeqFrom (normalizeExpr start)
    ArithSeqFromThen start step -> CArithSeqFromThen (normalizeExpr start) (normalizeExpr step)
    ArithSeqFromTo start end -> CArithSeqFromTo (normalizeExpr start) (normalizeExpr end)
    ArithSeqFromThenTo start step end ->
      CArithSeqFromThenTo (normalizeExpr start) (normalizeExpr step) (normalizeExpr end)

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

renderDeclTokens :: [DeclToken] -> Text
renderDeclTokens =
  snd . foldl step (Nothing, T.empty)
  where
    step (prevTok, acc) tok =
      let spaced =
            case prevTok of
              Nothing -> acc
              Just prev ->
                if needsSpace prev tok
                  then acc <> " "
                  else acc
       in (Just tok, spaced <> tokenText tok)

    tokenText tok =
      case tok of
        TokWord txt -> txt
        TokSymbol txt -> txt
        TokString txt -> txt
        TokChar txt -> txt
        TokPunct c -> T.singleton c

    needsSpace prev curr =
      case (prev, curr) of
        (TokPunct p, _) | p `elem` ("([{" :: String) -> False
        (_, TokPunct c) | c `elem` (")]},;" :: String) -> False
        (TokPunct ',', _) -> True
        (TokPunct ';', _) -> True
        (TokSymbol "!", TokWord _) -> False
        _ -> True
