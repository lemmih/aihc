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
      canonicalDecls = concatMap normalizeDecls (moduleDecls m)
    }

normalizeDecl :: Decl -> CanonicalDecl
normalizeDecl decl =
  case normalizeDecls decl of
    (firstDecl : _) -> firstDecl
    [] -> CanonicalPatternDecl ""

normalizeDecls :: Decl -> [CanonicalDecl]
normalizeDecls d =
  case d of
    DeclValue valueDecl ->
      case valueDecl of
        FunctionBind name matches ->
          case matches of
            [Match {matchPats = [], matchRhs = UnguardedRhs expr}] ->
              [ CanonicalValueDecl
                  { canonicalDeclName = name,
                    canonicalDeclExpr = normalizeExpr expr
                  }
              ]
            _ ->
              [ CanonicalFunctionDecl
                  { canonicalFunctionName = name
                  }
              ]
        PatternBind pat _ ->
          [ CanonicalPatternDecl
              { canonicalPatternLhs = renderPattern pat
              }
          ]
    DeclTypeSig names _ ->
      [ CanonicalTypeSigDecl
          { canonicalTypeSigName = name
          }
      | name <- names
      ]
    DeclFixity assoc prec ops ->
      [ CanonicalFixityDecl
          { canonicalFixityAssoc = fixityAssocText assoc,
            canonicalFixityPrecedence = prec,
            canonicalFixityOperator = op
          }
      | op <- ops
      ]
    DeclTypeSyn synDecl ->
      [ CanonicalTypeDecl
          { canonicalTypeDeclName = typeSynName synDecl
          }
      ]
    DeclData dataDecl ->
      [ CanonicalDataDecl
          { canonicalTypeName = dataDeclName dataDecl,
            canonicalConstructors = map constructorName (dataDeclConstructors dataDecl)
          }
      ]
    DeclNewtype newtypeDecl ->
      [ CanonicalNewtypeDecl
          { canonicalNewtypeName = newtypeDeclName newtypeDecl,
            canonicalNewtypeConstructor = constructorName <$> newtypeDeclConstructor newtypeDecl
          }
      ]
    DeclClass classDecl ->
      [ CanonicalClassDecl
          { canonicalClassName = classDeclName classDecl
          }
      ]
    DeclInstance instanceDecl ->
      [ CanonicalInstanceDecl
          { canonicalInstanceClassName = instanceDeclClassName instanceDecl
          }
      ]
    DeclDefault tys ->
      [ CanonicalDefaultDecl
          { canonicalDefaultTypes = map renderTypeToken tys
          }
      ]
    DeclForeign foreignDecl ->
      [ CanonicalForeignDecl
          { canonicalForeignDirection = normalizeDirection (foreignDirection foreignDecl),
            canonicalForeignCallConv = normalizeCallConv (foreignCallConv foreignDecl),
            canonicalForeignSafety = fmap normalizeSafety (foreignSafety foreignDecl),
            canonicalForeignEntity = classifyForeignEntity (foreignEntity foreignDecl),
            canonicalForeignName = foreignName foreignDecl
          }
      ]

constructorName :: DataConDecl -> Text
constructorName conDecl =
  case conDecl of
    PrefixCon name _ -> name
    InfixCon _ op _ -> op
    RecordCon name _ -> name

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
    ETypeSig inner sigType -> CTypeSig (normalizeExpr inner) (renderTypeToken sigType)
    EParen inner -> normalizeExpr inner
    EWhere body binds ->
      CLet [(name, normalizeExpr value) | (name, value) <- binds] (normalizeExpr body)
    EList values -> CList (fmap normalizeExpr values)
    ETuple values -> CTuple (fmap normalizeExpr values)
    ETupleCon arity -> CTupleCon arity
    EApp fn arg -> CApp (normalizeExpr fn) (normalizeExpr arg)

normalizeCaseAlt :: CaseAlt -> CanonicalCaseAlt
normalizeCaseAlt alt =
  case caseAltRhs alt of
    UnguardedRhs expr ->
      CanonicalCaseAlt
        { canonicalCaseAltPattern = renderPattern (caseAltPattern alt),
          canonicalCaseAltExpr = normalizeExpr expr
        }
    GuardedRhss grhss ->
      let fallbackExpr =
            case grhss of
              [] -> CInt 0
              (firstGuard : _) -> normalizeExpr (guardedRhsBody firstGuard)
       in CanonicalCaseAlt {canonicalCaseAltPattern = renderPattern (caseAltPattern alt), canonicalCaseAltExpr = fallbackExpr}

normalizeDoStmt :: DoStmt -> CanonicalDoStmt
normalizeDoStmt stmt =
  case stmt of
    DoBind pat expr -> CDoBind (renderPattern pat) (normalizeExpr expr)
    DoLet binds -> CDoLet [(name, normalizeExpr value) | (name, value) <- binds]
    DoExpr expr -> CDoExpr (normalizeExpr expr)

normalizeCompStmt :: CompStmt -> CanonicalCompStmt
normalizeCompStmt stmt =
  case stmt of
    CompGen pat expr -> CCompGen (renderPattern pat) (normalizeExpr expr)
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

classifyForeignEntity :: ForeignEntitySpec -> Maybe Text
classifyForeignEntity entity =
  case entity of
    ForeignEntityOmitted -> Nothing
    ForeignEntityDynamic -> Just "dynamic"
    ForeignEntityWrapper -> Just "wrapper"
    ForeignEntityStatic _ -> Just "static"
    ForeignEntityAddress _ -> Just "address"
    ForeignEntityNamed _ -> Just "named"

fixityAssocText :: FixityAssoc -> Text
fixityAssocText assoc =
  case assoc of
    Infix -> "infix"
    InfixL -> "infixl"
    InfixR -> "infixr"

renderTypeToken :: Type -> Text
renderTypeToken ty =
  case ty of
    TVar n -> n
    TCon n -> n
    TApp f x -> renderTypeToken f <> " " <> renderTypeTokenAtom x
    TFun a b -> renderTypeTokenAtom a <> " -> " <> renderTypeToken b
    TTuple elems -> "(" <> T.intercalate ", " (map renderTypeToken elems) <> ")"
    TList inner -> "[" <> renderTypeToken inner <> "]"
    TParen inner -> "(" <> renderTypeToken inner <> ")"
    TContext constraints inner -> renderConstraints constraints <> " => " <> renderTypeToken inner

renderTypeTokenAtom :: Type -> Text
renderTypeTokenAtom ty =
  case ty of
    TVar _ -> renderTypeToken ty
    TCon _ -> renderTypeToken ty
    TList _ -> renderTypeToken ty
    TTuple _ -> renderTypeToken ty
    _ -> "(" <> renderTypeToken ty <> ")"

renderConstraints :: [Constraint] -> Text
renderConstraints constraints =
  case constraints of
    [single] -> renderConstraint single
    _ -> "(" <> T.intercalate ", " (map renderConstraint constraints) <> ")"

renderConstraint :: Constraint -> Text
renderConstraint constraint =
  T.unwords (constraintClass constraint : map renderTypeTokenAtom (constraintArgs constraint))

renderPattern :: Pattern -> Text
renderPattern pat =
  case pat of
    PVar name -> name
    PWildcard -> "_"
    PLit lit -> renderLiteral lit
    PTuple elems -> "(" <> T.intercalate ", " (map renderPattern elems) <> ")"
    PList elems -> "[" <> T.intercalate ", " (map renderPattern elems) <> "]"
    PCon con args -> T.unwords (con : map renderPatternAtom args)
    PInfix lhs op rhs -> renderPatternAtom lhs <> " " <> op <> " " <> renderPatternAtom rhs
    PAs name inner -> name <> "@" <> renderPatternAtom inner
    PIrrefutable inner -> "~" <> renderPatternAtom inner
    PParen inner -> "(" <> renderPattern inner <> ")"
    PRecord con fields ->
      con
        <> " { "
        <> T.intercalate ", " [n <> " = " <> renderPattern p | (n, p) <- fields]
        <> " }"

renderPatternAtom :: Pattern -> Text
renderPatternAtom pat =
  case pat of
    PVar _ -> renderPattern pat
    PWildcard -> renderPattern pat
    PLit _ -> renderPattern pat
    PTuple _ -> renderPattern pat
    PList _ -> renderPattern pat
    PParen _ -> renderPattern pat
    _ -> "(" <> renderPattern pat <> ")"

renderLiteral :: Literal -> Text
renderLiteral lit =
  case lit of
    LitInt n -> T.pack (show n)
    LitFloat n -> T.pack (show n)
    LitChar c -> T.pack (show c)
    LitString s -> T.pack (show (T.unpack s))
