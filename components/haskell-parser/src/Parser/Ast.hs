module Parser.Ast
  ( ArithSeq (..),
    BangType (..),
    BinderName,
    CallConv (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    CompStmt (..),
    Constraint (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    DerivingClause (..),
    DoStmt (..),
    Expr (..),
    ExportSpec (..),
    FieldDecl (..),
    FixityAssoc (..),
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    GuardedRhs (..),
    ImportDecl (..),
    ImportItem (..),
    ImportSpec (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Literal (..),
    Match (..),
    Module (..),
    NewtypeDecl (..),
    OperatorName,
    Pattern (..),
    Rhs (..),
    Type (..),
    TypeSynDecl (..),
    ValueDecl (..),
    declValueBinderNames,
    valueDeclBinderName,
  )
where

import Data.Text (Text)

type BinderName = Text

type OperatorName = Text

data Module = Module
  { moduleName :: Maybe Text,
    moduleExports :: Maybe [ExportSpec],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Eq, Show)

data ExportSpec
  = ExportModule Text
  | ExportVar Text
  | ExportAbs Text
  | ExportAll Text
  | ExportWith Text [Text]
  deriving (Eq, Show)

data ImportDecl = ImportDecl
  { importDeclQualified :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Eq, Show)

data ImportSpec = ImportSpec
  { importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Eq, Show)

data ImportItem
  = ImportItemVar Text
  | ImportItemAbs Text
  | ImportItemAll Text
  | ImportItemWith Text [Text]
  deriving (Eq, Show)

data Decl
  = DeclValue ValueDecl
  | DeclTypeSig [BinderName] Type
  | DeclFixity FixityAssoc (Maybe Int) [OperatorName]
  | DeclTypeSyn TypeSynDecl
  | DeclData DataDecl
  | DeclNewtype NewtypeDecl
  | DeclClass ClassDecl
  | DeclInstance InstanceDecl
  | DeclDefault [Type]
  | DeclForeign ForeignDecl
  deriving (Eq, Show)

data ValueDecl
  = FunctionBind BinderName [Match]
  | PatternBind Pattern Rhs
  deriving (Eq, Show)

data Match = Match
  { matchPats :: [Pattern],
    matchRhs :: Rhs
  }
  deriving (Eq, Show)

data Rhs
  = UnguardedRhs Expr
  | GuardedRhss [GuardedRhs]
  deriving (Eq, Show)

data GuardedRhs = GuardedRhs
  { guardedRhsGuards :: [Expr],
    guardedRhsBody :: Expr
  }
  deriving (Eq, Show)

data Literal
  = LitInt Integer
  | LitIntBase Integer Text
  | LitFloat Double
  | LitChar Char
  | LitString Text
  deriving (Eq, Show)

data Pattern
  = PVar Text
  | PWildcard
  | PLit Literal
  | PTuple [Pattern]
  | PList [Pattern]
  | PCon Text [Pattern]
  | PInfix Pattern Text Pattern
  | PAs Text Pattern
  | PIrrefutable Pattern
  | PNegLit Literal
  | PParen Pattern
  | PRecord Text [(Text, Pattern)]
  deriving (Eq, Show)

data Type
  = TVar Text
  | TCon Text
  | TApp Type Type
  | TFun Type Type
  | TTuple [Type]
  | TList Type
  | TParen Type
  | TContext [Constraint] Type
  deriving (Eq, Show)

data Constraint = Constraint
  { constraintClass :: Text,
    constraintArgs :: [Type],
    constraintParen :: Bool
  }
  deriving (Eq, Show)

data TypeSynDecl = TypeSynDecl
  { typeSynName :: Text,
    typeSynParams :: [Text],
    typeSynBody :: Type
  }
  deriving (Eq, Show)

data DataDecl = DataDecl
  { dataDeclContext :: [Constraint],
    dataDeclName :: Text,
    dataDeclParams :: [Text],
    dataDeclConstructors :: [DataConDecl],
    dataDeclDeriving :: Maybe DerivingClause
  }
  deriving (Eq, Show)

data NewtypeDecl = NewtypeDecl
  { newtypeDeclContext :: [Constraint],
    newtypeDeclName :: Text,
    newtypeDeclParams :: [Text],
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: Maybe DerivingClause
  }
  deriving (Eq, Show)

data DataConDecl
  = PrefixCon Text [BangType]
  | InfixCon BangType Text BangType
  | RecordCon Text [FieldDecl]
  deriving (Eq, Show)

data BangType = BangType
  { bangStrict :: Bool,
    bangType :: Type
  }
  deriving (Eq, Show)

data FieldDecl = FieldDecl
  { fieldNames :: [Text],
    fieldType :: BangType
  }
  deriving (Eq, Show)

newtype DerivingClause = DerivingClause
  { derivingClasses :: [Text]
  }
  deriving (Eq, Show)

data ClassDecl = ClassDecl
  { classDeclContext :: [Constraint],
    classDeclName :: Text,
    classDeclParam :: Text,
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Eq, Show)

data ClassDeclItem
  = ClassItemTypeSig [BinderName] Type
  | ClassItemFixity FixityAssoc (Maybe Int) [OperatorName]
  | ClassItemDefault ValueDecl
  deriving (Eq, Show)

data InstanceDecl = InstanceDecl
  { instanceDeclContext :: [Constraint],
    instanceDeclClassName :: Text,
    instanceDeclTypes :: [Type],
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Eq, Show)

data InstanceDeclItem
  = InstanceItemBind ValueDecl
  | InstanceItemTypeSig [BinderName] Type
  | InstanceItemFixity FixityAssoc (Maybe Int) [OperatorName]
  deriving (Eq, Show)

data FixityAssoc
  = Infix
  | InfixL
  | InfixR
  deriving (Eq, Show)

data ForeignDecl = ForeignDecl
  { foreignDirection :: ForeignDirection,
    foreignCallConv :: CallConv,
    foreignSafety :: Maybe ForeignSafety,
    foreignEntity :: ForeignEntitySpec,
    foreignName :: Text,
    foreignType :: Type
  }
  deriving (Eq, Show)

data ForeignEntitySpec
  = ForeignEntityDynamic
  | ForeignEntityWrapper
  | ForeignEntityStatic (Maybe Text)
  | ForeignEntityAddress (Maybe Text)
  | ForeignEntityNamed Text
  | ForeignEntityOmitted
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
  | EIntBase Integer Text
  | EFloat Double
  | EChar Char
  | EString Text
  | EIf Expr Expr Expr
  | ELambda [Text] Expr
  | ELambdaPats [Pattern] Expr
  | EInfix Expr Text Expr
  | ENegate Expr
  | ESectionL Expr Text
  | ESectionR Text Expr
  | ELet [(Text, Expr)] Expr
  | ELetDecls [Decl] Expr
  | ECase Expr [CaseAlt]
  | EDo [DoStmt]
  | EListComp Expr [CompStmt]
  | EArithSeq ArithSeq
  | ERecordCon Text [(Text, Expr)]
  | ERecordUpd Expr [(Text, Expr)]
  | ETypeSig Expr Type
  | EParen Expr
  | EWhere Expr [(Text, Expr)]
  | EWhereDecls Expr [Decl]
  | EList [Expr]
  | ETuple [Expr]
  | ETupleCon Int
  | EApp Expr Expr
  deriving (Eq, Show)

data CaseAlt = CaseAlt
  { caseAltPattern :: Pattern,
    caseAltRhs :: Rhs
  }
  deriving (Eq, Show)

data DoStmt
  = DoBind Pattern Expr
  | DoLet [(Text, Expr)]
  | DoLetDecls [Decl]
  | DoExpr Expr
  deriving (Eq, Show)

data CompStmt
  = CompGen Pattern Expr
  | CompGuard Expr
  | CompLet [(Text, Expr)]
  | CompLetDecls [Decl]
  deriving (Eq, Show)

data ArithSeq
  = ArithSeqFrom Expr
  | ArithSeqFromThen Expr Expr
  | ArithSeqFromTo Expr Expr
  | ArithSeqFromThenTo Expr Expr Expr
  deriving (Eq, Show)

valueDeclBinderName :: ValueDecl -> Maybe Text
valueDeclBinderName vdecl =
  case vdecl of
    FunctionBind name _ -> Just name
    PatternBind pat _ ->
      case pat of
        PVar name -> Just name
        _ -> Nothing

declValueBinderNames :: Decl -> [Text]
declValueBinderNames decl =
  case decl of
    DeclValue vdecl ->
      case valueDeclBinderName vdecl of
        Just name -> [name]
        Nothing -> []
    _ -> []
