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
    SourceSpan (..),
    Type (..),
    TypeSynDecl (..),
    ValueDecl (..),
    declValueBinderNames,
    noSourceSpan,
    valueDeclBinderName,
  )
where

import Data.Text (Text)

data SourceSpan
  = NoSourceSpan
  | SourceSpan
      { sourceSpanStartLine :: !Int,
        sourceSpanStartCol :: !Int,
        sourceSpanEndLine :: !Int,
        sourceSpanEndCol :: !Int
      }
  deriving (Eq, Ord, Show)

noSourceSpan :: SourceSpan
noSourceSpan = NoSourceSpan

type BinderName = Text

type OperatorName = Text

data Module = Module
  { moduleSpan :: SourceSpan,
    moduleName :: Maybe Text,
    moduleLanguagePragmas :: [Text],
    moduleExports :: Maybe [ExportSpec],
    moduleImports :: [ImportDecl],
    moduleDecls :: [Decl]
  }
  deriving (Eq, Show)

data ExportSpec
  = ExportModule SourceSpan Text
  | ExportVar SourceSpan Text
  | ExportAbs SourceSpan Text
  | ExportAll SourceSpan Text
  | ExportWith SourceSpan Text [Text]
  deriving (Eq, Show)

data ImportDecl = ImportDecl
  { importDeclSpan :: SourceSpan,
    importDeclQualified :: Bool,
    importDeclModule :: Text,
    importDeclAs :: Maybe Text,
    importDeclSpec :: Maybe ImportSpec
  }
  deriving (Eq, Show)

data ImportSpec = ImportSpec
  { importSpecSpan :: SourceSpan,
    importSpecHiding :: Bool,
    importSpecItems :: [ImportItem]
  }
  deriving (Eq, Show)

data ImportItem
  = ImportItemVar SourceSpan Text
  | ImportItemAbs SourceSpan Text
  | ImportItemAll SourceSpan Text
  | ImportItemWith SourceSpan Text [Text]
  deriving (Eq, Show)

data Decl
  = DeclValue SourceSpan ValueDecl
  | DeclTypeSig SourceSpan [BinderName] Type
  | DeclFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  | DeclTypeSyn SourceSpan TypeSynDecl
  | DeclData SourceSpan DataDecl
  | DeclNewtype SourceSpan NewtypeDecl
  | DeclClass SourceSpan ClassDecl
  | DeclInstance SourceSpan InstanceDecl
  | DeclDefault SourceSpan [Type]
  | DeclForeign SourceSpan ForeignDecl
  deriving (Eq, Show)

data ValueDecl
  = FunctionBind SourceSpan BinderName [Match]
  | PatternBind SourceSpan Pattern Rhs
  deriving (Eq, Show)

data Match = Match
  { matchSpan :: SourceSpan,
    matchPats :: [Pattern],
    matchRhs :: Rhs
  }
  deriving (Eq, Show)

data Rhs
  = UnguardedRhs SourceSpan Expr
  | GuardedRhss SourceSpan [GuardedRhs]
  deriving (Eq, Show)

data GuardedRhs = GuardedRhs
  { guardedRhsSpan :: SourceSpan,
    guardedRhsGuards :: [Expr],
    guardedRhsBody :: Expr
  }
  deriving (Eq, Show)

data Literal
  = LitInt SourceSpan Integer Text
  | LitIntBase SourceSpan Integer Text
  | LitFloat SourceSpan Double Text
  | LitChar SourceSpan Char
  | LitString SourceSpan Text
  deriving (Eq, Show)

data Pattern
  = PVar SourceSpan Text
  | PWildcard SourceSpan
  | PLit SourceSpan Literal
  | PQuasiQuote SourceSpan Text Text
  | PTuple SourceSpan [Pattern]
  | PList SourceSpan [Pattern]
  | PCon SourceSpan Text [Pattern]
  | PInfix SourceSpan Pattern Text Pattern
  | PView SourceSpan Expr Pattern
  | PAs SourceSpan Text Pattern
  | PIrrefutable SourceSpan Pattern
  | PNegLit SourceSpan Literal
  | PParen SourceSpan Pattern
  | PRecord SourceSpan Text [(Text, Pattern)]
  deriving (Eq, Show)

data Type
  = TVar SourceSpan Text
  | TCon SourceSpan Text
  | TQuasiQuote SourceSpan Text Text
  | TApp SourceSpan Type Type
  | TFun SourceSpan Type Type
  | TTuple SourceSpan [Type]
  | TList SourceSpan Type
  | TParen SourceSpan Type
  | TContext SourceSpan [Constraint] Type
  deriving (Eq, Show)

data Constraint = Constraint
  { constraintSpan :: SourceSpan,
    constraintClass :: Text,
    constraintArgs :: [Type],
    constraintParen :: Bool
  }
  deriving (Eq, Show)

data TypeSynDecl = TypeSynDecl
  { typeSynSpan :: SourceSpan,
    typeSynName :: Text,
    typeSynParams :: [Text],
    typeSynBody :: Type
  }
  deriving (Eq, Show)

data DataDecl = DataDecl
  { dataDeclSpan :: SourceSpan,
    dataDeclContext :: [Constraint],
    dataDeclName :: Text,
    dataDeclParams :: [Text],
    dataDeclConstructors :: [DataConDecl],
    dataDeclDeriving :: Maybe DerivingClause
  }
  deriving (Eq, Show)

data NewtypeDecl = NewtypeDecl
  { newtypeDeclSpan :: SourceSpan,
    newtypeDeclContext :: [Constraint],
    newtypeDeclName :: Text,
    newtypeDeclParams :: [Text],
    newtypeDeclConstructor :: Maybe DataConDecl,
    newtypeDeclDeriving :: Maybe DerivingClause
  }
  deriving (Eq, Show)

data DataConDecl
  = PrefixCon SourceSpan Text [BangType]
  | InfixCon SourceSpan BangType Text BangType
  | RecordCon SourceSpan Text [FieldDecl]
  deriving (Eq, Show)

data BangType = BangType
  { bangSpan :: SourceSpan,
    bangStrict :: Bool,
    bangType :: Type
  }
  deriving (Eq, Show)

data FieldDecl = FieldDecl
  { fieldSpan :: SourceSpan,
    fieldNames :: [Text],
    fieldType :: BangType
  }
  deriving (Eq, Show)

newtype DerivingClause = DerivingClause
  { derivingClasses :: [Text]
  }
  deriving (Eq, Show)

data ClassDecl = ClassDecl
  { classDeclSpan :: SourceSpan,
    classDeclContext :: [Constraint],
    classDeclName :: Text,
    classDeclParam :: Text,
    classDeclItems :: [ClassDeclItem]
  }
  deriving (Eq, Show)

data ClassDeclItem
  = ClassItemTypeSig SourceSpan [BinderName] Type
  | ClassItemFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  | ClassItemDefault SourceSpan ValueDecl
  deriving (Eq, Show)

data InstanceDecl = InstanceDecl
  { instanceDeclSpan :: SourceSpan,
    instanceDeclContext :: [Constraint],
    instanceDeclClassName :: Text,
    instanceDeclTypes :: [Type],
    instanceDeclItems :: [InstanceDeclItem]
  }
  deriving (Eq, Show)

data InstanceDeclItem
  = InstanceItemBind SourceSpan ValueDecl
  | InstanceItemTypeSig SourceSpan [BinderName] Type
  | InstanceItemFixity SourceSpan FixityAssoc (Maybe Int) [OperatorName]
  deriving (Eq, Show)

data FixityAssoc
  = Infix
  | InfixL
  | InfixR
  deriving (Eq, Show)

data ForeignDecl = ForeignDecl
  { foreignDeclSpan :: SourceSpan,
    foreignDirection :: ForeignDirection,
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
  = EVar SourceSpan Text
  | EInt SourceSpan Integer Text
  | EIntBase SourceSpan Integer Text
  | EFloat SourceSpan Double Text
  | EChar SourceSpan Char
  | EString SourceSpan Text
  | EQuasiQuote SourceSpan Text Text
  | EIf SourceSpan Expr Expr Expr
  | ELambdaPats SourceSpan [Pattern] Expr
  | EInfix SourceSpan Expr Text Expr
  | ENegate SourceSpan Expr
  | ESectionL SourceSpan Expr Text
  | ESectionR SourceSpan Text Expr
  | ELetDecls SourceSpan [Decl] Expr
  | ECase SourceSpan Expr [CaseAlt]
  | EDo SourceSpan [DoStmt]
  | EListComp SourceSpan Expr [CompStmt]
  | EListCompParallel SourceSpan Expr [[CompStmt]]
  | EArithSeq SourceSpan ArithSeq
  | ERecordCon SourceSpan Text [(Text, Expr)]
  | ERecordUpd SourceSpan Expr [(Text, Expr)]
  | ETypeSig SourceSpan Expr Type
  | EParen SourceSpan Expr
  | EWhereDecls SourceSpan Expr [Decl]
  | EList SourceSpan [Expr]
  | ETuple SourceSpan [Expr]
  | ETupleCon SourceSpan Int
  | ETypeApp SourceSpan Expr Type
  | EApp SourceSpan Expr Expr
  deriving (Eq, Show)

data CaseAlt = CaseAlt
  { caseAltSpan :: SourceSpan,
    caseAltPattern :: Pattern,
    caseAltRhs :: Rhs
  }
  deriving (Eq, Show)

data DoStmt
  = DoBind SourceSpan Pattern Expr
  | DoLet SourceSpan [(Text, Expr)]
  | DoLetDecls SourceSpan [Decl]
  | DoExpr SourceSpan Expr
  deriving (Eq, Show)

data CompStmt
  = CompGen SourceSpan Pattern Expr
  | CompGuard SourceSpan Expr
  | CompLet SourceSpan [(Text, Expr)]
  | CompLetDecls SourceSpan [Decl]
  deriving (Eq, Show)

data ArithSeq
  = ArithSeqFrom SourceSpan Expr
  | ArithSeqFromThen SourceSpan Expr Expr
  | ArithSeqFromTo SourceSpan Expr Expr
  | ArithSeqFromThenTo SourceSpan Expr Expr Expr
  deriving (Eq, Show)

valueDeclBinderName :: ValueDecl -> Maybe Text
valueDeclBinderName vdecl =
  case vdecl of
    FunctionBind _ name _ -> Just name
    PatternBind _ pat _ ->
      case pat of
        PVar _ name -> Just name
        _ -> Nothing

declValueBinderNames :: Decl -> [Text]
declValueBinderNames decl =
  case decl of
    DeclValue _ vdecl ->
      case valueDeclBinderName vdecl of
        Just name -> [name]
        Nothing -> []
    _ -> []
