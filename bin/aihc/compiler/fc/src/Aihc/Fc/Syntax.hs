-- | System FC abstract syntax.
module Aihc.Fc.Syntax
  ( Type (..),
    Binder (..),
    Expr (..),
    Bind (..),
    Alt (..),
    AltCon (..),
    Literal (..),
    Role (..),
    Coercion (..),
    Program (..),
    Imports (..),
    Decl (..),
    TypeDecl (..),
    ConDecl (..),
    SynonymDecl (..),
    AxiomDecl (..),
    ValDecl (..),
    ForeignCall (..),
    ForeignImportDependency (..),
    CallingConvention (..),
    CCallSpec (..),
    CCallTarget (..),
    CAbiType (..),
    ForeignEffect (..),
    ForeignSafety (..),
  )
where

import Aihc.Fc.Name
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)

-- | A type. Kinds are types.
data Type
  = TyVar Name
  | TyCon Name
  | TyApp Type Type
  | -- | @FUN r1 r2 a b@.
    TyFun Type Type Type Type
  | TyForAll Binder Type
  | TyEq Type Type
  deriving (Eq, Ord, Show, Read)

data Binder = Binder
  { binderName :: Name,
    binderType :: Type
  }
  deriving (Eq, Ord, Show, Read)

data Expr
  = ExVar Name
  | ExLit Literal
  | ExApp Expr Expr
  | ExTyApp Expr Type
  | ExLam Binder Expr
  | ExTyLam Binder Expr
  | ExLet Bind Expr
  | ExRec [Bind] Expr
  | ExCase Expr Binder Type [Alt]
  | ExCast Expr Coercion
  | -- | Equality evidence has no runtime fields.
    ExCoercion Coercion
  | -- | A saturated call of a foreign import. The type arguments instantiate
    -- the leading binders of the foreign type. The value arguments fill every
    -- arrow of the foreign type.
    ExForeignCall ForeignCall [Type] [Expr]
  deriving (Eq, Ord, Show, Read)

-- | The foreign import that a call names, with the facts that lower it.
data ForeignCall = ForeignCall
  { foreignCallName :: Name,
    foreignCallConvention :: CallingConvention,
    foreignCallDependencies :: [ForeignImportDependency],
    foreignCallType :: Type
  }
  deriving (Eq, Ord, Show, Read)

data Bind = Bind
  { bindBinder :: Binder,
    bindRhs :: Expr
  }
  deriving (Eq, Ord, Show, Read)

data Alt = Alt
  { altCon :: AltCon,
    altTypeBinders :: [Binder],
    altBinders :: [Binder],
    altRhs :: Expr
  }
  deriving (Eq, Ord, Show, Read)

data AltCon
  = AltData Name
  | AltLit Literal
  | AltDefault
  deriving (Eq, Ord, Show, Read)

-- | A literal. Integer, character, and address store the representation type.
data Literal
  = LitInt Type Integer
  | LitChar Type Char
  | LitAddr Type ByteString
  deriving (Eq, Ord, Show, Read)

data Role
  = Nominal
  | Representational
  | Phantom
  deriving (Eq, Ord, Show, Read)

data Coercion
  = CoVar Name
  | CoRefl Type
  | CoSym Coercion
  | CoTrans Coercion Coercion
  | CoApp Coercion Coercion
  | CoFun Coercion Coercion
  | CoTyConApp Name [Coercion]
  | CoAxiom Name [Type]
  deriving (Eq, Ord, Show, Read)

data Program = Program
  { programScopes :: ScopeTable,
    programImports :: Imports,
    programDecls :: [Decl]
  }
  deriving (Eq, Ord, Show, Read)

data Imports = Imports
  { importHeaders :: Map Name Type,
    importSynonyms :: Map Name Type,
    importAxioms :: Map Name AxiomDecl,
    importBinders :: Map Name Type
  }
  deriving (Eq, Ord, Show, Read)

data Decl
  = DeclType TypeDecl
  | DeclSynonym SynonymDecl
  | DeclAxiom AxiomDecl
  | DeclVal ValDecl
  deriving (Eq, Ord, Show, Read)

data TypeDecl = TypeDecl
  { typeVis :: Vis,
    typeName :: Name,
    typeBinders :: [Binder],
    typeResult :: Type,
    typeRoles :: [Role],
    typeCons :: [ConDecl]
  }
  deriving (Eq, Ord, Show, Read)

data ConDecl = ConDecl
  { conVis :: Vis,
    conName :: Name,
    conType :: Type
  }
  deriving (Eq, Ord, Show, Read)

data SynonymDecl = SynonymDecl
  { synVis :: Vis,
    synName :: Name,
    synBinders :: [Binder],
    synResult :: Type,
    synBody :: Type
  }
  deriving (Eq, Ord, Show, Read)

data AxiomDecl = AxiomDecl
  { axiomVis :: Vis,
    axiomName :: Name,
    axiomBinders :: [Binder],
    axiomRole :: Role,
    axiomLeft :: Type,
    axiomRight :: Type
  }
  deriving (Eq, Ord, Show, Read)

data ValDecl = ValDecl
  { valVis :: Vis,
    valName :: Name,
    valType :: Type,
    valBody :: Expr
  }
  deriving (Eq, Ord, Show, Read)

data ForeignImportDependency
  = ForeignAxiom Name
  | ForeignConstructor Name
  deriving (Eq, Ord, Show, Read)

data CallingConvention
  = Prim
  | CCall CCallSpec
  deriving (Eq, Ord, Show, Read)

data CCallSpec = CCallSpec
  { ccallSymbol :: Text,
    ccallTarget :: CCallTarget,
    ccallSafety :: ForeignSafety,
    ccallArgumentTypes :: [CAbiType],
    ccallResultType :: CAbiType,
    ccallEffect :: ForeignEffect
  }
  deriving (Eq, Ord, Show, Read)

-- | What a C entity string names: a function to call, or a static symbol
-- whose address is the imported value (@foreign import ccall "&sym"@).
data CCallTarget
  = CCallFunction
  | CCallAddress
  deriving (Eq, Ord, Show, Read)

data CAbiType
  = CAbiInt
  | CAbiInt8
  | CAbiInt16
  | CAbiInt32
  | CAbiInt64
  | CAbiWord
  | CAbiWord8
  | CAbiWord16
  | CAbiWord32
  | CAbiWord64
  | CAbiFloat
  | CAbiDouble
  | CAbiAddr
  | -- | The result of a C procedure, which has no value.
    CAbiVoid
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ForeignEffect
  = ForeignPure
  | ForeignRealWorld
  deriving (Eq, Ord, Show, Read)

-- | Safety of a foreign call. The runtime is single-threaded, so safe and
-- unsafe calls are lowered the same way; the mark is kept for fidelity.
data ForeignSafety
  = ForeignUnsafe
  | ForeignSafe
  deriving (Eq, Ord, Show, Read)
