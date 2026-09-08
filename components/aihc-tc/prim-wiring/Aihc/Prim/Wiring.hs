{-# LANGUAGE OverloadedStrings #-}

-- | The type checker configuration of the aihc core libraries.
--
-- The type checker knows no library. Every fact it needs about one -- the
-- type constructors that built-in syntax denotes, the names that generated
-- deriving code mentions, the classes stock deriving writes code for --
-- reaches it as a table in its 'TcConfig'. This module holds those tables
-- for @aihc-prim@ and the core libraries built on it, so that the layout of
-- those packages is stated in one place outside the type checker.
--
-- This is not a library component of any package. The compiler and the
-- type checker test suites each compile this directory in place, so the
-- wiring is shared without becoming part of a released interface.
module Aihc.Prim.Wiring
  ( primTcConfig,
    primTcWiring,
    primDerivingReferences,
    boxedTupleTyConName,
    unboxedTupleTyConName,
    unboxedSumTyConName,
  )
where

import Aihc.Parser.Syntax (NameType (..))
import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Aihc.Tc
  ( DerivingReference (..),
    DerivingReferences (..),
    TcConfig,
    TcType (..),
    TcWiring (..),
    TyCon,
    mkTcConfig,
    mkTyConWithNamespace,
  )
import Data.Text (Text)
import Data.Text qualified as T

-- | The configuration of a compiler whose core libraries are the aihc ones,
-- given the identity of the primitive package.
primTcConfig :: PackageId -> TcConfig
primTcConfig prim =
  mkTcConfig prim (primDerivingReferences prim) (primTcWiring prim)

-- | The names that the aihc core libraries give the type checker.
--
-- @aihc-prim@ declares the boxed tuples in @GHC.Tuple@ under GHC\'s current
-- names -- @Unit@, @Solo@, @Tuple2@, and so on -- and the unboxed ones in
-- @GHC.Types@, which also holds the list, the arrow, the kinds, and the
-- equality constraint. The unlifted primitives live in @GHC.Prim@, the
-- implicit-parameter constraints in @GHC.Classes@, and the application
-- operator in @GHC.Base@. The Template Haskell @Lift@ class lives in
-- aihc-internal, the standin for ghc-internal, as it does in GHC 9.12
-- and later.
primTcWiring :: PackageId -> TcWiring
primTcWiring prim =
  TcWiring
    { tcWiringBoxedTupleTyCon = boxedTuple ResolutionNamespaceType,
      tcWiringBoxedTupleDataCon = boxedTuple ResolutionNamespaceTerm,
      tcWiringUnboxedTupleTyCon = unboxedTuple ResolutionNamespaceType,
      tcWiringUnboxedTupleDataCon = unboxedTuple ResolutionNamespaceTerm,
      tcWiringUnboxedSumTyCon = \arity ->
        types ResolutionNamespaceType (unboxedSumTyConName arity) arity,
      tcWiringListTyCon = listTyCon,
      tcWiringNilDataCon = nilDataCon,
      tcWiringConsDataCon = consDataCon,
      tcWiringArrowTyCon = types ResolutionNamespaceType "(->)" 2,
      tcWiringTypeTyCon = types ResolutionNamespaceType "Type" 0,
      tcWiringConstraintTyCon = types ResolutionNamespaceType "Constraint" 0,
      tcWiringBoolTyCon = types ResolutionNamespaceType "Bool" 0,
      tcWiringCharTyCon = types ResolutionNamespaceType "Char" 0,
      tcWiringEqualityTyCon = types ResolutionNamespaceType "~" 2,
      tcWiringCoercibleTyCon = types ResolutionNamespaceType "Coercible" 2,
      tcWiringImplicitParamTyCon = \name ->
        tyCon ResolutionNamespaceType "GHC.Classes" name 1,
      tcWiringPrimitiveTyCon = \name ->
        tyCon ResolutionNamespaceType "GHC.Prim" name 0,
      tcWiringApplyOperator = ("GHC.Base", "$"),
      tcWiringLiftClass = ("GHC.Internal.TH.Lift", "Lift"),
      tcWiringDataConKinds =
        [ (nilDataCon, listOf),
          (consDataCon, \elementKind -> TcFunTy elementKind (TcFunTy (listOf elementKind) (listOf elementKind)))
        ]
    }
  where
    listTyCon = types ResolutionNamespaceType "[]" 1
    nilDataCon = types ResolutionNamespaceTerm "[]" 0
    consDataCon = types ResolutionNamespaceTerm ":" 2
    listOf elementKind = TcTyCon listTyCon [elementKind]
    boxedTuple namespace arity =
      tyCon namespace "GHC.Tuple" (boxedTupleTyConName arity) arity
    unboxedTuple namespace arity =
      types namespace (unboxedTupleTyConName arity) arity
    types namespace = tyCon namespace "GHC.Types"
    tyCon :: ResolutionNamespace -> Text -> Text -> Int -> TyCon
    tyCon namespace = mkTyConWithNamespace namespace prim

-- | The name of the boxed tuple of one arity.
boxedTupleTyConName :: Int -> Text
boxedTupleTyConName arity =
  case arity of
    0 -> "Unit"
    1 -> "Solo"
    _ -> "Tuple" <> T.pack (show arity)

-- | The name of the unboxed tuple of one arity.
unboxedTupleTyConName :: Int -> Text
unboxedTupleTyConName arity = "Tuple" <> T.pack (show arity) <> "#"

-- | The name of the unboxed sum of one arity, such as @(#|#)@ for two
-- alternatives.
unboxedSumTyConName :: Int -> Text
unboxedSumTyConName arity =
  "(#" <> T.replicate (max 0 (arity - 1)) "|" <> "#)"

-- | The deriving-reference table of the aihc core libraries, given the
-- identity of the @aihc-prim@ package.
primDerivingReferences :: PackageId -> DerivingReferences
primDerivingReferences prim =
  DerivingReferences
    { derivingTrue = term prim "GHC.Types" NameConId "True",
      derivingFalse = term prim "GHC.Types" NameConId "False",
      derivingLT = term prim "GHC.Types" NameConId "LT",
      derivingEQ = term prim "GHC.Types" NameConId "EQ",
      derivingGT = term prim "GHC.Types" NameConId "GT",
      derivingIntCon = term prim "GHC.Types" NameConId "I#",
      derivingIntPrimType = DerivingReference prim "GHC.Prim" "Int#" NameConId ResolutionNamespaceType,
      derivingGreaterOrEqual = term prim "GHC.Classes" NameVarSym ">=",
      derivingCons = term prim "GHC.Types" NameConSym ":",
      derivingBind = term prim "GHC.Prim.Base" NameVarSym ">>=",
      derivingThen = term prim "GHC.Prim.Base" NameVarSym ">>",
      derivingReturn = term prim "GHC.Prim.Base" NameVarId "return",
      derivingReadParens = term prim readModule NameVarId "parens",
      derivingReadPrecContext = term prim readModule NameVarId "prec",
      derivingReadStep = term prim readModule NameVarId "step",
      derivingReadReset = term prim readModule NameVarId "reset",
      derivingReadAlternative = term prim readModule NameVarSym "+++",
      derivingReadFail = term prim readModule NameVarId "pfail",
      derivingReadExpect = term prim readModule NameVarId "expectP",
      derivingReadField = term prim readModule NameVarId "readField",
      derivingReadSymField = term prim readModule NameVarId "readSymField",
      derivingLexemeIdent = term prim readModule NameConId "Ident",
      derivingLexemeSymbol = term prim readModule NameConId "Symbol",
      derivingLexemePunc = term prim readModule NameConId "Punc",
      derivingStockClasses = coreStockClasses prim,
      derivingRecognizedClasses = coreRecognizedClasses
    }
  where
    readModule = "GHC.Prim.Read"
    term package moduleName nameType name =
      DerivingReference package moduleName name nameType ResolutionNamespaceTerm

-- | The stock classes that the aihc core libraries declare in the primitive
-- package, where each is defined. GHC keeps the same list as known-key
-- names, which carry a unit id; the package here plays that part.
coreStockClasses :: PackageId -> [(PackageId, Text, Text)]
coreStockClasses prim =
  [ (prim, "GHC.Classes", "Eq"),
    (prim, "GHC.Classes", "Ord"),
    (prim, "GHC.Prim.Enum", "Enum"),
    (prim, "GHC.Prim.Enum", "Bounded"),
    (prim, "GHC.Prim.Show", "Show"),
    (prim, "GHC.Prim.Read", "Read"),
    (prim, "GHC.Prim.Base", "Functor")
  ]

-- | The stock classes of GHC that the core libraries declare outside the
-- primitive package. The generator writes no code for them, so they carry
-- no package.
coreRecognizedClasses :: [(Text, Text)]
coreRecognizedClasses =
  [ ("GHC.Ix", "Ix"),
    ("GHC.Internal.Foldable", "Foldable"),
    ("GHC.Internal.Traversable", "Traversable"),
    ("Data.Data", "Data"),
    ("Type.Reflection", "Typeable"),
    ("GHC.Generics", "Generic"),
    ("GHC.Generics", "Generic1"),
    ("GHC.Internal.TH.Lift", "Lift")
  ]
