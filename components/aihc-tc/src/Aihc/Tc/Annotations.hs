{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Type checker annotations for AST nodes.
--
-- Following the pattern established by @aihc-resolve@, the type checker
-- attaches its results as 'Annotation' values on AST nodes using the
-- existing 'DeclAnn'/'EAnn'/'PAnn'/'TAnn' wrappers.
module Aihc.Tc.Annotations
  ( -- * Annotation type
    TcAnnotation (..),
    TcCastAnnotation (..),
    PendingTcCastAnnotation (..),
    annotateRhsCast,
    TcForeignImportAnnotation (..),
    TcForeignImportInfo (..),
    TcForeignSafety (..),
    TcForeignEffect (..),
    TcForeignTarget (..),
    TcForeignMarshal (..),
    TcForeignAbiType (..),
    PendingTcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcInstanceAnnotation (..),
    TcNewtypeDeriving (..),
    TcNewtypeInstance (..),
    TcNewtypeMethod (..),
    TcPatSynAnnotation (..),
    TcInstanceMethodAnnotation (..),

    -- * Pattern synonyms for extracting annotations
    pattern ETcAnn,
    pattern DTcAnn,
    pattern PTcAnn,
    pattern TTcAnn,

    -- * Helpers
    annotateExpr,
    annotateDecl,
    pendingAnnotation,
    pendingTypeLambdaAnnotation,

    -- * Pretty-printing
    renderPred,
    renderTcType,
    renderTcTypeInModule,
    renderTcSignature,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    Match,
    Pattern (..),
    Rhs (..),
    SourceSpan,
    Type (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (ResolutionNamespace (..))
import Aihc.Tc.Env (AssociatedTypeInfo, DataTypeInfo, TypeFamilyInstanceInfo)
import Aihc.Tc.Evidence (Coercion, EvTerm, EvVar)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), boxedTupleTyConName, tyConModuleName, tyConNamespace, pattern KType)
import Data.Text (Text)
import Data.Text qualified as T

-- | A checked cast on the result of a right-hand side.
newtype TcCastAnnotation = TcCastAnnotation Coercion
  deriving (Eq, Show)

-- | The solver must supply the proof before FC desugaring.
data PendingTcCastAnnotation = PendingTcCastAnnotation TcType EvVar
  deriving (Eq, Show)

annotateRhsCast :: TcType -> EvVar -> Rhs body -> Rhs body
annotateRhsCast ty evidence rhs =
  let annotation = mkAnnotation (PendingTcCastAnnotation ty evidence)
   in case rhs of
        UnguardedRhs annotations body locals -> UnguardedRhs (annotation : annotations) body locals
        GuardedRhss annotations bodies locals -> GuardedRhss (annotation : annotations) bodies locals

-- | Annotation attached to AST nodes by the type checker.
--
-- Not every field is populated for every node. A variable reference gets
-- a type; a top-level binding gets the generalized scheme, etc.
data TcAnnotation = TcAnnotation
  { -- | The inferred/checked type of this node.
    tcAnnType :: !TcType,
    -- | Type variables abstracted at this expression.
    tcAnnTypeBinders :: ![TyVarId],
    -- | Type arguments made explicit at this occurrence.
    tcAnnTypeArgs :: ![TcType],
    -- | Evidence terms whose dictionaries must be passed at this occurrence.
    tcAnnEvidenceTerms :: ![EvTerm],
    -- | Evidence terms whose dictionaries are abstracted at this expression.
    tcAnnEvidenceBinders :: ![EvTerm],
    -- | Term argument types made explicit for lambda-like binders.
    tcAnnTermArgTypes :: ![TcType]
  }
  deriving (Eq, Show)

-- | The fully checked lowering plan for a foreign import.  Keeping this in
-- the type-checker output prevents System FC desugaring from rediscovering
-- Haskell FFI representation rules from constructor names.
data TcForeignImportAnnotation = TcForeignImportAnnotation
  { tcForeignArguments :: ![TcForeignMarshal],
    tcForeignResult :: !TcForeignMarshal,
    tcForeignEffect :: !TcForeignEffect,
    -- | The C symbol that the entity string names.
    tcForeignSymbol :: !Text,
    tcForeignTarget :: !TcForeignTarget
  }
  deriving (Eq, Show, Read)

-- | The checked calling convention of a foreign import. The interface keeps
-- this fact for each foreign import, so a module that uses the import can
-- desugar each use to a saturated foreign call.
data TcForeignImportInfo
  = -- | A @foreign import prim@. The name of the import selects the primitive.
    TcForeignPrimImport
  | -- | A @foreign import ccall@ with its safety mark and checked plan.
    TcForeignCCallImport !TcForeignSafety !TcForeignImportAnnotation
  deriving (Eq, Show, Read)

-- | The safety mark of a @ccall@ foreign import. A missing mark is safe.
data TcForeignSafety
  = TcForeignSafe
  | TcForeignUnsafe
  | TcForeignInterruptible
  deriving (Eq, Show, Read)

-- | Whether a foreign import calls the C symbol or takes its address.
data TcForeignTarget
  = TcForeignCall
  | TcForeignAddress
  deriving (Eq, Show, Read)

-- | Whether a raw foreign call is pure or explicitly threads the real-world
-- state token.
data TcForeignEffect
  = TcForeignPure
  | TcForeignRealWorld
  deriving (Eq, Show, Read)

-- | A source value's path to its primitive ABI representation.  Constructor
-- names are ordered outermost to innermost; for example, a @CInt@ is lowered
-- through @CInt@ and @I32#@ to @Int32#@.
data TcForeignMarshal = TcForeignMarshal
  { tcForeignSourceType :: !TcType,
    tcForeignPrimitiveType :: !TcType,
    tcForeignConstructors :: ![Text],
    tcForeignAbiType :: !TcForeignAbiType
  }
  deriving (Eq, Show, Read)

-- | Primitive values understood by the C ABI bridge.  This is deliberately
-- independent from lifted Haskell wrapper types.
data TcForeignAbiType
  = TcForeignInt
  | TcForeignInt8
  | TcForeignInt16
  | TcForeignInt32
  | TcForeignInt64
  | TcForeignWord
  | TcForeignWord8
  | TcForeignWord16
  | TcForeignWord32
  | TcForeignWord64
  | TcForeignFloat
  | TcForeignDouble
  | TcForeignAddr
  | -- | The unit result of a C procedure.
    TcForeignVoid
  deriving (Eq, Show, Read)

-- | Type-checker annotation payload before constraint solving has finished.
--
-- The generator attaches this directly to the syntax node that produced it.
-- A finalization pass zonks the types and resolves evidence variables into
-- ordinary 'TcAnnotation' values after solving.
data PendingTcAnnotation = PendingTcAnnotation
  { pendingTcAnnType :: !TcType,
    pendingTcAnnTypeBinders :: ![TyVarId],
    pendingTcAnnTypeArgs :: ![TcType],
    pendingTcAnnEvidenceVars :: ![EvVar],
    pendingTcAnnEvidenceBinders :: ![EvVar],
    pendingTcAnnTermArgTypes :: ![TcType]
  }
  deriving (Eq, Show)

data TcDictBinderAnnotation = TcDictBinderAnnotation
  { tcDictBinderClassName :: !Text,
    tcDictBinderArgs :: ![TcType],
    tcDictBinderType :: !TcType
  }
  deriving (Eq, Show)

data TcClassMethodAnnotation = TcClassMethodAnnotation
  { tcClassMethodName :: !Text,
    tcClassMethodType :: !TcType,
    tcClassMethodTyVars :: ![TyVarId],
    tcClassMethodDictType :: !TcType,
    tcClassMethodIndex :: !Int
  }
  deriving (Eq, Show)

data TcClassAnnotation = TcClassAnnotation
  { tcClassTyCon :: !TyCon,
    tcClassKindTyVars :: ![TyVarId],
    tcClassTyVars :: ![TyVarId],
    tcClassSuperClasses :: ![TcDictBinderAnnotation],
    tcClassMethods :: ![TcClassMethodAnnotation],
    tcClassDefaultMethods :: ![Text],
    tcClassDefaultSignatures :: ![(Text, TcType)],
    tcClassAssociatedTypes :: ![AssociatedTypeInfo]
  }
  deriving (Eq, Show)

-- | The effective deriving strategy selected by the type checker. Source
-- declarations without an explicit strategy are resolved before a plan is
-- attached, so System FC never has to reproduce extension-sensitive policy.
data TcDerivingStrategy
  = TcDerivingStock
  | TcDerivingNewtype
  | TcDerivingAnyclass
  | TcDerivingVia !TcType
  deriving (Eq, Show)

-- | How the derived instance context is supplied. Standalone deriving has an
-- explicit checked context; attached clauses require strategy-specific
-- inference in a later type-checker step.
data TcDerivingContext
  = TcDerivingInferContext
  | TcDerivingExplicitContext ![Pred]
  deriving (Eq, Show)

-- | The checked shape of one deriving request. The context is inferred
-- for an attached clause and checked for a standalone declaration; the
-- generated instance declaration is an ordinary 'DeclInstance' that the
-- instance checker and System FC lowering treat like source.
data TcDerivingPlan = TcDerivingPlan
  { tcDerivingSourceSpan :: !SourceSpan,
    tcDerivingStrategy :: !TcDerivingStrategy,
    tcDerivingStockFallback :: !Bool,
    tcDerivingClassName :: !Text,
    tcDerivingClassTyCon :: !TyCon,
    tcDerivingClassOrigin :: !(Maybe (Text, Text)),
    tcDerivingTyVars :: ![TyVarId],
    tcDerivingHeadTypes :: ![TcType],
    -- | Checked constructor layout of the final instance-head type, when the
    -- target is a data or newtype constructor known to this compilation.
    tcDerivingDataType :: !(Maybe DataTypeInfo),
    tcDerivingContext :: !TcDerivingContext,
    tcDerivingClassTyVars :: ![TyVarId],
    tcDerivingClassSuperClasses :: ![TcDictBinderAnnotation],
    tcDerivingClassMethods :: ![TcClassMethodAnnotation],
    tcDerivingDefaultMethods :: ![Text],
    tcDerivingDefaultSignatures :: ![(Text, [Pred])]
  }
  deriving (Eq, Show)

newtype TcDerivingAnnotation = TcDerivingAnnotation
  { tcDerivingPlans :: [TcDerivingPlan]
  }
  deriving (Eq, Show)

-- | The checked matcher equation, builder equations, and record field
-- selector equations of a pattern synonym. The desugarer emits them as
-- ordinary functions. A selector pairs its field label with its equation.
data TcPatSynAnnotation = TcPatSynAnnotation
  { tcPatSynMatcher :: !Match,
    tcPatSynBuilder :: !(Maybe [Match]),
    tcPatSynSelectors :: ![(Text, Match)]
  }
  deriving (Eq, Show)

-- | A generated instance retains its source derivation plan.
newtype TcNewtypeDeriving = TcNewtypeDeriving TcDerivingPlan
  deriving (Eq, Show)

-- | Checked evidence and casts for a newtype instance.
data TcNewtypeInstance = TcNewtypeInstance
  { tcNewtypeHeadTypes :: ![TcType],
    tcNewtypeEvidence :: !(Maybe EvTerm),
    tcNewtypeDictionaryCast :: !(Maybe Coercion),
    tcNewtypeMethods :: ![TcNewtypeMethod]
  }
  deriving (Eq, Show)

-- | A method cast applies after its type and dictionary arguments.
data TcNewtypeMethod = TcNewtypeMethod
  { tcNewtypeMethodName :: !Text,
    tcNewtypeMethodTyVars :: ![TyVarId],
    tcNewtypeMethodPredicates :: ![Pred],
    tcNewtypeMethodCoercion :: !Coercion
  }
  deriving (Eq, Show)

data TcInstanceAnnotation = TcInstanceAnnotation
  { tcInstanceDictName :: !Text,
    tcInstanceDictType :: !TcType,
    tcInstanceClassTyCon :: !TyCon,
    tcInstanceTyVars :: ![TyVarId],
    tcInstanceHeadTypes :: ![TcType],
    tcInstanceClassTyVars :: ![TyVarId],
    tcInstanceClassOrigin :: !(Maybe (Text, Text)),
    tcInstanceClassSuperClasses :: ![TcDictBinderAnnotation],
    tcInstanceClassMethods :: ![TcClassMethodAnnotation],
    tcInstanceContextDicts :: ![TcDictBinderAnnotation],
    tcInstanceSuperClasses :: ![(TcDictBinderAnnotation, EvTerm)],
    tcInstanceMethodOrder :: ![Text],
    tcInstanceDefaultMethods :: ![Text],
    -- | For each default method whose class gives it a default signature,
    -- evidence for the constraints of that signature at the instance head,
    -- in signature order. The default-method worker takes them after the
    -- instance dictionary itself.
    tcInstanceDefaultMethodEvidence :: ![(Text, [EvTerm])],
    -- | The checked associated type family equations of the instance,
    -- explicit ones and instantiated class defaults.
    tcInstanceAssociatedTypes :: ![TypeFamilyInstanceInfo],
    tcInstanceNewtype :: !(Maybe TcNewtypeInstance)
  }
  deriving (Eq, Show)

data TcInstanceMethodAnnotation = TcInstanceMethodAnnotation
  { tcInstanceMethodName :: !Text,
    tcInstanceMethodType :: !TcType
  }
  deriving (Eq, Show)

-- | Extract a 'TcAnnotation' from an 'Expr'.
pattern ETcAnn :: TcAnnotation -> Expr -> Expr
pattern ETcAnn ann inner <- EAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Decl'.
pattern DTcAnn :: TcAnnotation -> Decl -> Decl
pattern DTcAnn ann inner <- DeclAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Pattern'.
pattern PTcAnn :: TcAnnotation -> Pattern -> Pattern
pattern PTcAnn ann inner <- PAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Type'.
pattern TTcAnn :: TcAnnotation -> Type -> Type
pattern TTcAnn ann inner <- TAnn (fromAnnotation -> Just ann) inner

-- | Wrap an expression with a type annotation.
annotateExpr :: TcAnnotation -> Expr -> Expr
annotateExpr ann = EAnn (mkAnnotation ann)

-- | Wrap a declaration with a type annotation.
annotateDecl :: TcAnnotation -> Decl -> Decl
annotateDecl ann = DeclAnn (mkAnnotation ann)

pendingAnnotation :: TcType -> [TcType] -> [EvVar] -> [TcType] -> PendingTcAnnotation
pendingAnnotation ty typeArgs evidenceVars =
  PendingTcAnnotation ty [] typeArgs evidenceVars []

pendingTypeLambdaAnnotation :: TcType -> [TyVarId] -> [EvVar] -> PendingTcAnnotation
pendingTypeLambdaAnnotation ty binders evidenceBinders =
  PendingTcAnnotation ty binders [] [] evidenceBinders []

-- | Render a binder and its 'TcType' as a human-readable signature.
renderTcSignature :: Text -> TcType -> String
renderTcSignature name ty = T.unpack name ++ " ∷ " ++ renderTcType ty

-- | Render a class or equality predicate as source-like text.
renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred classTyCon args ->
      renderTcType (TcTyCon classTyCon args)
    EqPred left right ->
      renderTcType left ++ " ~ " ++ renderTcType right
    IParamPred name payload ->
      T.unpack name ++ " ∷ " ++ renderTcType payload
    QuantifiedPred variables antecedents consequent ->
      "∀ "
        ++ unwords (map (T.unpack . tvName) variables)
        ++ ". "
        ++ (if null antecedents then "" else "(" ++ commaSep (map renderPred antecedents) ++ ") ⇒ ")
        ++ renderPred consequent
  where
    commaSep = T.unpack . T.intercalate (T.pack ", ") . map T.pack

-- | Render a 'TcType' as a human-readable string.
--
-- Uses a precedence level to decide when to insert parentheses:
--   0 = no parens needed (top level or right of ->)
--   1 = parens needed for function types (left of ->)
--   2 = parens needed for function types and type applications (inside type con args)
renderTcType :: TcType -> String
renderTcType = renderTcTypeInModule Nothing

renderTcTypeInModule :: Maybe Text -> TcType -> String
renderTcTypeInModule currentModule = go 0
  where
    go :: Int -> TcType -> String
    go _ (TcTyVar tv) = T.unpack (tvName tv)
    go _ (TcMetaTv (Unique u)) = "?" ++ show u
    go _ KType = "Type"
    go _ (TcTyCon (TyCon name 1) [arg])
      | name == T.pack "[]" = "[" ++ go 0 arg ++ "]"
    go _ (TcTyCon tc@(TyCon name arity) args)
      | tyConNamespace tc == ResolutionNamespaceType,
        isBoxedTupleCon name arity,
        arity == length args =
          "(" ++ commaSep (map (go 0) args) ++ ")"
    go _ (TcTyCon tc []) = T.unpack (renderTyConName tc)
    go p (TcTyCon tc args) =
      parenIf (p >= 2) $
        unwords (T.unpack (renderTyConName tc) : map (go 2) args)
    go p (TcFunTy a b) =
      parenIf (p >= 1) $
        go 1 a ++ " → " ++ go 0 b
    go p (TcForAllTy tv body) =
      let (tvs, inner) = collectForAlls body
       in parenIf (p >= 1) $
            "∀ " ++ unwords (map (T.unpack . tvName) (tv : tvs)) ++ ". " ++ go 0 inner
    go p (TcQualTy preds body) =
      parenIf (p >= 1) $
        "(" ++ commaSep (map showPred preds) ++ ") ⇒ " ++ go 0 body
    go p (TcAppTy f a) =
      parenIf (p >= 2) $
        go 1 f ++ " " ++ go 2 a
    showPred (ClassPred cls args) =
      T.unpack (renderTyConName cls) ++ " " ++ unwords (map (go 2) args)
    showPred (EqPred t1 t2) =
      go 2 t1 ++ " ~ " ++ go 2 t2
    showPred (IParamPred name payload) =
      T.unpack name ++ " ∷ " ++ go 0 payload
    showPred predicate@QuantifiedPred {} = renderPred predicate

    parenIf False s = s
    parenIf True s = "(" ++ s ++ ")"

    commaSep = T.unpack . T.intercalate (T.pack ", ") . map T.pack

    isBoxedTupleCon name arity =
      arity /= 1 && name == boxedTupleTyConName arity

    renderTyConName tyCon =
      namespacePrefix <> qualifiedName
      where
        definingModule = tyConModuleName tyCon
        namespacePrefix
          | tyConNamespace tyCon == ResolutionNamespaceTerm = T.pack "'"
          | otherwise = T.empty
        qualifiedName
          | Nothing <- currentModule = tyConName tyCon
          | Just definingModule == currentModule = tyConName tyCon
          | otherwise = definingModule <> T.pack "." <> tyConName tyCon

-- | Collect nested forall binders into a list.
collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)
