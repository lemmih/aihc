{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Constraint generation for declarations.
--
-- Processes top-level data declarations and value bindings from a module.
module Aihc.Tc.Generate.Decl
  ( tcModule,
    tcModuleScc,
    moduleBindings,
    moduleInstances,
    moduleClasses,
    defaultMethodName,
    TcBindingResult (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BangType (..),
    BinderHead (..),
    BuiltinCon (..),
    CallConv (..),
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    DataFamilyDecl (..),
    DataFamilyInst (..),
    Decl (..),
    ExportSpec (..),
    Expr (..),
    Extension (..),
    FieldDecl (..),
    ForallTelescope (..),
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    GadtBody (..),
    IEBundledMember (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Literal (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    PatSynArgs (..),
    PatSynDecl (..),
    PatSynDir (..),
    Pattern (..),
    Pragma (..),
    PragmaType (..),
    PragmaUnpackKind (..),
    RecordField (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor (..),
    TyVarBinder,
    Type (..),
    TypeFamilyDecl (..),
    TypeFamilyEq (..),
    TypeFamilyInst (..),
    TypeFamilyResultSig (..),
    TypeSynDecl (..),
    UnqualifiedName (..),
    ValueDecl (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    binderHeadName,
    binderHeadParams,
    fromAnnotation,
    gadtBodyResultType,
    instanceHeadName,
    instanceHeadTypes,
    mkAnnotation,
    mkUnqualifiedName,
    moduleExports,
    moduleName,
    nameQualifier,
    nameText,
    peelClassDeclItemAnn,
    peelDeclAnn,
    peelInstanceDeclItemAnn,
    peelTypeHead,
    qualifyName,
    tyVarBinderKind,
    tyVarBinderName,
    unqualifiedNameAnns,
  )
import Aihc.Resolve (Identifier (..), PackageId (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDerivingPlan (..),
    TcDictBinderAnnotation (..),
    TcForeignAbiType (..),
    TcForeignEffect (..),
    TcForeignImportAnnotation (..),
    TcForeignImportInfo (..),
    TcForeignMarshal (..),
    TcForeignSafety (..),
    TcForeignTarget (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    TcNewtypeDeriving (..),
    TcPatSynAnnotation (..),
    annotateDecl,
    annotateRhsCast,
    renderTcType,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Deriving (annotateAttachedDerivingTc, annotateStandaloneDerivingTc)
import Aihc.Tc.Deriving.Context (inferDerivingContexts, typeTyVars)
import Aihc.Tc.Deriving.Generate (generateDerivedInstances)
import Aihc.Tc.Deriving.Newtype (checkNewtypeInstance)
import Aihc.Tc.Env (AssociatedTypeInfo (..), ClassInfo (..), DataConFieldInfo (..), DataConFieldUnpack (..), DataConInfo (..), DataConSourceForm (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), InstanceInfo (..), PatSynDirection (..), PatSynInfo (..), TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..), TypeSynonymInfo (..), dataConArgTypes, dataFamilyAxiomName, dataFamilyRepresentationName, instanceEnvFromList, instanceEnvList, instanceInfoKey, typeFamilyAxiomKey, typeFamilyAxiomName)
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Finalize (finalizeModuleTc)
import Aihc.Tc.Generalize (collectMetaVars, environmentMetaVars, generalizeAndCommit, generalizeAndCommitIgnoring, predMetaVars)
import Aihc.Tc.Generate.Bind (freeVarsDecl, freeVarsMatch, inferRhsWithLocals)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Generate.PatternBranch (solvePatternBranch)
import Aihc.Tc.Kind (ParamInfo (..), TvKindEnv, checkRuntimeType, checkSurfaceType, classPredicateArgKinds, convertSurfaceTypeWithKinds, defaultKindMetas, explicitForallNames, freeTypeVars, freshKindMeta, makeParamEnv, makeParamEnvWith, scopedSigTyVars, sigToScheme, standaloneKindSigToScheme, surfacePredToPred, surfaceTypeSpan, takeVisibleArgumentKinds, tcTypeKind, tyConKindFromParams, tyConKindFromParamsWith, unifyKinds, unifyKindsAt, zonkKind)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (SolveResult (..), solveConstraints, solveWithImpls)
import Aihc.Tc.Solve.Defaulting (defaultAmbiguousMetas)
import Aihc.Tc.Solve.Dict (DictResult (..), isCallStackPred, reportUnsolvedDict, solveDictWithGivens)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality, solveGivenEquality)
import Aihc.Tc.Solve.InertSet (InertSet (..))
import Aihc.Tc.Types
import Aihc.Tc.Zonk (defaultPredKinds, defaultTyConKindScheme, defaultTyVarKinds, defaultTypeKinds, defaultTypeSchemeKinds, zonkType)
import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, forM, forM_, replicateM, unless, when, zipWithM, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, modify')
import Data.Char (isAlpha, isAlphaNum, ord)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (elemIndex, find, mapAccumL, nub, nubBy, partition, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Merge concrete source spans embedded in a list of annotations.
sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    s : _ -> s

peelDeclSpan :: SourceSpan -> Decl -> SourceSpan
peelDeclSpan ambient (DeclAnn ann inner) =
  peelDeclSpan (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
peelDeclSpan ambient _ = ambient

-- | Result of type-checking a single binding.
data TcBindingResult = TcBindingResult
  { -- | Canonical binder identity. Symbolic binders are stored without
    -- prefix-position parentheses, e.g. @++@ rather than @(++)@.
    tbName :: !Text,
    -- | Human-facing rendering for diagnostics and golden output.
    tbDisplayName :: !Text,
    tbType :: !TcType
  }
  deriving (Show, Read)

data UserSig = UserSig
  { userSigName :: !Text,
    userSigType :: !Type,
    userSigSpan :: !SourceSpan
  }
  deriving (Show)

data CheckedSig = CheckedSig
  { checkedSigName :: !Text,
    checkedSigScheme :: !TypeScheme,
    checkedSigSpan :: !SourceSpan,
    -- | The names of the explicit @forall@ variables. They scope over the
    -- binding.
    checkedSigScopedNames :: ![Text]
  }
  deriving (Show)

moduleBindings :: Module -> [TcBindingResult]
moduleBindings modu =
  concatMap (declBindings (resolvedModuleOrigin modu)) (moduleDecls modu)

-- | Recover instance-environment entries from finalized module annotations.
moduleInstances :: Module -> [InstanceInfo]
moduleInstances modu =
  concatMap (declInstances (resolvedModuleOrigin modu)) (moduleDecls modu)

resolvedModuleOrigin :: Module -> (Text, Text)
resolvedModuleOrigin resolvedModule =
  fromMaybe ("", fromMaybe "Main" (moduleName resolvedModule)) $ do
    resolved <- listToMaybe (mapMaybe definitionResolution (moduleDecls resolvedModule))
    case resolutionTarget resolved of
      ResolvedTopLevel packageId name ->
        pure (packageIdText packageId, fromMaybe (fromMaybe "Main" (moduleName resolvedModule)) (nameQualifier name))
      _ -> Nothing

definitionResolution :: Decl -> Maybe ResolutionAnnotation
definitionResolution declaration =
  case peelDeclAnn declaration of
    DeclValue (FunctionBind name _) -> nameResolution name
    DeclValue (PatternBind _ pattern' _) -> patternResolution pattern'
    DeclData dataDeclaration -> nameResolution (binderHeadName (dataDeclHead dataDeclaration))
    DeclNewtype newtypeDeclaration -> nameResolution (binderHeadName (newtypeDeclHead newtypeDeclaration))
    DeclClass classDeclaration -> nameResolution (binderHeadName (classDeclHead classDeclaration))
    _ -> Nothing

patternResolution :: Pattern -> Maybe ResolutionAnnotation
patternResolution pattern' =
  case pattern' of
    PVar name -> nameResolution name
    PAnn _ inner -> patternResolution inner
    PParen inner -> patternResolution inner
    PStrict inner -> patternResolution inner
    PIrrefutable inner -> patternResolution inner
    PAs name _ -> nameResolution name
    PTypeSig inner _ -> patternResolution inner
    _ -> Nothing

nameResolution :: UnqualifiedName -> Maybe ResolutionAnnotation
nameResolution = listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns

-- | Recover class-environment entries from finalized module annotations.
moduleClasses :: Module -> [ClassInfo]
moduleClasses modu = map setOrigin (concatMap declClasses (moduleDecls modu))
  where
    setOrigin classInfo = classInfo {ciOrigin = Just (resolvedModuleOrigin modu)}

declClasses :: Decl -> [ClassInfo]
declClasses decl =
  case decl of
    DeclAnn ann inner ->
      annotationClasses ann inner <> declClasses inner
    _ -> []

annotationClasses :: Annotation -> Decl -> [ClassInfo]
annotationClasses ann decl =
  case (fromAnnotation ann, peelDeclAnn decl) of
    (Just classAnn, DeclClass classDecl) ->
      [ ClassInfo
          { ciName = unqualifiedNameText (binderHeadName (classDeclHead classDecl)),
            ciTyCon = tcClassTyCon classAnn,
            ciOrigin = Nothing,
            ciKindTyVars = tcClassKindTyVars classAnn,
            ciTyVars = tcClassTyVars classAnn,
            ciSuperClassTypes = map tcDictBinderType (tcClassSuperClasses classAnn),
            ciMethods =
              [ (tcClassMethodName method, typeToScheme (tcClassMethodType method))
              | method <- tcClassMethods classAnn
              ],
            ciDefaultMethods = tcClassDefaultMethods classAnn,
            ciDefaultSignatures =
              [ (methodName, typeToScheme signature)
              | (methodName, signature) <- tcClassDefaultSignatures classAnn
              ],
            ciAssociatedTypes = tcClassAssociatedTypes classAnn
          }
      ]
    _ -> []

typeToScheme :: TcType -> TypeScheme
typeToScheme ty =
  let (tyVars, body) = peelForAlls ty
   in case body of
        TcQualTy predicates result -> ForAll tyVars predicates result
        result -> ForAll tyVars [] result

declInstances :: (Text, Text) -> Decl -> [InstanceInfo]
declInstances origin decl =
  case decl of
    DeclAnn ann inner ->
      annotationInstances origin ann inner <> declInstances origin inner
    _ -> []

annotationInstances :: (Text, Text) -> Annotation -> Decl -> [InstanceInfo]
annotationInstances origin ann decl =
  explicitInstance
  where
    explicitInstance =
      case (fromAnnotation @TcInstanceAnnotation ann, peelDeclAnn decl) of
        (Just instAnn, DeclInstance instanceDecl)
          | Just className <- instanceHeadName (instanceDeclHead instanceDecl) ->
              [ InstanceInfo
                  { iiClassName = nameText className,
                    iiDictName = tcInstanceDictName instAnn,
                    iiDictOrigin = origin,
                    iiDictType = tcInstanceDictType instAnn,
                    iiTyVars = tcInstanceTyVars instAnn,
                    iiContext = map dictBinderPred (tcInstanceContextDicts instAnn),
                    iiHead = tcInstanceHeadTypes instAnn
                  }
              ]
        _ -> []

dictBinderPred :: TcDictBinderAnnotation -> Pred
dictBinderPred dictBinder =
  case constraintTypeToPred (tcDictBinderType dictBinder) of
    Just predicate -> predicate
    Nothing -> error "invalid checked dictionary binder type"

declBindings :: (Text, Text) -> Decl -> [TcBindingResult]
declBindings origin decl =
  case decl of
    DeclAnn ann inner ->
      annotationBindings ann inner <> declBindings origin inner
    DeclData dataDecl ->
      concatMap dataConBindings (dataDeclConstructors dataDecl)
        <> concatMap recordSelectorBindings (dataDeclConstructors dataDecl)
    DeclNewtype newtypeDecl ->
      maybe [] (\constructor -> dataConBindings constructor <> recordSelectorBindings constructor) (newtypeDeclConstructor newtypeDecl)
    DeclDataFamilyInst familyInst ->
      concatMap dataConBindings (dataFamilyInstConstructors familyInst)
    _ -> []

annotationBindings :: Annotation -> Decl -> [TcBindingResult]
annotationBindings ann decl =
  tcAnnotationBindings ann decl
    <> classAnnotationBindings ann decl
    <> instanceAnnotationBindings ann

tcAnnotationBindings :: Annotation -> Decl -> [TcBindingResult]
tcAnnotationBindings ann decl =
  case fromAnnotation ann of
    Nothing -> []
    Just tcAnn ->
      case decl of
        DeclValue valueDecl ->
          [ TcBindingResult name displayName (tcAnnType tcAnn)
          | (name, displayName) <- valueDeclBindingNames valueDecl
          ]
        DeclData dataDecl ->
          let name = unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))
           in [TcBindingResult name name (tcAnnType tcAnn)]
        DeclNewtype newtypeDecl ->
          let name = unqualifiedNameText (binderHeadName (newtypeDeclHead newtypeDecl))
           in [TcBindingResult name name (tcAnnType tcAnn)]
        DeclDataFamilyDecl familyDecl ->
          let name = unqualifiedNameText (binderHeadName (dataFamilyDeclHead familyDecl))
           in [TcBindingResult name name (tcAnnType tcAnn)]
        DeclForeign foreignDecl ->
          let name = unqualifiedNameText (foreignName foreignDecl)
              displayName = renderBinderName (foreignName foreignDecl)
           in [TcBindingResult name displayName (tcAnnType tcAnn)]
        _ -> []

classAnnotationBindings :: Annotation -> Decl -> [TcBindingResult]
classAnnotationBindings ann decl =
  case (fromAnnotation ann, decl) of
    (Just classAnn, DeclClass {}) ->
      [ TcBindingResult (tcClassMethodName method) (tcClassMethodName method) (tcClassMethodType method)
      | method <- tcClassMethods classAnn
      ]
        <> [ TcBindingResult (defaultMethodName (tcClassMethodName method)) (defaultMethodName (tcClassMethodName method)) (classDefaultWorkerType classAnn method)
           | method <- tcClassMethods classAnn,
             tcClassMethodName method `elem` tcClassDefaultMethods classAnn
           ]
    _ -> []

classDefaultWorkerType :: TcClassAnnotation -> TcClassMethodAnnotation -> TcType
classDefaultWorkerType classAnnotation method =
  case lookup (tcClassMethodName method) (tcClassDefaultSignatures classAnnotation) of
    Just signature
      | Just classPredicate <- constraintTypeToPred (tcClassMethodDictType method) ->
          let (tyVars, body) = peelForAlls signature
              qualifiedBody =
                case body of
                  TcQualTy predicates result -> TcQualTy (classPredicate : predicates) result
                  result -> TcQualTy [classPredicate] result
           in foldr TcForAllTy qualifiedBody tyVars
    _ -> tcClassMethodType method

instanceAnnotationBindings :: Annotation -> [TcBindingResult]
instanceAnnotationBindings ann =
  case fromAnnotation ann of
    Just instAnn ->
      [TcBindingResult (tcInstanceDictName instAnn) (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)]
    Nothing -> []

dataConBindings :: DataConDecl -> [TcBindingResult]
dataConBindings dataConDecl =
  case dataConDecl of
    DataConAnn ann inner ->
      case fromAnnotation ann of
        Just tcAnn ->
          [ TcBindingResult name displayName (tcAnnType tcAnn)
          | (name, displayName) <- dataConBindingNames inner
          ]
        Nothing -> dataConBindings inner
    _ -> []

recordSelectorBindings :: DataConDecl -> [TcBindingResult]
recordSelectorBindings declaration =
  case declaration of
    DataConAnn ann inner ->
      case fromAnnotation ann of
        Just tcAnn -> selectorBindingsFromConstructorType (tcAnnType tcAnn) inner
        Nothing -> recordSelectorBindings inner
    _ -> []
  where
    selectorBindingsFromConstructorType constructorType inner =
      let (typeVariables, qualifiedConstructor) = peelForAlls constructorType
          body =
            case qualifiedConstructor of
              TcQualTy _ result -> result
              result -> result
          (fieldTypes, resultType) = splitFunctionType body
          (_, sourceFields, _) = dataConSourceLayout inner
          -- The constructor context and the existential variables do not
          -- reach the selector type. A field whose type mentions an
          -- existential variable has no selector. A kind variable of a
          -- universal variable, such as the runtime representation in
          -- @TExp (a :: TYPE r)@, is universal as well.
          universals = filter (`elem` closeOverKinds (typeTyVars resultType)) typeVariables
          closeOverKinds variables =
            let kindVariables = [variable | variable <- typeVariables, variable `notElem` variables, any (\universal -> variable `elem` typeTyVars (tvKind universal)) variables]
             in if null kindVariables then variables else closeOverKinds (variables <> kindVariables)
       in [ TcBindingResult label label (foldr TcForAllTy (TcFunTy resultType fieldType) universals)
          | ((maybeLabel, _), fieldType) <- zip sourceFields fieldTypes,
            all (`elem` universals) (typeTyVars fieldType),
            Just label <- [maybeLabel]
          ]

valueDeclBindingNames :: ValueDecl -> [(Text, Text)]
valueDeclBindingNames valueDecl =
  case valueDecl of
    FunctionBind binder _ -> [binderBindingName binder]
    PatternBind _ pat _ -> patternBindingNames pat

patternBindingNames :: Pattern -> [(Text, Text)]
patternBindingNames = map binderBindingName . patternBinderNames

dataConBindingNames :: DataConDecl -> [(Text, Text)]
dataConBindingNames dataConDecl =
  case dataConDecl of
    DataConAnn _ inner -> dataConBindingNames inner
    PrefixCon _ _ name _ -> [dataConBindingName name]
    InfixCon _ _ _ name _ -> [dataConBindingName name]
    RecordCon _ _ name _ -> [dataConBindingName name]
    GadtCon _ _ names _ -> map dataConBindingName names
    TupleCon _ _ flavor fields ->
      let name = tupleConText flavor (length fields)
       in [(name, name)]
    UnboxedSumCon _ _ pos arity _ ->
      let name = unboxedSumConText pos arity
       in [(name, name)]
    ListCon {} -> [("[]", "[]")]

binderBindingName :: UnqualifiedName -> (Text, Text)
binderBindingName name =
  (unqualifiedNameText name, renderBinderName name)

dataConBindingName :: UnqualifiedName -> (Text, Text)
dataConBindingName name =
  let raw = unqualifiedNameText name
   in (raw, raw)

-- | Type-check a module, returning the same syntax tree annotated with the
-- inferred interface. Call 'moduleBindings' when a flat compatibility view is
-- needed by older callers.
tcModule :: Module -> TcM Module
tcModule m = do
  modules <- tcModuleScc [m]
  case modules of
    [result] -> pure result
    _ -> pure m

-- | Type-check one strongly connected module component. Data declarations and
-- explicit signatures are registered for the whole component before any
-- value body is checked, allowing a module to refer back to a signed binding
-- in another member of the same import cycle.
tcModuleScc :: [Module] -> TcM [Module]
tcModuleScc modules = do
  initialKeys <- globalStateKeys <$> lift get
  -- Phase 1: register type constructor headers before expanding synonym
  -- bodies, then register value-level declarations against those expanded
  -- types. This permits forward references from synonyms to data types while
  -- making aliases available in constructor fields and class methods.
  let declarations = concatMap moduleDecls modules
      standaloneKindSignatures = collectStandaloneKindSignatures declarations
      polyKindOrigins = [resolvedModuleOrigin modu | modu <- modules, PolyKinds `elem` moduleEnabledExtensions modu]
  mapM_ predeclareTypeConstructor declarations
  mapM_ predeclareTypeLevelDataConstructors declarations
  standaloneKindSchemes <- traverse standaloneKindSigToScheme standaloneKindSignatures
  mapM_ (registerTypeDeclHeader standaloneKindSchemes) declarations
  mapM_ registerTypeSynonymBody declarations
  mapM_ checkTypeSynonymBody declarations
  let structuralDeclarations =
        [ (resolvedModuleOrigin modu, declaration)
        | modu <- modules,
          declaration <- moduleDecls modu
        ]
  mapM_ (uncurry registerStructuralDecl) (filter (not . isInstanceDecl . snd) structuralDeclarations)
  -- Generalize data kinds before instances use them.
  generalizeDataKinds polyKindOrigins initialKeys
  mapM_ (uncurry registerStructuralDecl) (filter (isInstanceDecl . snd) structuralDeclarations)
  -- Deriving strategy and context inference depends only on registered type,
  -- class, and explicit-instance information. Finalize the entire SCC as one
  -- batch before checking signatures and bodies so sibling derived instances
  -- are mutually visible and ordinary values can use them.
  defaultGlobalKindMetas initialKeys
  structuralKeys <- globalStateKeys <$> lift get
  derivingAnnotated <- mapM annotateModuleDerivingTc modules
  derivingInferred <- inferDerivingContexts derivingAnnotated
  derivingFinalized <- mapM registerDerivedInstances derivingInferred
  -- Phase 2: collect type signatures and convert them to schemes.
  rawSigs <- mapM (collectUserSigs . moduleDecls) derivingFinalized
  schemes <- mapM (traverse checkUserSig) rawSigs
  mapM_ (uncurry registerCheckedSig) (concatMap Map.toList schemes)
  pending <- zipWithM tcModuleBody schemes derivingFinalized
  mapM_ checkBundledPatSyns derivingFinalized
  -- No module interface in the SCC may retain state-local kind metavariables.
  defaultGlobalKindMetas structuralKeys
  annotated <- mapM annotatePendingModule pending
  mapM finalizeModuleTc annotated

registerCheckedSig :: TcTermKey -> CheckedSig -> TcM ()
registerCheckedSig key sig = do
  extendTermEnvPermanent (checkedSigName sig) binder
  extendTermKeyEnvPermanent key binder
  where
    binder = TcIdBinder (flattenSchemeContexts (checkedSigScheme sig)) Closed

-- | Merge nested contexts into one context. A pattern synonym signature
-- @req => prov => body@ keeps the split in its checked signature, but its
-- binder has the constructor-like type with one context.
flattenSchemeContexts :: TypeScheme -> TypeScheme
flattenSchemeContexts (ForAll tyVars predicates body) =
  case body of
    TcQualTy more inner -> flattenSchemeContexts (ForAll tyVars (predicates <> more) inner)
    _ -> ForAll tyVars predicates body

data PendingModule = PendingModule
  { pendingSyntax :: !Module,
    pendingValueResults :: ![TcBindingResult]
  }

tcModuleBody :: Map TcTermKey CheckedSig -> Module -> TcM PendingModule
tcModuleBody schemes m = do
  declaredDefaults <- moduleDefaultTypes (moduleDecls m)
  localDefaultTypes declaredDefaults (tcModuleBodyWithDefaults schemes m)

-- | The candidate types of the module @default@ declaration.
--
-- A module without the declaration gives 'Nothing', and defaulting then uses
-- the Haskell 2010 standard list. @default ()@ gives @Just []@ and turns
-- defaulting off. A later declaration replaces an earlier one, as GHC
-- permits only one for each module.
moduleDefaultTypes :: [Decl] -> TcM (Maybe [TcType])
moduleDefaultTypes decls =
  case [tys | decl <- decls, DeclDefault tys <- [peelDeclAnn decl]] of
    [] -> pure Nothing
    groups -> Just <$> mapM checkDefaultType (last groups)
  where
    checkDefaultType ty = checkSurfaceType Map.empty ty typeKindType

tcModuleBodyWithDefaults :: Map TcTermKey CheckedSig -> Module -> TcM PendingModule
tcModuleBodyWithDefaults schemes m = do
  -- Phase 3: group and type-check value bindings using signatures.
  let sourceGroups = zip [0 :: Int ..] (groupValueDecls (moduleDecls m))
  grouped <- sortDeclGroups sourceGroups
  groupResults <- mapM (tcDeclGroup schemes) grouped
  let valueResults = concatMap tcGroupBindingResults groupResults
      checkedGroups =
        Map.fromList
          [ (tcGroupId result, decls)
          | result <- groupResults,
            Just decls <- [tcGroupAnnotatedDecls result]
          ]
      valueAnnotatedModule =
        m {moduleDecls = concatMap (renderCheckedGroup checkedGroups) sourceGroups}
  -- Phase 4: type-check instance method bodies. They are not top-level
  -- value bindings, but their occurrences still need the same instantiation
  -- and evidence records as ordinary expressions.
  classDecls <- mapM tcClassDeclBodies (moduleDecls valueAnnotatedModule)
  instanceHeaders <- mapM (annotateInstanceHeaderTc (resolvedModuleOrigin m)) classDecls
  instanceDecls <- mapM tcInstanceDeclBodies instanceHeaders
  let pendingModule = valueAnnotatedModule {moduleDecls = instanceDecls}
  -- Phase 5: reject source top-level values whose finalized types are
  -- unlifted. Generated declarations without source spans are permitted so
  -- downstream passes can introduce internal unlifted bindings.
  checkTopLevelUnliftedBindings sourceGroups groupResults
  pure (PendingModule pendingModule valueResults)

annotatePendingModule :: PendingModule -> TcM Module
annotatePendingModule pending = do
  -- Only bindings that checked without errors are eligible for value
  -- annotations. Failed bindings remain in the recovery environment, but
  -- they must not be rendered as successful inferred types.
  annotateModuleTc (Map.fromList [(tbName result, tbType result) | result <- pendingValueResults pending]) (pendingSyntax pending)

data GlobalStateKeys = GlobalStateKeys
  { globalTermKeys :: !(Set.Set TcTermKey),
    globalTyConKeys :: !(Set.Set TcTypeKey),
    globalDataTypeKeys :: !(Set.Set TcTypeKey),
    globalClassKeys :: !(Set.Set TcTypeKey),
    globalInstanceKeys :: !(Set.Set ((Text, Text), Text)),
    globalDataFamilyInstanceKeys :: !(Set.Set TcAxiomKey),
    globalTypeFamilyInstanceKeys :: !(Set.Set TcAxiomKey),
    globalPatSynKeys :: !(Set.Set TcTermKey)
  }

globalStateKeys :: TcState -> GlobalStateKeys
globalStateKeys state =
  GlobalStateKeys
    { globalTermKeys = Map.keysSet (tcsGlobalTerms state),
      globalTyConKeys = Map.keysSet (tcsGlobalTyCons state),
      globalDataTypeKeys = Map.keysSet (tcsDataTypes state),
      globalClassKeys = Map.keysSet (tcsClasses state),
      globalInstanceKeys = Set.fromList (map instanceInfoKey (instanceEnvList (tcsInstances state))),
      globalDataFamilyInstanceKeys = Map.keysSet (tcsDataFamilyInstances state),
      globalTypeFamilyInstanceKeys = Map.keysSet (tcsTypeFamilyInstances state),
      globalPatSynKeys = Map.keysSet (tcsPatSyns state)
    }

-- | Generalize each data kind in its own module extension scope.
generalizeDataKinds :: [(Text, Text)] -> GlobalStateKeys -> TcM ()
generalizeDataKinds polyKindOrigins initialKeys = do
  state <- lift get
  let newDataTypes = Map.filterWithKey (\(_, _, namespace, _) info -> namespace == ResolutionNamespaceType && tciFlavor info == DataTyCon && (packageIdText (tyConPackageId (tciTyCon info)), tyConModuleName (tciTyCon info)) `elem` polyKindOrigins) (Map.withoutKeys (tcsGlobalTyCons state) (globalTyConKeys initialKeys))
  generalized <- traverse generalizeDataKindInfo newDataTypes
  lift $ modify' (\current -> current {tcsGlobalTyCons = generalized `Map.union` tcsGlobalTyCons current})

generalizeDataKindInfo :: TyConInfo -> TcM TyConInfo
generalizeDataKindInfo info = do
  scheme <- generalizeDataKind (tciKindScheme info)
  pure info {tciKindScheme = scheme}

generalizeDataKind :: TypeScheme -> TcM TypeScheme
generalizeDataKind (ForAll variables predicates body) = do
  kind <- zonkKind body
  let metas = nub (collectMetaVars kind)
  forM_ (zip [0 :: Int ..] metas) $ \(index, meta) -> do
    variable <- freshSkolemTv ("k" <> T.pack (show index))
    writeMetaTv meta (TcTyVar variable)
  kind' <- zonkKind kind
  pure (ForAll (uniqueKindVariables (variables <> freeKindVariables kind')) predicates kind')

closeKindVariables :: [TyVarId] -> [TyVarId]
closeKindVariables variables =
  uniqueKindVariables (concatMap (freeKindVariables . tvKind) variables <> variables)

uniqueKindVariables :: [TyVarId] -> [TyVarId]
uniqueKindVariables = nubBy (\left right -> tvUnique left == tvUnique right)

freeKindVariables :: TcType -> [TyVarId]
freeKindVariables ty = case ty of
  TcTyVar variable -> freeKindVariables (tvKind variable) <> [variable]
  TcMetaTv {} -> []
  TcTyCon _ arguments -> concatMap freeKindVariables arguments
  TcFunTy argument result -> freeKindVariables argument <> freeKindVariables result
  TcAppTy function argument -> freeKindVariables function <> freeKindVariables argument
  TcForAllTy variable body -> filter (/= variable) (freeKindVariables body)
  TcQualTy _ body -> freeKindVariables body

defaultGlobalKindMetas :: GlobalStateKeys -> TcM ()
defaultGlobalKindMetas initialKeys = do
  state <- lift get
  tyCons <- traverseNewMap globalTyConKeys defaultTyConInfoKinds (tcsGlobalTyCons state)
  terms <- traverseNewMap globalTermKeys defaultBinderKinds (tcsGlobalTerms state)
  dataTypes <- traverseNewMap globalDataTypeKeys defaultDataTypeKinds (tcsDataTypes state)
  classes <- traverseNewMap globalClassKeys defaultClassKinds (tcsClasses state)
  instances <- instanceEnvFromList <$> mapM (traverseNewList globalInstanceKeys instanceInfoKey defaultInstanceKinds) (instanceEnvList (tcsInstances state))
  dataFamilyInstances <- traverseNewMap globalDataFamilyInstanceKeys defaultDataFamilyInstanceKinds (tcsDataFamilyInstances state)
  typeFamilyInstances <- traverseNewMap globalTypeFamilyInstanceKeys defaultTypeFamilyInstanceKinds (tcsTypeFamilyInstances state)
  patSyns <- traverseNewMap globalPatSynKeys defaultPatSynKinds (tcsPatSyns state)
  lift $
    modify' $ \current ->
      current
        { tcsGlobalTerms = terms,
          tcsPatSyns = patSyns,
          tcsGlobalTyCons = tyCons,
          tcsDataTypes = dataTypes,
          tcsClasses = classes,
          tcsInstances = instances,
          tcsDataFamilyInstances = dataFamilyInstances,
          tcsTypeFamilyInstances = typeFamilyInstances
        }
  where
    -- Only the entries that this component added need defaulting. Restrict
    -- the walk to them before the traversal.
    traverseNewMap selectKeys transform current = do
      let fresh = Map.withoutKeys current (selectKeys initialKeys)
      defaulted <- traverse transform fresh
      pure (Map.union defaulted current)
    traverseNewList selectKeys key transform value
      | key value `Set.member` selectKeys initialKeys = pure value
      | otherwise = transform value
    defaultPatSynKinds info = do
      scheme <- defaultTypeSchemeKinds (psiScheme info)
      required <- mapM defaultPredKinds (psiReqTheta info)
      provided <- mapM defaultPredKinds (psiProvTheta info)
      pure info {psiScheme = scheme, psiReqTheta = required, psiProvTheta = provided}
    defaultBinderKinds binder =
      case binder of
        TcIdBinder scheme closedness -> do
          ForAll variables predicates body <- defaultTypeSchemeKinds scheme
          pure (TcIdBinder (ForAll (closeKindVariables variables) predicates body) closedness)
        TcMonoIdBinder ty -> TcMonoIdBinder <$> defaultTypeKinds ty
    defaultTyConInfoKinds info = do
      ForAll variables predicates body <- defaultTyConKindScheme (tciKindScheme info)
      let kindScheme = ForAll (closeKindVariables variables) predicates body
      synonym <- traverse defaultTypeSynonymKinds (tciTypeSynonym info)
      pure
        info
          { tciKindScheme = kindScheme,
            tciTypeSynonym = synonym
          }
    defaultTypeSynonymKinds synonym =
      TypeSynonymInfo
        <$> mapM defaultTyVarKinds (tsiParams synonym)
        <*> traverse defaultTypeKinds (tsiBody synonym)
    defaultDataTypeKinds info = do
      tyVars <- mapM defaultTyVarKinds (dtiTyVars info)
      resultKind <- defaultKindMetas (dtiResultKind info)
      constructors <- mapM defaultDataConKinds (dtiConstructors info)
      pure
        info
          { dtiTyVars = tyVars,
            dtiResultKind = resultKind,
            dtiConstructors = constructors
          }
    defaultDataConKinds info = do
      universalTyVars <- mapM defaultTyVarKinds (dciUnivTyVars info)
      existentialTyVars <- mapM defaultTyVarKinds (dciExTyVars info)
      predicates <- mapM defaultPredKinds (dciTheta info)
      fields <- mapM defaultDataConFieldKinds (dciFields info)
      resultType <- defaultTypeKinds (dciResTy info)
      pure
        info
          { dciUnivTyVars = closeKindVariables universalTyVars,
            dciExTyVars = filter (`notElem` closeKindVariables universalTyVars) (closeKindVariables existentialTyVars),
            dciTheta = predicates,
            dciFields = fields,
            dciResTy = resultType
          }
    defaultDataConFieldKinds field = do
      fieldType' <- defaultTypeKinds (dcfiType field)
      pure field {dcfiType = fieldType'}
    defaultClassKinds info = do
      kindTyVars <- mapM defaultTyVarKinds (ciKindTyVars info)
      tyVars <- mapM defaultTyVarKinds (ciTyVars info)
      superClassTypes <- mapM defaultTypeKinds (ciSuperClassTypes info)
      methods <- mapM (traverse defaultTypeSchemeKinds) (ciMethods info)
      defaultSignatures <- mapM (traverse defaultTypeSchemeKinds) (ciDefaultSignatures info)
      pure
        info
          { ciKindTyVars = kindTyVars,
            ciTyVars = tyVars,
            ciSuperClassTypes = superClassTypes,
            ciMethods = methods,
            ciDefaultSignatures = defaultSignatures
          }
    defaultInstanceKinds info =
      InstanceInfo
        (iiClassName info)
        (iiDictName info)
        (iiDictOrigin info)
        <$> defaultTypeKinds (iiDictType info)
        <*> mapM defaultTyVarKinds (iiTyVars info)
        <*> mapM defaultPredKinds (iiContext info)
        <*> mapM defaultTypeKinds (iiHead info)
    defaultDataFamilyInstanceKinds info = do
      familyType <- defaultTypeKinds (dfiiFamilyType info)
      tyVars <- mapM defaultTyVarKinds (dfiiTyVars info)
      pure
        info
          { dfiiFamilyType = familyType,
            dfiiTyVars = tyVars
          }
    defaultTypeFamilyInstanceKinds info = do
      tyVars <- mapM defaultTyVarKinds (tfiiTyVars info)
      left <- defaultTypeKinds (tfiiLeft info)
      right <- defaultTypeKinds (tfiiRight info)
      pure
        info
          { tfiiTyVars = tyVars,
            tfiiLeft = left,
            tfiiRight = right
          }

data TcDeclGroupResult = TcDeclGroupResult
  { tcGroupId :: !Int,
    tcGroupBindingResults :: ![TcBindingResult],
    tcGroupAnnotatedDecls :: !(Maybe [Decl])
  }

checkTopLevelUnliftedBindings :: [(Int, DeclGroup)] -> [TcDeclGroupResult] -> TcM ()
checkTopLevelUnliftedBindings sourceGroups results =
  forM_ results $ \result ->
    case Map.lookup (tcGroupId result) groupsById of
      Just group
        | sourceSpan <- declGroupSourceSpan group,
          sourceSpan /= NoSourceSpan ->
            forM_ (tcGroupBindingResults result) $ \binding -> do
              kind <- tcTypeKind (tbType binding)
              when (isUnliftedKind kind) $
                emitError sourceSpan (TopLevelUnliftedBinding (tbDisplayName binding) (tbType binding))
      _ -> pure ()
  where
    groupsById = Map.fromList sourceGroups
    isUnliftedKind kind =
      case runtimeRepFromKind kind of
        Right (BoxedRep Lifted) -> False
        Right _ -> True
        Left _ -> False

declGroupSourceSpan :: DeclGroup -> SourceSpan
declGroupSourceSpan group =
  case group of
    SingleDecl decl -> peelDeclSpan NoSourceSpan decl
    MergedFunctionBind sourceSpan _ _ _ -> sourceSpan

renderCheckedGroup :: Map Int [Decl] -> (Int, DeclGroup) -> [Decl]
renderCheckedGroup checkedGroups (groupId, group) =
  fromMaybe (renderDeclGroup group) (Map.lookup groupId checkedGroups)

annotateModuleTc :: Map Text TcType -> Module -> TcM Module
annotateModuleTc checkedValueTypes m = do
  let classMethods = collectClassMethodNames (moduleDecls m)
  decls <- mapM (annotateDeclTc (resolvedModuleOrigin m) classMethods checkedValueTypes) (moduleDecls m)
  pure (m {moduleDecls = decls})

annotateModuleDerivingTc :: Module -> TcM Module
annotateModuleDerivingTc modu = do
  declarations <- mapM (annotateDeclDerivingTc extensions) (moduleDecls modu)
  pure modu {moduleDecls = declarations}
  where
    extensions = moduleEnabledExtensions modu

-- | Append the instance declarations that the deriving plans of a module
-- generate, registered like source instances so that the signatures and
-- bodies checked afterwards can use them.
registerDerivedInstances :: Module -> TcM Module
registerDerivedInstances modu = do
  generated <- generateDerivedInstances origin modu
  mapM_ (registerStructuralDecl origin) generated
  pure modu {moduleDecls = moduleDecls modu <> generated}
  where
    origin = resolvedModuleOrigin modu

annotateDeclDerivingTc :: [Extension] -> Decl -> TcM Decl
annotateDeclDerivingTc extensions decl =
  case decl of
    DeclAnn annotation inner -> DeclAnn annotation <$> annotateDeclDerivingTc extensions inner
    DeclData dataDecl ->
      annotateAttachedDerivingTc extensions DataTyCon (dataDeclHead dataDecl) (dataDeclDeriving dataDecl) decl
    DeclNewtype newtypeDecl ->
      annotateAttachedDerivingTc extensions NewtypeTyCon (newtypeDeclHead newtypeDecl) (newtypeDeclDeriving newtypeDecl) decl
    DeclStandaloneDeriving derivingDecl -> annotateStandaloneDerivingTc extensions derivingDecl
    _ -> pure decl

moduleEnabledExtensions :: Module -> [Extension]
moduleEnabledExtensions modu =
  applyImpliedExtensions $
    foldr applyExtensionSetting [] (moduleLanguagePragmas modu)

annotateDeclTc :: (Text, Text) -> Map Text [Text] -> Map Text TcType -> Decl -> TcM Decl
annotateDeclTc origin classMethods checkedValueTypes decl =
  case decl of
    DeclAnn ann _
      | Just _ <- fromAnnotation @TcInstanceAnnotation ann -> pure decl
    DeclAnn ann inner -> DeclAnn ann <$> annotateDeclTc origin classMethods checkedValueTypes inner
    DeclValue valueDecl
      | valueDeclWasChecked checkedValueTypes valueDecl -> do
          (ty, valueDecl') <- annotateValueDeclTc checkedValueTypes valueDecl
          pure (annotateDeclAt (valueDeclSpan valueDecl) (TcAnnotation ty [] [] [] [] []) (DeclValue valueDecl'))
      | otherwise -> pure decl
    DeclData dataDecl -> annotateDataDeclTc dataDecl
    DeclNewtype newtypeDecl -> annotateNewtypeDeclTc newtypeDecl
    DeclTypeSyn typeSynDecl -> annotateTypeSynDeclTc typeSynDecl
    DeclDataFamilyDecl familyDecl -> annotateDataFamilyDeclTc familyDecl
    DeclDataFamilyInst familyInst -> annotateDataFamilyInstTc familyInst
    DeclTypeFamilyDecl familyDecl -> annotateTypeFamilyDeclTc familyDecl
    DeclTypeFamilyInst familyInst -> annotateTypeFamilyInstTc origin familyInst
    DeclForeign foreignDecl
      | isForeignImport foreignDecl -> annotateForeignDeclTc foreignDecl
    DeclClass classDecl -> annotateClassDeclTc classDecl
    DeclInstance instanceDecl -> annotateInstanceDeclTc origin instanceDecl
    DeclStandaloneDeriving {} -> pure decl
    DeclPatSyn patSynDecl
      | Just ty <- Map.lookup (unqualifiedNameText (patSynDeclName patSynDecl)) checkedValueTypes ->
          pure (annotateDeclAt (patSynBinderSpan (patSynDeclName patSynDecl)) (TcAnnotation ty [] [] [] [] []) decl)
    _ -> pure decl

annotateInstanceHeaderTc :: (Text, Text) -> Decl -> TcM Decl
annotateInstanceHeaderTc origin decl =
  case decl of
    DeclAnn ann inner
      | Just (TcNewtypeDeriving plan) <- fromAnnotation ann,
        DeclInstance instanceDecl <- peelDeclAnn inner ->
          DeclAnn (mkAnnotation (tcDerivingSourceSpan plan)) <$> annotateInstanceDeclWithNewtype origin (Just plan) instanceDecl
    DeclAnn ann inner -> DeclAnn ann <$> annotateInstanceHeaderTc origin inner
    DeclInstance instanceDecl -> annotateInstanceDeclTc origin instanceDecl
    _ -> pure decl

valueDeclWasChecked :: Map Text TcType -> ValueDecl -> Bool
valueDeclWasChecked checkedValueTypes valueDecl =
  any (`Map.member` checkedValueTypes) (valueDeclBinderNames valueDecl)

valueDeclBinderNames :: ValueDecl -> [Text]
valueDeclBinderNames valueDecl =
  case valueDecl of
    FunctionBind binder _ -> [unqualifiedNameText binder]
    PatternBind _ pat _ -> patternBinders pat

annotateClassDeclTc :: ClassDecl -> TcM Decl
annotateClassDeclTc classDecl = do
  let className = unqualifiedNameText (binderHeadName (classDeclHead classDecl))
  classInfo <- lookupDeclaredClass (binderHeadName (classDeclHead classDecl))
  case classInfo of
    Nothing -> missingTypeInfo ("class " <> T.unpack className)
    Just info -> do
      methods <- zipWithM annotateClassMethod [0 :: Int ..] (classDeclMethodNames classDecl)
      items <- mapM annotateClassDefaultItem (classDeclItems classDecl)
      pure
        ( DeclAnn
            ( mkAnnotation
                TcClassAnnotation
                  { tcClassTyCon = ciTyCon info,
                    tcClassKindTyVars = ciKindTyVars info,
                    tcClassTyVars = ciTyVars info,
                    tcClassSuperClasses = map constraintTypeDictBinder (ciSuperClassTypes info),
                    tcClassMethods = methods,
                    tcClassDefaultMethods = ciDefaultMethods info,
                    tcClassDefaultSignatures =
                      [(methodName, schemeToType signature) | (methodName, signature) <- ciDefaultSignatures info],
                    tcClassAssociatedTypes = ciAssociatedTypes info
                  }
            )
            (DeclClass (classDecl {classDeclItems = items}))
        )

annotateClassDefaultItem :: ClassDeclItem -> TcM ClassDeclItem
annotateClassDefaultItem item =
  case item of
    ClassItemAnn ann inner -> ClassItemAnn ann <$> annotateClassDefaultItem inner
    ClassItemDefault valueDecl ->
      case valueDeclBinderName valueDecl of
        Just (methodName, _) -> do
          methodTy <- bindingType (defaultMethodName methodName)
          pure (ClassItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName methodTy)) item)
        Nothing -> pure item
    ClassItemTypeFamilyDecl familyDecl ->
      case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of
        Nothing -> pure item
        Just familyBinder -> do
          ty <- tyConBindingType (unqualifiedNameText familyBinder)
          let annotatedHead = annotateTypeFamilyHead (TcAnnotation ty [] [] [] [] []) (typeFamilyDeclHead familyDecl)
          pure (ClassItemTypeFamilyDecl (familyDecl {typeFamilyDeclHead = annotatedHead}))
    _ -> pure item

annotateClassMethod :: Int -> Text -> TcM TcClassMethodAnnotation
annotateClassMethod index methodName = do
  methodTy <- bindingType methodName
  let (tvs, _) = peelForAlls methodTy
  dictTy <- selectorDictTypeTc methodName methodTy
  pure
    TcClassMethodAnnotation
      { tcClassMethodName = methodName,
        tcClassMethodType = methodTy,
        tcClassMethodTyVars = tvs,
        tcClassMethodDictType = dictTy,
        tcClassMethodIndex = index
      }

annotateDataDeclTc :: DataDecl -> TcM Decl
annotateDataDeclTc dataDecl = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))
  ty <- tyConBindingType tyName
  constructors <- mapM annotateDataConDeclTc (dataDeclConstructors dataDecl)
  let annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (dataDeclHead dataDecl)
  pure (DeclData (dataDecl {dataDeclHead = annotatedHead, dataDeclConstructors = constructors}))

annotateNewtypeDeclTc :: NewtypeDecl -> TcM Decl
annotateNewtypeDeclTc newtypeDecl = do
  let tyName = unqualifiedNameText (binderHeadName (newtypeDeclHead newtypeDecl))
  ty <- tyConBindingType tyName
  constructor <- mapM annotateDataConDeclTc (newtypeDeclConstructor newtypeDecl)
  let annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (newtypeDeclHead newtypeDecl)
  pure (DeclNewtype (newtypeDecl {newtypeDeclHead = annotatedHead, newtypeDeclConstructor = constructor}))

annotateTypeSynDeclTc :: TypeSynDecl -> TcM Decl
annotateTypeSynDeclTc typeSynDecl = do
  let tyName = unqualifiedNameText (binderHeadName (typeSynHead typeSynDecl))
  ty <- tyConBindingType tyName
  let annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (typeSynHead typeSynDecl)
  pure (DeclTypeSyn (typeSynDecl {typeSynHead = annotatedHead}))

annotateDataFamilyDeclTc :: DataFamilyDecl -> TcM Decl
annotateDataFamilyDeclTc familyDecl = do
  let familyName = unqualifiedNameText (binderHeadName (dataFamilyDeclHead familyDecl))
  ty <- tyConBindingType familyName
  let annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (dataFamilyDeclHead familyDecl)
  pure (DeclDataFamilyDecl (familyDecl {dataFamilyDeclHead = annotatedHead}))

annotateTypeFamilyDeclTc :: TypeFamilyDecl -> TcM Decl
annotateTypeFamilyDeclTc familyDecl =
  case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of
    Nothing -> pure (DeclTypeFamilyDecl familyDecl)
    Just familyBinder -> do
      ty <- tyConBindingType (unqualifiedNameText familyBinder)
      let annotatedHead = annotateTypeFamilyHead (TcAnnotation ty [] [] [] [] []) (typeFamilyDeclHead familyDecl)
      pure (DeclTypeFamilyDecl (familyDecl {typeFamilyDeclHead = annotatedHead}))

annotateTypeFamilyHead :: TcAnnotation -> Type -> Type
annotateTypeFamilyHead tcAnn ty =
  case peelTypeHead ty of
    TCon name promo -> TCon (annotateName tcAnn name) promo
    TInfix left name promo right -> TInfix left (annotateName tcAnn name) promo right
    TApp function argument -> TApp (annotateTypeFamilyHead tcAnn function) argument
    TTypeApp function argument -> TTypeApp (annotateTypeFamilyHead tcAnn function) argument
    other -> other

annotateName :: TcAnnotation -> Name -> Name
annotateName tcAnn name =
  name {nameAnns = nameAnns name <> [mkAnnotation tcAnn]}

annotateTypeFamilyInstTc :: (Text, Text) -> TypeFamilyInst -> TcM Decl
annotateTypeFamilyInstTc (packageName, moduleName') familyInst = do
  familyInstances <- getTypeFamilyInstances
  let expectedKey =
        TcAxiomKey (PackageId packageName) moduleName' (sourceTypeFamilyAxiomName (typeFamilyInstLhs familyInst))
  case find ((== expectedKey) . typeFamilyAxiomKey) familyInstances of
    Just familyInstance ->
      pure (DeclAnn (mkAnnotation familyInstance) (DeclTypeFamilyInst familyInst))
    Nothing -> pure (DeclTypeFamilyInst familyInst)

annotateDataFamilyInstTc :: DataFamilyInst -> TcM Decl
annotateDataFamilyInstTc familyInst = do
  constructors <- mapM annotateRegisteredDataConDeclTc (dataFamilyInstConstructors familyInst)
  let annotated = DeclDataFamilyInst (familyInst {dataFamilyInstConstructors = constructors})
      constructorNames = concatMap (map fst . dataConBindingNames) constructors
  familyInstances <- getDataFamilyInstances
  case constructorNames of
    firstConstructor : _ ->
      case find (elem firstConstructor . dfiiConstructorNames) familyInstances of
        Just familyInstance -> pure (DeclAnn (mkAnnotation familyInstance) annotated)
        Nothing -> pure annotated
    [] -> pure annotated

annotateRegisteredDataConDeclTc :: DataConDecl -> TcM DataConDecl
annotateRegisteredDataConDeclTc dataConDecl =
  case dataConBindingNames dataConDecl of
    [] -> pure dataConDecl
    (name, _) : _ -> do
      maybeBinder <- lookupTerm name
      case maybeBinder of
        Just (TcIdBinder scheme _) -> annotateWithType (schemeToType scheme)
        Just (TcMonoIdBinder ty) -> annotateWithType ty
        Nothing -> pure dataConDecl
  where
    annotateWithType ty = do
      zonkedTy <- zonkType ty
      pure (DataConAnn (mkAnnotation (TcAnnotation zonkedTy [] [] [] [] [])) dataConDecl)

annotateBinderHeadName :: TcAnnotation -> BinderHead UnqualifiedName -> BinderHead UnqualifiedName
annotateBinderHeadName tcAnn head' =
  case head' of
    PrefixBinderHead name params ->
      PrefixBinderHead (annotateUnqualifiedName tcAnn name) params
    InfixBinderHead lhs name rhs params ->
      InfixBinderHead lhs (annotateUnqualifiedName tcAnn name) rhs params

annotateUnqualifiedName :: TcAnnotation -> UnqualifiedName -> UnqualifiedName
annotateUnqualifiedName tcAnn name =
  name {unqualifiedNameAnns = unqualifiedNameAnns name <> [mkAnnotation tcAnn]}

annotateDataConDeclTc :: DataConDecl -> TcM DataConDecl
annotateDataConDeclTc dataConDecl = do
  case dataConBindingNames dataConDecl of
    [] -> pure dataConDecl
    (name, _) : _ -> do
      ty <- dataConBindingType name
      selectors <- annotateRecordSelectorNames dataConDecl
      pure (DataConAnn (mkAnnotation (TcAnnotation ty [] [] [] [] [])) selectors)

annotateRecordSelectorNames :: DataConDecl -> TcM DataConDecl
annotateRecordSelectorNames declaration =
  case declaration of
    RecordCon forallVars context constructor fields ->
      RecordCon forallVars context constructor <$> mapM annotateField fields
    GadtCon forallBinders context constructors (GadtRecordBody fields result) ->
      GadtCon forallBinders context constructors . (`GadtRecordBody` result) <$> mapM annotateField fields
    _ -> pure declaration
  where
    annotateField field = do
      names <- mapM annotateSelectorName (fieldNames field)
      pure field {fieldNames = names}
    annotateSelectorName name = do
      ty <- bindingType (unqualifiedNameText name)
      pure (annotateUnqualifiedName (TcAnnotation ty [] [] [] [] []) name)

dataConBindingType :: Text -> TcM TcType
dataConBindingType name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder scheme _) -> zonkType (schemeToType scheme)
    Just (TcMonoIdBinder ty) -> zonkType ty
    Nothing -> missingTypeInfo ("data constructor " <> T.unpack name)

annotateForeignDeclTc :: ForeignDecl -> TcM Decl
annotateForeignDeclTc foreignDecl = do
  ty <- bindingType (unqualifiedNameText (foreignName foreignDecl))
  key <- resolvedUnqualifiedTermKey (foreignName foreignDecl)
  let sourceSpan = unqualifiedNameSpan (foreignName foreignDecl)
      annotated = annotateDeclAt sourceSpan (TcAnnotation ty [] [] [] [] []) (DeclForeign foreignDecl)
  case foreignCallConv foreignDecl of
    CCall -> do
      let declaredName = unqualifiedNameText (foreignName foreignDecl)
      (target, symbol) <- checkForeignEntity sourceSpan declaredName (foreignEntity foreignDecl)
      plan <- checkForeignImportType sourceSpan target symbol ty
      checkedPlan <- checkForeignTarget sourceSpan plan
      registerForeignImport key (TcForeignCCallImport (foreignSafetyMark (foreignSafety foreignDecl)) checkedPlan)
      pure (DeclAnn (mkAnnotation checkedPlan) annotated)
    CPrim -> do
      registerForeignImport key TcForeignPrimImport
      pure annotated
    _ -> pure annotated

-- | Record the checked calling convention of a foreign import, so that the
-- interface of the module carries it.
registerForeignImport :: TcTermKey -> TcForeignImportInfo -> TcM ()
registerForeignImport key info =
  lift $ modify' $ \state -> state {tcsForeignImports = Map.insert key info (tcsForeignImports state)}

-- | The safety mark of a foreign import. A missing mark is safe.
foreignSafetyMark :: Maybe ForeignSafety -> TcForeignSafety
foreignSafetyMark safety =
  case safety of
    Nothing -> TcForeignSafe
    Just Safe -> TcForeignSafe
    Just Unsafe -> TcForeignUnsafe
    Just Interruptible -> TcForeignInterruptible

-- | Read the C entity of a foreign import and report a bad entity.
checkForeignEntity :: SourceSpan -> Text -> ForeignEntitySpec -> TcM (TcForeignTarget, Text)
checkForeignEntity sourceSpan declaredName entity =
  case resolveForeignEntity declaredName entity of
    Right resolved -> pure resolved
    Left message -> do
      emitError sourceSpan (OtherError message)
      pure (TcForeignCall, declaredName)

-- | The C entity string has the form @[static] [header] [&] [symbol]@.  The
-- @static@ keyword and the header file name give no information to this
-- compiler, because it does not include the header when it makes the call.
-- Thus it accepts both and then ignores them, as GHC does.
--
-- The parser removes the @static@ keyword.  This function must remove an
-- optional header file name and read an optional @&@ address mark.  An empty
-- entity, or an entity that gives only a header file name, names the declared
-- Haskell function.
resolveForeignEntity :: Text -> ForeignEntitySpec -> Either String (TcForeignTarget, Text)
resolveForeignEntity declaredName entity =
  case entity of
    ForeignEntityOmitted -> Right (TcForeignCall, declaredName)
    ForeignEntityStatic Nothing -> Right (TcForeignCall, declaredName)
    ForeignEntityStatic (Just text) -> readEntityText TcForeignCall text
    ForeignEntityNamed text -> readEntityText TcForeignCall text
    ForeignEntityAddress Nothing -> Right (TcForeignAddress, declaredName)
    ForeignEntityAddress (Just text) -> readEntityText TcForeignAddress text
    ForeignEntityDynamic -> Left "a dynamic foreign import is not supported"
    ForeignEntityWrapper -> Left "a wrapper foreign import is not supported"
  where
    -- Read the entity words. If that fails, read them again without the first
    -- word, which is then the header file name.
    readEntityText defaultTarget text =
      let entityWords = T.words text
       in case readEntityWords defaultTarget entityWords <|> readEntityWords defaultTarget (drop 1 entityWords) of
            Just resolved -> Right resolved
            Nothing -> Left ("unsupported foreign import entity: " <> T.unpack text)
    readEntityWords defaultTarget entityWords =
      case entityWords of
        [] -> Just (defaultTarget, declaredName)
        ["&"] -> Just (TcForeignAddress, declaredName)
        ["&", name] -> (TcForeignAddress,) <$> cIdentifier name
        [name]
          | Just addressName <- T.stripPrefix "&" name -> (TcForeignAddress,) <$> cIdentifier addressName
          | otherwise -> (defaultTarget,) <$> cIdentifier name
        _ -> Nothing
    cIdentifier name =
      case T.uncons name of
        Just (first, rest)
          | isIdentifierStart first && T.all isIdentifierPart rest -> Just name
        _ -> Nothing
    isIdentifierStart character = isAlpha character || character == '_'
    isIdentifierPart character = isAlphaNum character || character == '_'

-- | An address import (@foreign import ccall "&sym"@) names static data
-- rather than a function, so it takes no arguments and its value is the
-- symbol address itself.
checkForeignTarget :: SourceSpan -> TcForeignImportAnnotation -> TcM TcForeignImportAnnotation
checkForeignTarget sourceSpan plan =
  case tcForeignTarget plan of
    TcForeignAddress -> do
      unless (null (tcForeignArguments plan)) $
        emitError sourceSpan (OtherError "an address foreign import must not take arguments")
      unless (tcForeignEffect plan == TcForeignPure) $
        emitError sourceSpan (OtherError "an address foreign import must not return IO")
      unless (tcForeignAbiType (tcForeignResult plan) == TcForeignAddr) $
        emitError sourceSpan (OtherError "an address foreign import must produce a pointer")
      pure plan
    TcForeignCall -> pure plan

checkForeignImportType :: SourceSpan -> TcForeignTarget -> Text -> TcType -> TcM TcForeignImportAnnotation
checkForeignImportType sourceSpan target symbol ty = do
  let (argumentTypes, resultType) = splitFunctionType ty
      (effect, valueResultType) =
        case resultType of
          TcTyCon (TyCon "IO" 1) [ioResult] -> (TcForeignRealWorld, ioResult)
          _ -> (TcForeignPure, resultType)
  arguments <- mapM (checkForeignValueType sourceSpan) argumentTypes
  result <- checkForeignValueType sourceSpan valueResultType
  when (any ((== TcForeignVoid) . tcForeignAbiType) arguments) $
    emitError sourceSpan (OtherError "a foreign import argument must not have a unit type")
  pure
    TcForeignImportAnnotation
      { tcForeignArguments = arguments,
        tcForeignResult = result,
        tcForeignEffect = effect,
        tcForeignSymbol = symbol,
        tcForeignTarget = target
      }

splitFunctionType :: TcType -> ([TcType], TcType)
splitFunctionType ty =
  case ty of
    TcForAllTy _ body -> splitFunctionType body
    TcFunTy argument result ->
      let (arguments, finalResult) = splitFunctionType result
       in (argument : arguments, finalResult)
    _ -> ([], ty)

checkForeignValueType :: SourceSpan -> TcType -> TcM TcForeignMarshal
checkForeignValueType sourceSpan ty = do
  resolved <- resolveForeignValueType ty
  case resolved of
    Right marshal -> pure marshal
    Left problem -> do
      emitError sourceSpan (OtherError ("unsupported foreign import value type: " <> problem))
      primitiveMarshal ty [] "Int32#" TcForeignInt32

-- | Find the primitive representation of a foreign value type.  The FFI
-- chapter of the Haskell report marshals a value through any number of
-- newtypes, and the boxed integer and pointer types of the base library are
-- single-constructor, single-field data types around a primitive type.  Both
-- unwrap the same way: through the one constructor of the type to its one
-- field, until a primitive type or a nullary constructor (the unit type of a
-- result) appears.
resolveForeignValueType :: TcType -> TcM (Either String TcForeignMarshal)
resolveForeignValueType sourceType = go (0 :: Int) [] sourceType
  where
    go depth constructors ty
      | depth > maximumUnwrapDepth = pure (Left ("too many newtype layers in " <> renderTcType sourceType))
      | otherwise =
          case ty of
            TcTyCon tyCon _
              | Just (primitiveName, abiType) <- lookup (tyConName tyCon, tyConArity tyCon) primitiveForeignTypes ->
                  Right <$> primitiveMarshal sourceType (reverse constructors) primitiveName abiType
              -- A byte array argument passes the address of its payload.
              | (tyConName tyCon, tyConArity tyCon) `elem` [("ByteArray#", 0), ("MutableByteArray#", 1)] ->
                  pure (Right (byteArrayMarshal ty))
              | otherwise -> do
                  mDataType <- lookupDataType tyCon
                  case mDataType of
                    Just dataType
                      | [constructor] <- dtiConstructors dataType,
                        null (dciExTyVars constructor),
                        null (dciTheta constructor) ->
                          case dciFields constructor of
                            [field]
                              | Just substitution <- matchTypes [dciResTy constructor] [ty] ->
                                  go (depth + 1) (dciName constructor : constructors) (applySubst substitution (dcfiType field))
                            [] -> pure (Right (voidMarshal (reverse (dciName constructor : constructors))))
                            _ -> unsupported ty
                    _ -> unsupported ty
            _ -> unsupported ty
    unsupported ty
      | ty == sourceType = pure (Left (renderTcType ty))
      | otherwise = pure (Left (renderTcType ty <> " in " <> renderTcType sourceType))
    maximumUnwrapDepth = 64
    byteArrayMarshal ty =
      TcForeignMarshal
        { tcForeignSourceType = sourceType,
          tcForeignPrimitiveType = ty,
          tcForeignConstructors = [],
          tcForeignAbiType = TcForeignAddr
        }
    voidMarshal constructors =
      TcForeignMarshal
        { tcForeignSourceType = sourceType,
          tcForeignPrimitiveType = sourceType,
          tcForeignConstructors = constructors,
          tcForeignAbiType = TcForeignVoid
        }

-- | Primitive types that the C ABI bridge understands, with the ABI value
-- each one marshals as.
primitiveForeignTypes :: [((Text, Int), (Text, TcForeignAbiType))]
primitiveForeignTypes =
  [ (("Int#", 0), ("Int#", TcForeignInt)),
    (("Int8#", 0), ("Int8#", TcForeignInt8)),
    (("Int16#", 0), ("Int16#", TcForeignInt16)),
    (("Int32#", 0), ("Int32#", TcForeignInt32)),
    (("Int64#", 0), ("Int64#", TcForeignInt64)),
    (("Word#", 0), ("Word#", TcForeignWord)),
    (("Word8#", 0), ("Word8#", TcForeignWord8)),
    (("Word16#", 0), ("Word16#", TcForeignWord16)),
    (("Word32#", 0), ("Word32#", TcForeignWord32)),
    (("Word64#", 0), ("Word64#", TcForeignWord64)),
    (("Float#", 0), ("Float#", TcForeignFloat)),
    (("Double#", 0), ("Double#", TcForeignDouble)),
    (("Addr#", 0), ("Addr#", TcForeignAddr))
  ]

primitiveMarshal :: TcType -> [Text] -> Text -> TcForeignAbiType -> TcM TcForeignMarshal
primitiveMarshal sourceType constructors primitiveName abiType = do
  primitiveTyCon <- mkKnownTyCon "GHC.Prim" primitiveName 0 typeKindType
  pure
    TcForeignMarshal
      { tcForeignSourceType = sourceType,
        tcForeignPrimitiveType = TcTyCon primitiveTyCon [],
        tcForeignConstructors = constructors,
        tcForeignAbiType = abiType
      }

annotateDeclAt :: SourceSpan -> TcAnnotation -> Decl -> Decl
annotateDeclAt NoSourceSpan tcAnn decl =
  annotateDecl tcAnn decl
annotateDeclAt sp tcAnn decl =
  DeclAnn (mkAnnotation sp) (annotateDecl tcAnn decl)

valueDeclSpan :: ValueDecl -> SourceSpan
valueDeclSpan valueDecl =
  case valueDecl of
    FunctionBind name _ -> unqualifiedNameSpan name
    PatternBind _ pat _ -> patternSpan pat

unqualifiedNameSpan :: UnqualifiedName -> SourceSpan
unqualifiedNameSpan =
  sourceSpanFromAnns . unqualifiedNameAnns

tyConBindingType :: Text -> TcM TcType
tyConBindingType name = do
  mInfo <- lookupTyCon name
  case mInfo of
    Just info -> defaultKindMetas (typeSchemeBody (tciKindScheme info))
    Nothing -> missingTypeInfo ("type constructor " <> T.unpack name)

annotateValueDeclTc :: Map Text TcType -> ValueDecl -> TcM (TcType, ValueDecl)
annotateValueDeclTc checkedValueTypes valueDecl =
  case valueDecl of
    FunctionBind name matches -> do
      bindingTy <- checkedBindingType (unqualifiedNameText name)
      pure (bindingTy, FunctionBind name matches)
    PatternBind anns pat rhs ->
      case patternBinderName pat of
        Just (_, name) -> do
          bindingTy <- checkedBindingType name
          pure (bindingTy, PatternBind anns pat rhs)
        Nothing -> do
          ty <- checkedBindingType (patternBindingResultName pat)
          pure (ty, valueDecl)
  where
    checkedBindingType name =
      maybe (bindingType name) pure (Map.lookup name checkedValueTypes)

annotateInstanceDeclTc :: (Text, Text) -> InstanceDecl -> TcM Decl
annotateInstanceDeclTc origin = annotateInstanceDeclWithNewtype origin Nothing

annotateInstanceDeclWithNewtype :: (Text, Text) -> Maybe TcDerivingPlan -> InstanceDecl -> TcM Decl
annotateInstanceDeclWithNewtype origin newtypePlan instanceDecl =
  case (instanceHeadName (instanceDeclHead instanceDecl), instanceHeadTypes (instanceDeclHead instanceDecl)) of
    (_, []) -> pure (DeclInstance instanceDecl)
    (Nothing, _) -> pure (DeclInstance instanceDecl)
    (Just className, headArgTypes) -> do
      (rawTvIds, tvEnv) <- makeInstanceTyVarEnv instanceDecl headArgTypes
      let classNameText = nameText className
      rawHeadTys <- checkInstanceHeadTypes className tvEnv headArgTypes
      rawContext <- mapM (surfacePredToPred tvEnv) (instanceDeclContext instanceDecl)
      tvIds <- mapM defaultTyVarKinds rawTvIds
      headTys <- mapM defaultTypeKinds rawHeadTys
      context <- mapM defaultPredKinds rawContext
      dictName <- lookupInstanceDictName origin classNameText headTys
      classInfo <- lookupClassNamed className
      info <- maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure classInfo
      let classSubstitution =
            Map.fromList [(tvUnique tyVar, ty) | (tyVar, ty) <- zip (ciTyVars info) headTys]
          superClassTypes = map (applySubst classSubstitution) (ciSuperClassTypes info)
          defaults = ciDefaultMethods info
      superClasses <- mapM constraintTypePred superClassTypes
      superClassEvidence <- mapM (solveInstanceSuperClass classNameText context) superClasses
      -- Only a method the instance leaves to its class default needs the
      -- evidence of the default signature.
      let definedMethods =
            [ name
            | item <- instanceDeclItems instanceDecl,
              InstanceItemBind valueDecl <- [peelInstanceDeclItemAnn item],
              name <- valueDeclBinderNames valueDecl
            ]
      defaultMethodEvidence <-
        sequence
          [ (methodName,) <$> mapM (solveInstanceSuperClass classNameText context) predicates
          | methodName <- defaults,
            methodName `notElem` definedMethods,
            isNothing newtypePlan,
            Just (ForAll _ signaturePredicates _) <- [lookup methodName (ciDefaultSignatures info)],
            let predicates = filter (not . isPredicateOfClass (ciTyCon info)) (map (applySubstPred classSubstitution) signaturePredicates)
          ]
      contextDicts <- mapM predDictBinder context
      superClassBinders <- mapM predDictBinder superClasses
      familyInstances <- getTypeFamilyInstances
      let lookupEquation axiomName =
            find ((== TcAxiomKey (PackageId (fst origin)) (snd origin) axiomName) . typeFamilyAxiomKey) familyInstances
          explicitNames = mapMaybe typeFamilyInstName (instanceDeclTypeFamilyInsts instanceDecl)
          explicitEquation familyInst = lookupEquation (sourceTypeFamilyAxiomName (typeFamilyInstLhs familyInst))
          defaultEquations =
            [ equation
            | associated <- ciAssociatedTypes info,
              tyConName (atiTyCon associated) `notElem` explicitNames,
              Just _ <- [atiDefault associated],
              Just equation <- [lookupEquation (associatedDefaultAxiomName associated headTys)]
            ]
          annotateItem item =
            case item of
              InstanceItemAnn ann inner -> InstanceItemAnn ann (annotateItem inner)
              InstanceItemTypeFamilyInst familyInst
                | Just equation <- explicitEquation familyInst -> InstanceItemAnn (mkAnnotation equation) item
              _ -> item
          items = map annotateItem (instanceDeclItems instanceDecl)
          associatedEquations = mapMaybe explicitEquation (instanceDeclTypeFamilyInsts instanceDecl) <> defaultEquations
      let dictTy = foldr TcForAllTy (TcQualTy context (TcTyCon (ciTyCon info) headTys)) tvIds
          methodOrder = map fst (ciMethods info)
          classMethods = zipWith (classMethodFromInfo info) [0 :: Int ..] (ciMethods info)
          instAnn =
            TcInstanceAnnotation
              { tcInstanceDictName = dictName,
                tcInstanceDictType = dictTy,
                tcInstanceClassTyCon = ciTyCon info,
                tcInstanceTyVars = tvIds,
                tcInstanceHeadTypes = headTys,
                tcInstanceClassTyVars = ciTyVars info,
                tcInstanceClassOrigin = ciOrigin info,
                tcInstanceClassSuperClasses = map constraintTypeDictBinder (ciSuperClassTypes info),
                tcInstanceClassMethods = classMethods,
                tcInstanceContextDicts = contextDicts,
                tcInstanceSuperClasses = zip superClassBinders superClassEvidence,
                tcInstanceMethodOrder = methodOrder,
                tcInstanceDefaultMethods = defaults,
                tcInstanceDefaultMethodEvidence = defaultMethodEvidence,
                tcInstanceAssociatedTypes = associatedEquations,
                tcInstanceNewtype = Nothing
              }
      checkedAnn <- case newtypePlan of
        Nothing -> pure instAnn
        Just plan -> checkNewtypeInstance origin solveInstanceSuperClass methodExpectedScheme plan info context instAnn
      pure (DeclAnn (mkAnnotation checkedAnn) (DeclInstance (instanceDecl {instanceDeclItems = items})))

classMethodFromInfo :: ClassInfo -> Int -> (Text, TypeScheme) -> TcClassMethodAnnotation
classMethodFromInfo info index (methodName, scheme) =
  let methodType = schemeToType scheme
      (typeVariables, _) = peelForAlls methodType
      dictionaryType = TcTyCon (ciTyCon info) (map TcTyVar (ciTyVars info))
   in TcClassMethodAnnotation
        { tcClassMethodName = methodName,
          tcClassMethodType = methodType,
          tcClassMethodTyVars = typeVariables,
          tcClassMethodDictType = dictionaryType,
          tcClassMethodIndex = index
        }

isPredicateOfClass :: TyCon -> Pred -> Bool
isPredicateOfClass classTyCon predicate =
  case predicate of
    ClassPred predicateClass _ -> tyConKey predicateClass == tyConKey classTyCon
    _ -> False

solveInstanceSuperClass :: Text -> [Pred] -> Pred -> TcM EvTerm
solveInstanceSuperClass className givens predicate = do
  evidenceVariable <- freshEvVar
  let constraint = mkWantedCt predicate evidenceVariable (InstOrigin className) NoSourceSpan
  result <- case predicate of
    EqPred {} -> do
      equality <- withGivenPredicates givens (solveEquality constraint)
      pure $ case equality of
        EqSolved -> DictSolved
        _ -> DictStuck constraint
    _ -> solveDictWithGivens givens constraint
  case result of
    DictSolved -> do
      evidence <- lookupEvidence evidenceVariable
      case evidence of
        Just term -> pure term
        Nothing -> missingTypeInfo ("superclass evidence for " <> T.unpack className)
    DictStuck stuck -> do
      emitError (ctLoc stuck) (UnsolvedWanted (ctPred stuck) (ctOrigin stuck))
      pure (EvVarTerm evidenceVariable)

tcClassDeclBodies :: Decl -> TcM Decl
tcClassDeclBodies (DeclAnn ann inner) =
  DeclAnn ann <$> tcClassDeclBodies inner
tcClassDeclBodies (DeclClass classDecl) = do
  items <- mapM tcClassDefaultBody (classDeclItems classDecl)
  pure (DeclClass (classDecl {classDeclItems = items}))
tcClassDeclBodies decl = pure decl

tcClassDefaultBody :: ClassDeclItem -> TcM ClassDeclItem
tcClassDefaultBody item =
  case item of
    ClassItemAnn ann inner -> ClassItemAnn ann <$> tcClassDefaultBody inner
    ClassItemDefault valueDecl -> do
      checked <- tcClassDefaultValue valueDecl
      pure (ClassItemDefault checked)
    _ -> pure item

tcClassDefaultValue :: ValueDecl -> TcM ValueDecl
tcClassDefaultValue valueDecl =
  case valueDeclBinderName valueDecl of
    Nothing -> pure valueDecl
    Just (methodName, _) -> do
      binder <- lookupTerm (defaultMethodName methodName)
      case binder of
        Just (TcIdBinder (ForAll methodTyVars givens methodTy) _) ->
          case valueDecl of
            FunctionBind name matches -> do
              let (argumentTypes, resultType) = splitFunTy methodTy (matchArity matches)
              results <-
                withScopedTyVars (tyVarScope methodTyVars) $
                  mapM (tcMatchEquation Nothing argumentTypes resultType) matches
              solveInstanceBodyConstraints givens [(constraints, implications) | (_, constraints, implications) <- results]
              pure (FunctionBind name [match | (match, _, _) <- results])
            PatternBind annotations pattern' rhs -> do
              results <-
                withScopedTyVars (tyVarScope methodTyVars) $
                  mapM (tcMatchEquation Nothing [] methodTy) [zeroArgMatch (patternSpan pattern') rhs]
              solveInstanceBodyConstraints givens [(constraints, implications) | (_, constraints, implications) <- results]
              case results of
                [(match, _, _)] -> pure (PatternBind annotations pattern' (matchRhs match))
                _ -> pure valueDecl
        _ -> missingTypeInfo ("class default method " <> T.unpack methodName)

tcInstanceDeclBodies :: Decl -> TcM Decl
tcInstanceDeclBodies (DeclAnn ann inner)
  | Just annotation <- fromAnnotation @TcInstanceAnnotation ann,
    DeclInstance instanceDecl <- peelDeclAnn inner = do
      let classNameText = tyConName (tcInstanceClassTyCon annotation)
          headTys = tcInstanceHeadTypes annotation
      givens <- mapM (constraintTypePred . tcDictBinderType) (tcInstanceContextDicts annotation)
      classInfo <- lookupClass (tcInstanceClassTyCon annotation) >>= maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure
      items <-
        withScopedTyVars (tyVarScope (tcInstanceTyVars annotation)) $
          mapM (tcInstanceItemBody classInfo givens headTys) (instanceDeclItems instanceDecl)
      pure (DeclAnn ann (DeclInstance (instanceDecl {instanceDeclItems = items})))
  | otherwise = DeclAnn ann <$> tcInstanceDeclBodies inner
tcInstanceDeclBodies (DeclInstance instanceDecl) =
  case (instanceHeadName (instanceDeclHead instanceDecl), instanceHeadTypes (instanceDeclHead instanceDecl)) of
    (_, []) -> pure (DeclInstance instanceDecl)
    (Nothing, _) -> pure (DeclInstance instanceDecl)
    (Just className, headArgTypes) -> do
      let classNameText = nameText className
      (_, tvEnv) <- makeInstanceTyVarEnv instanceDecl headArgTypes
      rawHeadTys <- checkInstanceHeadTypes className tvEnv headArgTypes
      rawGivens <- mapM (surfacePredToPred tvEnv) (instanceDeclContext instanceDecl)
      headTys <- mapM defaultTypeKinds rawHeadTys
      givens <- mapM defaultPredKinds rawGivens
      classInfo <- lookupClassNamed className >>= maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure
      items <-
        withScopedTyVars tvEnv $
          mapM (tcInstanceItemBody classInfo givens headTys) (instanceDeclItems instanceDecl)
      pure (DeclInstance (instanceDecl {instanceDeclItems = items}))
tcInstanceDeclBodies decl =
  pure decl

-- | The scope of type variables that keep their source names, for an
-- instance head or a class head.
tyVarScope :: [TyVarId] -> Map Text (TyVarId, TcType)
tyVarScope tyVars = Map.fromList [(tvName tyVar, (tyVar, tvKind tyVar)) | tyVar <- tyVars]

tcInstanceItemBody :: ClassInfo -> [Pred] -> [TcType] -> InstanceDeclItem -> TcM InstanceDeclItem
tcInstanceItemBody classInfo givens headTys item =
  case item of
    InstanceItemAnn ann inner ->
      InstanceItemAnn ann <$> tcInstanceItemBody classInfo givens headTys inner
    InstanceItemBind (FunctionBind name matches) -> do
      ForAll methodTyVars methodGivens methodTy <- methodExpectedScheme classInfo headTys (unqualifiedNameText name)
      let (argTys, resTy) = splitFunTy methodTy (matchArity matches)
      (results, failed) <-
        withErrorTracking $ do
          results <- mapM (tcMatchEquation Nothing argTys resTy) matches
          solveInstanceBodyConstraints (givens <> methodGivens) [(cts, impls) | (_match, cts, impls) <- results]
          pure results
      -- A body with a type error keeps pending annotations that have no
      -- evidence. Keep the unchecked body so finalization does not abort.
      if failed
        then pure item
        else do
          let methodName = unqualifiedNameText name
              checkedType = foldr TcForAllTy (qualifiedType methodGivens methodTy) methodTyVars
              checkedBind = InstanceItemBind (FunctionBind name [match | (match, _cts, _impls) <- results])
          pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName checkedType)) checkedBind)
    InstanceItemBind (PatternBind _ pat rhs) ->
      case patternBinderName pat of
        Just (_, methodName) -> do
          ForAll methodTyVars methodGivens methodTy <- methodExpectedScheme classInfo headTys methodName
          (results, failed) <-
            withErrorTracking $ do
              results <- mapM (tcMatchEquation Nothing [] methodTy) [zeroArgMatch (patternSpan pat) rhs]
              solveInstanceBodyConstraints (givens <> methodGivens) [(cts, impls) | (_match, cts, impls) <- results]
              pure results
          case results of
            [(match, _cts, _impls)]
              | not failed ->
                  let checkedType = foldr TcForAllTy (qualifiedType methodGivens methodTy) methodTyVars
                      checkedBind = replaceInstancePatternBindRhs (matchRhs match) item
                   in pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName checkedType)) checkedBind)
            _ -> pure item
        Nothing -> pure item
    _ -> pure item
  where
    qualifiedType predicates ty
      | null predicates = ty
      | otherwise = TcQualTy predicates ty

matchArity :: [Match] -> Int
matchArity (match : _) = length (matchPats match)
matchArity [] = 0

solveInstanceBodyConstraints :: [Pred] -> [([Ct], [Implication])] -> TcM ()
solveInstanceBodyConstraints givens results = do
  let (ctsList, implsList) = unzip results
      cts = concat ctsList
      impls = concat implsList
  solveBodyConstraintsWithGivens givens cts impls

-- Keep the givens available after metavariable assignments and during decomposition.
solveBodyConstraintsWithGivens :: [Pred] -> [Ct] -> [Implication] -> TcM ()
solveBodyConstraintsWithGivens givens cts impls = withGivenPredicates givens $ do
  implications <- mapM addOuterGivens impls
  residual <- filterM (fmap not . solveGivenEquality givens) cts
  solveResult <- solveWithImpls residual implications
  -- The inert set holds the stuck flat wanteds, which are in @cts@, and
  -- the wanteds that the implications deferred, which are not.
  let deferred = filter (\ct -> ctEvVar ct `notElem` map ctEvVar cts) (inertDicts (srInerts solveResult))
  -- The residual holds the equalities that wait on a type family
  -- application.
  stuck <- (srResidual solveResult <>) . concat <$> mapM attemptClassCt (cts <> deferred)
  -- The signature makes every type variable of the binding rigid, so a
  -- meta-variable that survives the solve is ambiguous. Defaulting may make
  -- it concrete, which lets a second attempt discharge the constraint.
  defaulted <- defaultAmbiguousMetas [] stuck
  remaining <-
    if defaulted
      then concat <$> mapM attemptStuckCt stuck
      else pure stuck
  mapM_ reportUnsolvedDict remaining
  where
    addOuterGivens implication = do
      outerGivens <- mapM givenConstraint givens
      pure (implication {implGivenCts = outerGivens <> implGivenCts implication})
    givenConstraint predicate = do
      evidence <- freshEvVar
      let origin = InstOrigin "class body"
      pure ((mkWantedCt predicate evidence origin NoSourceSpan) {ctFlavor = Given})
    -- Solve what it can and collect the rest. Reporting waits until
    -- defaulting has had its turn.
    attemptClassCt ct
      | isDictionaryPred (ctPred ct) = do
          result <- solveDictWithGivens givens ct
          case result of
            DictSolved -> pure []
            DictStuck stuck -> pure [stuck]
      | otherwise = pure []
    -- An equality that waits on a type family application gets another
    -- attempt after defaulting.
    attemptStuckCt ct
      | EqPred {} <- ctPred ct = do
          result <- solveEquality ct
          case result of
            EqSolved -> pure []
            EqStuck stuck -> pure [stuck]
            EqError errCt -> do
              case ctPred errCt of
                EqPred left right ->
                  emitError (ctLoc errCt) (UnificationError left right (ctOrigin errCt) (ctEqProvenance errCt))
                predicate ->
                  emitError (ctLoc errCt) (UnsolvedWanted predicate (ctOrigin errCt))
              pure []
      | otherwise = attemptClassCt ct
    isDictionaryPred predicate =
      case predicate of
        ClassPred {} -> True
        IParamPred {} -> True
        EqPred {} -> False
        QuantifiedPred {} -> False

bindingType :: Text -> TcM TcType
bindingType name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just binder -> pure (binderType binder)
    Nothing -> missingTypeInfo ("binding " <> T.unpack name)

binderType :: TcBinder -> TcType
binderType (TcIdBinder scheme _) = schemeToType scheme
binderType (TcMonoIdBinder ty) = ty

methodExpectedScheme :: ClassInfo -> [TcType] -> Text -> TcM TypeScheme
methodExpectedScheme classInfo headTys methodName =
  case lookup methodName (ciMethods classInfo) of
    Just (ForAll tyVars predicates body) ->
      case splitClassReceiver predicates headTys of
        Just (receiverSubst, methodPredicates) -> do
          headKinds <- mapM tcTypeKind headTys
          let classKinds = map tvKind (ciTyVars classInfo)
              kindSubst = fromMaybe Map.empty (matchTypes classKinds headKinds)
              subst = receiverSubst <> kindSubst
          pure
            ( ForAll
                (filter (\tyVar -> not (Map.member (tvUnique tyVar) subst)) tyVars)
                (map (applySubstPred subst) methodPredicates)
                (applySubst subst body)
            )
        Nothing -> missingTypeInfo ("class method receiver for " <> T.unpack methodName)
    Nothing -> missingTypeInfo ("class method " <> T.unpack methodName)

splitClassReceiver :: [Pred] -> [TcType] -> Maybe (Map Unique TcType, [Pred])
splitClassReceiver [] _ = Nothing
splitClassReceiver (predicate : predicates) headTys =
  case predicate of
    ClassPred _ classArgs -> (,predicates) <$> matchTypes classArgs headTys
    _ -> do
      (subst, rest) <- splitClassReceiver predicates headTys
      pure (subst, predicate : rest)

missingTypeInfo :: String -> TcM a
missingTypeInfo msg =
  abortTc ("internal type annotation error: missing " <> msg)

selectorDictTypeTc :: Text -> TcType -> TcM TcType
selectorDictTypeTc methodName methodTy =
  case snd (peelForAlls methodTy) of
    TcQualTy (pred' : _) _ -> predType pred'
    _ -> missingTypeInfo ("class dictionary type for method selector " <> T.unpack methodName)

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tv body) =
  let (tvs, inner) = peelForAlls body
   in (tv : tvs, inner)
peelForAlls ty = ([], ty)

predDictBinder :: Pred -> TcM TcDictBinderAnnotation
predDictBinder pred' =
  case pred' of
    ClassPred classTyCon args ->
      pure (TcDictBinderAnnotation (tyConName classTyCon) args (TcTyCon classTyCon args))
    EqPred {} -> do
      ty <- predType pred'
      pure (TcDictBinderAnnotation "<constraint>" [] ty)
    QuantifiedPred {} -> do
      ty <- predType pred'
      pure (TcDictBinderAnnotation "<quantified>" [] ty)
    IParamPred name payload -> do
      ty <- predType pred'
      pure (TcDictBinderAnnotation name [payload] ty)

constraintTypeDictBinder :: TcType -> TcDictBinderAnnotation
constraintTypeDictBinder ty =
  case constraintTypeToPred ty of
    Just (ClassPred classTyCon args) -> TcDictBinderAnnotation (tyConName classTyCon) args ty
    _ -> TcDictBinderAnnotation "<constraint>" [] ty

constraintTypePred :: TcType -> TcM Pred
constraintTypePred ty =
  case constraintTypeToPred ty of
    Just predicate -> pure predicate
    Nothing -> missingTypeInfo ("class predicate for constraint " <> show ty)

collectClassMethodNames :: [Decl] -> Map Text [Text]
collectClassMethodNames = Map.fromList . mapMaybe collect
  where
    collect decl =
      case peelDeclAnn decl of
        DeclClass classDecl ->
          Just (unqualifiedNameText (binderHeadName (classDeclHead classDecl)), classDeclMethodNames classDecl)
        _ -> Nothing

classDeclMethodNames :: ClassDecl -> [Text]
classDeclMethodNames classDecl = concatMap classItemMethodNames (classDeclItems classDecl)

classItemMethodNames :: ClassDeclItem -> [Text]
classItemMethodNames item =
  case peelClassDeclItemAnn item of
    ClassItemTypeSig names _ -> map unqualifiedNameText names
    _ -> []

classDeclDefaultMethodNames :: ClassDecl -> [Text]
classDeclDefaultMethodNames classDecl = mapMaybe classItemDefaultMethodName (classDeclItems classDecl)

classItemDefaultMethodName :: ClassDeclItem -> Maybe Text
classItemDefaultMethodName item =
  case peelClassDeclItemAnn item of
    ClassItemDefault valueDecl -> fst <$> valueDeclBinderName valueDecl
    _ -> Nothing

valueDeclBinderName :: ValueDecl -> Maybe (Text, Text)
valueDeclBinderName valueDecl =
  case valueDecl of
    FunctionBind name _ -> Just (binderBindingName name)
    PatternBind _ pat _ -> patternBinderName pat

defaultMethodName :: Text -> Text
defaultMethodName methodName = "$dm" <> T.concatMap encodeCharacter methodName
  where
    encodeCharacter character
      | isAlphaNum character || character `elem` ("_$#'" :: String) = T.singleton character
      | otherwise = "$" <> T.pack (show (ord character)) <> "$"

matchTypes :: [TcType] -> [TcType] -> Maybe (Map Unique TcType)
matchTypes patterns targets
  | length patterns /= length targets = Nothing
  | otherwise = foldM matchOne Map.empty (zip patterns targets)

matchOne :: Map Unique TcType -> (TcType, TcType) -> Maybe (Map Unique TcType)
matchOne subst (TcTyVar tv, target) =
  case Map.lookup (tvUnique tv) subst of
    Nothing -> Just (Map.insert (tvUnique tv) target subst)
    Just existing
      | existing == target -> Just subst
      | otherwise -> Nothing
matchOne subst (TcTyCon tc args, TcTyCon targetTc targetArgs)
  | tc == targetTc,
    length args == length targetArgs =
      foldM matchOne subst (zip args targetArgs)
matchOne subst (TcFunTy a b, TcFunTy targetA targetB) =
  matchOne subst (a, targetA) >>= \subst' -> matchOne subst' (b, targetB)
matchOne subst (TcAppTy f a, TcAppTy targetF targetA) =
  matchOne subst (f, targetF) >>= \subst' -> matchOne subst' (a, targetA)
matchOne subst (patternTy, targetTy)
  | patternTy == targetTy = Just subst
  | otherwise = Nothing

-- | Collect type signatures from a list of declarations.
collectUserSigs :: [Decl] -> TcM (Map TcTermKey UserSig)
collectUserSigs decls = do
  signatures <- concat <$> mapM (extractSig NoSourceSpan) decls
  foldM insertSignature Map.empty signatures
  where
    insertSignature collected (key, signature)
      | Map.member key collected = abortTc ("duplicate source signature key: " <> show key)
      | otherwise = pure (Map.insert key signature collected)
    extractSig ambient (DeclTypeSig names ty) =
      mapM
        ( \n -> do
            key <- resolvedUnqualifiedTermKey n
            let name = unqualifiedNameText n
                sigSp = ambient `orSourceSpan` unqualifiedNameSpan n `orSourceSpan` typeSpan ty
            pure (key, UserSig name ty sigSp)
        )
        names
    extractSig ambient (DeclForeign foreignDecl)
      | isForeignImport foreignDecl =
          do
            key <- resolvedUnqualifiedTermKey (foreignName foreignDecl)
            let name = unqualifiedNameText (foreignName foreignDecl)
                sigSp = ambient `orSourceSpan` unqualifiedNameSpan (foreignName foreignDecl) `orSourceSpan` typeSpan (foreignType foreignDecl)
            pure [(key, UserSig name (foreignType foreignDecl) sigSp)]
    extractSig ambient (DeclAnn ann inner) =
      extractSig (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
    extractSig ambient (DeclPatSynSig names ty) = extractSig ambient (DeclTypeSig names ty)
    extractSig _ _ = pure []

checkUserSig :: UserSig -> TcM CheckedSig
checkUserSig userSig = do
  scheme <- sigToScheme (userSigType userSig) >>= defaultTypeSchemeKinds
  pure
    CheckedSig
      { checkedSigName = userSigName userSig,
        checkedSigScheme = scheme,
        checkedSigSpan = userSigSpan userSig,
        checkedSigScopedNames = explicitForallNames (userSigType userSig)
      }

splitContext :: Type -> ([Type], Type)
splitContext (TAnn _ inner) = splitContext inner
splitContext (TContext preds inner) = (preds, inner)
splitContext ty = ([], ty)

makeInstanceTyVarEnv :: InstanceDecl -> [Type] -> TcM ([TyVarId], TvKindEnv)
makeInstanceTyVarEnv instanceDecl headArgTypes = do
  explicitParams <- makeParamEnv (instanceDeclForall instanceDecl)
  let explicitNames = map paramName explicitParams
      freeVars = nub (explicitNames <> concatMap freeTypeVars (instanceDeclContext instanceDecl <> headArgTypes))
      implicitNames = freeVars \\ explicitNames
      explicitEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- explicitParams]
  rawImplicitTyVars <- mapM freshSkolemTv implicitNames
  implicitKinds <- mapM (const freshKindMeta) implicitNames
  let implicitTyVars = zipWith setTyVarKind implicitKinds rawImplicitTyVars
      implicitEnv = Map.fromList (zip implicitNames (zip implicitTyVars implicitKinds))
  pure (map paramTyVar explicitParams <> implicitTyVars, explicitEnv <> implicitEnv)

checkInstanceHeadTypes :: Name -> TvKindEnv -> [Type] -> TcM [TcType]
checkInstanceHeadTypes className tvEnv headArgTypes = do
  argKinds <- classPredicateArgKinds className (length headArgTypes)
  zipWithM (checkSurfaceType tvEnv) headArgTypes argKinds

-- | Instantiate a type scheme with fresh skolems for type-checking while
-- preserving the scheme predicates as scoped givens for the checked body.
-- Unlike regular instantiation (which uses metas), this produces rigid
-- type variables that cannot be unified during constraint solving.
skolemizeQualified :: TypeScheme -> TcM ([TyVarId], [Pred], TcType)
skolemizeQualified (ForAll tvs preds body) = do
  (skolems, subst) <- foldM extendSubst ([], Map.empty) tvs
  pure (skolems, map (applySubstPred subst) preds, applySubst subst body)
  where
    extendSubst (skolems, subst) tv = do
      rawSkolem <- freshSkolemTv (tvName tv)
      let skolem = setTyVarKind (applySubst subst (tvKind tv)) rawSkolem
      pure (skolems <> [skolem], Map.insert (tvUnique tv) (TcTyVar skolem) subst)

-- | Split a function type into argument types and result type.
splitFunTy :: TcType -> Int -> ([TcType], TcType)
splitFunTy ty 0 = ([], ty)
splitFunTy (TcFunTy a rest) n =
  let (args, res) = splitFunTy rest (n - 1)
   in (a : args, res)
splitFunTy ty _ = ([], ty)

-- | A group of declarations that should be typechecked together.
-- Multiple FunctionBind equations for the same name are merged.
data DeclGroup
  = SingleDecl Decl
  | MergedFunctionBind SourceSpan UnqualifiedName [Decl] [Match]

data DeclGraphKey
  = DeclGraphBinder !TcTermKey
  | DeclGraphSynthetic !Int
  deriving (Eq, Ord, Show)

-- | Group consecutive FunctionBind declarations with the same name.
groupValueDecls :: [Decl] -> [DeclGroup]
groupValueDecls [] = []
groupValueDecls (d : ds) = case extractFunctionBind d of
  Just (sp, name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        groupDecls = d : sameNameDecls
        allMatches = matches ++ concatMap (maybe [] (\(_, _, ms) -> ms) . extractFunctionBind) sameNameDecls
     in MergedFunctionBind sp name groupDecls allMatches : groupValueDecls rest
  Nothing -> SingleDecl d : groupValueDecls ds

-- | Extract function bind info from a declaration.
extractFunctionBind :: Decl -> Maybe (SourceSpan, UnqualifiedName, [Match])
extractFunctionBind decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) ->
      let sp = peelDeclSpan NoSourceSpan decl
       in Just (sp, name, matches)
    _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: UnqualifiedName -> Decl -> Bool
hasSameName name d = case extractFunctionBind d of
  Just (_, n, _) -> unqualifiedNameText n == unqualifiedNameText name
  Nothing -> False

-- | Sort top-level groups so acyclic forward references are checked after
-- their dependencies have been generalized into the global environment.
sortDeclGroups :: [(Int, DeclGroup)] -> TcM [(Int, DeclGroup)]
sortDeclGroups groups = do
  -- A group can bind more than one name, so a dependency edge goes to the
  -- graph key of the group that binds the name, not to the name itself.
  keyed <- mapM groupNodeKeys groups
  let owners = Map.fromList [(binder, nodeKey) | (_, nodeKey, binders) <- keyed, binder <- binders]
  nodes <- mapM (declGraphNode owners) keyed
  pure (concatMap flattenScc (stronglyConnComp nodes))
  where
    groupNodeKeys numberedGroup@(groupId, group) = do
      nodeKey <- groupKey groupId group
      binders <- declGroupBinderKeys group
      pure (numberedGroup, nodeKey, binders)
    flattenScc (AcyclicSCC group) = [group]
    flattenScc (CyclicSCC cyclicGroups) = cyclicGroups

declGraphNode :: Map TcTermKey DeclGraphKey -> ((Int, DeclGroup), DeclGraphKey, [TcTermKey]) -> TcM ((Int, DeclGroup), DeclGraphKey, [DeclGraphKey])
declGraphNode owners (numberedGroup, nodeKey, _) = do
  freeVars <- freeVarsGroup (snd numberedGroup)
  let deps = nub (mapMaybe (`Map.lookup` owners) (Set.toList freeVars))
  pure (numberedGroup, nodeKey, deps)

groupKey :: Int -> DeclGroup -> TcM DeclGraphKey
groupKey ix group = do
  keys <- declGroupBinderKeys group
  case keys of
    key : _ -> pure (DeclGraphBinder key)
    [] -> pure (DeclGraphSynthetic ix)

declGroupBinderKeys :: DeclGroup -> TcM [TcTermKey]
declGroupBinderKeys group =
  case group of
    MergedFunctionBind _sp binder _decls _matches -> (: []) <$> resolvedUnqualifiedTermKey binder
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (FunctionBind binder _) -> (: []) <$> resolvedUnqualifiedTermKey binder
        DeclValue (PatternBind _ pat _) -> maybe (pure []) (fmap (: []) . resolvedUnqualifiedTermKey) (patternBinderSyntaxName pat)
        -- The record fields of a pattern synonym are top-level binders of
        -- the same group, so a use of a field selector orders its group
        -- after the pattern synonym.
        DeclPatSyn patSyn -> do
          key <- resolvedUnqualifiedTermKey (patSynDeclName patSyn)
          fieldKeys <- mapM (patSynFieldTermKey key) (patSynRecordFields (patSynDeclArgs patSyn))
          pure (key : fieldKeys)
        _ -> pure []

freeVarsGroup :: DeclGroup -> TcM (Set.Set TcTermKey)
freeVarsGroup group =
  case group of
    MergedFunctionBind _sp binder _decls matches -> do
      vars <- Set.unions <$> mapM freeVarsMatch matches
      binderKey <- resolvedUnqualifiedTermKey binder
      pure (Set.delete binderKey vars)
    SingleDecl decl -> freeVarsDecl decl

renderDeclGroup :: DeclGroup -> [Decl]
renderDeclGroup group =
  case group of
    SingleDecl decl -> [decl]
    MergedFunctionBind _ _ decls _ -> decls

replaceFunctionDeclMatches :: [Match] -> [Decl] -> [Decl]
replaceFunctionDeclMatches matches decls =
  snd (mapAccumL replace matches decls)
  where
    replace remaining decl =
      let count = functionDeclMatchCount decl
          (here, rest) = splitAt count remaining
       in (rest, replaceDeclFunctionMatches here decl)

functionDeclMatchCount :: Decl -> Int
functionDeclMatchCount decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind _ matches) -> length matches
    _ -> 0

replaceDeclFunctionMatches :: [Match] -> Decl -> Decl
replaceDeclFunctionMatches matches decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (replaceDeclFunctionMatches matches inner)
    DeclValue (FunctionBind name _) -> DeclValue (FunctionBind name matches)
    _ -> decl

replacePatternBindRhs :: Rhs Expr -> Decl -> Decl
replacePatternBindRhs rhs decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (replacePatternBindRhs rhs inner)
    DeclValue (PatternBind mult pat _) -> DeclValue (PatternBind mult pat rhs)
    _ -> decl

replaceInstancePatternBindRhs :: Rhs Expr -> InstanceDeclItem -> InstanceDeclItem
replaceInstancePatternBindRhs rhs item =
  case item of
    InstanceItemAnn ann inner -> InstanceItemAnn ann (replaceInstancePatternBindRhs rhs inner)
    InstanceItemBind (PatternBind mult pat _) -> InstanceItemBind (PatternBind mult pat rhs)
    _ -> item

patternBinders :: Pattern -> [Text]
patternBinders = map unqualifiedNameText . patternBinderNames

-- | Type-check a declaration group.
tcDeclGroup :: Map TcTermKey CheckedSig -> (Int, DeclGroup) -> TcM TcDeclGroupResult
tcDeclGroup sigs (groupId, group) =
  case group of
    SingleDecl d -> tcSingleDeclGroup sigs groupId d
    MergedFunctionBind _sp binder decls matches -> tcMergedFunctionGroup sigs groupId binder decls matches

tcSingleDeclGroup :: Map TcTermKey CheckedSig -> Int -> Decl -> TcM TcDeclGroupResult
tcSingleDeclGroup sigs groupId d =
  case peelDeclAnn d of
    DeclValue (PatternBind _ pat rhs) ->
      case patternBinderSyntaxName pat of
        Just binder -> do
          key <- resolvedUnqualifiedTermKey binder
          let displayName = renderBinderName binder
              name = unqualifiedNameText binder
          (maybeMatches, bindings) <-
            case Map.lookup key sigs of
              Just sig ->
                tcFunctionWithSig displayName name sig [zeroArgMatch (patternSpan pat `orSourceSpan` peelDeclSpan NoSourceSpan d) rhs]
              Nothing ->
                tcFunctionInfer key displayName name [zeroArgMatch (patternSpan pat) rhs]
          let annotatedDecls = fmap (\case [match] -> [replacePatternBindRhs (matchRhs match) d]; _ -> [d]) maybeMatches
          pure (TcDeclGroupResult groupId bindings annotatedDecls)
        Nothing -> tcTopLevelPatternBind sigs groupId d pat rhs
    DeclPatSyn patSyn -> tcPatSynDecl sigs groupId d patSyn
    _ -> do
      bindings <- tcDecl d
      pure (TcDeclGroupResult groupId bindings Nothing)

-- | Type-check a top-level pattern binding that binds several variables,
-- such as @(low, high) = range x@. The monomorphism restriction applies:
-- the binders get the monomorphic types that the pattern gives them, and a
-- signature must be a monomorphic type.
tcTopLevelPatternBind :: Map TcTermKey CheckedSig -> Int -> Decl -> Pattern -> Rhs Expr -> TcM TcDeclGroupResult
tcTopLevelPatternBind sigs groupId d pat rhs = do
  let sp = patternSpan pat `orSourceSpan` peelDeclSpan NoSourceSpan d
      binders = patternBinderNames pat
  -- A binder with a signature is already in the environment. The others
  -- get a placeholder that the checked pattern fills in.
  placeholders <- forM binders $ \binder -> do
    key <- resolvedUnqualifiedTermKey binder
    let name = unqualifiedNameText binder
    case Map.lookup key sigs of
      Just sig -> do
        ty <- monomorphicSigType sig
        pure (binder, key, ty, True)
      Nothing -> do
        ty <- freshMetaTv
        extendTermEnvPermanent name (TcMonoIdBinder ty)
        extendTermKeyEnvPermanent key (TcMonoIdBinder ty)
        pure (binder, key, ty, False)
  ((rhs', rhsTy, pat'), failed) <-
    withErrorTracking $ do
      (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr rhs
      patCheck <- checkPattern sp pat rhsTy
      tieCts <- forM (pcBindings patCheck) $ \(name, ty) ->
        case [placeholderTy | (binder, _, placeholderTy, _) <- placeholders, binder == name] of
          placeholderTy : _ -> do
            ev <- freshEvVar
            pure [mkWantedCt (EqPred placeholderTy ty) ev (LetOrigin sp) sp]
          [] -> pure []
      solveResult <- solveWithImpls (rhsCts <> pcWantedCts patCheck <> concat tieCts) []
      residualPreds <- generalizableResidualPreds rhsTy solveResult
      -- The monomorphism restriction leaves a pattern binding without
      -- quantified constraints.
      forM_ residualPreds $ \predicate ->
        emitError sp (UnsolvedWanted predicate (LetOrigin sp))
      pure (rhs', rhsTy, annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck))
  if failed
    then pure (TcDeclGroupResult groupId [] Nothing)
    else do
      results <- forM placeholders $ \(binder, key, ty, hasSig) -> do
        zonkedTy <- zonkType ty
        let name = unqualifiedNameText binder
        unless hasSig $
          finalizeInferredTermEnvPermanent name key ty (ForAll [] [] zonkedTy)
        pure (TcBindingResult name (renderBinderName binder) zonkedTy)
      zonkedRhsTy <- zonkType rhsTy
      let decl' = replacePatternBind pat' rhs' d
          patternResult = TcBindingResult (patternBindingResultName pat) "<pattern>" zonkedRhsTy
      pure (TcDeclGroupResult groupId (patternResult : results) (Just [decl']))
  where
    monomorphicSigType sig =
      case checkedSigScheme sig of
        ForAll [] [] ty -> pure ty
        scheme -> do
          emitError (checkedSigSpan sig) (OtherError ("the signature of a pattern binding must be a monomorphic type: " <> T.unpack (checkedSigName sig)))
          pure (typeSchemeBody scheme)

-- | The binding-result name that carries the type of the right-hand side
-- of a top-level pattern binding with several binders.
patternBindingResultName :: Pattern -> Text
patternBindingResultName pat = "<pattern " <> T.unwords (patternBinders pat) <> ">"

replacePatternBind :: Pattern -> Rhs Expr -> Decl -> Decl
replacePatternBind pat rhs decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (replacePatternBind pat rhs inner)
    DeclValue (PatternBind mult _ _) -> DeclValue (PatternBind mult pat rhs)
    _ -> decl

-- | Type-check a pattern synonym declaration.
--
-- The matcher @$mP scrutinee continue fail = case scrutinee of { pat ->
-- continue x1 .. xn; _ -> fail }@ is a synthesized function. Its type
-- gives the pattern synonym type @x1 -> .. -> xn -> scrutinee@. The
-- builder @$bP@ is the right-hand side pattern as an expression, or the
-- explicit builder equations. It is checked against the pattern synonym
-- type. The checked pattern and the checked builder equations replace the
-- source forms in the declaration.
tcPatSynDecl :: Map TcTermKey CheckedSig -> Int -> Decl -> PatSynDecl -> TcM TcDeclGroupResult
tcPatSynDecl sigs groupId decl patSyn = do
  let binder = patSynDeclName patSyn
      name = unqualifiedNameText binder
      displayName = renderBinderName binder
      nameSpan = patSynBinderSpan binder
      argNames = patSynArgNames (patSynDeclArgs patSyn)
      arity = length argNames
      pat = patSynDeclPat patSyn
      failedResult = TcDeclGroupResult groupId [] Nothing
  key <- resolvedUnqualifiedTermKey binder
  (package, moduleName') <-
    case key of
      TcTermGlobal package moduleName' _ -> pure (package, moduleName')
      TcTermLocal {} -> abortTc ("pattern synonym " <> T.unpack name <> " is not a top-level binder")
  case mapM (`patternVarBinder` pat) argNames of
    Nothing -> do
      emitError nameSpan (OtherError ("pattern synonym " <> T.unpack name <> " has an argument that its pattern does not bind"))
      pure failedResult
    Just argBinders -> do
      let matcherName = "$m" <> name
          builderName = "$b" <> name
          matcherKey = TcTermGlobal package moduleName' matcherName
          builderKey = TcTermGlobal package moduleName' builderName
          matcherMatch = patSynMatcherMatch pat argBinders
      maybeLayout <-
        case Map.lookup key sigs of
          Just sig -> patSynLayoutFromSig name arity sig
          Nothing -> inferPatSynLayout nameSpan name pat argBinders
      case maybeLayout of
        Nothing -> pure failedResult
        Just layout -> do
          let scheme = patSynLayoutScheme layout
          when (Map.notMember key sigs) $
            registerCheckedSig key (CheckedSig name scheme nameSpan [])
          matcherSig <- patSynMatcherSig matcherName nameSpan layout
          registerCheckedSig matcherKey matcherSig
          (maybeMatcherMatches, matcherResults) <- tcFunctionWithSig matcherName matcherName matcherSig [matcherMatch]
          commitCheckedHelper matcherKey matcherName matcherResults
          case maybeMatcherMatches of
            Just [matcherMatch'] -> do
              checkedPat <- maybe (abortTc ("pattern synonym " <> T.unpack name <> " lost its checked pattern")) pure (matcherPattern matcherMatch')
              let builderSig = CheckedSig builderName scheme nameSpan []
              (direction, sourceBuilder) <-
                case patSynDeclDir patSyn of
                  PatSynUnidirectional -> pure (PatSynUnidirectionalInfo, Just Nothing)
                  PatSynExplicitBidirectional matches -> pure (PatSynExplicitBidirectionalInfo, Just (Just matches))
                  PatSynBidirectional ->
                    case patternToExpr pat of
                      Just expr ->
                        pure (PatSynImplicitBidirectionalInfo, Just (Just [Match [] MatchHeadPrefix (map PVar argBinders) (UnguardedRhs [] expr Nothing)]))
                      Nothing -> do
                        emitError nameSpan (OtherError ("the pattern of the bidirectional pattern synonym " <> T.unpack name <> " is not an expression; give explicit builder equations"))
                        pure (PatSynImplicitBidirectionalInfo, Nothing)
              case sourceBuilder of
                Nothing -> pure failedResult
                Just builderMatches -> do
                  (maybeBuilderMatches, builderResults) <-
                    case builderMatches of
                      Nothing -> pure (Nothing, [])
                      Just matches -> do
                        registerCheckedSig builderKey builderSig
                        checked <- tcFunctionWithSig builderName builderName builderSig matches
                        commitCheckedHelper builderKey builderName (snd checked)
                        pure checked
                  case (builderMatches, maybeBuilderMatches) of
                    (Just _, Nothing) -> pure failedResult
                    _ -> do
                      (selectorMatches, selectorResults) <-
                        tcPatSynRecordSelectors package moduleName' nameSpan layout (patSynDeclArgs patSyn) pat argBinders
                      addPatSyn
                        PatSynInfo
                          { psiName = name,
                            psiOrigin = (package, moduleName'),
                            psiArity = arity,
                            psiDirection = direction,
                            psiScheme = scheme,
                            psiReqTheta = patSynLayoutRequired layout,
                            psiProvTheta = patSynLayoutProvided layout
                          }
                      zonkedTy <- zonkType (schemeToType scheme)
                      -- The source keeps the explicit builder equations for the
                      -- annotated output. The synthesized builder of an
                      -- implicit pattern synonym stays in the annotation.
                      let dir' =
                            case (patSynDeclDir patSyn, maybeBuilderMatches) of
                              (PatSynExplicitBidirectional {}, Just matches) -> PatSynExplicitBidirectional matches
                              (dir, _) -> dir
                          -- The annotated output needs a span on the checked pattern.
                          spannedPat =
                            case patternSpan checkedPat of
                              NoSourceSpan -> checkedPat
                              patSpan -> PAnn (mkAnnotation patSpan) checkedPat
                          patSyn' = patSyn {patSynDeclPat = spannedPat, patSynDeclDir = dir'}
                          annotation = TcPatSynAnnotation matcherMatch' maybeBuilderMatches selectorMatches
                          decl' = DeclAnn (mkAnnotation annotation) (replacePatSynDecl patSyn' decl)
                          results = TcBindingResult name displayName zonkedTy : matcherResults <> builderResults <> selectorResults
                      pure (TcDeclGroupResult groupId results (Just [decl']))
            _ -> pure failedResult

-- | A pattern synonym bundled with a type in an export list must have that
-- type as its scrutinee type.
checkBundledPatSyns :: Module -> TcM ()
checkBundledPatSyns modu =
  mapM_ (go NoSourceSpan) (fromMaybe [] (moduleExports modu))
  where
    go sp spec =
      case spec of
        ExportAnn ann inner -> go (fromMaybe sp (fromAnnotation ann)) inner
        ExportWith _ _ typeName members -> mapM_ (checkMember sp (nameText typeName)) members
        ExportWithAll _ _ typeName _ members -> mapM_ (checkMember sp (nameText typeName)) members
        _ -> pure ()
    checkMember sp typeName member = do
      let memberName = nameText (ieBundledMemberName member)
      patSyns <- getPatSyns
      forM_ [info | info <- patSyns, psiName info == memberName] $ \info ->
        case patSynResultTyConName info of
          Just resultName
            | resultName /= typeName ->
                emitError sp (OtherError ("pattern synonym " <> T.unpack memberName <> " has the scrutinee type " <> T.unpack resultName <> " and cannot be bundled with " <> T.unpack typeName))
          _ -> pure ()
    patSynResultTyConName info =
      let ForAll _ _ body = psiScheme info
       in case resultType body of
            TcTyCon tyCon _ -> Just (tyConName tyCon)
            _ -> Nothing
    resultType ty =
      case ty of
        TcFunTy _ result -> resultType result
        _ -> ty

-- | The parts of a pattern synonym type
-- @forall univ. req => forall ex. prov => x1 -> .. -> xn -> scrutinee@.
data PatSynLayout = PatSynLayout
  { patSynLayoutUniversals :: ![TyVarId],
    patSynLayoutExistentials :: ![TyVarId],
    patSynLayoutRequired :: ![Pred],
    patSynLayoutProvided :: ![Pred],
    patSynLayoutArgTypes :: ![TcType],
    patSynLayoutResultType :: !TcType
  }

-- | The constructor-like scheme of a pattern synonym. The predicates are
-- the required predicates and then the provided predicates.
patSynLayoutScheme :: PatSynLayout -> TypeScheme
patSynLayoutScheme layout =
  ForAll
    (patSynLayoutUniversals layout <> patSynLayoutExistentials layout)
    (patSynLayoutRequired layout <> patSynLayoutProvided layout)
    (foldr TcFunTy (patSynLayoutResultType layout) (patSynLayoutArgTypes layout))

-- | The layout of a pattern synonym signature. A signature
-- @req => prov => body@ gives a scheme with the required context and a
-- qualified body with the provided context. A variable that the scrutinee
-- type mentions is universal. The other variables are existential.
patSynLayoutFromSig :: Text -> Int -> CheckedSig -> TcM (Maybe PatSynLayout)
patSynLayoutFromSig name arity sig = do
  let ForAll tyVars required qualifiedBody = checkedSigScheme sig
      (provided, body) =
        case qualifiedBody of
          TcQualTy predicates inner -> (predicates, inner)
          _ -> ([], qualifiedBody)
      (argTys, resultType) = splitFunTy body arity
      (universals, existentials) = partition (`typeMentionsTyVar` resultType) tyVars
  if length argTys /= arity
    then do
      emitError (checkedSigSpan sig) (OtherError ("pattern synonym signature for " <> T.unpack name <> " does not have " <> show arity <> " arguments"))
      pure Nothing
    else
      pure
        ( Just
            PatSynLayout
              { patSynLayoutUniversals = universals,
                patSynLayoutExistentials = existentials,
                patSynLayoutRequired = required,
                patSynLayoutProvided = provided,
                patSynLayoutArgTypes = argTys,
                patSynLayoutResultType = resultType
              }
        )

-- | Infer the layout of a pattern synonym from its pattern. The pattern
-- binds the argument types. The unsolved constraints of the pattern are
-- required. The class constraints that constructors in the pattern give
-- are provided, and their skolems are existential.
inferPatSynLayout :: SourceSpan -> Text -> Pattern -> [UnqualifiedName] -> TcM (Maybe PatSynLayout)
inferPatSynLayout sp name pat argBinders = do
  scrutTy <- freshMetaTv
  ((patCheck, argTys, residual), failed) <-
    withErrorTracking $ do
      patCheck <- checkPattern sp pat scrutTy
      argTys <- mapM (argumentType patCheck) argBinders
      residual <-
        if null (pcGivenCts patCheck) && null (pcSkolems patCheck)
          then do
            solveResult <- solveWithImpls (pcWantedCts patCheck) []
            -- The scrutinee and the argument types carry every meta-variable
            -- that the pattern synonym quantifies over.
            generalizableResidualPreds (foldr TcFunTy scrutTy argTys) solveResult
          else do
            _ <- solvePatternBranch sp patCheck scrutTy []
            pure []
      pure (patCheck, argTys, residual)
  if failed
    then pure Nothing
    else do
      ForAll universals required body <- generalizeAndCommit (foldr TcFunTy scrutTy argTys) residual
      provided <- mapM zonkPred [ctPred ct | ct <- pcGivenCts patCheck, isClassPredicate (ctPred ct)]
      let (argTys', resultType) = splitFunTy body (length argTys)
      pure
        ( Just
            PatSynLayout
              { patSynLayoutUniversals = universals,
                patSynLayoutExistentials = pcSkolems patCheck,
                patSynLayoutRequired = required,
                patSynLayoutProvided = provided,
                patSynLayoutArgTypes = argTys',
                patSynLayoutResultType = resultType
              }
        )
  where
    argumentType patCheck binder =
      case [ty | (bound, ty) <- pcBindings patCheck, unqualifiedNameText bound == unqualifiedNameText binder] of
        ty : _ -> pure ty
        [] -> abortTc ("pattern synonym " <> T.unpack name <> " does not bind " <> T.unpack (unqualifiedNameText binder))
    isClassPredicate predicate =
      case predicate of
        ClassPred {} -> True
        _ -> False

-- | The matcher signature
-- @forall univ r. req => scrutinee -> (forall ex. prov => x1 -> .. -> xn -> r) -> r -> r@.
patSynMatcherSig :: Text -> SourceSpan -> PatSynLayout -> TcM CheckedSig
patSynMatcherSig matcherName sp layout = do
  result <- freshSkolemTv "r"
  let resultTy = TcTyVar result
      continuationBody = foldr TcFunTy resultTy (patSynLayoutArgTypes layout)
      qualifiedContinuation =
        case patSynLayoutProvided layout of
          [] -> continuationBody
          provided -> TcQualTy provided continuationBody
      continuation = foldr TcForAllTy qualifiedContinuation (patSynLayoutExistentials layout)
      matcherTy = TcFunTy (patSynLayoutResultType layout) (TcFunTy continuation (TcFunTy resultTy resultTy))
  pure (CheckedSig matcherName (ForAll (patSynLayoutUniversals layout <> [result]) (patSynLayoutRequired layout) matcherTy) sp [])

-- | Give a checked matcher or builder the type of its checked body. The
-- signature check closes the body over fresh skolems, and the desugarer
-- reads the exported type.
commitCheckedHelper :: TcTermKey -> Text -> [TcBindingResult] -> TcM ()
commitCheckedHelper key name results =
  case [tbType result | result <- results, tbName result == name] of
    ty : _ -> do
      let binder = TcIdBinder (typeToScheme ty) Closed
      replaceTermKeyEnvPermanent key binder
      replaceTermKeyEnvPermanent (unqualifiedTermKey name) binder
    [] -> pure ()

-- | The field labels of a record pattern synonym. Other forms have none.
patSynRecordFields :: PatSynArgs -> [Text]
patSynRecordFields args =
  case args of
    PatSynRecordArgs fields -> fields
    _ -> []

-- | A field selector of a record pattern synonym lives in the module of
-- the synonym.
patSynFieldTermKey :: TcTermKey -> Text -> TcM TcTermKey
patSynFieldTermKey key field =
  case key of
    TcTermGlobal package moduleName' _ -> pure (TcTermGlobal package moduleName' field)
    TcTermLocal {} -> abortTc ("record pattern synonym field " <> T.unpack field <> " is not a top-level binder")

-- | Check the field selectors of a record pattern synonym. The selector
-- of the field @f@ that the argument binder @x@ names is the function
-- @f $scrutinee = case $scrutinee of pat -> x@. Its signature quantifies
-- the universal variables and keeps the required context, so it has the
-- type @req => scrutinee -> field@. A field whose type mentions an
-- existential variable has no selector.
tcPatSynRecordSelectors :: PackageId -> Text -> SourceSpan -> PatSynLayout -> PatSynArgs -> Pattern -> [UnqualifiedName] -> TcM ([(Text, Match)], [TcBindingResult])
tcPatSynRecordSelectors package moduleName' nameSpan layout args pat argBinders = do
  checked <- sequence (zipWith3 selector (patSynRecordFields args) argBinders (patSynLayoutArgTypes layout))
  pure (concatMap fst checked, concatMap snd checked)
  where
    selector field argBinder argType
      | any (`typeMentionsTyVar` argType) (patSynLayoutExistentials layout) = do
          emitError nameSpan (OtherError ("the field " <> T.unpack field <> " of a record pattern synonym has an existential type, so it has no selector"))
          pure ([], [])
      | otherwise = do
          let key = TcTermGlobal package moduleName' field
              scheme = ForAll (patSynLayoutUniversals layout) (patSynLayoutRequired layout) (TcFunTy (patSynLayoutResultType layout) argType)
              sig = CheckedSig field scheme nameSpan []
          registerCheckedSig key sig
          (maybeMatches, results) <- tcFunctionWithSig field field sig [patSynSelectorMatch pat argBinder]
          commitCheckedHelper key field results
          case maybeMatches of
            Just [match] -> pure ([(field, match)], results)
            _ -> pure ([], results)

-- | The equation of a record pattern synonym field selector.
patSynSelectorMatch :: Pattern -> UnqualifiedName -> Match
patSynSelectorMatch pat argBinder =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = [PVar scrutinee],
      matchRhs =
        UnguardedRhs
          []
          (ECase (localVar scrutinee) [CaseAlt [] pat (UnguardedRhs [] (localVar argBinder) Nothing)])
          Nothing
    }
  where
    scrutinee = synthesizedLocal (-1) "$scrutinee"

patSynArgNames :: PatSynArgs -> [Text]
patSynArgNames args =
  case args of
    PatSynPrefixArgs names -> names
    PatSynInfixArgs left right -> [left, right]
    PatSynRecordArgs fields -> fields

-- | The span of a pattern synonym binder. The resolver gives the name its
-- definition span.
patSynBinderSpan :: UnqualifiedName -> SourceSpan
patSynBinderSpan binder =
  unqualifiedNameSpan binder
    `orSourceSpan` case [resolutionSpan resolution | Just resolution <- map fromAnnotation (unqualifiedNameAnns binder)] of
      sp : _ -> sp
      [] -> NoSourceSpan

-- | The binder in a pattern that has the given name.
patternVarBinder :: Text -> Pattern -> Maybe UnqualifiedName
patternVarBinder target = go
  where
    go pat =
      case pat of
        PVar name
          | unqualifiedNameText name == target -> Just name
          | otherwise -> Nothing
        PAs name inner
          | unqualifiedNameText name == target -> Just name
          | otherwise -> go inner
        PAnn _ inner -> go inner
        PParen inner -> go inner
        PStrict inner -> go inner
        PIrrefutable inner -> go inner
        PTypeSig inner _ -> go inner
        PView _ inner -> go inner
        PUnboxedSum _ _ inner -> go inner
        PList items -> firstJust items
        PTuple _ items -> firstJust items
        PCon _ _ items -> firstJust items
        PBuiltinCon _ _ items -> firstJust items
        PInfix left _ right -> firstJust [left, right]
        PRecord _ fields _ -> firstJust (map recordFieldValue fields)
        _ -> Nothing
    firstJust = listToMaybe . mapMaybe go

-- | A local binder that the type checker makes. The negative unique does
-- not collide with a resolver local.
synthesizedLocal :: Int -> Text -> UnqualifiedName
synthesizedLocal unique text =
  UnqualifiedName
    NameVarId
    text
    [mkAnnotation (ResolutionAnnotation NoSourceSpan (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedLocal unique (mkUnqualifiedName NameVarId text)))]

localVar :: UnqualifiedName -> Expr
localVar = EVar . qualifyName Nothing

patSynMatcherMatch :: Pattern -> [UnqualifiedName] -> Match
patSynMatcherMatch pat argBinders =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = map PVar [scrutinee, continue, failure],
      matchRhs =
        UnguardedRhs
          []
          ( ECase
              (localVar scrutinee)
              [ CaseAlt [] pat (UnguardedRhs [] success Nothing),
                CaseAlt [] PWildcard (UnguardedRhs [] (localVar failure) Nothing)
              ]
          )
          Nothing
    }
  where
    scrutinee = synthesizedLocal (-1) "$scrutinee"
    continue = synthesizedLocal (-2) "$continue"
    failure = synthesizedLocal (-3) "$failure"
    success = foldl EApp (localVar continue) (map localVar argBinders)

-- | The checked pattern inside a checked matcher equation.
matcherPattern :: Match -> Maybe Pattern
matcherPattern match =
  case matchRhs match of
    UnguardedRhs _ expr _ -> go expr
    GuardedRhss {} -> Nothing
  where
    go expr =
      case expr of
        EAnn _ inner -> go inner
        EParen inner -> go inner
        EPragma _ inner -> go inner
        ECase _ (alt : _) -> Just (caseAltPattern alt)
        _ -> Nothing

-- | The pattern of an implicitly bidirectional pattern synonym as an
-- expression.
patternToExpr :: Pattern -> Maybe Expr
patternToExpr pat =
  case pat of
    PAnn ann inner -> EAnn ann <$> patternToExpr inner
    PVar name -> Just (localVar name)
    PLit literal -> Just (literalToExpr literal)
    PTuple flavor items -> ETuple flavor <$> mapM (fmap Just . patternToExpr) items
    PList items -> EList <$> mapM patternToExpr items
    PCon name _ items -> foldl EApp (EVar name) <$> mapM patternToExpr items
    PBuiltinCon (BuiltinTuple flavor arity) _ items
      | length items == arity -> ETuple flavor <$> mapM (fmap Just . patternToExpr) items
    PInfix left name right -> EApp . EApp (EVar name) <$> patternToExpr left <*> patternToExpr right
    PParen inner -> EParen <$> patternToExpr inner
    PStrict inner -> patternToExpr inner
    PTypeSig inner ty -> (`ETypeSig` ty) <$> patternToExpr inner
    _ -> Nothing

literalToExpr :: Literal -> Expr
literalToExpr literal =
  case literal of
    LitAnn ann inner -> EAnn ann (literalToExpr inner)
    LitInt value numericType source -> EInt value numericType source
    LitFloat value floatType source -> EFloat value floatType source
    LitChar value source -> EChar value source
    LitCharHash value source -> ECharHash value source
    LitString value source -> EString value source
    LitStringHash value source -> EStringHash value source

replacePatSynDecl :: PatSynDecl -> Decl -> Decl
replacePatSynDecl patSyn decl =
  case decl of
    DeclAnn ann inner -> DeclAnn ann (replacePatSynDecl patSyn inner)
    DeclPatSyn {} -> DeclPatSyn patSyn
    _ -> decl

tcMergedFunctionGroup :: Map TcTermKey CheckedSig -> Int -> UnqualifiedName -> [Decl] -> [Match] -> TcM TcDeclGroupResult
tcMergedFunctionGroup sigs groupId binder decls matches = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  key <- resolvedUnqualifiedTermKey binder
  (maybeMatches, bindings) <- case Map.lookup key sigs of
    Just sig -> do
      -- Use the declared type signature for checking.
      tcFunctionWithSig displayName name sig matches
    Nothing -> do
      -- No signature: infer the type.
      tcFunctionInfer key displayName name matches
  let annotatedDecls = fmap (`replaceFunctionDeclMatches` decls) maybeMatches
  pure (TcDeclGroupResult groupId bindings annotatedDecls)

-- | Type-check a function with a known type signature.
-- The signature's type variables are opened as rigid skolems so that
-- the body is checked against them. GADT patterns generate implication
-- constraints using the signature's skolems as given equalities.
tcFunctionWithSig :: Text -> Text -> CheckedSig -> [Match] -> TcM (Maybe [Match], [TcBindingResult])
tcFunctionWithSig displayName name sig matches = do
  let scheme = checkedSigScheme sig
  ((skolems, sigPreds, sigTy, matches'), failed) <-
    withErrorTracking $ do
      -- Open the scheme with skolems (not metas) for checking.
      (skolems, sigPreds, sigTy) <- skolemizeQualified scheme
      let nArgs = case matches of
            (m : _) -> length (matchPats m)
            [] -> 0
          (argTys, resTy) = splitFunTy sigTy nArgs
      -- Check each equation against the signature types. The explicit
      -- forall variables scope over the equations.
      results <-
        withGivenPredicates sigPreds $
          withScopedTyVars (scopedSigTyVars (checkedSigScopedNames sig) skolems) $
            mapM (tcMatchEquation (Just (TypeSignatureOrigin (checkedSigName sig) (checkedSigSpan sig))) argTys resTy) matches
      let (_matches', ctsList, implsList) = unzip3 results
          allCts = concat ctsList
          allImpls = concat implsList
      solveBodyConstraintsWithGivens sigPreds allCts allImpls
      rejectEscapingExistentials sigTy allImpls
      pure (skolems, sigPreds, sigTy, _matches')
  if failed
    then pure (Nothing, [])
    else do
      -- Close the binding over the same skolems that occur in its checked body.
      let qualifiedTy
            | null sigPreds = sigTy
            | otherwise = TcQualTy sigPreds sigTy
          checkedTy = foldr TcForAllTy qualifiedTy skolems
      zonkedTy <- zonkType checkedTy
      pure (Just matches', [TcBindingResult name displayName zonkedTy])

-- | Type-check a function without a type signature (infer).
tcFunctionInfer :: TcTermKey -> Text -> Text -> [Match] -> TcM (Maybe [Match], [TcBindingResult])
tcFunctionInfer key displayName name matches = do
  placeholderTy <- freshMetaTv
  ((matches', ty, residualPreds), failed) <-
    withErrorTracking $ do
      extendTermEnvPermanent name (TcMonoIdBinder placeholderTy)
      extendTermKeyEnvPermanent key (TcMonoIdBinder placeholderTy)
      (matches', ty, cts', impls') <- tcMatches matches
      solveResult <- solveWithImpls cts' impls'
      rejectEscapingExistentials ty impls'
      residualPreds <- generalizableResidualPreds ty solveResult
      pure (matches', ty, residualPreds)
  if failed
    then pure (Nothing, [])
    else do
      scheme <- generalizeAndCommitIgnoring (Set.fromList [unqualifiedTermKey name, key]) ty residualPreds
      let schemeTy = schemeToType scheme
      zonkedTy <- zonkType schemeTy
      finalizeInferredTermEnvPermanent name key placeholderTy scheme
      pure (Just matches', [TcBindingResult name displayName zonkedTy])

generalizableResidualPreds :: TcType -> SolveResult -> TcM [Pred]
generalizableResidualPreds inferredType solveResult = do
  initialCts <- mapM zonkCtPred (srResidual solveResult <> inertDicts (srInerts solveResult))
  -- A meta-variable that the binding type or the environment mentions still
  -- becomes a quantified type variable, so defaulting must leave it alone.
  -- Anything else is ambiguous and the Haskell 2010 rule may make it
  -- concrete.
  keep <- generalizedMetaVars inferredType
  _ <- defaultAmbiguousMetas keep initialCts
  allResidualCts <- mapM zonkCtPred initialCts
  -- GHC never infers a HasCallStack constraint. An unsolved call-stack
  -- parameter gets the empty call stack.
  let (callStackCts, residualCts) = partition (isCallStackPred . ctPred) allResidualCts
  mapM_ reportUnsolvedDict callStackCts
  let uniqueResidualCts = nubBy sameCtPred residualCts
      (polymorphicCts, concreteCts) = partition (predicateCanGeneralize . ctPred) uniqueResidualCts
  -- Every occurrence still needs evidence, even when equal predicates share
  -- one constraint in the generalized type.
  forM_ residualCts $ \ct ->
    when (predicateCanGeneralize (ctPred ct)) $
      bindEvidence (ctEvVar ct) (EvGiven (ctPred ct))
  -- A fully concrete residual cannot be discharged by a caller-supplied
  -- dictionary, so reject it at the originating expression.
  forM_ concreteCts $ \ct ->
    emitError (ctLoc ct) (UnsolvedWanted (ctPred ct) (ctOrigin ct))
  pure (map ctPred polymorphicCts)
  where
    zonkCtPred ct = do
      pred' <- zonkPred (ctPred ct)
      pure (ct {ctPred = pred'})

    sameCtPred left right = ctPred left == ctPred right

-- | The meta-variables that generalization turns into quantified type
-- variables: those of the binding type plus those the environment mentions.
generalizedMetaVars :: TcType -> TcM [Unique]
generalizedMetaVars inferredType = do
  zonked <- zonkType inferredType
  envMetaVars <- environmentMetaVars Set.empty
  pure (collectMetaVars zonked <> envMetaVars)

predicateCanGeneralize :: Pred -> Bool
predicateCanGeneralize predicate =
  case predicate of
    -- A caller always supplies an implicit parameter, even at a concrete type.
    IParamPred {} -> True
    -- An equality that waits on a type family application is not a
    -- dictionary a caller can supply.
    EqPred {} -> False
    _ -> not (null (predMetaVars predicate))

rejectEscapingExistentials :: TcType -> [Implication] -> TcM ()
rejectEscapingExistentials outerType implications = do
  zonkedOuterType <- zonkType outerType
  let skolems = concatMap implSkols implications
      escaping = filter (`typeMentionsTyVar` zonkedOuterType) skolems
  unless (null escaping) $
    emitError
      NoSourceSpan
      ( OtherError
          ( "existential type variable escapes its pattern-match branch: "
              <> T.unpack (T.intercalate ", " (map tvName escaping))
          )
      )

typeMentionsTyVar :: TyVarId -> TcType -> Bool
typeMentionsTyVar target ty =
  case ty of
    TcTyVar tyVar -> tyVar == target || kindMentionsUnique (tvUnique target) (tvKind tyVar)
    TcMetaTv {} -> False
    TcTyCon _ arguments -> any (typeMentionsTyVar target) arguments
    TcFunTy argument result -> typeMentionsTyVar target argument || typeMentionsTyVar target result
    TcForAllTy tyVar body -> tyVar /= target && typeMentionsTyVar target body
    TcQualTy predicates body -> any (predicateMentionsTyVar target) predicates || typeMentionsTyVar target body
    TcAppTy function argument -> typeMentionsTyVar target function || typeMentionsTyVar target argument

kindMentionsUnique :: Unique -> TcType -> Bool
kindMentionsUnique target kind =
  case kind of
    TcTyVar tyVar -> tvUnique tyVar == target || kindMentionsUnique target (tvKind tyVar)
    TcMetaTv unique -> unique == target
    TcTyCon _ arguments -> any (kindMentionsUnique target) arguments
    TcFunTy argument result -> kindMentionsUnique target argument || kindMentionsUnique target result
    TcForAllTy tyVar body -> tvUnique tyVar /= target && kindMentionsUnique target body
    TcQualTy predicates body -> any (predicateMentionsUnique target) predicates || kindMentionsUnique target body
    TcAppTy function argument -> kindMentionsUnique target function || kindMentionsUnique target argument
  where
    predicateMentionsUnique unique predicate =
      case predicate of
        ClassPred _ arguments -> any (kindMentionsUnique unique) arguments
        EqPred left right -> kindMentionsUnique unique left || kindMentionsUnique unique right
        IParamPred _ payload -> kindMentionsUnique unique payload
        QuantifiedPred variables antecedents consequent ->
          all ((/= unique) . tvUnique) variables
            && (any (predicateMentionsUnique unique) antecedents || predicateMentionsUnique unique consequent)

predicateMentionsTyVar :: TyVarId -> Pred -> Bool
predicateMentionsTyVar target predicate =
  case predicate of
    ClassPred _ arguments -> any (typeMentionsTyVar target) arguments
    EqPred left right -> typeMentionsTyVar target left || typeMentionsTyVar target right
    IParamPred _ payload -> typeMentionsTyVar target payload
    QuantifiedPred variables antecedents consequent ->
      target `notElem` variables
        && (any (predicateMentionsTyVar target) antecedents || predicateMentionsTyVar target consequent)

zonkPred :: Pred -> TcM Pred
zonkPred pred' =
  case pred' of
    ClassPred className args -> ClassPred className <$> mapM zonkType args
    EqPred left right -> EqPred <$> zonkType left <*> zonkType right
    IParamPred name payload -> IParamPred name <$> zonkType payload
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred <$> mapM defaultTyVarKinds variables <*> mapM zonkPred antecedents <*> zonkPred consequent

collectStandaloneKindSignatures :: [Decl] -> Map TcTypeKey Type
collectStandaloneKindSignatures = Map.fromList . mapMaybe collect
  where
    collect declaration =
      case declaration of
        DeclAnn _ inner -> collect inner
        DeclStandaloneKindSig name kind -> (,kind) <$> resolvedTypeKey name
        _ -> Nothing

resolvedTypeKey :: UnqualifiedName -> Maybe TcTypeKey
resolvedTypeKey name = do
  ResolutionAnnotation {resolutionNamespace = namespace, resolutionTarget = ResolvedTopLevel packageId resolvedName} <- nameResolution name
  moduleName' <- nameQualifier resolvedName
  pure (packageId, moduleName', namespace, nameText resolvedName)

registerTypeDeclHeader :: Map TcTypeKey TypeScheme -> Decl -> TcM [TcBindingResult]
registerTypeDeclHeader kindSchemes (DeclData dataDecl) =
  registerDataDeclHeader (resolvedTypeKey (binderHeadName (dataDeclHead dataDecl)) >>= (`Map.lookup` kindSchemes)) dataDecl
registerTypeDeclHeader kindSchemes (DeclNewtype newtypeDecl) =
  registerNewtypeDeclHeader (resolvedTypeKey (binderHeadName (newtypeDeclHead newtypeDecl)) >>= (`Map.lookup` kindSchemes)) newtypeDecl
registerTypeDeclHeader kindSchemes (DeclDataFamilyDecl familyDecl) =
  registerDataFamilyDeclHeader (resolvedTypeKey (binderHeadName (dataFamilyDeclHead familyDecl)) >>= (`Map.lookup` kindSchemes)) familyDecl
registerTypeDeclHeader kindSchemes (DeclTypeFamilyDecl familyDecl) =
  registerTypeFamilyDeclHeader (typeFamilyHeadName (typeFamilyDeclHead familyDecl) >>= resolvedTypeKey >>= (`Map.lookup` kindSchemes)) familyDecl
registerTypeDeclHeader kindSchemes (DeclTypeSyn typeSynDecl) =
  registerTypeSynonymHeader (resolvedTypeKey (binderHeadName (typeSynHead typeSynDecl)) >>= (`Map.lookup` kindSchemes)) typeSynDecl
registerTypeDeclHeader kindSchemes (DeclClass classDecl) =
  concat <$> mapM (registerTypeDeclHeader kindSchemes . DeclTypeFamilyDecl) (classDeclTypeFamilies classDecl)
registerTypeDeclHeader kindSchemes (DeclAnn _ inner) = registerTypeDeclHeader kindSchemes inner
registerTypeDeclHeader _ _ = pure []

predeclareTypeConstructor :: Decl -> TcM ()
predeclareTypeConstructor declaration =
  case declaration of
    DeclAnn _ inner -> predeclareTypeConstructor inner
    DeclData dataDeclaration ->
      let binder = binderHeadName (dataDeclHead dataDeclaration)
          name = unqualifiedNameText binder
       in predeclare binder (if name == "List" then "[]" else name) (length (binderHeadParams (dataDeclHead dataDeclaration))) DataTyCon
    DeclNewtype newtypeDeclaration ->
      let binder = binderHeadName (newtypeDeclHead newtypeDeclaration)
       in predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (newtypeDeclHead newtypeDeclaration))) NewtypeTyCon
    DeclTypeSyn synonymDeclaration ->
      let binder = binderHeadName (typeSynHead synonymDeclaration)
       in predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (typeSynHead synonymDeclaration))) SynonymTyCon
    DeclDataFamilyDecl familyDeclaration ->
      let binder = binderHeadName (dataFamilyDeclHead familyDeclaration)
       in predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (dataFamilyDeclHead familyDeclaration))) DataFamilyTyCon
    DeclClass classDeclaration -> do
      let binder = binderHeadName (classDeclHead classDeclaration)
      predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (classDeclHead classDeclaration))) ClassTyCon
      mapM_ (predeclareTypeConstructor . DeclTypeFamilyDecl) (classDeclTypeFamilies classDeclaration)
    DeclTypeFamilyDecl familyDeclaration ->
      case typeFamilyHeadName (typeFamilyDeclHead familyDeclaration) of
        Just binder -> predeclare binder (unqualifiedNameText binder) (length (typeFamilyDeclParams familyDeclaration)) TypeFamilyTyCon
        Nothing -> pure ()
    _ -> pure ()
  where
    predeclare binder name arity flavor = do
      provisionalKind <-
        case flavor of
          ClassTyCon -> foldr KFun KConstraint <$> replicateM arity freshKindMeta
          _ -> freshKindMeta
      tyCon <- mkDeclaredTyCon binder name arity
      storeTyConInfo
        TyConInfo
          { tciName = name,
            tciArity = arity,
            tciTyCon = tyCon,
            tciKindScheme = ForAll [] [] provisionalKind,
            tciFlavor = flavor,
            tciTypeSynonym = Nothing
          }

storeTyConInfo :: TyConInfo -> TcM ()
storeTyConInfo info = do
  existing <- lookupTyConByIdentity (tciTyCon info)
  case existing of
    Just provisional -> do
      unifyKinds (typeSchemeBody (tciKindScheme provisional)) (typeSchemeBody (tciKindScheme info))
      replaceTyConEnvPermanent info
    Nothing -> extendTyConEnvPermanent info

registerStructuralDecl :: (Text, Text) -> Decl -> TcM [TcBindingResult]
registerStructuralDecl origin (DeclData dataDecl) = registerDataConstructors origin dataDecl
registerStructuralDecl origin (DeclNewtype newtypeDecl) = registerNewtypeConstructor origin newtypeDecl
registerStructuralDecl origin (DeclDataFamilyInst familyInst) = registerDataFamilyInstance origin familyInst
registerStructuralDecl origin (DeclTypeFamilyDecl familyDecl) = registerClosedTypeFamilyEquations origin familyDecl
registerStructuralDecl origin (DeclTypeFamilyInst familyInst) = registerTypeFamilyInstance origin familyInst
registerStructuralDecl origin (DeclClass classDecl) = registerClassDecl origin classDecl
registerStructuralDecl origin (DeclInstance instanceDecl) = registerInstanceDecl origin instanceDecl
registerStructuralDecl origin (DeclAnn _ inner) = registerStructuralDecl origin inner
registerStructuralDecl _ _ = pure []

isInstanceDecl :: Decl -> Bool
isInstanceDecl (DeclAnn _ inner) = isInstanceDecl inner
isInstanceDecl DeclInstance {} = True
isInstanceDecl _ = False

predeclareTypeLevelDataConstructors :: Decl -> TcM ()
predeclareTypeLevelDataConstructors declaration =
  case declaration of
    DeclAnn _ inner -> predeclareTypeLevelDataConstructors inner
    DeclData dataDeclaration -> do
      let parentBinder = binderHeadName (dataDeclHead dataDeclaration)
          parentName = unqualifiedNameText parentBinder
          parentArity = length (binderHeadParams (dataDeclHead dataDeclaration))
      parent <- dataDeclTyCon parentBinder parentName parentArity
      mapM_ (predeclareConstructor parent) (dataDeclConstructors dataDeclaration)
    DeclNewtype newtypeDeclaration -> do
      let parentBinder = binderHeadName (newtypeDeclHead newtypeDeclaration)
          parentName = unqualifiedNameText parentBinder
          parentArity = length (binderHeadParams (newtypeDeclHead newtypeDeclaration))
      parent <- mkDeclaredTyCon parentBinder parentName parentArity
      maybe (pure ()) (predeclareConstructor parent) (newtypeDeclConstructor newtypeDeclaration)
    _ -> pure ()
  where
    predeclareConstructor parent constructor = do
      let (_, fields, names) = dataConSourceLayout constructor
          arity = length fields
      mapM_ (predeclareName parent arity) names
    predeclareName parent arity name = do
      let isListConstructor =
            tyConModuleName parent == "GHC.Types"
              && tyConName parent == "[]"
              && name `elem` ["[]", ":"]
      kindScheme <-
        if isListConstructor
          then listDataConstructorKind name
          else ForAll [] [] <$> freshKindMeta
      let dataConTyCon =
            mkTyConWithNamespace
              ResolutionNamespaceTerm
              (tyConPackageId parent)
              (tyConModuleName parent)
              name
              arity
      storeTyConInfo
        TyConInfo
          { tciName = name,
            tciArity = arity,
            tciTyCon = dataConTyCon,
            tciKindScheme = kindScheme,
            tciFlavor = DataTyCon,
            tciTypeSynonym = Nothing
          }

isForeignImport :: ForeignDecl -> Bool
isForeignImport foreignDecl =
  foreignDirection foreignDecl == ForeignImport

registerClassDecl :: (Text, Text) -> ClassDecl -> TcM [TcBindingResult]
registerClassDecl origin classDecl = do
  let classBinder = binderHeadName (classDeclHead classDecl)
      className = unqualifiedNameText classBinder
      params = binderHeadParams (classDeclHead classDecl)
  kindParams <-
    if isTemplateHaskellLift origin className || (snd origin == "GHC.Types" && className == "~")
      then implicitBinderKindParams params
      else pure []
  let kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams]
  paramInfos <- makeParamEnvWith kindEnv params
  let paramTyVars = map paramTyVar paramInfos
      allClassTyVars = map paramTyVar kindParams <> paramTyVars
      paramKinds = map paramKind paramInfos
      paramTvEnv = kindEnv <> Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- paramInfos]
  superClassTypes <- mapM (\ty -> checkSurfaceType paramTvEnv ty KConstraint) (fromMaybe [] (classDeclContext classDecl))
  let classKind = foldr KFun KConstraint paramKinds
  classTyCon <- mkDeclaredTyCon classBinder className (length params)
  let classPred = ClassPred classTyCon (map TcTyVar paramTyVars)
  storeTyConInfo
    TyConInfo
      { tciName = className,
        tciArity = length params,
        tciTyCon = classTyCon,
        tciKindScheme = ForAll (map paramTyVar kindParams) [] classKind,
        tciFlavor = ClassTyCon,
        tciTypeSynonym = Nothing
      }
  methodResults <- concat <$> mapM (registerClassItem classPred paramTvEnv allClassTyVars) (classDeclItems classDecl)
  methods <- mapM registeredMethod (classDeclMethodNames classDecl)
  defaultSignatures <- catMaybes <$> mapM (registerClassDefaultSignature paramTvEnv allClassTyVars) (classDeclItems classDecl)
  let defaults = classDeclDefaultMethodNames classDecl
  defaultResults <- mapM (registerDefaultMethod defaults defaultSignatures) methods
  associatedTypes <-
    catMaybes
      <$> mapM
        (registerAssociatedTypeFamily origin (map tyVarBinderName params) (classDeclTypeFamilyDefaults classDecl))
        (classDeclTypeFamilies classDecl)
  addClass
    ClassInfo
      { ciName = className,
        ciTyCon = classTyCon,
        ciOrigin = Just origin,
        ciKindTyVars = map paramTyVar kindParams,
        ciTyVars = paramTyVars,
        ciSuperClassTypes = superClassTypes,
        ciMethods = methods,
        ciDefaultMethods = defaults,
        ciDefaultSignatures = defaultSignatures,
        ciAssociatedTypes = associatedTypes
      }
  pure (methodResults <> catMaybes defaultResults)
  where
    registeredMethod methodName = do
      binder <- lookupTerm methodName
      case binder of
        Just (TcIdBinder scheme _) -> pure (methodName, scheme)
        _ -> missingTypeInfo ("class method " <> T.unpack methodName)

    registerDefaultMethod defaults defaultSignatures (methodName, scheme)
      | methodName `elem` defaults = do
          let workerName = defaultMethodName methodName
              workerScheme = maybe scheme (defaultWorkerScheme scheme) (lookup methodName defaultSignatures)
              workerType = schemeToType workerScheme
          extendTermEnvPermanent workerName (TcIdBinder workerScheme Closed)
          pure (Just (TcBindingResult workerName workerName workerType))
      | otherwise = pure Nothing

    defaultWorkerScheme ordinaryScheme (ForAll tyVars predicates body) =
      case ordinaryScheme of
        ForAll _ (classPredicate : _) _ -> ForAll tyVars (classPredicate : predicates) body
        _ -> ForAll tyVars predicates body

-- | The associated type families that a class declares.
classDeclTypeFamilies :: ClassDecl -> [TypeFamilyDecl]
classDeclTypeFamilies classDecl = mapMaybe familyItem (classDeclItems classDecl)
  where
    familyItem item =
      case peelClassDeclItemAnn item of
        ClassItemTypeFamilyDecl familyDecl -> Just familyDecl
        _ -> Nothing

-- | The associated type family defaults that a class declares.
classDeclTypeFamilyDefaults :: ClassDecl -> [TypeFamilyInst]
classDeclTypeFamilyDefaults classDecl = mapMaybe defaultItem (classDeclItems classDecl)
  where
    defaultItem item =
      case peelClassDeclItemAnn item of
        ClassItemDefaultTypeInst familyInst -> Just familyInst
        _ -> Nothing

-- | The associated type family equations that an instance declares.
instanceDeclTypeFamilyInsts :: InstanceDecl -> [TypeFamilyInst]
instanceDeclTypeFamilyInsts instanceDecl = mapMaybe familyItem (instanceDeclItems instanceDecl)
  where
    familyItem item =
      case item of
        InstanceItemAnn _ inner -> familyItem inner
        InstanceItemTypeFamilyInst familyInst -> Just familyInst
        _ -> Nothing

typeFamilyInstName :: TypeFamilyInst -> Maybe Text
typeFamilyInstName familyInst = unqualifiedNameText <$> typeFamilyHeadName (typeFamilyInstLhs familyInst)

-- | Record an associated type family of a class: its type constructor,
-- the class parameters that its parameters name, and its checked
-- default equation.
registerAssociatedTypeFamily :: (Text, Text) -> [Text] -> [TypeFamilyInst] -> TypeFamilyDecl -> TcM (Maybe AssociatedTypeInfo)
registerAssociatedTypeFamily origin classParamNames defaults familyDecl =
  case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of
    Nothing -> pure Nothing
    Just familyBinder -> do
      let familyName = unqualifiedNameText familyBinder
          params = typeFamilyDeclParams familyDecl
          familyDefaults = [familyInst | familyInst <- defaults, typeFamilyInstName familyInst == Just familyName]
      familyTyCon <- mkDeclaredTyCon familyBinder familyName (length params)
      defaultEquation <-
        case familyDefaults of
          [] -> pure Nothing
          [familyInst] -> checkTypeFamilyEquation origin False (typeFamilyInstForall familyInst) (typeFamilyInstEquation familyInst)
          _ -> do
            emitError NoSourceSpan (OtherError ("more than one default equation for associated type " <> T.unpack familyName))
            pure Nothing
      pure
        ( Just
            AssociatedTypeInfo
              { atiTyCon = familyTyCon,
                atiClassParams = [elemIndex (tyVarBinderName param) classParamNames | param <- params],
                atiDefault = defaultEquation
              }
        )

-- | Register the associated type family equations of an instance: the
-- explicit items first, then the class default of each family that the
-- instance does not define.
registerInstanceAssociatedTypes :: (Text, Text) -> ClassInfo -> [TyVarId] -> [TcType] -> InstanceDecl -> TcM ()
registerInstanceAssociatedTypes origin classInfo instanceTyVars headTys instanceDecl = do
  let explicit = instanceDeclTypeFamilyInsts instanceDecl
      explicitNames = mapMaybe typeFamilyInstName explicit
  mapM_ (registerTypeFamilyInstance origin) explicit
  mapM_
    (\(info, defaultEquation) -> instantiateAssociatedDefault origin instanceTyVars headTys info defaultEquation >>= addTypeFamilyInstance)
    [ (info, defaultEquation)
    | info <- ciAssociatedTypes classInfo,
      tyConName (atiTyCon info) `notElem` explicitNames,
      Just defaultEquation <- [atiDefault info]
    ]

-- | Instantiate the default equation of an associated type family at the
-- head types of an instance. A family parameter that is not a class
-- parameter becomes a fresh type variable.
instantiateAssociatedDefault :: (Text, Text) -> [TyVarId] -> [TcType] -> AssociatedTypeInfo -> TypeFamilyInstanceInfo -> TcM TypeFamilyInstanceInfo
instantiateAssociatedDefault (packageName, moduleName') instanceTyVars headTys info defaultEquation = do
  args <- mapM argumentType (atiClassParams info)
  let substitution =
        Map.fromList [(tvUnique tyVar, arg) | (TcTyVar tyVar, arg) <- zip (typeArguments (tfiiLeft defaultEquation)) args]
      freshTyVars = [tyVar | TcTyVar tyVar <- args, tyVar `notElem` instanceTyVars]
  pure
    TypeFamilyInstanceInfo
      { tfiiFamilyName = tyConName (atiTyCon info),
        tfiiAxiomName = associatedDefaultAxiomName info headTys,
        tfiiOrigin = (PackageId packageName, moduleName'),
        tfiiTyVars = instanceTyVars <> freshTyVars,
        tfiiLeft = TcTyCon (atiTyCon info) args,
        tfiiRight = applySubst substitution (tfiiRight defaultEquation),
        tfiiClosed = False
      }
  where
    argumentType maybeIndex =
      case associatedClassArgument headTys maybeIndex of
        Just ty -> pure ty
        Nothing -> do
          rawTyVar <- freshSkolemTv "a"
          kind <- freshKindMeta
          pure (TcTyVar (setTyVarKind kind rawTyVar))

associatedClassArgument :: [TcType] -> Maybe Int -> Maybe TcType
associatedClassArgument headTys maybeIndex = maybeIndex >>= \index -> listToMaybe (drop index headTys)

-- | The axiom name of an instantiated associated type default. The name
-- depends only on the class head types, so the header and body passes
-- agree on it.
associatedDefaultAxiomName :: AssociatedTypeInfo -> [TcType] -> Text
associatedDefaultAxiomName info headTys =
  "$ax$"
    <> tyConName (atiTyCon info)
    <> T.concat ["$" <> maybe "a" typeSuffix (associatedClassArgument headTys maybeIndex) | maybeIndex <- atiClassParams info]

typeArguments :: TcType -> [TcType]
typeArguments ty =
  case ty of
    TcTyCon _ args -> args
    TcAppTy function argument -> typeArguments function <> [argument]
    _ -> []

-- | The @Lift@ class lives in aihc-internal, the standin for ghc-internal,
-- as it does in GHC 9.12 and later.
isTemplateHaskellLift :: (Text, Text) -> Text -> Bool
isTemplateHaskellLift (packageId, moduleName') className =
  "aihc-internal-" `T.isPrefixOf` packageId
    && moduleName' == "GHC.Internal.TH.Lift"
    && className == "Lift"

registerClassItem :: Pred -> TvKindEnv -> [TyVarId] -> ClassDeclItem -> TcM [TcBindingResult]
registerClassItem classPred classTvEnv classTyVars item =
  case peelClassDeclItemAnn item of
    ClassItemTypeSig names ty -> do
      let (context, body) = splitContext ty
          classVarNames = Map.keys classTvEnv
          freeVars = freeTypeVars ty \\ classVarNames
      rawExtraTyVars <- mapM freshSkolemTv freeVars
      extraKinds <- mapM (const freshKindMeta) freeVars
      let extraTyVars = zipWith setTyVarKind extraKinds rawExtraTyVars
      let tvEnv = classTvEnv <> Map.fromList (zip freeVars (zip extraTyVars extraKinds))
      methodBody <- checkSurfaceType tvEnv body KType
      contextPreds <- mapM (surfacePredToPred tvEnv) context
      let preds = classPred : contextPreds
          scheme = ForAll (classTyVars <> extraTyVars) preds methodBody
          declaredTy = schemeToType scheme
      mapM
        ( \methodName -> do
            let name = unqualifiedNameText methodName
                displayName = renderBinderName methodName
            extendResolvedTermEnvPermanent methodName (TcIdBinder scheme Closed)
            zonkedTy <- zonkType declaredTy
            pure (TcBindingResult name displayName zonkedTy)
        )
        names
    _ -> pure []

registerClassDefaultSignature :: TvKindEnv -> [TyVarId] -> ClassDeclItem -> TcM (Maybe (Text, TypeScheme))
registerClassDefaultSignature classTvEnv classTyVars item =
  case peelClassDeclItemAnn item of
    ClassItemDefaultSig methodName ty -> do
      let (context, body) = splitContext ty
          classVarNames = Map.keys classTvEnv
          freeVars = freeTypeVars ty \\ classVarNames
      rawExtraTyVars <- mapM freshSkolemTv freeVars
      extraKinds <- mapM (const freshKindMeta) freeVars
      let extraTyVars = zipWith setTyVarKind extraKinds rawExtraTyVars
      let tvEnv = classTvEnv <> Map.fromList (zip freeVars (zip extraTyVars extraKinds))
      methodBody <- checkSurfaceType tvEnv body KType
      contextPreds <- mapM (surfacePredToPred tvEnv) context
      pure
        ( Just
            ( unqualifiedNameText methodName,
              ForAll (classTyVars <> extraTyVars) contextPreds methodBody
            )
        )
    _ -> pure Nothing

registerInstanceDecl :: (Text, Text) -> InstanceDecl -> TcM [TcBindingResult]
registerInstanceDecl origin instanceDecl =
  case instanceHeadName (instanceDeclHead instanceDecl) of
    Nothing -> pure []
    Just className -> do
      let headArgs = instanceHeadTypes (instanceDeclHead instanceDecl)
      (tvIds, tvEnv) <- makeInstanceTyVarEnv instanceDecl headArgs
      let classNameText = nameText className
      headTys <- checkInstanceHeadTypes className tvEnv headArgs
      dictName <- allocateInstanceDictName origin classNameText headTys
      context <- mapM (surfacePredToPred tvEnv) (instanceDeclContext instanceDecl)
      classInfo <- lookupClassNamed className >>= maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure
      registerInstanceAssociatedTypes origin classInfo tvIds headTys instanceDecl
      let dictTy = foldr TcForAllTy (TcQualTy context (TcTyCon (ciTyCon classInfo) headTys)) tvIds
      addInstance
        InstanceInfo
          { iiClassName = classNameText,
            iiDictName = dictName,
            iiDictOrigin = origin,
            iiDictType = dictTy,
            iiTyVars = tvIds,
            iiContext = context,
            iiHead = headTys
          }
      pure [TcBindingResult dictName dictName dictTy]

predType :: Pred -> TcM TcType
predType (ClassPred classTyCon args) = pure (TcTyCon classTyCon args)
predType (EqPred left right) = do
  equalityTyCon <- mkKnownTyCon "GHC.Types" "~" 2 (KFun KType (KFun KType KConstraint))
  pure (TcTyCon equalityTyCon [left, right])
predType (IParamPred name payload) = implicitParamType name payload
predType (QuantifiedPred variables antecedents consequent) = do
  consequentType <- predType consequent
  let qualifiedType
        | null antecedents = consequentType
        | otherwise = TcQualTy antecedents consequentType
  pure (foldr TcForAllTy qualifiedType variables)

instanceDictName :: Text -> [TcType] -> Text
instanceDictName className tys = "$f" <> className <> T.concat (map typeSuffix tys)

typeSuffix :: TcType -> Text
typeSuffix ty =
  case ty of
    TcTyVar tv -> tvName tv
    TcTyCon tc [] -> tyConName tc
    TcTyCon (TyCon "[]" _) [_] -> "List"
    TcTyCon tc args -> tyConName tc <> T.concat (map typeSuffix args)
    _ -> "T"

instanceHeadIdentity :: [TcType] -> Text
instanceHeadIdentity = T.concat . map typeIdentity

typeIdentity :: TcType -> Text
typeIdentity ty =
  case ty of
    TcTyVar tv -> tvName tv
    TcTyCon tc [] -> tyConIdentity tc
    TcTyCon (TyCon "[]" _) [_] -> "List"
    TcTyCon tc args -> tyConIdentity tc <> T.concat (map typeIdentity args)
    TcFunTy argument result -> typeIdentity argument <> "->" <> typeIdentity result
    _ -> "T"

tyConIdentity :: TyCon -> Text
tyConIdentity tyCon =
  packageIdText (tyConPackageId tyCon) <> "." <> tyConModuleName tyCon <> "." <> tyConName tyCon

allocateInstanceDictName :: (Text, Text) -> Text -> [TcType] -> TcM Text
allocateInstanceDictName origin className headTys = do
  instances <- getInstances
  let taken = Set.fromList [iiDictName info | info <- instances, iiDictOrigin info == origin]
      shortName = instanceDictName className headTys
      modules = nub (mapMaybe typeConstructorModule headTys)
      qualifiedName = shortName <> T.concat (map ("$" <>) modules)
  pure
    ( if shortName `Set.notMember` taken
        then shortName
        else
          if qualifiedName `Set.notMember` taken && qualifiedName /= shortName
            then qualifiedName
            else shortName <> "$" <> T.pack (show (Set.size taken))
    )

lookupInstanceDictName :: (Text, Text) -> Text -> [TcType] -> TcM Text
lookupInstanceDictName origin className headTys = do
  instances <- getInstances
  let identity = instanceHeadIdentity headTys
      matches info =
        iiDictOrigin info == origin
          && iiClassName info == className
          && instanceHeadIdentity (iiHead info) == identity
  case find matches instances of
    Just info -> pure (iiDictName info)
    Nothing -> allocateInstanceDictName origin className headTys

typeConstructorModule :: TcType -> Maybe Text
typeConstructorModule ty =
  case ty of
    TcTyCon tyCon _ -> Just (tyConModuleName tyCon)
    _ -> Nothing

registerDataFamilyDeclHeader :: Maybe TypeScheme -> DataFamilyDecl -> TcM [TcBindingResult]
registerDataFamilyDeclHeader maybeKindScheme familyDecl = do
  let familyBinder = binderHeadName (dataFamilyDeclHead familyDecl)
      familyName = unqualifiedNameText familyBinder
      params = binderHeadParams (dataFamilyDeclHead familyDecl)
      arity = length params
  (kindParams, paramInfos) <- typeDeclParamInfos maybeKindScheme params
  inferredKind <- tyConKindFromParams paramInfos (dataFamilyDeclKind familyDecl)
  familyTyCon <- mkDeclaredTyCon familyBinder familyName arity
  let declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme
  storeTyConInfo
    TyConInfo
      { tciName = familyName,
        tciArity = arity,
        tciTyCon = familyTyCon,
        tciKindScheme = ForAll (map paramTyVar kindParams) [] declaredKind,
        tciFlavor = DataFamilyTyCon,
        tciTypeSynonym = Nothing
      }
  zonkedKind <- defaultKindMetas declaredKind
  pure [TcBindingResult familyName familyName zonkedKind]

registerDataFamilyInstance :: (Text, Text) -> DataFamilyInst -> TcM [TcBindingResult]
registerDataFamilyInstance (packageName, moduleName') familyInst = do
  paramInfos <- dataFamilyInstanceParams familyInst
  let tvEnv =
        Map.fromList
          [ (paramName param, (paramTyVar param, paramKind param))
          | param <- paramInfos
          ]
      constructorNames = concatMap (map fst . dataConBindingNames) (dataFamilyInstConstructors familyInst)
  familyType <- checkSurfaceType tvEnv (dataFamilyInstHead familyInst) KType
  case (familyType, constructorNames) of
    (_, []) -> do
      emitError NoSourceSpan (OtherError "data-family instances without constructors are not supported")
      pure []
    (TcTyCon familyTyCon _, firstConstructor : _) -> do
      maybeFamilyInfo <- lookupTyConByIdentity familyTyCon
      case maybeFamilyInfo of
        Just familyInfo
          | tciFlavor familyInfo == DataFamilyTyCon -> do
              representationKind <- tyConKindFromParams paramInfos (dataFamilyInstKind familyInst)
              let familyName = tciName familyInfo
                  representationName = dataFamilyRepresentationName familyName firstConstructor
                  representationTyCon =
                    mkTyConWithOrigin
                      (PackageId packageName)
                      moduleName'
                      representationName
                      (length paramInfos)
                  axiomName = dataFamilyAxiomName familyName firstConstructor
                  representationInfo =
                    TyConInfo
                      { tciName = representationName,
                        tciArity = length paramInfos,
                        tciTyCon = representationTyCon,
                        tciKindScheme = ForAll [] [] representationKind,
                        tciFlavor = DataTyCon,
                        tciTypeSynonym = Nothing
                      }
                  instanceInfo =
                    DataFamilyInstanceInfo
                      { dfiiFamilyName = familyName,
                        dfiiFamilyType = familyType,
                        dfiiTyVars = map paramTyVar paramInfos,
                        dfiiRepresentationTyCon = representationTyCon,
                        dfiiAxiomName = axiomName,
                        dfiiConstructorNames = constructorNames,
                        dfiiIsNewtype = dataFamilyInstIsNewtype familyInst
                      }
              extendTyConEnvPermanent representationInfo
              addDataFamilyInstance instanceInfo
              mapM (registerDataConWithResult paramInfos familyType) (dataFamilyInstConstructors familyInst)
        _ -> do
          emitError NoSourceSpan (OtherError ("data-family instance head does not name a data family: " <> T.unpack (tyConName familyTyCon)))
          pure []
    _ -> do
      emitError NoSourceSpan (OtherError ("invalid data-family instance head: " <> show familyType))
      pure []

dataFamilyInstanceParams :: DataFamilyInst -> TcM [ParamInfo]
dataFamilyInstanceParams familyInst = do
  explicitParams <- makeParamEnv (dataFamilyInstForall familyInst)
  let explicitNames = map paramName explicitParams
      implicitNames = freeTypeVars (dataFamilyInstHead familyInst) \\ explicitNames
  implicitParams <- mapM makeImplicitParam implicitNames
  pure (explicitParams <> implicitParams)
  where
    makeImplicitParam name = do
      rawTyVar <- freshSkolemTv name
      kind <- freshKindMeta
      pure
        ParamInfo
          { paramName = name,
            paramTyVar = setTyVarKind kind rawTyVar,
            paramKind = kind
          }

typeFamilyHeadName :: Type -> Maybe UnqualifiedName
typeFamilyHeadName ty =
  case peelTypeHead ty of
    TCon name _ -> Just (unqualifiedFromResolvedName name)
    TInfix _ name _ _ -> Just (unqualifiedFromResolvedName name)
    TApp function _ -> typeFamilyHeadName function
    TTypeApp function _ -> typeFamilyHeadName function
    _ -> Nothing

unqualifiedFromResolvedName :: Name -> UnqualifiedName
unqualifiedFromResolvedName name =
  UnqualifiedName
    { unqualifiedNameType = nameType name,
      unqualifiedNameText = nameText name,
      unqualifiedNameAnns = nameAnns name
    }

sourceTypeFamilyAxiomName :: Type -> Text
sourceTypeFamilyAxiomName ty = "$ax$" <> sourceTypeKey ty

sourceTypeKey :: Type -> Text
sourceTypeKey ty =
  case peelTypeHead ty of
    TCon name _ -> nameText name
    TVar name -> unqualifiedNameText name
    TApp function argument -> sourceTypeKey function <> "$" <> sourceTypeKey argument
    TTypeApp function argument -> sourceTypeKey function <> "$" <> sourceTypeKey argument
    TInfix left name _ right -> sourceTypeKey left <> "$" <> nameText name <> "$" <> sourceTypeKey right
    _ -> "T"

registerTypeFamilyDeclHeader :: Maybe TypeScheme -> TypeFamilyDecl -> TcM [TcBindingResult]
registerTypeFamilyDeclHeader maybeKindScheme familyDecl =
  case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of
    Nothing -> do
      emitError NoSourceSpan (OtherError "type family head does not name a type family")
      pure []
    Just familyBinder -> do
      let familyName = unqualifiedNameText familyBinder
          params = typeFamilyDeclParams familyDecl
          arity = length params
      (_, paramInfos) <- typeDeclParamInfos maybeKindScheme params
      inferredKind <- tyConKindFromParams paramInfos (typeFamilyResultKindType familyDecl)
      familyTyCon <- mkDeclaredTyCon familyBinder familyName arity
      let declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme
      storeTyConInfo
        TyConInfo
          { tciName = familyName,
            tciArity = arity,
            tciTyCon = familyTyCon,
            tciKindScheme = ForAll [] [] declaredKind,
            tciFlavor = TypeFamilyTyCon,
            tciTypeSynonym = Nothing
          }
      zonkedKind <- defaultKindMetas declaredKind
      pure [TcBindingResult familyName familyName zonkedKind]

typeFamilyResultKindType :: TypeFamilyDecl -> Maybe Type
typeFamilyResultKindType familyDecl =
  case typeFamilyDeclResultSig familyDecl of
    Just (TypeFamilyKindSig ty) -> Just ty
    _ -> Nothing

registerClosedTypeFamilyEquations :: (Text, Text) -> TypeFamilyDecl -> TcM [TcBindingResult]
registerClosedTypeFamilyEquations origin familyDecl =
  case typeFamilyDeclEquations familyDecl of
    Nothing -> pure []
    Just equations -> do
      mapM_ (registerTypeFamilyEquation origin True (typeFamilyDeclParams familyDecl)) equations
      pure []

registerTypeFamilyInstance :: (Text, Text) -> TypeFamilyInst -> TcM [TcBindingResult]
registerTypeFamilyInstance origin familyInst = do
  registerTypeFamilyEquation origin False (typeFamilyInstForall familyInst) (typeFamilyInstEquation familyInst)
  pure []

typeFamilyInstEquation :: TypeFamilyInst -> TypeFamilyEq
typeFamilyInstEquation familyInst =
  TypeFamilyEq
    { typeFamilyEqAnns = [],
      typeFamilyEqForall = typeFamilyInstForall familyInst,
      typeFamilyEqHeadForm = typeFamilyInstHeadForm familyInst,
      typeFamilyEqLhs = typeFamilyInstLhs familyInst,
      typeFamilyEqRhs = typeFamilyInstRhs familyInst
    }

registerTypeFamilyEquation :: (Text, Text) -> Bool -> [TyVarBinder] -> TypeFamilyEq -> TcM ()
registerTypeFamilyEquation origin isClosed extraBinders equation =
  checkTypeFamilyEquation origin isClosed extraBinders equation >>= mapM_ addTypeFamilyInstance

-- | Check one type family equation. The result is @Nothing@ when the
-- equation head does not name a type family.
checkTypeFamilyEquation :: (Text, Text) -> Bool -> [TyVarBinder] -> TypeFamilyEq -> TcM (Maybe TypeFamilyInstanceInfo)
checkTypeFamilyEquation (packageName, moduleName') isClosed extraBinders equation = do
  paramInfos <- typeFamilyEquationParams extraBinders equation
  let tvEnv =
        Map.fromList
          [ (paramName param, (paramTyVar param, paramKind param))
          | param <- paramInfos
          ]
  lhs <- checkSurfaceType tvEnv (typeFamilyEqLhs equation) KType
  rhs <- checkSurfaceType tvEnv (typeFamilyEqRhs equation) KType
  case typeFamilyApplicationHead lhs of
    Just familyTyCon -> do
      maybeFamilyInfo <- lookupTyConByIdentity familyTyCon
      case maybeFamilyInfo of
        Just familyInfo
          | tciFlavor familyInfo == TypeFamilyTyCon -> do
              existing <- getTypeFamilyInstances
              let familyName = tciName familyInfo
                  axiomName =
                    if isClosed
                      then typeFamilyAxiomName familyName (length existing)
                      else sourceTypeFamilyAxiomName (typeFamilyEqLhs equation)
                  instanceInfo =
                    TypeFamilyInstanceInfo
                      { tfiiFamilyName = familyName,
                        tfiiAxiomName = axiomName,
                        tfiiOrigin = (PackageId packageName, moduleName'),
                        tfiiTyVars = map paramTyVar paramInfos,
                        tfiiLeft = lhs,
                        tfiiRight = rhs,
                        tfiiClosed = isClosed
                      }
              pure (Just instanceInfo)
        _ -> do
          emitError NoSourceSpan (OtherError ("type-family instance head does not name a type family: " <> T.unpack (tyConName familyTyCon)))
          pure Nothing
    Nothing -> do
      emitError NoSourceSpan (OtherError ("invalid type-family instance head: " <> show lhs))
      pure Nothing

typeFamilyApplicationHead :: TcType -> Maybe TyCon
typeFamilyApplicationHead ty =
  case ty of
    TcTyCon tyCon _ -> Just tyCon
    TcAppTy function _ -> typeFamilyApplicationHead function
    _ -> Nothing

typeFamilyEquationParams :: [TyVarBinder] -> TypeFamilyEq -> TcM [ParamInfo]
typeFamilyEquationParams extraBinders equation = do
  explicitParams <- makeParamEnv (extraBinders <> typeFamilyEqForall equation)
  let explicitNames = map paramName explicitParams
      implicitNames =
        nub (freeTypeVars (typeFamilyEqLhs equation) <> freeTypeVars (typeFamilyEqRhs equation)) \\ explicitNames
  implicitParams <- mapM makeImplicitParam implicitNames
  pure (explicitParams <> implicitParams)
  where
    makeImplicitParam name = do
      rawTyVar <- freshSkolemTv name
      kind <- freshKindMeta
      pure
        ParamInfo
          { paramName = name,
            paramTyVar = setTyVarKind kind rawTyVar,
            paramKind = kind
          }

-- | Register a data declaration's type constructor and data constructors.
--
-- For @data Bool = True | False@, this produces:
--   - @Bool :: *@
--   - @True :: Bool@
--   - @False :: Bool@
typeDeclParamInfos :: Maybe TypeScheme -> [TyVarBinder] -> TcM ([ParamInfo], [ParamInfo])
typeDeclParamInfos maybeKindScheme params =
  case maybeKindScheme of
    Nothing -> ([],) <$> makeParamEnv params
    Just scheme@(ForAll kindTyVars _ _) -> do
      let kindParams = map kindParam kindTyVars
          kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams]
          expectedKinds = takeVisibleArgumentKinds (length params) (typeSchemeBody scheme)
      paramInfos <- makeParamEnvWith kindEnv params
      if length expectedKinds == length params
        then do
          zipWithM_ (unifyKinds . paramKind) paramInfos expectedKinds
          let checkedParams = zipWith setParamKind expectedKinds paramInfos
          pure (kindParams, checkedParams)
        else do
          emitError NoSourceSpan (OtherError "standalone kind signature arity does not match its type declaration")
          pure (kindParams, paramInfos)
  where
    kindParam tyVar = ParamInfo (tvName tyVar) tyVar (tvKind tyVar)
    setParamKind kind param =
      param
        { paramTyVar = setTyVarKind kind (paramTyVar param),
          paramKind = kind
        }

implicitBinderKindParams :: [TyVarBinder] -> TcM [ParamInfo]
implicitBinderKindParams binders = mapM makeImplicitParam implicitNames
  where
    explicitNames = map tyVarBinderName binders
    implicitNames = nub (concatMap (maybe [] freeTypeVars . tyVarBinderKind) binders) \\ explicitNames
    makeImplicitParam name = do
      rawTyVar <- freshSkolemTv name
      kind <- freshKindMeta
      pure
        ParamInfo
          { paramName = name,
            paramTyVar = setTyVarKind kind rawTyVar,
            paramKind = kind
          }

dataDeclParamInfos :: Maybe TypeScheme -> DataDecl -> TcM ([ParamInfo], [ParamInfo])
dataDeclParamInfos maybeKindScheme declaration =
  case maybeKindScheme of
    Just {} -> typeDeclParamInfos maybeKindScheme binders
    Nothing -> do
      kindParams <- implicitBinderKindParams binders
      let kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams]
      params <- makeParamEnvWith kindEnv binders
      pure (kindParams, params)
  where
    binders = binderHeadParams (dataDeclHead declaration)

registerDataDeclHeader :: Maybe TypeScheme -> DataDecl -> TcM [TcBindingResult]
registerDataDeclHeader maybeKindScheme dd = do
  let tyBinder = binderHeadName (dataDeclHead dd)
      tyName = unqualifiedNameText tyBinder
      params = binderHeadParams (dataDeclHead dd)
      arity = length params
  (kindParams, paramInfos) <- dataDeclParamInfos maybeKindScheme dd
  let kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams]
  inferredKind <- tyConKindFromParamsWith kindEnv paramInfos (dataDeclKind dd)
  tc <- dataDeclTyCon tyBinder tyName arity
  let declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme
  storeTyConInfo
    TyConInfo
      { tciName = tyName,
        tciArity = arity,
        tciTyCon = tc,
        tciKindScheme = ForAll (map paramTyVar kindParams) [] declaredKind,
        tciFlavor = DataTyCon,
        tciTypeSynonym = Nothing
      }
  -- The parameter kinds stay open until the constructor fields of the whole
  -- declaration group are checked; 'defaultGlobalKindMetas' closes them.
  pure [TcBindingResult tyName tyName declaredKind]

registerDataConstructors :: (Text, Text) -> DataDecl -> TcM [TcBindingResult]
registerDataConstructors origin dataDecl = do
  let tyBinder = binderHeadName (dataDeclHead dataDecl)
      tyName = unqualifiedNameText tyBinder
  maybeInfo <- lookupDeclaredTyCon tyBinder
  case maybeInfo of
    Nothing -> missingTypeInfo ("data type " <> T.unpack tyName)
    Just info -> do
      (kindParams, paramInfos) <- dataDeclParamInfos (Just (tciKindScheme info)) dataDecl
      bindings <- mapM (registerDataCon (tciTyCon info) kindParams paramInfos) (dataDeclConstructors dataDecl)
      constructors <- concat <$> mapM (checkedDataConInfos (tciTyCon info)) (dataDeclConstructors dataDecl)
      mapM_ registerTypeLevelDataCon constructors
      selectorBindings <- registerRecordSelectors origin constructors
      let tyVars = map paramTyVar paramInfos
      resultKind <- tcTypeKind (TcTyCon (tciTyCon info) (map TcTyVar tyVars))
      addDataType
        DataTypeInfo
          { dtiName = tyName,
            dtiTyCon = tciTyCon info,
            dtiTyVars = tyVars,
            dtiResultKind = resultKind,
            dtiFlavor = DataTyCon,
            dtiConstructors = constructors
          }
      pure (bindings <> selectorBindings)

-- | Register a newtype declaration's type constructor and representation
-- constructor.  Newtype erasure/coercion semantics are handled elsewhere; at
-- this stage the type checker only needs the source-level names and types.
registerNewtypeDeclHeader :: Maybe TypeScheme -> NewtypeDecl -> TcM [TcBindingResult]
registerNewtypeDeclHeader maybeKindScheme nd = do
  let tyBinder = binderHeadName (newtypeDeclHead nd)
      tyName = unqualifiedNameText tyBinder
      params = binderHeadParams (newtypeDeclHead nd)
      arity = length params
  (kindParams, paramInfos) <- typeDeclParamInfos maybeKindScheme params
  inferredKind <- tyConKindFromParams paramInfos (newtypeDeclKind nd)
  tc <- mkDeclaredTyCon tyBinder tyName arity
  let declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme
  storeTyConInfo
    TyConInfo
      { tciName = tyName,
        tciArity = arity,
        tciTyCon = tc,
        tciKindScheme = ForAll (map paramTyVar kindParams) [] declaredKind,
        tciFlavor = NewtypeTyCon,
        tciTypeSynonym = Nothing
      }
  -- The parameter kinds stay open until the constructor fields of the whole
  -- declaration group are checked; 'defaultGlobalKindMetas' closes them.
  pure [TcBindingResult tyName tyName declaredKind]

registerNewtypeConstructor :: (Text, Text) -> NewtypeDecl -> TcM [TcBindingResult]
registerNewtypeConstructor origin newtypeDecl = do
  let tyBinder = binderHeadName (newtypeDeclHead newtypeDecl)
      tyName = unqualifiedNameText tyBinder
  maybeInfo <- lookupDeclaredTyCon tyBinder
  case maybeInfo of
    Nothing -> missingTypeInfo ("newtype " <> T.unpack tyName)
    Just info -> do
      (kindParams, paramInfos) <- typeDeclParamInfos (Just (tciKindScheme info)) (binderHeadParams (newtypeDeclHead newtypeDecl))
      constructor <- mapM (registerDataCon (tciTyCon info) kindParams paramInfos) (newtypeDeclConstructor newtypeDecl)
      constructors <- maybe (pure []) (checkedDataConInfos (tciTyCon info)) (newtypeDeclConstructor newtypeDecl)
      mapM_ registerTypeLevelDataCon constructors
      selectorBindings <- registerRecordSelectors origin constructors
      let tyVars = map paramTyVar paramInfos
      resultKind <- tcTypeKind (TcTyCon (tciTyCon info) (map TcTyVar tyVars))
      addDataType
        DataTypeInfo
          { dtiName = tyName,
            dtiTyCon = tciTyCon info,
            dtiTyVars = tyVars,
            dtiResultKind = resultKind,
            dtiFlavor = NewtypeTyCon,
            dtiConstructors = constructors
          }
      pure (maybeToList constructor <> selectorBindings)

registerTypeLevelDataCon :: DataConInfo -> TcM ()
registerTypeLevelDataCon constructor = do
  let name = dciName constructor
      fieldTypes = dataConArgTypes constructor
      arity = length fieldTypes
      (packageId, moduleName') = dciOrigin constructor
      dataConTyCon = mkTyConWithNamespace ResolutionNamespaceTerm packageId moduleName' name arity
  kindScheme <-
    if moduleName' == "GHC.Types" && name `elem` ["[]", ":"]
      then listDataConstructorKind name
      else
        pure
          ( ForAll
              (dciUnivTyVars constructor <> dciExTyVars constructor)
              (dciTheta constructor)
              (foldr TcFunTy (dciResTy constructor) fieldTypes)
          )
  let info =
        TyConInfo
          { tciName = name,
            tciArity = arity,
            tciTyCon = dataConTyCon,
            tciKindScheme = kindScheme,
            tciFlavor = DataTyCon,
            tciTypeSynonym = Nothing
          }
  if moduleName' == "GHC.Types" && name `elem` ["[]", ":"]
    then replaceTyConEnvPermanent info
    else storeTyConInfo info

listDataConstructorKind :: Text -> TcM TypeScheme
listDataConstructorKind name = do
  rawElementKind <- freshSkolemTv "k"
  let elementKindVar = setTyVarKind KType rawElementKind
      elementKind = TcTyVar elementKindVar
  resultKind <- listTypeForKind elementKind
  let body =
        case name of
          "[]" -> resultKind
          _ -> TcFunTy elementKind (TcFunTy resultKind resultKind)
  pure (ForAll [elementKindVar] [] body)

listTypeForKind :: TcType -> TcM TcType
listTypeForKind elementKind = do
  maybeList <- lookupTyCon "[]"
  listTyCon <- maybe (mkKnownTyCon "GHC.Types" "[]" 1 (TcFunTy KType KType)) (pure . tciTyCon) maybeList
  pure (TcTyCon listTyCon [elementKind])

registerRecordSelectors :: (Text, Text) -> [DataConInfo] -> TcM [TcBindingResult]
registerRecordSelectors origin constructors =
  mapM registerSelector (Map.toList selectors)
  where
    -- A field whose type mentions an existential variable has no selector.
    -- The constructor context does not reach the selector type.
    selectors =
      Map.fromListWith
        (++)
        [ (label, [(constructor, field)])
        | constructor <- constructors,
          field <- dciFields constructor,
          not (any (`elem` dciExTyVars constructor) (typeTyVars (dcfiType field))),
          Just label <- [dcfiLabel field]
        ]
    registerSelector (label, (constructor, field) : _) = do
      let scheme =
            ForAll
              (dciUnivTyVars constructor)
              []
              (TcFunTy (dciResTy constructor) (dcfiType field))
      let binder = TcIdBinder scheme Closed
          (packageId, moduleName') = origin
      extendTermEnvPermanent label binder
      extendTermKeyEnvPermanent (TcTermGlobal (PackageId packageId) moduleName' label) binder
      zonkedType <- zonkType (schemeToType scheme)
      pure (TcBindingResult label label zonkedType)
    registerSelector (label, []) =
      abortTc ("record selector has no fields: " <> T.unpack label)

registerTypeSynonymHeader :: Maybe TypeScheme -> TypeSynDecl -> TcM [TcBindingResult]
registerTypeSynonymHeader maybeKindScheme typeSynDecl = do
  let tyBinder = binderHeadName (typeSynHead typeSynDecl)
      tyName = unqualifiedNameText tyBinder
      params = binderHeadParams (typeSynHead typeSynDecl)
      arity = length params
  (_, paramInfos) <- typeDeclParamInfos maybeKindScheme params
  inferredResultKind <- freshKindMeta
  let inferredKind = foldr (KFun . paramKind) inferredResultKind paramInfos
  tyCon <- mkDeclaredTyCon tyBinder tyName arity
  let declaredKindScheme = fromMaybe (ForAll [] [] inferredKind) maybeKindScheme
      declaredKind = typeSchemeBody declaredKindScheme
  let synonym = TypeSynonymInfo (map paramTyVar paramInfos) Nothing
  storeTyConInfo
    TyConInfo
      { tciName = tyName,
        tciArity = arity,
        tciTyCon = tyCon,
        tciKindScheme = declaredKindScheme,
        tciFlavor = SynonymTyCon,
        tciTypeSynonym = Just synonym
      }
  pure [TcBindingResult tyName tyName declaredKind]

registerTypeSynonymBody :: Decl -> TcM ()
registerTypeSynonymBody (DeclAnn _ inner) = registerTypeSynonymBody inner
registerTypeSynonymBody (DeclTypeSyn typeSynDecl) = do
  let tyBinder = binderHeadName (typeSynHead typeSynDecl)
      tyName = unqualifiedNameText tyBinder
  maybeInfo <- lookupDeclaredTyCon tyBinder
  case maybeInfo of
    Just info
      | Just synonym <- tciTypeSynonym info -> do
          let params = tsiParams synonym
              tvEnv = Map.fromList [(tvName param, (param, tvKind param)) | param <- params]
          (body, _) <- convertSurfaceTypeWithKinds tvEnv (typeSynBody typeSynDecl)
          replaceTyConEnvPermanent (info {tciTypeSynonym = Just (synonym {tsiBody = Just body})})
    _ -> missingTypeInfo ("type synonym " <> T.unpack tyName)
registerTypeSynonymBody _ = pure ()

checkTypeSynonymBody :: Decl -> TcM ()
checkTypeSynonymBody (DeclAnn _ inner) = checkTypeSynonymBody inner
checkTypeSynonymBody (DeclTypeSyn typeSynDecl) = do
  let tyBinder = binderHeadName (typeSynHead typeSynDecl)
      tyName = unqualifiedNameText tyBinder
  maybeInfo <- lookupDeclaredTyCon tyBinder
  case maybeInfo of
    Just info
      | Just synonym <- tciTypeSynonym info -> do
          let params = tsiParams synonym
              tvEnv = Map.fromList [(tvName param, (param, tvKind param)) | param <- params]
              resultKind = typeResultKind (length params) (typeSchemeBody (tciKindScheme info))
          (_, bodyKind) <- convertSurfaceTypeWithKinds tvEnv (typeSynBody typeSynDecl)
          unifyKindsAt (surfaceTypeSpan (typeSynBody typeSynDecl)) resultKind bodyKind
    _ -> missingTypeInfo ("type synonym " <> T.unpack tyName)
checkTypeSynonymBody _ = pure ()

typeResultKind :: Int -> TcType -> TcType
typeResultKind remaining kind
  | remaining <= 0 = kind
typeResultKind remaining (KFun _ result) = typeResultKind (remaining - 1) result
typeResultKind _ kind = kind

dataDeclTyCon :: UnqualifiedName -> Text -> Int -> TcM TyCon
dataDeclTyCon binder "List" 1 = mkDeclaredTyCon binder "[]" 1
dataDeclTyCon binder name arity = mkDeclaredTyCon binder name arity

mkDeclaredTyCon :: UnqualifiedName -> Text -> Int -> TcM TyCon
mkDeclaredTyCon binder name arity =
  case nameResolution binder of
    Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId resolvedName}
      | Just definingModuleName <- nameQualifier resolvedName ->
          pure (mkTyConWithOrigin packageId definingModuleName name arity)
    _ -> abortTc ("type declaration has no package or module identity: " <> T.unpack name)

-- | Register a single data constructor as a polymorphic binding.
-- Returns the binding result for the constructor.
registerDataCon :: TyCon -> [ParamInfo] -> [ParamInfo] -> DataConDecl -> TcM TcBindingResult
registerDataCon tc kindParams paramInfos =
  registerDataConWithResult (kindParams <> paramInfos) (TcTyCon tc (map (TcTyVar . paramTyVar) paramInfos))

registerDataConWithResult :: [ParamInfo] -> TcType -> DataConDecl -> TcM TcBindingResult
registerDataConWithResult paramInfos resTy con = case con of
  DataConAnn _ inner -> registerDataConWithResult paramInfos resTy inner
  PrefixCon forallVars context conName args ->
    registerH98DataCon forallVars context (Just conName) (unqualifiedNameText conName) (map bangType args)
  InfixCon forallVars context lhs conName rhs ->
    registerH98DataCon forallVars context (Just conName) (unqualifiedNameText conName) (map bangType [lhs, rhs])
  RecordCon forallVars context conName fields ->
    registerH98DataCon forallVars context (Just conName) (unqualifiedNameText conName) (map bangType (recordBangFields fields))
  TupleCon forallVars context flavor fields ->
    registerH98DataCon forallVars context Nothing (tupleConText flavor (length fields)) (map bangType fields)
  UnboxedSumCon forallVars context pos arity field ->
    registerH98DataCon forallVars context Nothing (unboxedSumConText pos arity) [bangType field]
  ListCon forallVars context ->
    registerH98DataCon forallVars context Nothing "[]" []
  GadtCon forallBinders context names body -> do
    explicitParams <- makeParamEnv (concatMap forallTelescopeBinders forallBinders)
    let explicitNames = map paramName (explicitParams <> paramInfos)
        implicitNames = filter (`notElem` explicitNames) (nub (concatMap freeTypeVars (gadtBodyResultType body : gadtBodyArgTypes body <> context)))
    implicitParams <- forM implicitNames $ \name -> do
      variable <- freshSkolemTv name
      kind <- freshKindMeta
      pure (ParamInfo name (setTyVarKind kind variable) kind)
    let constructorParams = explicitParams <> implicitParams
        constructorEnv =
          Map.fromList
            [ (paramName param, (paramTyVar param, paramKind param))
            | param <- constructorParams
            ]
            <> paramEnv
        constructorTyVars = map paramTyVar constructorParams
    let resultSurfTy = gadtBodyResultType body
        argSurfTys = gadtBodyArgTypes body
    gadtResTy <- checkSurfaceType constructorEnv resultSurfTy KType
    gadtArgTys <- mapM (checkRuntimeType constructorEnv) argSurfTys
    predicates <- mapM (surfacePredToPred constructorEnv) context
    let conTy = foldr TcFunTy gadtResTy gadtArgTys
        candidateTyVars = paramVarIds <> constructorTyVars
        quantifiedTyVars = filter (\tyVar -> typeMentionsTyVar tyVar conTy || any (predicateMentionsTyVar tyVar) predicates) candidateTyVars
        gadtScheme = ForAll quantifiedTyVars predicates conTy
    mapM_
      ( \n -> do
          let nm = unqualifiedNameText n
          extendResolvedTermEnvPermanent n (TcIdBinder gadtScheme Closed)
          markGadtCon nm
      )
      names
    case names of
      (n : _) -> do
        zonkedTy <- zonkType conTy
        let name = unqualifiedNameText n
         in pure (TcBindingResult name name zonkedTy)
      [] -> pure (TcBindingResult "<gadt>" "<gadt>" gadtResTy)
  where
    paramEnv =
      Map.fromList
        [ (paramName param, (paramTyVar param, paramKind param))
        | param <- paramInfos
        ]
    paramVarIds = map paramTyVar paramInfos
    registerH98DataCon forallVars context maybeName name fieldTypes = do
      constructorParams <- makeParamEnv forallVars
      let constructorEnv =
            Map.fromList
              [ (paramName param, (paramTyVar param, paramKind param))
              | param <- constructorParams
              ]
              <> paramEnv
          constructorTyVars = map paramTyVar constructorParams
      argTys <- mapM (checkRuntimeType constructorEnv) fieldTypes
      predicates <- mapM (surfacePredToPred constructorEnv) context
      let conTy = foldr TcFunTy resTy argTys
          scheme = ForAll (paramVarIds <> constructorTyVars) predicates conTy
      case maybeName of
        Just sourceName -> extendResolvedTermEnvPermanent sourceName (TcIdBinder scheme Closed)
        Nothing ->
          case resTy of
            TcTyCon resultTyCon _ -> extendTyConTermEnvPermanent resultTyCon name (TcIdBinder scheme Closed)
            _ -> extendTermEnvPermanent name (TcIdBinder scheme Closed)
      zonkedTy <- zonkType (schemeToType scheme)
      pure (TcBindingResult name name zonkedTy)

tupleConText :: TupleFlavor -> Int -> Text
tupleConText flavor arity =
  case flavor of
    Boxed -> "(" <> commas arity <> ")"
    Unboxed -> "(#" <> commas arity <> "#)"

unboxedSumConText :: Int -> Int -> Text
unboxedSumConText pos arity = "(#" <> bars (pos - 1) <> "_" <> bars (arity - pos) <> "#)"

commas :: Int -> Text
commas n
  | n <= 1 = ""
  | otherwise = mconcat (replicate (n - 1) ",")

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = mconcat (replicate n "|")

-- | Extract argument types from a GadtBody.
gadtBodyArgTypes :: GadtBody -> [Type]
gadtBodyArgTypes (GadtPrefixBody argsWithKinds _) = map (bangType . fst) argsWithKinds
gadtBodyArgTypes (GadtRecordBody fields _) = map bangType (recordBangFields fields)

recordBangFields :: [FieldDecl] -> [BangType]
recordBangFields = concatMap $ \field -> replicate (length (fieldNames field)) (fieldType field)

checkedDataConInfos :: TyCon -> DataConDecl -> TcM [DataConInfo]
checkedDataConInfos tyCon declaration = do
  let (sourceForm, sourceFields, constructorNames) = dataConSourceLayout declaration
      origin = (tyConPackageId tyCon, tyConModuleName tyCon)
  mapM (checkedDataConInfo origin sourceForm sourceFields) constructorNames

checkedDataConInfo :: (PackageId, Text) -> DataConSourceForm -> [(Maybe Text, BangType)] -> Text -> TcM DataConInfo
checkedDataConInfo origin sourceForm sourceFields constructorName = do
  maybeBinder <- lookupTerm constructorName
  case maybeBinder of
    Just (TcIdBinder (ForAll tyVars predicates constructorType) _) -> do
      let (argumentTypes, resultType) = splitFunctionType constructorType
      if length sourceFields /= length argumentTypes
        then abortTc ("constructor metadata arity disagrees with checked type for " <> T.unpack constructorName)
        else do
          let (universalTyVars, existentialTyVars) = partition (`typeMentionsTyVar` resultType) tyVars
          pure
            DataConInfo
              { dciName = constructorName,
                dciOrigin = origin,
                dciUnivTyVars = universalTyVars,
                dciExTyVars = existentialTyVars,
                dciTheta = predicates,
                dciFields = zipWith checkedFieldInfo sourceFields argumentTypes,
                dciResTy = resultType,
                dciSourceForm = sourceForm
              }
    Just TcMonoIdBinder {} ->
      abortTc ("data constructor has a monomorphic binder: " <> T.unpack constructorName)
    Nothing ->
      missingTypeInfo ("data constructor " <> T.unpack constructorName)

checkedFieldInfo :: (Maybe Text, BangType) -> TcType -> DataConFieldInfo
checkedFieldInfo (label, bang) fieldType' =
  DataConFieldInfo
    { dcfiLabel = label,
      dcfiType = fieldType',
      dcfiStrict = bangStrict bang,
      dcfiLazy = bangLazy bang,
      dcfiUnpack = fieldUnpack bang
    }

fieldUnpack :: BangType -> DataConFieldUnpack
fieldUnpack bang =
  case [unpack | Pragma (PragmaUnpack unpack) _ <- bangPragmas bang] of
    UnpackPragma : _ -> UnpackField
    NoUnpackPragma : _ -> NoUnpackField
    [] -> NoFieldUnpack

dataConSourceLayout :: DataConDecl -> (DataConSourceForm, [(Maybe Text, BangType)], [Text])
dataConSourceLayout declaration =
  case declaration of
    DataConAnn _ inner -> dataConSourceLayout inner
    PrefixCon _ _ constructor fields ->
      (PrefixDataCon, map (Nothing,) fields, [unqualifiedNameText constructor])
    InfixCon _ _ left constructor right ->
      (InfixDataCon, map (Nothing,) [left, right], [unqualifiedNameText constructor])
    RecordCon _ _ constructor fields ->
      (RecordDataCon, recordSourceFields fields, [unqualifiedNameText constructor])
    TupleCon _ _ flavor fields ->
      (SyntaxDataCon, map (Nothing,) fields, [tupleConText flavor (length fields)])
    UnboxedSumCon _ _ position arity field ->
      (SyntaxDataCon, [(Nothing, field)], [unboxedSumConText position arity])
    ListCon {} ->
      (SyntaxDataCon, [], ["[]"])
    GadtCon _ _ constructors body ->
      let names = map unqualifiedNameText constructors
       in case body of
            GadtPrefixBody fields _ -> (PrefixDataCon, map ((Nothing,) . fst) fields, names)
            GadtRecordBody fields _ -> (RecordDataCon, recordSourceFields fields, names)

recordSourceFields :: [FieldDecl] -> [(Maybe Text, BangType)]
recordSourceFields = concatMap $ \field ->
  [(Just (unqualifiedNameText label), fieldType field) | label <- fieldNames field]

-- | Type-check a declaration, returning binding results for value bindings.
tcDecl :: Decl -> TcM [TcBindingResult]
tcDecl (DeclValue vd) = tcValueDecl vd
tcDecl (DeclAnn _ inner) = tcDecl inner
tcDecl _ = pure []

-- | Type-check a value declaration.
tcValueDecl :: ValueDecl -> TcM [TcBindingResult]
tcValueDecl (FunctionBind binder matches) = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  key <- resolvedUnqualifiedTermKey binder
  snd <$> tcFunctionInfer key displayName name matches
tcValueDecl (PatternBind _ pat rhs) = case patternBinderName pat of
  -- Bare variable pattern (e.g. @x = 5@, @(.>.) = (++)@): type-check as a
  -- zero-argument function so that the binding gets generalized and registered
  -- in the environment.
  Just (displayName, name) -> do
    case patternBinderSyntaxName pat of
      Just binder -> do
        key <- resolvedUnqualifiedTermKey binder
        snd <$> tcFunctionInfer key displayName name [zeroArgMatch (patternSpan pat) rhs]
      Nothing -> abortTc "a named pattern binding does not have binder syntax"
  -- Non-trivial pattern binding: infer the RHS type without generalization.
  Nothing -> do
    (_rhs', ty) <- tcRhs rhs
    zonkedTy <- zonkType ty
    pure [TcBindingResult "<pattern>" "<pattern>" zonkedTy]

-- | Extract the binder name from a pattern binding's LHS, if it is a bare
-- variable pattern.  Returns @(displayName, envName)@ for simple variable
-- patterns (possibly wrapped in parens or annotations), 'Nothing' for
-- non-trivial patterns like tuples or constructors.
patternBinderSyntaxName :: Pattern -> Maybe UnqualifiedName
patternBinderSyntaxName (PVar n) = Just n
patternBinderSyntaxName (PParen inner) = patternBinderSyntaxName inner
patternBinderSyntaxName (PAnn _ inner) = patternBinderSyntaxName inner
patternBinderSyntaxName _ = Nothing

patternBinderName :: Pattern -> Maybe (Text, Text)
patternBinderName pat =
  (\n -> (renderBinderName n, unqualifiedNameText n)) <$> patternBinderSyntaxName pat

zeroArgMatch :: SourceSpan -> Rhs Expr -> Match
zeroArgMatch sp rhs =
  Match
    { matchAnns = sourceSpanAnn sp,
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }

sourceSpanAnn :: SourceSpan -> [Annotation]
sourceSpanAnn NoSourceSpan = []
sourceSpanAnn sp = [mkAnnotation sp]

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sp _ = sp

patternSpan :: Pattern -> SourceSpan
patternSpan pat =
  case pat of
    -- The parser gives some nodes an empty span. Use the inner span then.
    PAnn ann inner ->
      case fromAnnotation ann of
        Just sp | sp /= NoSourceSpan -> sp
        _ -> patternSpan inner
    PVar name -> sourceSpanFromAnns (unqualifiedNameAnns name)
    PParen inner -> patternSpan inner
    PAs name _ -> sourceSpanFromAnns (unqualifiedNameAnns name)
    PStrict inner -> patternSpan inner
    PIrrefutable inner -> patternSpan inner
    PCon name _ _ -> nameSpan name
    PInfix _ name _ -> nameSpan name
    _ -> NoSourceSpan
  where
    -- The resolver gives a constructor occurrence its span.
    nameSpan name =
      sourceSpanFromAnns (nameAnns name)
        `orSourceSpan` case [resolutionSpan resolution | Just resolution <- map fromAnnotation (nameAnns name)] of
          sp : _ -> sp
          [] -> NoSourceSpan

typeSpan :: Type -> SourceSpan
typeSpan ty =
  case ty of
    TAnn ann inner ->
      fromMaybe (typeSpan inner) (fromAnnotation @SourceSpan ann)
    TParen inner -> typeSpan inner
    TForall _ inner -> typeSpan inner
    TContext _ inner -> typeSpan inner
    TKindSig inner _ -> typeSpan inner
    _ -> NoSourceSpan

rhsExprSpan :: Rhs Expr -> SourceSpan
rhsExprSpan rhs =
  case rhs of
    UnguardedRhs anns expr _ -> exprSpan expr `orSourceSpan` sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

exprSpan :: Expr -> SourceSpan
exprSpan expr =
  case expr of
    EAnn ann inner ->
      fromMaybe (exprSpan inner) (fromAnnotation @SourceSpan ann)
    EParen inner -> exprSpan inner
    EPragma _ inner -> exprSpan inner
    ETypeSig inner _ -> exprSpan inner
    _ -> NoSourceSpan

-- | Convert a type scheme to a displayable type.
schemeToType :: TypeScheme -> TcType
schemeToType (ForAll [] [] ty) = ty
schemeToType (ForAll tvs [] ty) = foldr TcForAllTy ty tvs
schemeToType (ForAll [] preds ty) = TcQualTy preds ty
schemeToType (ForAll tvs preds ty) = foldr TcForAllTy (TcQualTy preds ty) tvs

-- | Type-check a list of matches (equations for a function binding).
--
-- All equations must have the same number of patterns and produce
-- a consistent function type. We infer the type from each equation
-- and unify them.
tcMatches :: [Match] -> TcM ([Match], TcType, [Ct], [Implication])
tcMatches [] = do
  ty <- freshMetaTv
  pure ([], ty, [], [])
tcMatches matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      -- No patterns: just infer the RHS of the first match.
      (rhs0, ty0, cts0) <- inferRhsExpr (matchRhs m0)
      restResults <- mapM (unifyMatchRhs ty0) (drop 1 matches)
      let firstMatch = m0 {matchRhs = rhs0}
          restMatches = map fst restResults
          restCts = concatMap snd restResults
      pure (firstMatch : restMatches, ty0, cts0 ++ restCts, [])
    else do
      -- Create fresh meta-variables for the argument types and result type.
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      -- Process each equation.
      results <- mapM (tcMatchEquation Nothing argTys resTy) matches
      let (matches', ctsList, implsList) = unzip3 results
          allCts = concat ctsList
          allImpls = concat implsList
          funTy = foldr TcFunTy resTy argTys
      pure (matches', funTy, allCts, allImpls)

-- | Type-check a single match equation against expected arg/result types.
-- Returns flat wanted constraints and implication constraints.
tcMatchEquation :: Maybe TypeOrigin -> [TcType] -> TcType -> Match -> TcM (Match, [Ct], [Implication])
tcMatchEquation expectedOrigin argTys resTy match = do
  let pats = matchPats match
      sp = sourceSpanFromAnns (matchAnns match)
  patCheck <- checkFunctionPatternsWithGivens sp (zip pats argTys)
  -- Infer the RHS under the extended environment.
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsExpr (matchRhs match))
  -- RHS type must match the expected result type.
  ev <- freshEvVar
  let rhsSp = rhsExprSpan (matchRhs match) `orSourceSpan` sp
      resCt =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = rhsTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin rhsSp
            }
          TypeTrace
            { typeTraceType = resTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = fromMaybe (ConstraintTypeOrigin (AppOrigin sp)) expectedOrigin
            }
          ev
          (AppOrigin rhsSp)
          rhsSp
  let pats' = map (annotatePatternBindings (pcBindings patCheck)) (pcPatterns patCheck)
      givenCts = pcGivenCts patCheck
      bodyWanteds = pcWantedCts patCheck ++ rhsCts ++ [resCt]
  if null givenCts && null (pcSkolems patCheck)
    then -- No constructor-local type variables or givens: keep flat wanteds.
      pure (match {matchPats = pats', matchRhs = annotateRhsCast resTy ev rhs'}, bodyWanteds, [])
    else do
      -- GADT givens: wrap body wanteds in an implication.
      level <- getTcLevel
      let impl =
            Implication
              { implSkols = pcSkolems patCheck,
                implGivenEvs = map ctEvVar givenCts,
                implGivenCts = givenCts,
                implWantedCts = bodyWanteds,
                implTcLevel = level,
                implInfo = AppOrigin sp
              }
      pure (match {matchPats = pats', matchRhs = annotateRhsCast resTy ev rhs'}, [], [impl])

-- | Unify an additional match equation's RHS with the expected type.
unifyMatchRhs :: TcType -> Match -> TcM (Match, [Ct])
unifyMatchRhs expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhsExpr (matchRhs match)
  ev <- freshEvVar
  let sp = sourceSpanFromAnns (matchAnns match)
      rhsSp = rhsExprSpan (matchRhs match) `orSourceSpan` sp
      eqCt =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = rhsTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin rhsSp
            }
          TypeTrace
            { typeTraceType = expectedTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ConstraintTypeOrigin (AppOrigin sp)
            }
          ev
          (AppOrigin rhsSp)
          rhsSp
  pure (match {matchRhs = rhs'}, rhsCts ++ [eqCt])

-- | Infer the type of a right-hand side expression.
inferRhsExpr :: Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhsExpr = inferRhsWithLocals inferExpr

-- | Type-check a right-hand side (solving constraints immediately).
tcRhs :: Rhs Expr -> TcM (Rhs Expr, TcType)
tcRhs rhs = do
  (rhs', ty, cts) <- inferRhsWithLocals inferExpr rhs
  _ <- solveConstraints cts
  pure (rhs', ty)

-- | Render an unqualified name for display.
-- Operators (NameVarSym, NameConSym) are wrapped in parentheses.
renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname
