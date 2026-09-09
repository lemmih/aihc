{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for the aihc type checker.
--
-- The type checker consumes a parsed and name-resolved AST
-- and produces the same AST annotated with typing information. It does
-- not transform the tree structure.
--
-- The implementation follows the OutsideIn(X) algorithm:
--
-- 1. Generate wanted constraints by walking the AST.
-- 2. Solve the constraints using the worklist/inert-set architecture.
-- 3. Zonk meta-variables.
-- 4. Attach type annotations to AST nodes.
module Aihc.Tc
  ( -- * Entry point
    typecheckExpr,
    typecheckModulesWithInterface,
    typecheckModuleSccWithInterface,

    -- * Result types
    TcResult (..),
    TcConfig,
    mkTcConfig,
    TcWiring (..),
    DerivingReferences (..),
    DerivingReference (..),
    TcBindingResult (..),
    defaultMethodName,
    TcTermKey (..),
    tcTermKeyIdentifier,
    TcInterface (..),
    InstanceKey,
    tcInterfaceTerms,
    tcInterfaceTyCons,
    tcInterfaceDataTypes,
    tcInterfaceClasses,
    tcInterfaceInstances,
    tcInterfaceDataFamilyInstances,
    tcInterfaceTypeFamilyInstances,
    tcInterfacePatSyns,
    tcInterfaceForeignImports,
    tcInterfaceFromLists,
    emptyTcInterface,
    mergeTcInterfaces,
    unionTcInterfaces,
    restrictTcInterfaceToModules,
    tcInterfaceBindings,

    -- * Module result projections
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleInstances,
    tcModuleClasses,
    tcModuleSuccess,

    -- * Re-exports for convenience
    TcType (..),
    TcTypeKey,
    TcAxiomKey (..),
    TcKindEnv,
    TyCon (..),
    mkTyConWithNamespace,
    tyConKey,
    tyConPackageId,
    tyConModuleName,
    TyVarId (..),
    tvKind,
    TypeScheme (..),
    Pred (..),
    InstanceInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    dataTypeKey,
    DataConInfo (..),
    PatSynDirection (..),
    PatSynInfo (..),
    patSynKey,
    DataConFieldInfo (..),
    DataConFieldUnpack (..),
    DataConSourceForm (..),
    dataConArgTypes,
    dataFamilyAxiomKey,
    dataFamilyAxiomName,
    dataFamilyRepresentationName,
    TypeFamilyInstanceInfo (..),
    typeFamilyAxiomKey,
    typeFamilyAxiomName,
    ClassInfo (..),
    AssociatedTypeInfo (..),
    TyConFlavor (..),
    TyConInfo (..),
    Unique (..),
    TcKinds (..),
    typeKind,
    liftedRep,
    mkTcKinds,
    typeKindInEnv,
    runtimeRepOfTypeInEnv,
    isUnliftedTypeInEnv,
    TcAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderPred,
    renderTcSignature,
    renderTcType,
    renderTcTypeInModule,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    DoStmt (..),
    ExportSpec (..),
    Expr (..),
    Extension (..),
    GuardQualifier (..),
    ImportItem (..),
    InstanceDeclItem (..),
    Literal (..),
    Module (..),
    Pattern (..),
    SourceSpan (..),
    Type (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (PackageId (..))
import Aihc.Resolve.Generic (everywhereM)
import Aihc.Resolve.Traverse (annotationList)
import Aihc.Tc.Annotations (TcAnnotation (..), TcDerivingAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcDerivingStrategy (..), TcForeignImportInfo (..), renderPred, renderTcSignature, renderTcType, renderTcTypeInModule)
import Aihc.Tc.Deriving.References (DerivingReference (..), DerivingReferences (..))
import Aihc.Tc.Env (AssociatedTypeInfo (..), ClassInfo (..), DataConFieldInfo (..), DataConFieldUnpack (..), DataConInfo (..), DataConSourceForm (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), InstanceInfo (..), PatSynDirection (..), PatSynInfo (..), TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..), classInfoKey, dataConArgTypes, dataFamilyAxiomKey, dataFamilyAxiomName, dataFamilyRepresentationName, dataTypeKey, instanceEnvFromList, instanceEnvList, instanceInfoKey, typeFamilyAxiomKey, typeFamilyAxiomName)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), defaultMethodName, moduleBindings, moduleClasses, moduleInstances, tcModule, tcModuleScc)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Wiring (mkTcKinds)
import Aihc.Tc.Zonk (finalizeDiagnostics, zonkType)
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Data (Data)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (cast)

-- | Result of type checking.
data TcResult = TcResult
  { -- | The inferred type of the top-level expression or binding.
    tcResultType :: !TcType,
    -- | Diagnostics (errors and warnings) produced.
    tcResultDiagnostics :: ![TcDiagnostic],
    -- | Whether type checking succeeded (no errors).
    tcResultSuccess :: !Bool
  }
  deriving (Show)

-- | The complete semantic interface shared between independently checked
-- module groups. Implementations never cross this boundary: only the facts
-- needed to type-check downstream source are retained.
--
-- Every fact is keyed by its global identity, so merging two interfaces is
-- a map union and the type-checker state can adopt an interface without
-- rebuilding it. The list views below present each map in key order.
data TcInterface = TcInterface
  { tcInterfaceTermMap :: !(Map.Map TcTermKey TypeScheme),
    tcInterfaceTyConMap :: !(Map.Map TcTypeKey TyConInfo),
    tcInterfaceDataTypeMap :: !(Map.Map TcTypeKey DataTypeInfo),
    tcInterfaceClassMap :: !(Map.Map TcTypeKey ClassInfo),
    tcInterfaceInstanceMap :: !(Map.Map InstanceKey InstanceInfo),
    tcInterfaceDataFamilyInstanceMap :: !(Map.Map TcAxiomKey DataFamilyInstanceInfo),
    tcInterfaceTypeFamilyInstanceMap :: !(Map.Map TcAxiomKey TypeFamilyInstanceInfo),
    tcInterfacePatSynMap :: !(Map.Map TcTermKey PatSynInfo),
    -- | The checked calling convention of each foreign import.
    tcInterfaceForeignImportMap :: !(Map.Map TcTermKey TcForeignImportInfo)
  }
  deriving (Eq, Show, Read)

-- | The identity of an instance: its dictionary origin and name.
type InstanceKey = ((Text, Text), Text)

tcInterfaceTerms :: TcInterface -> [(TcTermKey, TypeScheme)]
tcInterfaceTerms = Map.toList . tcInterfaceTermMap

tcInterfaceTyCons :: TcInterface -> [TyConInfo]
tcInterfaceTyCons = Map.elems . tcInterfaceTyConMap

tcInterfaceDataTypes :: TcInterface -> [DataTypeInfo]
tcInterfaceDataTypes = Map.elems . tcInterfaceDataTypeMap

tcInterfaceClasses :: TcInterface -> [ClassInfo]
tcInterfaceClasses = Map.elems . tcInterfaceClassMap

tcInterfaceInstances :: TcInterface -> [InstanceInfo]
tcInterfaceInstances = Map.elems . tcInterfaceInstanceMap

tcInterfaceDataFamilyInstances :: TcInterface -> [DataFamilyInstanceInfo]
tcInterfaceDataFamilyInstances = Map.elems . tcInterfaceDataFamilyInstanceMap

tcInterfaceTypeFamilyInstances :: TcInterface -> [TypeFamilyInstanceInfo]
tcInterfaceTypeFamilyInstances = Map.elems . tcInterfaceTypeFamilyInstanceMap

tcInterfacePatSyns :: TcInterface -> [PatSynInfo]
tcInterfacePatSyns = Map.elems . tcInterfacePatSynMap

tcInterfaceForeignImports :: TcInterface -> [(TcTermKey, TcForeignImportInfo)]
tcInterfaceForeignImports = Map.toList . tcInterfaceForeignImportMap

-- | Build an interface from lists of facts. Two facts with one identity
-- must be equal.
tcInterfaceFromLists :: [(TcTermKey, TypeScheme)] -> [TyConInfo] -> [DataTypeInfo] -> [ClassInfo] -> [InstanceInfo] -> [DataFamilyInstanceInfo] -> [TypeFamilyInstanceInfo] -> [PatSynInfo] -> [(TcTermKey, TcForeignImportInfo)] -> TcInterface
tcInterfaceFromLists terms tyCons dataTypes classes instances dataFamilyInstances typeFamilyInstances patSyns foreignImports =
  TcInterface
    { tcInterfaceTermMap = fromListChecked "term interface" id terms,
      tcInterfaceTyConMap = fromListChecked "type constructor interface" (keyed (tyConKey . tciTyCon)) tyCons,
      tcInterfaceDataTypeMap = fromListChecked "data type interface" (keyed dataTypeKey) dataTypes,
      tcInterfaceClassMap = fromListChecked "class interface" (keyed classInfoKey) classes,
      tcInterfaceInstanceMap = fromListChecked "instance interface" (keyed instanceInfoKey) instances,
      tcInterfaceDataFamilyInstanceMap = fromListChecked "data family instance interface" (keyed dataFamilyAxiomKey) dataFamilyInstances,
      tcInterfaceTypeFamilyInstanceMap = fromListChecked "type family instance interface" (keyed typeFamilyAxiomKey) typeFamilyInstances,
      tcInterfacePatSynMap = fromListChecked "pattern synonym interface" (keyed patSynKey) patSyns,
      tcInterfaceForeignImportMap = fromListChecked "foreign import interface" id foreignImports
    }
  where
    keyed key value = (key value, value)
    fromListChecked label key = Map.fromListWithKey (conflict label) . map key

emptyTcInterface :: TcInterface
emptyTcInterface =
  TcInterface
    { tcInterfaceTermMap = Map.empty,
      tcInterfaceTyConMap = Map.empty,
      tcInterfaceDataTypeMap = Map.empty,
      tcInterfaceClassMap = Map.empty,
      tcInterfaceInstanceMap = Map.empty,
      tcInterfaceDataFamilyInstanceMap = Map.empty,
      tcInterfaceTypeFamilyInstanceMap = Map.empty,
      tcInterfacePatSynMap = Map.empty,
      tcInterfaceForeignImportMap = Map.empty
    }

instance Semigroup TcInterface where
  (<>) = mergeTcInterface

instance Monoid TcInterface where
  mempty = emptyTcInterface

-- | Merge interfaces. Two facts with one identity must be equal; the
-- check runs only for identities present on both sides.
mergeTcInterfaces :: [TcInterface] -> TcInterface
mergeTcInterfaces [] = emptyTcInterface
mergeTcInterfaces (first : rest) = List.foldl' mergeTcInterface first rest

mergeTcInterface :: TcInterface -> TcInterface -> TcInterface
mergeTcInterface left right =
  TcInterface
    { tcInterfaceTermMap = merge "term interface" tcInterfaceTermMap,
      tcInterfaceTyConMap = merge "type constructor interface" tcInterfaceTyConMap,
      tcInterfaceDataTypeMap = merge "data type interface" tcInterfaceDataTypeMap,
      tcInterfaceClassMap = merge "class interface" tcInterfaceClassMap,
      tcInterfaceInstanceMap = merge "instance interface" tcInterfaceInstanceMap,
      tcInterfaceDataFamilyInstanceMap = merge "data family instance interface" tcInterfaceDataFamilyInstanceMap,
      tcInterfaceTypeFamilyInstanceMap = merge "type family instance interface" tcInterfaceTypeFamilyInstanceMap,
      tcInterfacePatSynMap = merge "pattern synonym interface" tcInterfacePatSynMap,
      tcInterfaceForeignImportMap = merge "foreign import interface" tcInterfaceForeignImportMap
    }
  where
    merge :: (Ord key, Show key, Eq value) => String -> (TcInterface -> Map.Map key value) -> Map.Map key value
    merge label select = Map.unionWithKey (conflict label) (select left) (select right)

conflict :: (Show key, Eq value) => String -> key -> value -> value -> value
conflict label key left right
  | left == right = left
  | otherwise = error ("conflicting " <> label <> " key: " <> show key)

-- | The left-biased union of interfaces. Use this when both sides are known
-- to agree, for example when one side extends the other.
unionTcInterfaces :: [TcInterface] -> TcInterface
unionTcInterfaces [] = emptyTcInterface
unionTcInterfaces (first : rest) = List.foldl' union first rest
  where
    union left right =
      TcInterface
        { tcInterfaceTermMap = Map.union (tcInterfaceTermMap left) (tcInterfaceTermMap right),
          tcInterfaceTyConMap = Map.union (tcInterfaceTyConMap left) (tcInterfaceTyConMap right),
          tcInterfaceDataTypeMap = Map.union (tcInterfaceDataTypeMap left) (tcInterfaceDataTypeMap right),
          tcInterfaceClassMap = Map.union (tcInterfaceClassMap left) (tcInterfaceClassMap right),
          tcInterfaceInstanceMap = Map.union (tcInterfaceInstanceMap left) (tcInterfaceInstanceMap right),
          tcInterfaceDataFamilyInstanceMap = Map.union (tcInterfaceDataFamilyInstanceMap left) (tcInterfaceDataFamilyInstanceMap right),
          tcInterfaceTypeFamilyInstanceMap = Map.union (tcInterfaceTypeFamilyInstanceMap left) (tcInterfaceTypeFamilyInstanceMap right),
          tcInterfacePatSynMap = Map.union (tcInterfacePatSynMap left) (tcInterfacePatSynMap right),
          tcInterfaceForeignImportMap = Map.union (tcInterfaceForeignImportMap left) (tcInterfaceForeignImportMap right)
        }

-- | Keep only facts that the selected modules define.
restrictTcInterfaceToModules :: PackageId -> [Text] -> TcInterface -> TcInterface
restrictTcInterfaceToModules package names interface =
  TcInterface
    { tcInterfaceTermMap = Map.filterWithKey (\key _ -> localTerm key) (tcInterfaceTermMap interface),
      tcInterfaceTyConMap = Map.filter (localTyCon . tciTyCon) (tcInterfaceTyConMap interface),
      tcInterfaceDataTypeMap = Map.filter (localTyCon . dtiTyCon) (tcInterfaceDataTypeMap interface),
      tcInterfaceClassMap = Map.filter (localTyCon . ciTyCon) (tcInterfaceClassMap interface),
      tcInterfaceInstanceMap = Map.filter localInstance (tcInterfaceInstanceMap interface),
      tcInterfaceDataFamilyInstanceMap = Map.filter (localTyCon . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstanceMap interface),
      tcInterfaceTypeFamilyInstanceMap = Map.filter localTypeFamilyInstance (tcInterfaceTypeFamilyInstanceMap interface),
      tcInterfacePatSynMap = Map.filterWithKey (\key _ -> localTerm key) (tcInterfacePatSynMap interface),
      tcInterfaceForeignImportMap = Map.filterWithKey (\key _ -> localTerm key) (tcInterfaceForeignImportMap interface)
    }
  where
    selected = Map.fromList [(name, ()) | name <- names]
    localModule moduleName' = Map.member moduleName' selected
    localTyCon tyCon = tyConPackageId tyCon == package && localModule (tyConModuleName tyCon)
    localTerm key =
      case key of
        TcTermGlobal package' moduleName' _ -> package' == package && localModule moduleName'
        TcTermLocal {} -> False
    localInstance info =
      let (packageName, moduleName') = iiDictOrigin info
       in packageName == packageIdText package && localModule moduleName'
    localTypeFamilyInstance info =
      let (originPackage, originModule) = tfiiOrigin info
       in originPackage == package && localModule originModule

tcTermKeyIdentifier :: TcTermKey -> Maybe Text
tcTermKeyIdentifier key =
  case key of
    TcTermLocal {} -> Nothing
    TcTermGlobal _ _ identifier -> Just identifier

-- | Convert stored type facts to the binding view required by System FC.
tcInterfaceBindings :: TcInterface -> [TcBindingResult]
tcInterfaceBindings interface =
  mapMaybe termBinding (tcInterfaceTerms interface)
    <> map instanceBinding (tcInterfaceInstances interface)
    <> concatMap classBindings (tcInterfaceClasses interface)
  where
    termBinding (TcTermGlobal _ _ identifier, scheme) = Just (TcBindingResult identifier identifier (interfaceSchemeType scheme))
    termBinding (TcTermLocal {}, _) = Nothing
    instanceBinding info = TcBindingResult (iiDictName info) (iiDictName info) (iiDictType info)
    classBindings info =
      [ TcBindingResult workerName workerName (interfaceSchemeType workerScheme)
      | methodName <- ciDefaultMethods info,
        Just methodScheme <- [lookup methodName (ciMethods info)],
        let workerName = defaultMethodName methodName
            workerScheme = maybe methodScheme (defaultWorkerScheme methodScheme) (lookup methodName (ciDefaultSignatures info))
      ]
    defaultWorkerScheme ordinaryScheme (ForAll variables predicates body) =
      case ordinaryScheme of
        ForAll _ (classPredicate : _) _ -> ForAll variables (classPredicate : predicates) body
        _ -> ForAll variables predicates body

interfaceSchemeType :: TypeScheme -> TcType
interfaceSchemeType (ForAll [] [] ty) = ty
interfaceSchemeType (ForAll variables [] ty) = foldr TcForAllTy ty variables
interfaceSchemeType (ForAll [] predicates ty) = TcQualTy predicates ty
interfaceSchemeType (ForAll variables predicates ty) = foldr TcForAllTy (TcQualTy predicates ty) variables

-- | Type-check a single expression in an empty environment.
--
-- This is the primary entry point for testing. For modules, use
-- `typecheckModulesWithInterface`.
typecheckExpr :: TcConfig -> Expr -> TcResult
typecheckExpr config expr =
  case runTcM (emptyTcEnv config) initTcState (typecheckExprM expr <* finalizeDiagnostics) of
    Left _abort ->
      TcResult
        { tcResultType = TcMetaTv (Unique (-1)),
          tcResultDiagnostics = [],
          tcResultSuccess = False
        }
    Right (ty, st) ->
      let diags = reverse (tcsDiagnostics st)
          hasErrors = any isError diags
       in TcResult
            { tcResultType = ty,
              tcResultDiagnostics = diags,
              tcResultSuccess = not hasErrors
            }
  where
    isError d = diagSeverity d == TcError

-- | Internal: type-check an expression in TcM.
typecheckExprM :: Expr -> TcM TcType
typecheckExprM expr = do
  -- 1. Generate constraints.
  (_expr', ty, cts) <- inferExpr expr
  -- 2. Solve constraints.
  _result <- solveConstraints cts
  -- 3. Zonk the result type.
  zonkType ty

-- | Top-level bindings recovered from a type-checked module's annotations.
tcModuleBindings :: TcKinds -> Module -> [TcBindingResult]
tcModuleBindings =
  moduleBindings

-- | Class instances recovered from a type-checked module's annotations.
tcModuleInstances :: TcKinds -> Module -> [InstanceInfo]
tcModuleInstances =
  moduleInstances

-- | Type classes recovered from a type-checked module's annotations.
tcModuleClasses :: Module -> [ClassInfo]
tcModuleClasses = moduleClasses

-- | Diagnostics recovered from type-checker annotations in a module.
tcModuleDiagnostics :: Module -> [TcDiagnostic]
tcModuleDiagnostics =
  collectTcDiagnostics

-- | Whether an annotated module contains no type-checker errors.
tcModuleSuccess :: Module -> Bool
tcModuleSuccess =
  not . any isError . tcModuleDiagnostics
  where
    isError diagnostic = diagSeverity diagnostic == TcError

-- | Type-check dependency-ordered modules with an imported semantic interface.
-- Return only facts that the specified modules define.
typecheckModulesWithInterface :: TcConfig -> TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModulesWithInterface config imported modules =
  let initialState = initialTcState imported
      persistentUnqualifiedTerms = Map.keys (Map.filterWithKey (\key _ -> isUnqualifiedTermKey key) (tcsGlobalTerms initialState))
      (checkedModules, finalState) = go persistentUnqualifiedTerms initialState modules
   in (checkedModules, tcInterfaceDifference initialState finalState)
  where
    go _ st [] = ([], st)
    go persistentUnqualifiedTerms st (m : ms) =
      let (result, st') = typecheckModuleWithState config st m
          nextState = removeTransientUnqualifiedTerms persistentUnqualifiedTerms st'
          (results, finalState) = go persistentUnqualifiedTerms nextState ms
       in (result : results, finalState)

removeTransientUnqualifiedTerms :: [TcTermKey] -> TcState -> TcState
removeTransientUnqualifiedTerms persistent state =
  state
    { tcsGlobalTerms =
        Map.filterWithKey
          (\key _ -> not (isUnqualifiedTermKey key) || key `elem` persistent)
          (tcsGlobalTerms state)
    }

isUnqualifiedTermKey :: TcTermKey -> Bool
isUnqualifiedTermKey key =
  case key of
    TcTermGlobal packageId moduleName _ -> T.null (packageIdText packageId) && T.null moduleName
    TcTermLocal {} -> False

-- | Type-check one strongly connected module component using only the
-- supplied imported interface.
typecheckModuleSccWithInterface :: TcConfig -> TcInterface -> [Module] -> ([Module], TcInterface)
typecheckModuleSccWithInterface config imported modules =
  let initialState = initialTcState imported
      (checkedModules, finalState) = typecheckModuleSccWithState config initialState modules
   in (checkedModules, tcInterfaceDifference initialState finalState)

initialTcState :: TcInterface -> TcState
initialTcState imported =
  initTcState
    { tcsGlobalTerms = Map.map (`TcIdBinder` Closed) (tcInterfaceTermMap imported) <> tcsGlobalTerms initTcState,
      tcsGlobalTyCons = tcInterfaceTyConMap imported <> tcsGlobalTyCons initTcState,
      tcsDataTypes = tcInterfaceDataTypeMap imported,
      tcsClasses = tcInterfaceClassMap imported,
      tcsInstances = instanceEnvFromList (tcInterfaceInstances imported),
      tcsDataFamilyInstances = tcInterfaceDataFamilyInstanceMap imported,
      tcsTypeFamilyInstances = tcInterfaceTypeFamilyInstanceMap imported,
      tcsPatSyns = tcInterfacePatSynMap imported,
      tcsForeignImports = tcInterfaceForeignImportMap imported
    }

tcInterfaceDifference :: TcState -> TcState -> TcInterface
tcInterfaceDifference initial state =
  TcInterface
    { tcInterfaceTermMap = exportedGlobalTerms (Map.difference (tcsGlobalTerms state) (tcsGlobalTerms initial)),
      tcInterfaceTyConMap = Map.difference (tcsGlobalTyCons state) (tcsGlobalTyCons initial),
      tcInterfaceDataTypeMap = Map.difference (tcsDataTypes state) (tcsDataTypes initial),
      tcInterfaceClassMap = Map.difference (tcsClasses state) (tcsClasses initial),
      tcInterfaceInstanceMap =
        Map.fromList
          [ (instanceInfoKey info, info)
          | info <- instanceEnvList (tcsInstances state),
            instanceInfoKey info `Set.notMember` initialInstanceKeys
          ],
      tcInterfaceDataFamilyInstanceMap = Map.difference (tcsDataFamilyInstances state) (tcsDataFamilyInstances initial),
      tcInterfaceTypeFamilyInstanceMap = Map.difference (tcsTypeFamilyInstances state) (tcsTypeFamilyInstances initial),
      tcInterfacePatSynMap = Map.difference (tcsPatSyns state) (tcsPatSyns initial),
      tcInterfaceForeignImportMap = Map.difference (tcsForeignImports state) (tcsForeignImports initial)
    }
  where
    initialInstanceKeys = Set.fromList (map instanceInfoKey (instanceEnvList (tcsInstances initial)))

exportedGlobalTerms :: Map.Map TcTermKey TcBinder -> Map.Map TcTermKey TypeScheme
exportedGlobalTerms globalTerms =
  Map.filterWithKey (\key _ -> not (isRedundantUnqualifiedAlias key)) terms
  where
    terms = Map.mapMaybe binderScheme globalTerms
    binderScheme binder =
      case binder of
        TcIdBinder scheme _ -> Just scheme
        _ -> Nothing
    qualifiedIdentifiers =
      Set.fromList
        [ name
        | TcTermGlobal packageId moduleName name <- Map.keys terms,
          not (T.null (packageIdText packageId)) || not (T.null moduleName)
        ]
    isRedundantUnqualifiedAlias key =
      case key of
        TcTermGlobal _ _ identifier
          | isUnqualifiedTermKey key -> identifier `Set.member` qualifiedIdentifiers
        _ -> False

typecheckModuleSccWithState :: TcConfig -> TcState -> [Module] -> ([Module], TcState)
typecheckModuleSccWithState config st modules =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModuleScc modules <* finalizeDiagnostics) of
    Left abort ->
      ( case modules of
          [] -> []
          first : rest -> annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] first : rest,
        st
      )
    Right (annotatedModules, st') ->
      let diags = reverse (tcsDiagnostics st')
          results = attachSccDiagnostics diags annotatedModules
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = mempty,
                tcsTrackedKindMetas = mempty,
                tcsEvBinds = Map.empty
              }
       in (results, nextState)
  where
    tcEnv =
      (emptyTcEnv config)
        { tcEnvMonoLocalBinds = any (elem MonoLocalBinds . moduleExtensions) modules,
          tcEnvMonomorphismRestriction = any (elem MonomorphismRestriction . moduleExtensions) modules,
          tcEnvScopedTypeVariables = any (elem ScopedTypeVariables . moduleExtensions) modules
        }
    moduleExtensions m = effectiveModuleExtensions (moduleLanguagePragmas m)

attachSccDiagnostics :: [TcDiagnostic] -> [Module] -> [Module]
attachSccDiagnostics diagnostics modules = foldl attachOne modules diagnostics
  where
    attachOne [] _ = []
    attachOne current@(first : rest) diagnostic =
      case diagLoc diagnostic of
        Nothing -> annotateModuleDiagnostics [diagnostic] first : rest
        Just span' ->
          let sourceName = sourceSpanSourceName span'
              matches m = sourceName `elem` moduleSourceNames m
           in if any matches current
                then map (\m -> if matches m then annotateModuleDiagnostics [diagnostic] m else m) current
                else annotateModuleDiagnostics [internalAbortDiagnostic "SCC diagnostic source did not match a module"] first : rest

moduleSourceNames :: Module -> [FilePath]
moduleSourceNames modu =
  case spanFromAnnotations (moduleAnns modu) of
    SourceSpan {sourceSpanSourceName = sourceName} -> [sourceName]
    NoSourceSpan -> []

typecheckModuleWithState :: TcConfig -> TcState -> Module -> (Module, TcState)
typecheckModuleWithState config st m =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule m <* finalizeDiagnostics) of
    Left abort ->
      ( annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] m,
        st
      )
    Right (annotatedModule, st') ->
      let diags = reverse (tcsDiagnostics st')
          result = annotateModuleDiagnostics diags annotatedModule
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = mempty,
                tcsTrackedKindMetas = mempty,
                tcsEvBinds = Map.empty
              }
       in (result, nextState)
  where
    tcEnv =
      (emptyTcEnv config)
        { tcEnvMonoLocalBinds = MonoLocalBinds `elem` enabledExtensions,
          tcEnvMonomorphismRestriction = MonomorphismRestriction `elem` enabledExtensions,
          tcEnvScopedTypeVariables = ScopedTypeVariables `elem` enabledExtensions
        }
    enabledExtensions = effectiveModuleExtensions (moduleLanguagePragmas m)

-- | The extensions of a module. The pragmas apply in source order, so a
-- later pragma wins, and an enabled extension brings its implied
-- extensions with it at once. A later NoMonoLocalBinds then turns off the
-- MonoLocalBinds that an earlier TypeFamilies implied, like in GHC.
annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  let (located, unlocated) = partitionDiagnostics diagnostics
      moduleWithLocated = foldl attachLocatedDiagnostic m located
   in moduleWithLocated {moduleAnns = moduleAnns moduleWithLocated <> map mkAnnotation unlocated}

partitionDiagnostics :: [TcDiagnostic] -> ([(SourceSpan, TcDiagnostic)], [TcDiagnostic])
partitionDiagnostics =
  foldr partitionOne ([], [])
  where
    partitionOne diagnostic (located, unlocated) =
      case diagLoc diagnostic of
        Just sp -> ((sp, diagnostic) : located, unlocated)
        Nothing -> (located, diagnostic : unlocated)

attachLocatedDiagnostic :: Module -> (SourceSpan, TcDiagnostic) -> Module
attachLocatedDiagnostic m (sp, diagnostic) =
  case runState (attachDiagnosticAt sp diagnostic m) False of
    (m', True) -> m'
    (_, False) ->
      error ("type checker diagnostic has no matching syntax node for source span: " <> show sp)

-- Attach bottom-up so an exact child span wins over an exact parent span.
-- Located diagnostics must never guess: if no exact syntax span exists, abort.
attachDiagnosticAt :: (Data a) => SourceSpan -> TcDiagnostic -> a -> State Bool a
attachDiagnosticAt sp diagnostic =
  everywhereM attachHere
  where
    attachHere :: forall node. (Data node) => node -> State Bool node
    attachHere value = do
      alreadyAttached <- get
      if alreadyAttached
        then pure value
        else case attachDiagnosticHere sp diagnostic value of
          Just value' -> do
            put True
            pure value'
          Nothing ->
            pure value

attachDiagnosticHere :: forall a. (Data a) => SourceSpan -> TcDiagnostic -> a -> Maybe a
attachDiagnosticHere sp diagnostic value =
  attachAnnotationList
    <|> attachExpr
    <|> attachPattern
    <|> attachType
    <|> attachDecl
    <|> attachDataConDecl
    <|> attachLiteral
    <|> attachGuardQualifier
    <|> attachDoStmtExpr
    <|> attachDoStmtCmd
    <|> attachCompStmt
    <|> attachArithSeq
    <|> attachClassDeclItem
    <|> attachInstanceDeclItem
    <|> attachCmd
    <|> attachExportSpec
    <|> attachImportItem
  where
    diagnosticAnn = mkAnnotation diagnostic
    atExactSpan span' wrap =
      if span' == sp
        then cast wrap
        else Nothing
    attachTyped :: forall node. (Data node) => (node -> Maybe node) -> Maybe a
    attachTyped f = do
      node <- cast value
      node' <- f node
      cast node'
    attachAnnotationList =
      attachTyped $ \(anns :: [Annotation]) ->
        atExactSpan (spanFromAnnotations anns) (anns <> [diagnosticAnn])
    attachExpr =
      attachTyped $ \(expr :: Expr) ->
        atExactSpan (wrappedSpan peelExprAnnOnce expr) (EAnn diagnosticAnn expr)
    attachPattern =
      attachTyped $ \(pat :: Pattern) ->
        atExactSpan (wrappedSpan peelPatternAnnOnce pat) (PAnn diagnosticAnn pat)
    attachType =
      attachTyped $ \(ty :: Type) ->
        atExactSpan (wrappedSpan peelTypeAnnOnce ty) (TAnn diagnosticAnn ty)
    attachDecl =
      attachTyped $ \(decl :: Decl) ->
        atExactSpan (wrappedSpan peelDeclAnnOnce decl) (DeclAnn diagnosticAnn decl)
    attachDataConDecl =
      attachTyped $ \(decl :: DataConDecl) ->
        atExactSpan (wrappedSpan peelDataConAnnOnce decl) (DataConAnn diagnosticAnn decl)
    attachLiteral =
      attachTyped $ \(lit :: Literal) ->
        atExactSpan (wrappedSpan peelLiteralAnnOnce lit) (LitAnn diagnosticAnn lit)
    attachGuardQualifier =
      attachTyped $ \(qualifier :: GuardQualifier) ->
        atExactSpan (wrappedSpan peelGuardAnnOnce qualifier) (GuardAnn diagnosticAnn qualifier)
    attachDoStmtExpr =
      attachTyped $ \(stmt :: DoStmt Expr) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachDoStmtCmd =
      attachTyped $ \(stmt :: DoStmt Cmd) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachCompStmt =
      attachTyped $ \(stmt :: CompStmt) ->
        atExactSpan (wrappedSpan peelCompAnnOnce stmt) (CompAnn diagnosticAnn stmt)
    attachArithSeq =
      attachTyped $ \(seq' :: ArithSeq) ->
        atExactSpan (wrappedSpan peelArithSeqAnnOnce seq') (ArithSeqAnn diagnosticAnn seq')
    attachClassDeclItem =
      attachTyped $ \(item :: ClassDeclItem) ->
        atExactSpan (wrappedSpan peelClassItemAnnOnce item) (ClassItemAnn diagnosticAnn item)
    attachInstanceDeclItem =
      attachTyped $ \(item :: InstanceDeclItem) ->
        atExactSpan (wrappedSpan peelInstanceItemAnnOnce item) (InstanceItemAnn diagnosticAnn item)
    attachCmd =
      attachTyped $ \(cmd :: Cmd) ->
        atExactSpan (wrappedSpan peelCmdAnnOnce cmd) (CmdAnn diagnosticAnn cmd)
    attachExportSpec =
      attachTyped $ \(spec :: ExportSpec) ->
        atExactSpan (wrappedSpan peelExportAnnOnce spec) (ExportAnn diagnosticAnn spec)
    attachImportItem =
      attachTyped $ \(item :: ImportItem) ->
        atExactSpan (wrappedSpan peelImportAnnOnce item) (ImportAnn diagnosticAnn item)

wrappedSpan :: (node -> Maybe (Annotation, node)) -> node -> SourceSpan
wrappedSpan peel =
  spanFromAnnotations . fst . peelLeading peel

peelLeading :: (node -> Maybe (Annotation, node)) -> node -> ([Annotation], node)
peelLeading peel =
  go []
  where
    go anns node =
      case peel node of
        Just (ann, inner) -> go (ann : anns) inner
        Nothing -> (reverse anns, node)

peelExprAnnOnce :: Expr -> Maybe (Annotation, Expr)
peelExprAnnOnce (EAnn ann inner) = Just (ann, inner)
peelExprAnnOnce _ = Nothing

peelPatternAnnOnce :: Pattern -> Maybe (Annotation, Pattern)
peelPatternAnnOnce (PAnn ann inner) = Just (ann, inner)
peelPatternAnnOnce _ = Nothing

peelTypeAnnOnce :: Type -> Maybe (Annotation, Type)
peelTypeAnnOnce (TAnn ann inner) = Just (ann, inner)
peelTypeAnnOnce _ = Nothing

peelDeclAnnOnce :: Decl -> Maybe (Annotation, Decl)
peelDeclAnnOnce (DeclAnn ann inner) = Just (ann, inner)
peelDeclAnnOnce _ = Nothing

peelDataConAnnOnce :: DataConDecl -> Maybe (Annotation, DataConDecl)
peelDataConAnnOnce (DataConAnn ann inner) = Just (ann, inner)
peelDataConAnnOnce _ = Nothing

peelLiteralAnnOnce :: Literal -> Maybe (Annotation, Literal)
peelLiteralAnnOnce (LitAnn ann inner) = Just (ann, inner)
peelLiteralAnnOnce _ = Nothing

peelGuardAnnOnce :: GuardQualifier -> Maybe (Annotation, GuardQualifier)
peelGuardAnnOnce (GuardAnn ann inner) = Just (ann, inner)
peelGuardAnnOnce _ = Nothing

peelDoAnnOnce :: DoStmt body -> Maybe (Annotation, DoStmt body)
peelDoAnnOnce (DoAnn ann inner) = Just (ann, inner)
peelDoAnnOnce _ = Nothing

peelCompAnnOnce :: CompStmt -> Maybe (Annotation, CompStmt)
peelCompAnnOnce (CompAnn ann inner) = Just (ann, inner)
peelCompAnnOnce _ = Nothing

peelArithSeqAnnOnce :: ArithSeq -> Maybe (Annotation, ArithSeq)
peelArithSeqAnnOnce (ArithSeqAnn ann inner) = Just (ann, inner)
peelArithSeqAnnOnce _ = Nothing

peelClassItemAnnOnce :: ClassDeclItem -> Maybe (Annotation, ClassDeclItem)
peelClassItemAnnOnce (ClassItemAnn ann inner) = Just (ann, inner)
peelClassItemAnnOnce _ = Nothing

peelInstanceItemAnnOnce :: InstanceDeclItem -> Maybe (Annotation, InstanceDeclItem)
peelInstanceItemAnnOnce (InstanceItemAnn ann inner) = Just (ann, inner)
peelInstanceItemAnnOnce _ = Nothing

peelCmdAnnOnce :: Cmd -> Maybe (Annotation, Cmd)
peelCmdAnnOnce (CmdAnn ann inner) = Just (ann, inner)
peelCmdAnnOnce _ = Nothing

peelExportAnnOnce :: ExportSpec -> Maybe (Annotation, ExportSpec)
peelExportAnnOnce (ExportAnn ann inner) = Just (ann, inner)
peelExportAnnOnce _ = Nothing

peelImportAnnOnce :: ImportItem -> Maybe (Annotation, ImportItem)
peelImportAnnOnce (ImportAnn ann inner) = Just (ann, inner)
peelImportAnnOnce _ = Nothing

spanFromAnnotations :: [Annotation] -> SourceSpan
spanFromAnnotations =
  fromMaybe NoSourceSpan . foldr ((<|>) . spanFromAnnotation) Nothing

spanFromAnnotation :: Annotation -> Maybe SourceSpan
spanFromAnnotation =
  concreteSpan <=< fromAnnotation

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan sp = Just sp

collectTcDiagnostics :: Module -> [TcDiagnostic]
collectTcDiagnostics = mapMaybe fromAnnotation . annotationList

internalAbortDiagnostic :: String -> TcDiagnostic
internalAbortDiagnostic msg =
  TcDiagnostic
    { diagLoc = Nothing,
      diagSeverity = TcError,
      diagKind = OtherError ("internal type checker abort: " <> msg)
    }
