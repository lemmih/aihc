{-# LANGUAGE OverloadedStrings #-}

-- | The type checker monad and state.
module Aihc.Tc.Monad
  ( -- * Monad
    TcM,
    runTcM,
    abortTc,
    tcAbortMessage,

    -- * State
    TcState (..),
    initTcState,

    -- * Fresh names
    freshUnique,
    freshMetaTv,
    freshMetaTvOfKind,
    freshSkolemTv,
    freshEvVar,
    getUniqueBoundary,

    -- * Meta-variable solutions
    writeMetaTv,
    readMetaTv,
    trackKindMeta,
    isTrackedKindMeta,
    readMetaTvKind,

    -- * Evidence
    bindEvidence,
    lookupEvidence,

    -- * Environment
    TcConfig (..),
    mkTcConfig,
    getDerivingReferences,
    getWiring,
    getKinds,
    arrowType,
    TcWiring (..),
    wiredTupleTyCon,
    wiredTupleDataCon,
    wiredTyCon,
    wiredTyConIdentity,
    lookupWiredTerm,
    boolType,
    charType,
    listTyConOfWiring,
    TcEnv (..),
    TcBinder (..),
    TcTermKey (..),
    unqualifiedTermKey,
    Closedness (..),
    emptyTcEnv,
    mkWiredTyCon,
    implicitParamType,
    lookupTerm,
    lookupResolvedTerm,
    lookupTermKey,
    resolvedTermKey,
    resolvedTargetTermKey,
    resolvedTermTarget,
    resolvedUnqualifiedTermKey,
    resolvedLocalTermKey,
    extendTermEnv,
    rebindTermEnv,
    extendResolvedTermEnv,
    extendTermKeyEnvPermanent,
    extendTermEnvPermanent,
    replaceTermKeyEnvPermanent,
    finalizeInferredTermEnvPermanent,
    extendTyConTermEnvPermanent,
    extendResolvedTermEnvPermanent,
    getTermEnv,
    withVisibleTerms,
    isTermVisible,
    lookupTyCon,
    lookupTyConQualified,
    lookupResolvedTyCon,
    lookupResolvedTypeSyntax,
    lookupDeclaredTyCon,
    lookupTyConByIdentity,
    extendTyConEnvPermanent,
    replaceTyConEnvPermanent,
    getTyConEnv,
    addDataType,
    addPatSyn,
    getPatSyns,
    lookupPatSyn,
    lookupPatSynTarget,
    patSynKey,
    getDataTypes,
    lookupDataType,
    localTcOptions,
    tcMonoLocalBinds,
    tcMonomorphismRestriction,
    localDefaultTypes,
    getDefaultTypes,
    withScopedTyVars,
    getScopedTyVars,
    withGivenPredicates,
    getGivenPredicates,
    getTcLevel,
    withTcLevel,
    addInstance,
    getInstances,
    getClassInstances,
    addDataFamilyInstance,
    getDataFamilyInstances,
    addTypeFamilyInstance,
    getTypeFamilyInstances,
    addClass,
    getClasses,
    lookupClass,
    lookupClassByName,
    lookupClassNamed,
    lookupDeclaredClass,

    -- * GADT constructor registry
    markGadtCon,
    isGadtCon,

    -- * Diagnostics
    emitDiagnostic,
    emitError,
    emitWarning,
    getDiagnostics,
    withErrorTracking,
    currentErrorCount,

    -- * Speculation
    tcSpeculate,
  )
where

import Aihc.Parser.Syntax (Annotation, Name (..), SourceSpan (..), TupleFlavor, UnqualifiedName (..), fromAnnotation, nameText, unqualifiedNameText)
import Aihc.Resolve (PackageId (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..), displayIdentifier)
import Aihc.Tc.Annotations (TcForeignImportInfo)
import Aihc.Tc.Deriving.References (DerivingReferences)
import Aihc.Tc.Env (ClassInfo (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), InstanceEnv, InstanceInfo (..), PatSynInfo (..), TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..), addInstanceEnv, classInfoKey, dataFamilyAxiomKey, dataTypeKey, emptyInstanceEnv, instanceEnvForClass, instanceEnvList, instanceInfoKey, typeFamilyAxiomKey)
import Aihc.Tc.Error
import Aihc.Tc.Evidence
import Aihc.Tc.Types
import Aihc.Tc.Wiring (TcWiring (..), mkTcKinds, tupleDataCon, tupleTyCon)
import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', put, runStateT)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | The type checker monad.
--
-- Uses 'ReaderT' for the local environment and 'StateT' for mutable state
-- (fresh name supply, meta-variable solutions, evidence bindings, diagnostics).
type TcM a = ReaderT TcEnv (StateT TcState TcResult) a

-- | Type-checker result, used as the base monad.
-- (We avoid IO/ST for the MVP; the Map-based meta-variable store is
-- functionally equivalent to STRef and can be migrated later.)
type TcResult = Either TcAbort

-- | Fatal abort (internal error, not a user-facing diagnostic).
newtype TcAbort = TcAbort String
  deriving (Show)

-- | Run the type checker computation.
runTcM :: TcEnv -> TcState -> TcM a -> Either TcAbort (a, TcState)
runTcM env st m = runStateT (runReaderT m env) st

abortTc :: String -> TcM a
abortTc msg = lift (lift (Left (TcAbort msg)))

tcAbortMessage :: TcAbort -> String
tcAbortMessage (TcAbort msg) = msg

-- | The local typing environment (read-only within a scope).
data TcEnv = TcEnv
  { tcEnvConfig :: !TcConfig,
    -- | Local term bindings in scope.
    --
    -- The keys come from @aihc-resolve@'s 'ResolvedLocal' identifiers, not
    -- from source text. This lets TC preserve lexical identity without doing
    -- name resolution or conflating duplicate textual names.
    tcEnvTerms :: !(Map TcTermKey TcBinder),
    -- | Whether local binding groups follow GHC's MonoLocalBinds rule.
    tcEnvMonoLocalBinds :: !Bool,
    -- | Whether the monomorphism restriction is active.
    tcEnvMonomorphismRestriction :: !Bool,
    -- | Current implication nesting level.
    tcEnvTcLevel :: !TcLevel,
    -- | The candidate types of the module @default@ declaration.
    --
    -- 'Nothing' means the module has no @default@ declaration, so defaulting
    -- uses the Haskell 2010 standard list. @default ()@ gives @Just []@ and
    -- turns defaulting off.
    tcEnvDefaultTypes :: !(Maybe [TcType]),
    -- | Whether ScopedTypeVariables is on. Without it, no binding scopes
    -- its type variables over its body.
    tcEnvScopedTypeVariables :: !Bool,
    -- | The lexically scoped type variables, by source name. A signature
    -- with an explicit @forall@, an instance head, or a class head binds
    -- them over the bodies it covers.
    tcEnvGivenPredicates :: ![Pred],
    tcEnvScopedTyVars :: !(Map Text (TyVarId, TcType)),
    tcEnvVisibleTerms :: !(Set.Set TcTermKey)
  }
  deriving (Show)

-- | Facts about the surrounding compiler that the type checker cannot
-- derive from source: the identity of the primitive package, the library
-- names that generated deriving code refers to, and the type constructors
-- that built-in syntax denotes.
data TcConfig = TcConfig
  { tcConfigPrimPackage :: !PackageId,
    tcConfigDerivingReferences :: !DerivingReferences,
    tcConfigWiring :: !TcWiring,
    -- | The kind vocabulary of the wiring, resolved once.
    tcConfigKinds :: !TcKinds
  }
  deriving (Show)

-- | A configuration from the tables of one compiler. The type checker has
-- no default: every table names a library it does not itself define.
mkTcConfig :: PackageId -> DerivingReferences -> TcWiring -> TcConfig
mkTcConfig primPackage references wiring =
  TcConfig primPackage references wiring (mkTcKinds wiring)

getDerivingReferences :: TcM DerivingReferences
getDerivingReferences = asks (tcConfigDerivingReferences . tcEnvConfig)

getWiring :: TcM TcWiring
getWiring = asks (tcConfigWiring . tcEnvConfig)

-- | The kind vocabulary the type checker builds its own kinds from.
getKinds :: TcM TcKinds
getKinds = asks (tcConfigKinds . tcEnvConfig)

-- | The tuple type constructor that one syntactic form denotes.
wiredTupleTyCon :: TupleFlavor -> Int -> TcM TyCon
wiredTupleTyCon flavor arity = do
  wiring <- getWiring
  pure (tupleTyCon wiring flavor arity)

-- | The tuple data constructor that one syntactic form denotes.
wiredTupleDataCon :: TupleFlavor -> Int -> TcM TyCon
wiredTupleDataCon flavor arity = do
  wiring <- getWiring
  pure (tupleDataCon wiring flavor arity)

-- | The identity that one wiring entry names, without registering a kind.
wiredTyConIdentity :: (TcWiring -> TyCon) -> TcM TyCon
wiredTyConIdentity select = select <$> getWiring

-- | The type constructor that one wiring entry names, with its kind
-- registered on first use. The wiring gives the whole identity, so a
-- declaration of that identity is found by key; a declaration of the same
-- bare name elsewhere is not this constructor and does not take its place.
wiredTyCon :: (TcWiring -> TyCon) -> TcType -> TcM TyCon
wiredTyCon select kind = do
  wired <- wiredTyConIdentity select
  mkWiredTyCon wired kind

-- | The function arrow as a type, and the declaration that goes with it.
--
-- 'TcArrowTy' is a form rather than a type constructor, so the type
-- checker recognises an arrow without consulting the wiring. The
-- desugarer does not: a partially applied arrow reaches it as an ordinary
-- type constructor and the name has to be bound there. Every place that
-- builds an arrow goes through here, so the constructor is declared in
-- the interface of exactly the modules that can mention it.
arrowType :: TcM TcType
arrowType = do
  kinds <- getKinds
  let kind = KFun (typeKind kinds) (KFun (typeKind kinds) (typeKind kinds))
  _ <- wiredTyCon tcWiringArrowTyCon kind
  pure TcArrowTy

-- | The binder of a term that a wiring entry names. The wiring gives the
-- whole identity, so the term is found by key and never by scope.
lookupWiredTerm :: TyCon -> TcM (Maybe TcBinder)
lookupWiredTerm wired =
  lookupTermKey (TcTermGlobal (tyConPackageId wired) (tyConModuleName wired) (tyConName wired))

-- | The @Bool@ type that a guard and an @if@ condition have.
boolType :: TcM TcType
boolType = do
  kinds <- getKinds
  tyCon <- wiredTyCon tcWiringBoolTyCon (typeKind kinds)
  pure (TcTyCon tyCon [])

-- | The @Char@ type that a character literal has.
charType :: TcM TcType
charType = do
  kinds <- getKinds
  tyCon <- wiredTyCon tcWiringCharTyCon (typeKind kinds)
  pure (TcTyCon tyCon [])

-- | The list type constructor, for a list literal or a comprehension.
listTyConOfWiring :: TcM TyCon
listTyConOfWiring = do
  kinds <- getKinds
  wiredTyCon tcWiringListTyCon (KFun (typeKind kinds) (typeKind kinds))

-- | The constraint type for one implicit parameter, such as @?x :: Int@.
--
-- Each parameter name gets its own type constructor of kind @Type -> Constraint@.
implicitParamType :: Text -> TcType -> TcM TcType
implicitParamType name payload = do
  wiring <- getWiring
  kinds <- getKinds
  tyCon <- mkWiredTyCon (tcWiringImplicitParamTyCon wiring name) (KFun (typeKind kinds) (constraintKind kinds))
  pure (TcTyCon tyCon [payload])

-- | Register the kind of a type constructor whose identity is already
-- known, such as one that comes from the wiring tables. An identity that
-- already has a kind keeps it.
mkWiredTyCon :: TyCon -> TcType -> TcM TyCon
mkWiredTyCon tyCon kind = do
  maybeInfo <- lookupTyConByIdentity tyCon
  case maybeInfo of
    Just info -> pure (tciTyCon info)
    Nothing -> do
      let info = TyConInfo (tyConName tyCon) (tyConArity tyCon) tyCon (ForAll [] [] kind) DataTyCon Nothing
      lift $ modify' $ \state -> state {tcsGlobalTyCons = Map.insert (tyConKey tyCon) info (tcsGlobalTyCons state)}
      pure tyCon

-- | Whether a polymorphic binding is known to have no free type variables.
data Closedness
  = Closed
  | NotClosed
  deriving (Eq, Show)

-- | A binding in the term environment.
data TcBinder
  = -- | Polymorphic binding (top-level or let with signature).
    TcIdBinder !TypeScheme !Closedness
  | -- | Monomorphic binding (lambda-bound, pattern-bound, local let).
    TcMonoIdBinder !TcType
  deriving (Show)

data TcTermKey
  = TcTermLocal !Int
  | TcTermGlobal !PackageId !Text !Text
  deriving (Eq, Ord, Show, Read)

unqualifiedTermKey :: Text -> TcTermKey
unqualifiedTermKey = TcTermGlobal (PackageId "") ""

-- | An empty environment at the top level.
emptyTcEnv :: TcConfig -> TcEnv
emptyTcEnv config =
  TcEnv
    { tcEnvConfig = config,
      tcEnvTerms = Map.empty,
      tcEnvMonoLocalBinds = True,
      tcEnvMonomorphismRestriction = True,
      tcEnvTcLevel = topTcLevel,
      tcEnvDefaultTypes = Nothing,
      tcEnvScopedTypeVariables = False,
      tcEnvGivenPredicates = [],
      tcEnvScopedTyVars = Map.empty,
      tcEnvVisibleTerms = Set.empty
    }

-- | The mutable state of the type checker.
data TcState = TcState
  { -- | Next unique identifier to allocate.
    tcsNextUnique :: !Int,
    -- | Solutions for meta (unification) variables.
    tcsMetaSolutions :: !(IntMap TcType),
    -- | Meta-variables that can default to 'Type' at a kind boundary.
    tcsTrackedKindMetas :: !IntSet,
    -- | Declared kinds of representation-polymorphic meta-variables.
    tcsMetaKinds :: !(IntMap TcType),
    -- | Evidence bindings accumulated during solving.
    tcsEvBinds :: !(Map Unique EvTerm),
    -- | Diagnostics (errors and warnings) collected.
    tcsDiagnostics :: ![TcDiagnostic],
    -- | Global term bindings accumulated from declarations and imported
    -- interfaces.
    --
    -- Global keys store the package, module, and identifier selected by
    -- @aihc-resolve@.
    tcsGlobalTerms :: !(Map TcTermKey TcBinder),
    -- | Global type constructors accumulated by top-level declarations.
    tcsGlobalTyCons :: !(Map TcTypeKey TyConInfo),
    -- | Checked constructor layouts for data and newtype declarations.
    tcsDataTypes :: !(Map TcTypeKey DataTypeInfo),
    -- | Type classes in scope, including their superclass layouts and defaults.
    tcsClasses :: !(Map TcTypeKey ClassInfo),
    -- | Class instances in scope.
    tcsInstances :: !InstanceEnv,
    -- | Standalone data-family instance equations in scope.
    tcsDataFamilyInstances :: !(Map TcAxiomKey DataFamilyInstanceInfo),
    -- | Type-family equations in scope.
    tcsTypeFamilyInstances :: !(Map TcAxiomKey TypeFamilyInstanceInfo),
    -- | Names of GADT constructors (have non-trivial result types).
    tcsGadtCons :: !(Set Text),
    -- | Pattern synonyms in scope, keyed like their builder term.
    tcsPatSyns :: !(Map TcTermKey PatSynInfo),
    -- | The checked calling convention of each foreign import in scope.
    tcsForeignImports :: !(Map TcTermKey TcForeignImportInfo)
  }
  deriving (Show)

-- | Initial state with no variables or bindings.
initTcState :: TcState
initTcState =
  TcState
    { tcsNextUnique = 0,
      tcsMetaSolutions = IntMap.empty,
      tcsTrackedKindMetas = IntSet.empty,
      tcsMetaKinds = IntMap.empty,
      tcsEvBinds = Map.empty,
      tcsDiagnostics = [],
      tcsGlobalTerms = Map.empty,
      tcsGlobalTyCons = Map.empty,
      tcsDataTypes = Map.empty,
      tcsPatSyns = Map.empty,
      tcsClasses = Map.empty,
      tcsInstances = emptyInstanceEnv,
      tcsDataFamilyInstances = Map.empty,
      tcsTypeFamilyInstances = Map.empty,
      tcsGadtCons = Set.empty,
      tcsForeignImports = Map.empty
    }

-- | Allocate a fresh 'Unique'.
freshUnique :: TcM Unique
freshUnique = lift $ do
  st <- get
  let u = tcsNextUnique st
  modify' (\s -> s {tcsNextUnique = u + 1})
  pure (Unique u)

-- | Allocate a fresh meta (unification) type variable.
freshMetaTv :: TcM TcType
freshMetaTv = do
  kinds <- getKinds
  kindUnique@(Unique kindKey) <- freshUnique
  lift $ modify' $ \state ->
    state
      { tcsMetaKinds = IntMap.insert kindKey (typeKind kinds) (tcsMetaKinds state),
        tcsTrackedKindMetas = IntSet.insert kindKey (tcsTrackedKindMetas state)
      }
  freshMetaTvOfKind (TcMetaTv kindUnique)

freshMetaTvOfKind :: TcType -> TcM TcType
freshMetaTvOfKind kind = do
  unique@(Unique key) <- freshUnique
  lift $ modify' $ \state ->
    state {tcsMetaKinds = IntMap.insert key kind (tcsMetaKinds state)}
  pure (TcMetaTv unique)

-- | Allocate a fresh skolem (rigid) type variable.
freshSkolemTv :: Text -> TcM TyVarId
freshSkolemTv name = do
  kinds <- getKinds
  u <- freshUnique
  pure (mkTyVarId name u (typeKind kinds))

-- | Allocate a fresh evidence variable.
freshEvVar :: TcM EvVar
freshEvVar = EvVar <$> freshUnique

-- | Snapshot the unique supply. Uniques below this boundary were allocated
-- before the current type-checking region.
getUniqueBoundary :: TcM Unique
getUniqueBoundary = Unique <$> lift (gets tcsNextUnique)

-- | Record the solution for a meta-variable.
writeMetaTv :: Unique -> TcType -> TcM ()
writeMetaTv (Unique key) ty = lift $ modify' $ \s ->
  s {tcsMetaSolutions = IntMap.insert key ty (tcsMetaSolutions s)}

-- | Look up the current solution for a meta-variable.
readMetaTv :: Unique -> TcM (Maybe TcType)
readMetaTv (Unique key) = lift $ gets $ \s ->
  IntMap.lookup key (tcsMetaSolutions s)

readMetaTvKind :: Unique -> TcM TcType
readMetaTvKind (Unique key) = do
  kinds <- getKinds
  lift $ gets $ IntMap.findWithDefault (typeKind kinds) key . tcsMetaKinds

trackKindMeta :: Unique -> TcM ()
trackKindMeta (Unique key) =
  lift $ modify' $ \state ->
    state {tcsTrackedKindMetas = IntSet.insert key (tcsTrackedKindMetas state)}

isTrackedKindMeta :: Unique -> TcM Bool
isTrackedKindMeta (Unique key) =
  lift $ gets $ IntSet.member key . tcsTrackedKindMetas

-- | Bind an evidence variable to an evidence term.
bindEvidence :: EvVar -> EvTerm -> TcM ()
bindEvidence (EvVar u) ev = lift $ modify' $ \s ->
  s {tcsEvBinds = Map.insert u ev (tcsEvBinds s)}

-- | Look up an evidence binding.
lookupEvidence :: EvVar -> TcM (Maybe EvTerm)
lookupEvidence (EvVar u) = lift $ gets $ \s ->
  Map.lookup u (tcsEvBinds s)

-- | Look up a global term by its selected global name.
lookupTerm :: Text -> TcM (Maybe TcBinder)
lookupTerm name =
  lift $ gets $ \s -> Map.lookup (unqualifiedTermKey name) (tcsGlobalTerms s)

lookupResolvedTerm :: Text -> ResolvedName -> TcM (Maybe TcBinder)
lookupResolvedTerm displayName resolved = do
  exact <- resolvedNameTermKey displayName resolved >>= lookupTermKey
  case (exact, resolved) of
    (Nothing, ResolvedTopLevel _ name) -> lookupTerm (nameText name)
    _ -> pure exact

lookupTermKey :: TcTermKey -> TcM (Maybe TcBinder)
lookupTermKey key =
  case key of
    TcTermLocal _ ->
      asks $ \env -> Map.lookup key (tcEnvTerms env)
    TcTermGlobal {} ->
      lift $ gets $ \s -> Map.lookup key (tcsGlobalTerms s)

resolvedTermKey :: Name -> TcM TcTermKey
resolvedTermKey name =
  resolvedTargetTermKey (nameText name) =<< resolvedTermTarget name

resolvedTargetTermKey :: Text -> ResolvedName -> TcM TcTermKey
resolvedTargetTermKey = resolvedNameTermKey

resolvedUnqualifiedTermKey :: UnqualifiedName -> TcM TcTermKey
resolvedUnqualifiedTermKey name =
  case termResolution (unqualifiedNameAnns name) of
    Just resolution ->
      resolvedNameTermKey (unqualifiedNameText name) (resolutionTarget resolution)
    Nothing ->
      abortTc ("missing resolver annotation for binder " <> show (unqualifiedNameText name))

resolvedNameTermKey :: Text -> ResolvedName -> TcM TcTermKey
resolvedNameTermKey displayName resolved =
  case resolved of
    ResolvedLocal unique _ ->
      pure (TcTermLocal unique)
    ResolvedTopLevel packageId name ->
      pure (TcTermGlobal packageId (fromMaybe "" (nameQualifier name)) (nameText name))
    ResolvedSyntax ->
      pure (unqualifiedTermKey displayName)
    ResolvedError msg ->
      abortTc ("resolver error reached type checker for term " <> show displayName <> ": " <> msg)

-- | Snapshot all visible term bindings keyed by resolver-selected identity.
getTermEnv :: TcM (Map TcTermKey TcBinder)
getTermEnv = do
  locals <- asks tcEnvTerms
  globals <- lift $ gets tcsGlobalTerms
  pure (locals <> globals)

-- | Use the resolver's scope facts without another name resolution pass.
withVisibleTerms :: [TcTermKey] -> TcM a -> TcM a
withVisibleTerms terms = local (\env -> env {tcEnvVisibleTerms = Set.fromList terms})

isTermVisible :: TcTermKey -> TcM Bool
isTermVisible key = asks (Set.member key . tcEnvVisibleTerms)

-- | Extend the term environment with a new binding for the duration
-- of the given computation.
extendTermEnv :: TcTermKey -> TcBinder -> TcM a -> TcM a
extendTermEnv key binder action = do
  terms <- asks tcEnvTerms
  terms' <- insertNewMap "local term environment" key binder terms
  local (\env -> env {tcEnvTerms = terms'}) action

rebindTermEnv :: TcTermKey -> TcBinder -> TcM a -> TcM a
rebindTermEnv key binder =
  local (\env -> env {tcEnvTerms = Map.insert key binder (tcEnvTerms env)})

extendResolvedTermEnv :: UnqualifiedName -> TcBinder -> TcM a -> TcM a
extendResolvedTermEnv name binder action = do
  key <- resolvedLocalTermKey name
  extendTermEnv key binder action

-- | Permanently extend the global term environment (for top-level
-- declarations like data constructors and top-level bindings).
extendTermKeyEnvPermanent :: TcTermKey -> TcBinder -> TcM ()
extendTermKeyEnvPermanent key binder = do
  terms <- lift $ gets tcsGlobalTerms
  terms' <- insertNewMap "global term environment" key binder terms
  lift $ modify' $ \state -> state {tcsGlobalTerms = terms'}

extendTermEnvPermanent :: Text -> TcBinder -> TcM ()
extendTermEnvPermanent name = extendTermKeyEnvPermanent (unqualifiedTermKey name)

-- | Replace a permanent global term entry. A synthesized binding registers
-- a provisional type before its check and the checked type after it.
replaceTermKeyEnvPermanent :: TcTermKey -> TcBinder -> TcM ()
replaceTermKeyEnvPermanent key binder =
  lift $ modify' $ \state -> state {tcsGlobalTerms = Map.insert key binder (tcsGlobalTerms state)}

-- | Replace the temporary monomorphic entries for one inferred top-level
-- binding. No other permanent term entry can use this operation.
finalizeInferredTermEnvPermanent :: Text -> TcTermKey -> TcType -> TypeScheme -> TcM ()
finalizeInferredTermEnvPermanent name key placeholderTy scheme = do
  terms <- lift $ gets tcsGlobalTerms
  terms' <- foldM finalizePlaceholder terms [unqualifiedTermKey name, key]
  lift $ modify' $ \state -> state {tcsGlobalTerms = terms'}
  where
    finalizedBinder = TcIdBinder scheme Closed
    finalizePlaceholder entries placeholderKey =
      case Map.lookup placeholderKey entries of
        Just (TcMonoIdBinder existingTy)
          | existingTy == placeholderTy ->
              pure (Map.insert placeholderKey finalizedBinder entries)
        Just _ ->
          abortTc ("global term key is not the expected inferred placeholder: " <> show placeholderKey)
        Nothing ->
          abortTc ("missing inferred term placeholder key: " <> show placeholderKey)

extendTyConTermEnvPermanent :: TyCon -> Text -> TcBinder -> TcM ()
extendTyConTermEnvPermanent tyCon name binder = do
  extendTermEnvPermanent name binder
  extendTermKeyEnvPermanent
    (TcTermGlobal (tyConPackageId tyCon) (tyConModuleName tyCon) name)
    binder

-- | Add a source binder under its resolver identity and its source name.
extendResolvedTermEnvPermanent :: UnqualifiedName -> TcBinder -> TcM ()
extendResolvedTermEnvPermanent name binder = do
  extendTermEnvPermanent (unqualifiedNameText name) binder
  case termResolution (unqualifiedNameAnns name) of
    Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId resolvedName} ->
      extendTermKeyEnvPermanent
        (TcTermGlobal packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName))
        binder
    _ -> pure ()

resolvedTermTarget :: Name -> TcM ResolvedName
resolvedTermTarget name =
  case termResolution (nameAnns name) of
    Just resolution -> pure (resolutionTarget resolution)
    Nothing ->
      abortTc ("missing resolver annotation for term occurrence " <> show (nameText name))

resolvedLocalTermKey :: UnqualifiedName -> TcM TcTermKey
resolvedLocalTermKey name =
  case termResolution (unqualifiedNameAnns name) of
    Just resolution ->
      case resolutionTarget resolution of
        ResolvedLocal unique _ -> pure (TcTermLocal unique)
        target ->
          abortTc ("expected local resolver annotation for binder " <> show (unqualifiedNameText name) <> ", got " <> show target)
    Nothing ->
      abortTc ("missing resolver annotation for binder " <> show (unqualifiedNameText name))

termResolution :: [Annotation] -> Maybe ResolutionAnnotation
termResolution =
  find ((== ResolutionNamespaceTerm) . resolutionNamespace)
    . mapMaybe fromAnnotation

lookupTyCon :: Text -> TcM (Maybe TyConInfo)
lookupTyCon = lookupTyConInNamespace ResolutionNamespaceType

lookupTyConInNamespace :: ResolutionNamespace -> Text -> TcM (Maybe TyConInfo)
lookupTyConInNamespace namespace name =
  lift $ gets $ find matches . Map.elems . tcsGlobalTyCons
  where
    matches info =
      tyConNamespace (tciTyCon info) == namespace
        && (tciName info == name || tyConName (tciTyCon info) == name)

lookupTyConQualified :: Text -> Text -> TcM (Maybe TyConInfo)
lookupTyConQualified = lookupTyConQualifiedInNamespace ResolutionNamespaceType

lookupTyConQualifiedInNamespace :: ResolutionNamespace -> Text -> Text -> TcM (Maybe TyConInfo)
lookupTyConQualifiedInNamespace namespace moduleName name =
  lift $ gets $ find matches . Map.elems . tcsGlobalTyCons
  where
    matches info =
      let tyCon = tciTyCon info
       in tyConNamespace tyCon == namespace
            && tyConModuleName tyCon == moduleName
            && (tciName info == name || tyConName tyCon == name)

lookupResolvedTyCon :: Name -> TcM (Maybe TyConInfo)
lookupResolvedTyCon name =
  case typeUseResolution (nameAnns name) of
    Just ResolutionAnnotation {resolutionNamespace = namespace, resolutionTarget = ResolvedTopLevel packageId resolvedName} -> do
      exact <- lookupTyConOrigin namespace packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName)
      maybe (lookupTyConInNamespace namespace (nameText name)) (pure . Just) exact
    Just ResolutionAnnotation {resolutionTarget = ResolvedError {}} -> pure Nothing
    Just ResolutionAnnotation {resolutionNamespace = namespace} ->
      maybe
        (lookupTyConInNamespace namespace (nameText name))
        (\moduleName -> lookupTyConQualifiedInNamespace namespace moduleName (nameText name))
        (nameQualifier name)
    _ -> maybe (lookupTyCon (nameText name)) (\moduleName -> lookupTyConQualified moduleName (nameText name)) (nameQualifier name)

lookupResolvedTypeSyntax :: ResolutionAnnotation -> TcM (Maybe TyConInfo)
lookupResolvedTypeSyntax resolution =
  case resolution of
    ResolutionAnnotation
      { resolutionNamespace = namespace,
        resolutionTarget = ResolvedTopLevel packageId resolvedName
      } ->
        lookupTyConOrigin namespace packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName)
    ResolutionAnnotation
      { resolutionTarget = ResolvedError {}
      } -> pure Nothing
    ResolutionAnnotation
      { resolutionIdentifier = identifier,
        resolutionNamespace = namespace
      } -> lookupTyConInNamespace namespace (displayIdentifier identifier)

lookupDeclaredTyCon :: UnqualifiedName -> TcM (Maybe TyConInfo)
lookupDeclaredTyCon name =
  case typeResolution (unqualifiedNameAnns name) of
    Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId resolvedName} -> do
      exact <- lookupTyConOrigin ResolutionNamespaceType packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName)
      maybe (lookupTyCon (unqualifiedNameText name)) (pure . Just) exact
    _ -> lookupTyCon (unqualifiedNameText name)

lookupTyConByIdentity :: TyCon -> TcM (Maybe TyConInfo)
lookupTyConByIdentity tyCon = lift $ gets $ Map.lookup (tyConKey tyCon) . tcsGlobalTyCons

lookupTyConOrigin :: ResolutionNamespace -> PackageId -> Text -> Text -> TcM (Maybe TyConInfo)
lookupTyConOrigin namespace packageId moduleName name =
  lift $ gets $ Map.lookup (packageId, moduleName, namespace, name) . tcsGlobalTyCons

typeResolution :: [Annotation] -> Maybe ResolutionAnnotation
typeResolution =
  find ((== ResolutionNamespaceType) . resolutionNamespace)
    . mapMaybe fromAnnotation

typeUseResolution :: [Annotation] -> Maybe ResolutionAnnotation
typeUseResolution =
  find ((/= ResolutionNamespaceModule) . resolutionNamespace)
    . mapMaybe fromAnnotation

getTyConEnv :: TcM (Map TyCon TyConInfo)
getTyConEnv = lift $ gets $ Map.fromList . map (\info -> (tciTyCon info, info)) . Map.elems . tcsGlobalTyCons

extendTyConEnvPermanent :: TyConInfo -> TcM ()
extendTyConEnvPermanent info = do
  tyCons <- lift $ gets tcsGlobalTyCons
  tyCons' <- insertNewMap "global type constructor environment" (tyConKey (tciTyCon info)) info tyCons
  lift $ modify' $ \state -> state {tcsGlobalTyCons = tyCons'}

replaceTyConEnvPermanent :: TyConInfo -> TcM ()
replaceTyConEnvPermanent info = do
  tyCons <- lift $ gets tcsGlobalTyCons
  tyCons' <- replaceMapEntry "global type constructor environment" (tyConKey (tciTyCon info)) info tyCons
  lift $ modify' $ \state -> state {tcsGlobalTyCons = tyCons'}

addDataType :: DataTypeInfo -> TcM ()
addDataType info = do
  dataTypes <- lift $ gets tcsDataTypes
  dataTypes' <- insertNewMap "data type state" (dataTypeKey info) info dataTypes
  lift $ modify' $ \state -> state {tcsDataTypes = dataTypes'}

getDataTypes :: TcM [DataTypeInfo]
getDataTypes = lift $ gets (Map.elems . tcsDataTypes)

-- | The term key of a pattern synonym. The builder term of a bidirectional
-- pattern synonym has the same key.
patSynKey :: PatSynInfo -> TcTermKey
patSynKey info =
  let (package, moduleName') = psiOrigin info
   in TcTermGlobal package moduleName' (psiName info)

addPatSyn :: PatSynInfo -> TcM ()
addPatSyn info = do
  patSyns <- lift $ gets tcsPatSyns
  patSyns' <- insertNewMap "pattern synonym state" (patSynKey info) info patSyns
  lift $ modify' $ \state -> state {tcsPatSyns = patSyns'}

lookupPatSyn :: TcTermKey -> TcM (Maybe PatSynInfo)
lookupPatSyn key = lift $ gets (Map.lookup key . tcsPatSyns)

getPatSyns :: TcM [PatSynInfo]
getPatSyns = lift $ gets (Map.elems . tcsPatSyns)

-- | The pattern synonym that a resolved top-level name refers to.
lookupPatSynTarget :: ResolvedName -> TcM (Maybe PatSynInfo)
lookupPatSynTarget target =
  case target of
    ResolvedTopLevel packageId resolvedName ->
      lookupPatSyn (TcTermGlobal packageId (fromMaybe "" (nameQualifier resolvedName)) (nameText resolvedName))
    _ -> pure Nothing

lookupDataType :: TyCon -> TcM (Maybe DataTypeInfo)
lookupDataType tyCon = lift $ gets (Map.lookup (tyConKey tyCon) . tcsDataTypes)

addInstance :: InstanceInfo -> TcM ()
addInstance instanceInfo = do
  instances <- lift $ gets tcsInstances
  when (any ((== instanceInfoKey instanceInfo) . instanceInfoKey) (instanceEnvList instances)) $
    abortTc ("duplicate instance state key: " <> show (iiDictName instanceInfo))
  lift $ modify' $ \state -> state {tcsInstances = addInstanceEnv instanceInfo instances}

-- | Every instance in scope, most recent first.
getInstances :: TcM [InstanceInfo]
getInstances = lift $ gets (instanceEnvList . tcsInstances)

-- | The instances of a class by source name, most recent first.
getClassInstances :: Text -> TcM [InstanceInfo]
getClassInstances className = lift $ gets (instanceEnvForClass className . tcsInstances)

addDataFamilyInstance :: DataFamilyInstanceInfo -> TcM ()
addDataFamilyInstance instanceInfo = do
  let key = dataFamilyAxiomKey instanceInfo
  instances <- lift $ gets tcsDataFamilyInstances
  when (Map.member key instances) $
    abortTc ("duplicate data family instance state key: " <> show key)
  lift $ modify' $ \state -> state {tcsDataFamilyInstances = Map.insert key instanceInfo instances}

getDataFamilyInstances :: TcM [DataFamilyInstanceInfo]
getDataFamilyInstances = lift $ gets (Map.elems . tcsDataFamilyInstances)

addTypeFamilyInstance :: TypeFamilyInstanceInfo -> TcM ()
addTypeFamilyInstance instanceInfo = do
  let key = typeFamilyAxiomKey instanceInfo
  instances <- lift $ gets tcsTypeFamilyInstances
  when (Map.member key instances) $
    abortTc ("duplicate type family instance state key: " <> show key)
  lift $ modify' $ \state -> state {tcsTypeFamilyInstances = Map.insert key instanceInfo instances}

getTypeFamilyInstances :: TcM [TypeFamilyInstanceInfo]
getTypeFamilyInstances = lift $ gets (Map.elems . tcsTypeFamilyInstances)

addClass :: ClassInfo -> TcM ()
addClass classInfo = do
  classes <- lift $ gets tcsClasses
  classes' <- insertNewMap "class state" (classInfoKey classInfo) classInfo classes
  lift $ modify' $ \state -> state {tcsClasses = classes'}

getClasses :: TcM [ClassInfo]
getClasses = lift $ gets (Map.elems . tcsClasses)

-- | Look up a class by its exact type constructor.
lookupClass :: TyCon -> TcM (Maybe ClassInfo)
lookupClass classTyCon = lift $ gets (Map.lookup (tyConKey classTyCon) . tcsClasses)

-- | Look up a class by its source name alone. Only for well-known classes
-- whose origin is not available, such as @Typeable@.
lookupClassByName :: Text -> TcM (Maybe ClassInfo)
lookupClassByName className =
  lift $ gets (find ((== className) . ciName) . Map.elems . tcsClasses)

-- | Look up the class that a resolved class-name occurrence refers to.
lookupClassNamed :: Name -> TcM (Maybe ClassInfo)
lookupClassNamed name = do
  maybeInfo <- lookupResolvedTyCon name
  maybe (pure Nothing) (lookupClass . tciTyCon) maybeInfo

-- | Look up the class declared by a class-declaration binder.
lookupDeclaredClass :: UnqualifiedName -> TcM (Maybe ClassInfo)
lookupDeclaredClass name = do
  maybeInfo <- lookupDeclaredTyCon name
  maybe (pure Nothing) (lookupClass . tciTyCon) maybeInfo

insertNewMap :: (Ord key, Show key) => String -> key -> value -> Map key value -> TcM (Map key value)
insertNewMap label key value entries =
  case Map.insertLookupWithKey (\_ _ existing -> existing) key value entries of
    (Nothing, entries') -> pure entries'
    (Just _, _) -> abortTc ("duplicate " <> label <> " key: " <> show key)

replaceMapEntry :: (Ord key, Show key) => String -> key -> value -> Map key value -> TcM (Map key value)
replaceMapEntry label key value entries
  | Map.member key entries = pure (Map.insert key value entries)
  | otherwise = abortTc ("missing " <> label <> " key for replacement: " <> show key)

-- | Run a computation with adjusted local type-checker options.
-- | Run an action with the candidate types of a module @default@ declaration.
localDefaultTypes :: Maybe [TcType] -> TcM a -> TcM a
localDefaultTypes types = local $ \env -> env {tcEnvDefaultTypes = types}

-- | The candidate types of the module @default@ declaration, if it has one.
getDefaultTypes :: TcM (Maybe [TcType])
getDefaultTypes = asks tcEnvDefaultTypes

-- | Run an action with more lexically scoped type variables. The new
-- variables shadow outer variables with the same name. Without
-- ScopedTypeVariables the action runs unchanged.
withScopedTyVars :: Map Text (TyVarId, TcType) -> TcM a -> TcM a
withScopedTyVars scoped action = do
  enabled <- asks tcEnvScopedTypeVariables
  if enabled && not (Map.null scoped)
    then local (\env -> env {tcEnvScopedTyVars = scoped `Map.union` tcEnvScopedTyVars env}) action
    else action

-- | Extend evidence scope for nested declarations.
withGivenPredicates :: [Pred] -> TcM a -> TcM a
withGivenPredicates predicates = local (\env -> env {tcEnvGivenPredicates = predicates <> tcEnvGivenPredicates env})

getGivenPredicates :: TcM [Pred]
getGivenPredicates = asks tcEnvGivenPredicates

-- | The lexically scoped type variables that are in scope.
getScopedTyVars :: TcM (Map Text (TyVarId, TcType))
getScopedTyVars = asks tcEnvScopedTyVars

localTcOptions :: (Bool -> Bool) -> (Bool -> Bool) -> TcM a -> TcM a
localTcOptions monoLocal monomorphism =
  local $ \env ->
    env
      { tcEnvMonoLocalBinds = monoLocal (tcEnvMonoLocalBinds env),
        tcEnvMonomorphismRestriction = monomorphism (tcEnvMonomorphismRestriction env)
      }

tcMonoLocalBinds :: TcM Bool
tcMonoLocalBinds = asks tcEnvMonoLocalBinds

tcMonomorphismRestriction :: TcM Bool
tcMonomorphismRestriction = asks tcEnvMonomorphismRestriction

-- | Get the current implication nesting level.
getTcLevel :: TcM TcLevel
getTcLevel = asks tcEnvTcLevel

-- | Run a computation at a deeper implication level.
withTcLevel :: TcM a -> TcM a
withTcLevel =
  local $ \env ->
    env {tcEnvTcLevel = pushLevel (tcEnvTcLevel env)}

-- | Emit a diagnostic (error or warning).
emitDiagnostic :: TcDiagnostic -> TcM ()
emitDiagnostic d = lift $ modify' $ \s ->
  s {tcsDiagnostics = d : tcsDiagnostics s}

-- | Emit an error diagnostic.
emitError :: SourceSpan -> TcErrorKind -> TcM ()
emitError loc kind =
  emitDiagnostic
    TcDiagnostic
      { diagLoc = diagnosticLoc loc,
        diagSeverity = TcError,
        diagKind = kind
      }

-- | Emit a warning diagnostic.
emitWarning :: SourceSpan -> TcErrorKind -> TcM ()
emitWarning loc kind =
  emitDiagnostic
    TcDiagnostic
      { diagLoc = diagnosticLoc loc,
        diagSeverity = TcWarning,
        diagKind = kind
      }

diagnosticLoc :: SourceSpan -> Maybe SourceSpan
diagnosticLoc NoSourceSpan = Nothing
diagnosticLoc sp = Just sp

-- | Get all diagnostics collected so far.
getDiagnostics :: TcM [TcDiagnostic]
getDiagnostics = lift $ gets (reverse . tcsDiagnostics)

-- | Run a recoverable phase and report whether it emitted any errors.
--
-- The type checker intentionally keeps going after many local errors so later
-- declarations can still be checked. Callers that produce successful
-- elaboration metadata use this to avoid treating a recovered binding as a
-- checked binding.
withErrorTracking :: TcM a -> TcM (a, Bool)
withErrorTracking action = do
  before <- currentErrorCount
  result <- action
  after <- currentErrorCount
  pure (result, after > before)

-- | Run an action, then discard every change it made to the type-checker
-- state.
--
-- Defaulting uses this to test a candidate type against a class constraint
-- without committing the evidence, the meta-variable solutions, or the
-- diagnostics that the trial produces. The unique supply rolls back too, so
-- the result must not mention a unique that the action allocated.
tcSpeculate :: TcM a -> TcM a
tcSpeculate action = do
  saved <- lift get
  result <- action
  lift (put saved)
  pure result

currentErrorCount :: TcM Int
currentErrorCount =
  lift $ gets $ length . filter isError . tcsDiagnostics
  where
    isError diagnostic = diagSeverity diagnostic == TcError

-- | Record that a constructor is a GADT constructor.
markGadtCon :: Text -> TcM ()
markGadtCon name = lift $ modify' $ \s ->
  s {tcsGadtCons = Set.insert name (tcsGadtCons s)}

-- | Check whether a constructor is a GADT constructor.
isGadtCon :: Text -> TcM Bool
isGadtCon name = lift $ gets $ \s ->
  Set.member name (tcsGadtCons s)
