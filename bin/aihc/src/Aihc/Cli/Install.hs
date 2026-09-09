module Aihc.Cli.Install
  ( InstallResult (..),
    InstallLocations (..),
    InstalledPackage (..),
    ModuleCompileConfig (..),
    ModuleCompileRequest (..),
    ModuleCompileResult (..),
    compileModules,
    buildEnvironmentIdentity,
    defaultBuildRoot,
    install,
    installWith,
    installPlanPackages,
    networkDependencyResolver,
    parsePackageTarget,
    resolvePreferredVersion,
    runInstall,
  )
where

import Aihc.Cli.ArtifactCache (compilerBuildIdentity, executableIdentity, hashChunks, sourceFilesHash)
import Aihc.Cli.Backend (BackendOutput (..), compileLir, lowerTargetFor, nativeSourceExtension)
import Aihc.Cli.BuildStamp
  ( BackendStamp (..),
    ModuleDigests (..),
    PackageDigests (..),
    ResolveStamp (..),
    UnitStamp (..),
    filesMatchStamps,
    packageDigestsPath,
    readStamp,
    stampFiles,
    writeStamp,
  )
import Aihc.Cli.Options (InstallOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest, writePackageManifest)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifactParts)
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Cli.TaskGraph
  ( Task (..),
    TaskId (..),
    TaskKind (..),
    TaskTiming,
    renderDuration,
    renderTaskTimeline,
    runTaskGraph,
  )
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact, encodeTypeArtifact, encodeTypeArtifactParts)
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..))
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Hackage.VersionResolver (getLatestVersion)
import Aihc.Lir qualified as Lir
import Aihc.Lir.Lower qualified as Lir
import Aihc.Native (NativeTarget (..), WasmSysroot (..), backendArchiver, backendCompiler, nativeTargetStoreDirectory, wasmSysroot)
import Aihc.PackagePlan
  ( DependencyResolver (..),
    DependencyVersions,
    PackagePlan (..),
    PlanOrigin (..),
    ResolvedSource (..),
    buildPackagePlanWithResolver,
    dependencyVersionsFromManifests,
    localDependencyResolverWithFallback,
    packageSpecFromSource,
  )
import Aihc.PackagePlan.Diagnostic (renderHumanDiagnostic)
import Aihc.PackagePlan.Source (ParsedInterfaceFile (ParsedInterfaceFile), parseInterfaceFile)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    ImportDecl (..),
    Module,
    Name (..),
    SourceSpan (..),
    moduleName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Prim.Wiring (primTcConfig, primTcWiring)
import Aihc.Resolve
  ( ModuleExports,
    ModuleKey (..),
    Package (..),
    PackageId (..),
    ResolutionNamespace (..),
    ResolveError (..),
    ResolveResult (..),
    ResolvedName (..),
    Scope (..),
    collectModuleExportsWithDeps,
    emptyScope,
    extractInterfaceWithDeps,
    lookupImportedModule,
    modulesInPackage,
    resolveWithDeps,
    unionScope,
  )
import Aihc.Tc
  ( AssociatedTypeInfo (..),
    ClassInfo (..),
    DataConFieldInfo (..),
    DataConInfo (..),
    DataFamilyInstanceInfo (..),
    DataTypeInfo (..),
    InstanceInfo (..),
    Pred (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcInterface (..),
    TcKinds,
    TcSeverity (..),
    TcTermKey (..),
    TcType (..),
    TyCon,
    TyConInfo (..),
    TypeFamilyInstanceInfo (..),
    TypeScheme (..),
    mergeTcInterfaces,
    mkTcKinds,
    renderPred,
    renderTcType,
    tcInterfaceClasses,
    tcInterfaceDataFamilyInstances,
    tcInterfaceDataTypes,
    tcInterfaceForeignImports,
    tcInterfaceInstances,
    tcInterfaceTerms,
    tcInterfaceTyCons,
    tcInterfaceTypeFamilyInstances,
    tcModuleBindings,
    tcModuleDiagnostics,
    tyConKey,
    typecheckModuleSccWithInterface,
    unionTcInterfaces,
  )
import Aihc.Tc.Annotations (TcForeignImportAnnotation (..), TcForeignImportInfo (..), TcForeignMarshal (..))
import Aihc.Tc.Env (TypeSynonymInfo (..))
import Aihc.Tc.Types (tvKind, tyConModuleName, tyConName, tyConNamespace, tyConPackageId)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM (TMVar, atomically, newEmptyTMVarIO, putTMVar, readTMVar)
import Control.DeepSeq (rnf)
import Control.Exception (IOException, bracket, evaluate, throwIO, try)
import Control.Monad (filterM, forM, forM_, unless, void, when, zipWithM)
import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (intercalate, isSuffixOf, nub, sortOn)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (GenericPackageDescription, package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.System (Arch (..), OS (..), buildArch, buildOS)
import Distribution.Version (nullVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Paths_aihc (getDataFileName)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, makeRelative, takeDirectory, (<.>), (</>))
import System.IO (Handle, hClose, hIsTerminalDevice, hPutStrLn, openBinaryTempFile, stdout)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)

data InstallResult = InstallResult
  { -- | The package directory: in the store for an immutable package, in
    -- the build directory for a local one.
    installStorePath :: !FilePath,
    installWrittenModules :: ![Text],
    installReusedModules :: ![Text]
  }
  deriving (Eq, Show)

data SourceModule = SourceModule
  { sourceModulePath :: !FilePath,
    sourceModuleSize :: !Int,
    sourceModuleHash :: !Text,
    sourceModuleAst :: Module,
    sourceModuleExtensions :: ![Extension],
    sourceModuleSourceLines :: !(Map.Map FilePath (Map.Map Int Text)),
    sourceModuleParseDiagnostics :: [Value]
  }

data InstalledPackage = InstalledPackage
  { installedResult :: !InstallResult,
    installedName :: !Text,
    installedVersion :: !Text,
    -- | The name of the package directory: @NAME-VERSION-FINGERPRINT@ in the
    -- store and @NAME-VERSION@ in a build directory.
    installedIdentity :: !Text,
    -- | Whether the package lives in the store. A store package depends on
    -- store packages only.
    installedImmutable :: !Bool,
    installedManifest :: !PackageManifest,
    installedExports :: !ModuleExports,
    installedTypes :: !(Map.Map Text TcInterface),
    installedScopeHashes :: !(Map.Map Text Text),
    installedTypeHashes :: !(Map.Map Text Text),
    installedInstanceDigest :: !Text,
    installedInstanceFacts :: !TcInterface,
    installedInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider))
  }

type InstanceProvider = (PackageId, Text)

data ModuleOutputPaths = ModuleOutputPaths
  { outputFcPath :: !FilePath,
    outputGrinPath :: !FilePath,
    outputCpsGrinPath :: !FilePath,
    outputGcGrinPath :: !FilePath,
    outputNativePath :: !FilePath,
    outputObjectPath :: !FilePath
  }

data FcModule = FcModule
  { fcModuleName :: !Text,
    fcProgram :: !Fc.Program
  }

data GrinModule = GrinModule
  { grinModuleName :: !Text,
    plainGrinProgram :: !Grin.GrinProgram,
    cpsGrinProgram :: !Grin.CpsGrinProgram,
    gcGrinProgram :: !Grin.GcGrinProgram
  }

data NativeModule = NativeModule
  { nativeModuleName :: !Text,
    nativeSource :: !(Maybe Text),
    nativeObject :: !(Maybe BL.ByteString)
  }

data PendingCompile = PendingCompile
  { pendingModules :: [Module],
    -- | How to desugar each module of the unit, by module name. The
    -- configuration carries the export list, which the desugarer turns into
    -- the visibility of every top-level name.
    pendingDesugarConfigs :: Map.Map Text DesugarConfig
  }

newtype UnitId = UnitId Int
  deriving (Eq, Ord, Show)

data SourceUnit = SourceUnit
  { sourceUnitId :: !UnitId,
    sourceUnitOrder :: !Int,
    sourceUnitSources :: ![SourceModule],
    sourceUnitDependencies :: ![UnitId]
  }

data ResolveUnitResult = ResolveUnitResult
  { resolveUnitExports :: !ModuleExports,
    resolveUnitScopeHashes :: !(Map.Map Text Text),
    resolveUnitResolved :: !(Maybe ResolveResult),
    resolveUnitErrors :: ![ResolveError],
    resolveUnitSuccess :: !Bool
  }

data TypeUnitResult = TypeUnitResult
  { typeUnitTypes :: !(Map.Map Text TcInterface),
    typeUnitHashes :: !(Map.Map Text Text),
    typeUnitOwnInstanceInterface :: !TcInterface,
    -- | The digest of the instance facts of the unit. The facts artifact
    -- carries the facts digests of the dependencies, so this digest changes
    -- when an instance anywhere below the unit changes.
    typeUnitFactsDigest :: !Text,
    typeUnitInstanceInterface :: !TcInterface,
    typeUnitDiagnostics :: ![TcDiagnostic],
    typeUnitWritten :: !(Set.Set Text),
    typeUnitReused :: !(Set.Set Text),
    -- | The stamp the backend writes once the objects of the unit exist.
    typeUnitPendingStamp :: !(Maybe PendingStamp),
    typeUnitPendingCompile :: !(Maybe PendingCompile),
    typeUnitDesugarInterface :: !TcInterface,
    typeUnitSuccess :: !Bool
  }

-- | A unit stamp whose artifact files are not all written yet.
data PendingStamp = PendingStamp
  { pendingStampPath :: !FilePath,
    pendingStampInputs :: ![(Text, Text)],
    pendingStampTypes :: !(Map.Map Text Text),
    pendingStampFacts :: !Text,
    -- | The type artifacts and the facts artifact, relative to the package.
    pendingStampFrontendFiles :: ![FilePath]
  }

data UnitRuntime = UnitRuntime
  { runtimeUnit :: !SourceUnit,
    runtimeResolveResult :: !(TMVar ResolveUnitResult),
    runtimeTypeResult :: !(TMVar TypeUnitResult)
  }

data ModuleCompileConfig = ModuleCompileConfig
  { compileBuildIdentity :: !String,
    compileKeepCore :: !Bool,
    compileKeepGrin :: !Bool,
    compileKeepNative :: !Bool,
    compileLint :: !Bool,
    compileNoCode :: !Bool,
    compileTarget :: !NativeTarget,
    compileVerbose :: String -> IO (),
    compilePrintTimings :: String -> IO (),
    compileUseColor :: !Bool
  }

data ModuleCompileRequest = ModuleCompileRequest
  { compileOutputRoot :: !FilePath,
    compilePackageRoot :: !FilePath,
    compilePackage :: !Package,
    compileSourceFiles :: ![HackageCabal.FileInfo],
    compileDependencyRoots :: ![FilePath]
  }

newtype ModuleCompileResult = ModuleCompileResult
  { compileObjectPaths :: [FilePath]
  }
  deriving (Eq, Show)

data CompiledPackageModules = CompiledPackageModules
  { compiledSources :: ![SourceModule],
    compiledExports :: !ModuleExports,
    compiledTypes :: !(Map.Map Text TcInterface),
    compiledScopeHashes :: !(Map.Map Text Text),
    compiledTypeHashes :: !(Map.Map Text Text),
    compiledInstanceDigest :: !Text,
    compiledInstanceFacts :: !TcInterface,
    compiledInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider)),
    compiledWritten :: !(Set.Set Text),
    compiledReused :: !(Set.Set Text)
  }

data BackendPhaseTimings = BackendPhaseTimings
  { backendDesugarNs :: !Word64,
    backendGrinNs :: !Word64,
    backendNativeNs :: !Word64,
    backendOtherNs :: !Word64
  }

instance Semigroup BackendPhaseTimings where
  left <> right =
    BackendPhaseTimings
      { backendDesugarNs = backendDesugarNs left + backendDesugarNs right,
        backendGrinNs = backendGrinNs left + backendGrinNs right,
        backendNativeNs = backendNativeNs left + backendNativeNs right,
        backendOtherNs = backendOtherNs left + backendOtherNs right
      }

instance Monoid BackendPhaseTimings where
  mempty = BackendPhaseTimings 0 0 0 0

data PackageTaskContext = PackageTaskContext
  { taskModuleCompileConfig :: !ModuleCompileConfig,
    taskStorePath :: !FilePath,
    taskResolvePackage :: !Package,
    taskPrimIdentity :: !PackageId,
    taskPackageRoot :: !FilePath,
    taskDependencyExports :: !ModuleExports,
    taskDependencyScopeHashes :: !(Map.Map Text Text),
    taskDependencyTypes :: !(Map.Map Text TcInterface),
    taskDependencyTypeHashes :: !(Map.Map Text Text),
    -- | The instance digest of each dependency package, with the modules
    -- it exposes. A unit that imports one of the modules takes the digest
    -- as an input, because the instances the package supplies come from
    -- the package as a whole.
    taskDependencyPackages :: ![(Text, Text, Set.Set Text)],
    taskDependencyInstanceFacts :: !TcInterface,
    taskDependencyInstanceProviders :: !(Map.Map Text (Set.Set InstanceProvider)),
    taskBackendPhaseTimings :: !(IORef BackendPhaseTimings)
  }

runInstall :: InstallOptions -> IO ()
runInstall options = do
  result <- install options
  putStrLn ("store: " <> installStorePath result)

-- | Install a package and write the verbose and timing messages to stdout.
install :: InstallOptions -> IO InstallResult
install = installWith stdout

-- | Install a package and write the verbose and timing messages to the given
-- handle. A test gives a file handle here and reads the file. The test must
-- not redirect the process stdout instead: the test runner writes its progress
-- to stdout from other threads, and a redirect would capture that progress.
installWith :: Handle -> InstallOptions -> IO InstallResult
installWith output options = do
  storeRoot <- maybe defaultStoreRoot pure (installStoreRoot options)
  useColor <- hIsTerminalDevice output
  let target = installTarget options
      targetDirectory = nativeTargetStoreDirectory target
  let verbose message = when (installVerbose options) (hPutStrLn output message)
      printTimings message = when (installPrintTimings options) (hPutStrLn output message)
  (root, origin) <- resolveInstallTarget (installPackageTarget options)
  let resolver = localDependencyResolverWithFallback networkDependencyResolver root
  spec <- packageSpecFromSource root
  plan <- buildPackagePlanWithResolver resolver spec
  buildRoot <- maybe (pure (defaultBuildRoot root)) pure (installBuildRoot options)
  buildIdentity <- buildEnvironmentIdentity target
  let config =
        ModuleCompileConfig
          { compileBuildIdentity = buildIdentity,
            compileKeepCore = installKeepCore options,
            compileKeepGrin = installKeepGrin options,
            compileKeepNative = installKeepNative options,
            compileLint = installLint options,
            compileNoCode = installNoCode options,
            compileTarget = target,
            compileVerbose = verbose,
            compilePrintTimings = printTimings,
            compileUseColor = useColor
          }
      locations =
        InstallLocations
          { locationStoreRoot = storeRoot </> targetDirectory,
            locationBuildRoot = buildRoot </> targetDirectory,
            locationImmutable = installImmutable options,
            locationReinstall = installReinstall options
          }
  -- The resolver finds the package being installed by its name, which marks
  -- it local. What the user asked for decides instead: a directory is local,
  -- a Hackage release is not.
  installedResult <$> installPackagePlan config locations plan {planOrigin = origin}

-- | Where a local package builds unless @--build-root@ says otherwise.
defaultBuildRoot :: FilePath -> FilePath
defaultBuildRoot root = root </> ".aihc-target"

-- | Turn the install argument into a local package directory and say where
-- it came from.
--
-- An existing directory is used as-is. Anything else is parsed as a Hackage
-- package name with an optional version (@NAME@ or @NAME-VERSION@) and
-- downloaded from Hackage; without a version the preferred version is used.
resolveInstallTarget :: String -> IO (FilePath, PlanOrigin)
resolveInstallTarget target = do
  isDirectory <- doesDirectoryExist target
  if isDirectory
    then pure (target, PlanLocal)
    else case parsePackageTarget target of
      Nothing ->
        ioError
          ( userError
              (target <> " is not an existing directory nor a Hackage package name (NAME[-VERSION])")
          )
      Just (name, requestedVersion) -> do
        version <- maybe (resolvePreferredVersion name) pure requestedVersion
        path <-
          HackageDownload.downloadPackageWithOptions
            HackageDownload.defaultDownloadOptions
            PackageSpec {pkgName = name, pkgVersion = version}
        pure (path, PlanHackage)

-- | Split a Hackage target into its package name and optional version.
parsePackageTarget :: String -> Maybe (String, Maybe String)
parsePackageTarget target = do
  packageId <- simpleParsec target :: Maybe CabalPackage.PackageIdentifier
  let version = CabalPackage.pkgVersion packageId
  pure
    ( CabalPackage.unPackageName (CabalPackage.pkgName packageId),
      if version == nullVersion then Nothing else Just (prettyShow version)
    )

resolvePreferredVersion :: String -> IO String
resolvePreferredVersion name = do
  result <- getLatestVersion Nothing name
  either (ioError . userError) pure result

networkDependencyResolver :: DependencyResolver
networkDependencyResolver =
  DependencyResolver
    { resolverResolveVersion = resolvePreferredVersion,
      resolverSourcePath = fmap (`ResolvedSource` PlanHackage) . HackageDownload.downloadPackageWithOptions HackageDownload.defaultDownloadOptions
    }

-- | Where the packages of a plan go.
--
-- A package that is immutable for this compiler, which is a Hackage release
-- or a core library, is installed into the store under a directory named by
-- its fingerprint and never changed afterwards. A local package builds in
-- place under the build root, where each unit keeps the artifacts of the
-- previous build and replaces only the ones whose inputs changed.
data InstallLocations = InstallLocations
  { locationStoreRoot :: !FilePath,
    locationBuildRoot :: !FilePath,
    -- | Treat local packages as immutable and install them into the store.
    locationImmutable :: !Bool,
    -- | Build the packages the user named again, even where they exist.
    locationReinstall :: !Bool
  }

installPackagePlan :: ModuleCompileConfig -> InstallLocations -> PackagePlan -> IO InstalledPackage
installPackagePlan config locations plan = do
  installed <- newIORef Map.empty
  installPlanNode config locations installed True plan

-- | Install every package of the plans and return the closure, each package
-- once.
installPlanPackages :: ModuleCompileConfig -> InstallLocations -> [PackagePlan] -> IO [InstalledPackage]
installPlanPackages config locations plans = do
  installed <- newIORef Map.empty
  mapM_ (installPlanNode config locations installed True) plans
  Map.elems <$> readIORef installed

installPlanNode :: ModuleCompileConfig -> InstallLocations -> IORef (Map.Map FilePath InstalledPackage) -> Bool -> PackagePlan -> IO InstalledPackage
installPlanNode config locations installed root plan = do
  key <- canonicalizePath (planSourcePath plan)
  known <- Map.lookup key <$> readIORef installed
  case known of
    Just package -> pure package
    Nothing -> do
      -- Only the package the user named is reinstalled.
      let reinstall = root && locationReinstall locations
      dependencies <- mapM (installPlanNode config locations installed False) (planDependencyPlans plan)
      package <-
        if locationImmutable locations || planOrigin plan /= PlanLocal
          then installStorePackage config root reinstall (locationStoreRoot locations) dependencies (planSourcePath plan)
          else installLocalPackage config reinstall (locationBuildRoot locations) dependencies (planSourcePath plan)
      modifyIORef' installed (Map.insert key package)
      pure package

data PackageInputs = PackageInputs
  { inputCabalFile :: !FilePath,
    inputDescription :: !GenericPackageDescription,
    inputSources :: ![HackageCabal.FileInfo],
    inputCCompileInfo :: !HackageCabal.CCompileInfo
  }

readPackageInputs :: ModuleCompileConfig -> FilePath -> IO PackageInputs
readPackageInputs config root = do
  cabalFiles <- HackageUtil.findCabalFiles root
  cabalFile <- case cabalFiles of
    [] -> ioError (userError ("No .cabal file found under " <> root))
    files -> pure (HackageUtil.chooseBestCabalFile root files)
  cabalBytes <- BS.readFile cabalFile
  gpd <- case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right value) -> pure value
    (_, Left (_, errors)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errors))
  let (targetOs, targetArch) = cabalPlatformForTarget (compileTarget config)
  files <- HackageCabal.collectLibraryFilesFor targetOs targetArch gpd root
  pure
    PackageInputs
      { inputCabalFile = cabalFile,
        inputDescription = gpd,
        inputSources = files,
        inputCCompileInfo = HackageCabal.collectLibraryCCompileInfoFor targetOs targetArch gpd root
      }

-- | Install an immutable package into the store, unless the store has it.
installStorePackage :: ModuleCompileConfig -> Bool -> Bool -> FilePath -> [InstalledPackage] -> FilePath -> IO InstalledPackage
installStorePackage config named reinstall storeRoot dependencies root = do
  inputs <- readPackageInputs config root
  (packageDirectory, unitIdentity) <- storePackageIdentity config dependencies inputs
  forM_ dependencies $ \dependency ->
    unless (installedImmutable dependency) $
      ioError
        ( userError
            ( "The package "
                <> T.unpack unitIdentity
                <> " is installed into the store but depends on the local package "
                <> T.unpack (installedIdentity dependency)
                <> ". Pass --immutable to install both into the store."
            )
        )
  let storePath = storeRoot </> packageDirectory
  exists <- doesDirectoryExist storePath
  if exists && not reinstall
    then do
      package <- loadInstalledPackage Set.empty True storePath
      requireInstalledFlags config named package
      pure package
    else do
      createDirectoryIfMissing True storeRoot
      bracket
        (createTemporaryStoreRoot storeRoot packageDirectory)
        removeTemporaryStoreRoot
        (buildAndPublish inputs packageDirectory unitIdentity storePath exists)
  where
    buildAndPublish inputs packageDirectory unitIdentity storePath exists temporaryRoot = do
      built <- installPackageDirect config packageDirectory unitIdentity True temporaryRoot dependencies root inputs
      when exists (removeDirectoryRecursive storePath)
      publishResult <- try (renameDirectory (installStorePath (installedResult built)) storePath)
      case publishResult of
        Right () -> pure (setInstalledStorePath storePath built)
        Left err -> do
          published <- doesDirectoryExist storePath
          if published
            then loadInstalledPackage Set.empty True storePath
            else throwIO (err :: IOException)

-- | Build a local package in place under the build root.
installLocalPackage :: ModuleCompileConfig -> Bool -> FilePath -> [InstalledPackage] -> FilePath -> IO InstalledPackage
installLocalPackage config reinstall buildRoot dependencies root = do
  inputs <- readPackageInputs config root
  let (packageDirectory, unitIdentity) = localPackageIdentity inputs
      buildPath = buildRoot </> packageDirectory
  exists <- doesDirectoryExist buildPath
  when (exists && reinstall) (removeDirectoryRecursive buildPath)
  createDirectoryIfMissing True buildPath
  installPackageDirect config packageDirectory unitIdentity False buildRoot dependencies root inputs

-- | The flags the store entry was built with must cover the flags of this
-- install: the entry is never changed, so a missing output stays missing.
-- The extra outputs matter for the package the user named; a dependency
-- only has to have code when code is wanted.
requireInstalledFlags :: ModuleCompileConfig -> Bool -> InstalledPackage -> IO ()
requireInstalledFlags config named package = do
  let built = packageManifestFlags (installedManifest package)
      missing =
        [ flag
        | named,
          (wanted, flag) <- [(compileKeepCore config, "keep-core"), (compileKeepGrin config, "keep-grin"), (compileKeepNative config, "keep-native")],
          wanted,
          flag `notElem` built
        ]
          <> ["code" | not (compileNoCode config), "no-code" `elem` built]
  unless (null missing) $
    ioError
      ( userError
          ( "The store holds "
              <> T.unpack (installedIdentity package)
              <> " built without "
              <> intercalate ", " (map (("--" <>) . T.unpack) missing)
              <> ". Pass --reinstall to build it again."
          )
      )

installPackageDirect :: ModuleCompileConfig -> FilePath -> Text -> Bool -> FilePath -> [InstalledPackage] -> FilePath -> PackageInputs -> IO InstalledPackage
installPackageDirect config packageDirectory unitIdentity immutable storeRoot dependencies root inputs = do
  let target = compileTarget config
      verbose = compileVerbose config
  verbose ("Read Cabal package: " <> root)
  let gpd = inputDescription inputs
      files = inputSources inputs
      cCompileInfo = inputCCompileInfo inputs
  let packageId = package (packageDescription gpd)
      packageNameText = T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId))
      packageVersionText = T.pack (prettyShow (CabalPackage.packageVersion packageId))
  let storePath = storeRoot </> packageDirectory
      resolvePackage = Package packageNameText (PackageId unitIdentity)
  compiled <- compileModulesWithDependencies config storePath root resolvePackage files dependencies
  let parsed = compiledSources compiled
      allExports = compiledExports compiled
      allTypes = compiledTypes compiled
      allScopeHashes = compiledScopeHashes compiled
      allTypeHashes = compiledTypeHashes compiled
      written = compiledWritten compiled
      reused = compiledReused compiled
  unless (compileNoCode config) $ do
    let archive = storePath </> "lib" </> "lib" <> T.unpack packageNameText <> ".a"
        moduleObjects =
          sortOn
            id
            [ outputObjectPath (moduleOutputPaths storePath target (sourceName source))
            | source <- parsed
            ]
    -- The archive follows its objects and the C sources. Both are known
    -- without reading the objects: a unit that wrote an object says so, and
    -- the C sources are hashed for the archive stamp.
    archiveInputs <- archiveInputsHash config root dependencies inputs
    let stampPath = storePath </> "lib" </> "archive.hash"
    previous <- readStampText stampPath
    archiveExists <- doesFileExist archive
    let current = not (Set.null written) || not archiveExists || previous /= Just archiveInputs
    if current
      then do
        cObjects <- compilePackageCFiles target verbose root storePath cCompileInfo
        buildLibraryArchive target verbose archive (moduleObjects <> cObjects)
        BS8.writeFile stampPath (BS8.pack archiveInputs)
      else verbose ("Reuse archive: " <> archive)
  let manifest =
        PackageManifest
          { packageManifestName = packageNameText,
            packageManifestVersion = packageVersionText,
            packageManifestIdentity = T.pack packageDirectory,
            packageManifestUnitId = unitIdentity,
            packageManifestDependencies = sortOn id (map installedIdentity dependencies),
            packageManifestModules = sortOn id (HackageCabal.collectLibraryExposedModules gpd),
            packageManifestFlags = compileFlagNames config
          }
  writePackageManifest (packageManifestPath storePath) manifest
  let exposedNames = Set.fromList (HackageCabal.collectLibraryExposedModules gpd)
      ownExports =
        Map.filterWithKey
          (\moduleKey _ -> moduleKeyPackage moduleKey == resolvePackage && moduleKeyName moduleKey `Set.member` exposedNames)
          allExports
  pure
    InstalledPackage
      { installedResult = InstallResult storePath (Set.toAscList written) (Set.toAscList reused),
        installedName = packageNameText,
        installedVersion = packageVersionText,
        installedIdentity = T.pack packageDirectory,
        installedImmutable = immutable,
        installedManifest = manifest,
        installedExports = ownExports,
        installedTypes = Map.restrictKeys allTypes exposedNames,
        installedScopeHashes = Map.restrictKeys allScopeHashes exposedNames,
        installedTypeHashes = Map.restrictKeys allTypeHashes exposedNames,
        installedInstanceDigest = compiledInstanceDigest compiled,
        installedInstanceFacts = compiledInstanceFacts compiled,
        installedInstanceProviders = Map.restrictKeys (compiledInstanceProviders compiled) exposedNames
      }

readStampText :: FilePath -> IO (Maybe String)
readStampText path = do
  exists <- doesFileExist path
  if exists then Just . BS8.unpack <$> BS.readFile path else pure Nothing

-- | The names of the flags an installed package records in its manifest.
compileFlagNames :: ModuleCompileConfig -> [Text]
compileFlagNames config =
  [ flag
  | (set, flag) <-
      [ (compileKeepCore config, "keep-core"),
        (compileKeepGrin config, "keep-grin"),
        (compileKeepNative config, "keep-native"),
        (compileLint config, "lint"),
        (compileNoCode config, "no-code")
      ],
    set
  ]

compileModules :: ModuleCompileConfig -> ModuleCompileRequest -> IO ModuleCompileResult
compileModules config request = do
  dependencies <- mapM (loadInstalledPackage Set.empty True) (compileDependencyRoots request)
  compiled <-
    compileModulesWithDependencies
      config
      (compileOutputRoot request)
      (compilePackageRoot request)
      (compilePackage request)
      (compileSourceFiles request)
      dependencies
  pure
    ModuleCompileResult
      { compileObjectPaths =
          sortOn
            id
            [ outputObjectPath (moduleOutputPaths (compileOutputRoot request) (compileTarget config) (sourceName source))
            | source <- compiledSources compiled
            ]
      }

compileModulesWithDependencies :: ModuleCompileConfig -> FilePath -> FilePath -> Package -> [HackageCabal.FileInfo] -> [InstalledPackage] -> IO CompiledPackageModules
compileModulesWithDependencies config outputRoot packageRoot resolvePackage files dependencies = do
  let verbose = compileVerbose config
  verbose ("Parse " <> show (length files) <> " modules")
  capabilities <- getNumCapabilities
  let versions =
        dependencyVersionsFromManifests
          [(installedName dependency, installedVersion dependency) | dependency <- dependencies]
  (parsed, importTimings) <- loadSourceModules (max 1 capabilities) packageRoot versions files
  loadedDependencies <- loadRequiredDependencies parsed dependencies
  let units = sourceModuleUnits parsed
      dependencyExports = Map.unions (map installedExports loadedDependencies)
      dependencyTypes = LazyMap.unions (map installedTypes loadedDependencies)
      dependencyScopeHashes = Map.unions (map installedScopeHashes loadedDependencies)
      dependencyTypeHashes = LazyMap.unions (map installedTypeHashes loadedDependencies)
      dependencyPackages =
        [ (installedName dependency, installedInstanceDigest dependency, Map.keysSet (installedTypeHashes dependency))
        | dependency <- loadedDependencies
        ]
      dependencyInstanceFacts = mergeTcInterfaces (map installedInstanceFacts loadedDependencies)
      dependencyInstanceProviders = Map.unions (map installedInstanceProviders loadedDependencies)
      primIdentity = packagePrimIdentity resolvePackage dependencyExports
  backendPhaseTimings <- newIORef mempty
  let taskContext =
        PackageTaskContext
          { taskModuleCompileConfig = config,
            taskStorePath = outputRoot,
            taskResolvePackage = resolvePackage,
            taskPrimIdentity = primIdentity,
            taskPackageRoot = packageRoot,
            taskDependencyExports = dependencyExports,
            taskDependencyScopeHashes = dependencyScopeHashes,
            taskDependencyTypes = dependencyTypes,
            taskDependencyTypeHashes = dependencyTypeHashes,
            taskDependencyPackages = dependencyPackages,
            taskDependencyInstanceFacts = dependencyInstanceFacts,
            taskDependencyInstanceProviders = dependencyInstanceProviders,
            taskBackendPhaseTimings = backendPhaseTimings
          }
  verbose ("Compute " <> show (length units) <> " SCC units")
  (runtimes, taskTimings) <- runPackageTasks taskContext (max 1 capabilities) units
  resolveResults <- mapM (atomically . readTMVar . runtimeResolveResult) runtimes
  phaseTimings <- readIORef backendPhaseTimings
  compilePrintTimings
    config
    ( renderTaskTimeline (compileUseColor config) (importTimings <> taskTimings)
        <> renderBackendPhaseTotals phaseTimings
    )
  typeResults <- mapM (atomically . readTMVar . runtimeTypeResult) runtimes
  let parseDiagnostics = concatMap (concatMap sourceModuleParseDiagnostics . sourceUnitSources . runtimeUnit) runtimes
      resolveDiagnostics = concatMap resolveUnitErrors resolveResults
      -- An unlocated diagnostic names the modules of its unit.
      typeDiagnostics =
        concat
          [ [(unitLabel (runtimeUnit runtime), diagnostic) | diagnostic <- typeUnitDiagnostics result, diagSeverity diagnostic == TcError]
          | (runtime, result) <- zip runtimes typeResults
          ]
      frontendFailure = renderFrontendFailure parsed parseDiagnostics resolveDiagnostics typeDiagnostics
  unless (null frontendFailure) (ioError (userError frontendFailure))
  let localExports = Map.unions (map resolveUnitExports resolveResults)
      localScopeHashes = Map.unions (map resolveUnitScopeHashes resolveResults)
      localTypes = Map.unions (map typeUnitTypes typeResults)
      localTypeHashes = Map.unions (map typeUnitHashes typeResults)
      allExports = localExports `Map.union` dependencyExports
      allScopeHashes = localScopeHashes `Map.union` dependencyScopeHashes
      allTypes = localTypes `LazyMap.union` dependencyTypes
      allTypeHashes = localTypeHashes `LazyMap.union` dependencyTypeHashes
      packageInstanceInterface = mergeTcInterfaces (dependencyInstanceFacts : map typeUnitOwnInstanceInterface typeResults)
      instanceProviders =
        Map.fromList
          [ (sourceName source, interfaceInstanceProviders (typeUnitInstanceInterface result))
          | (runtime, result) <- zip runtimes typeResults,
            source <- sourceUnitSources (runtimeUnit runtime)
          ]
  instanceDigest <- writePackageInstanceArtifact verbose outputRoot allTypeHashes instanceProviders packageInstanceInterface
  -- A consumer takes the digests from here rather than encoding the
  -- interfaces again.
  writeStamp
    (packageDigestsPath outputRoot)
    PackageDigests
      { packageDigestsModules =
          Map.fromList
            [ (sourceName source, ModuleDigests scopeDigest typeDigest)
            | source <- parsed,
              Just scopeDigest <- [Map.lookup (sourceName source) localScopeHashes],
              Just typeDigest <- [Map.lookup (sourceName source) localTypeHashes]
            ],
        packageDigestsInstances = instanceDigest
      }
  pure
    CompiledPackageModules
      { compiledSources = parsed,
        compiledExports = allExports,
        compiledTypes = allTypes,
        compiledScopeHashes = allScopeHashes,
        compiledTypeHashes = allTypeHashes,
        compiledInstanceDigest = instanceDigest,
        compiledInstanceFacts = packageInstanceInterface,
        compiledInstanceProviders = instanceProviders,
        compiledWritten = Set.unions (map typeUnitWritten typeResults),
        compiledReused = Set.unions (map typeUnitReused typeResults)
      }

-- | The kind vocabulary of the aihc core libraries, given the identity of
-- the primitive package.
primKinds :: PackageId -> TcKinds
primKinds = mkTcKinds . primTcWiring

packagePrimIdentity :: Package -> ModuleExports -> PackageId
packagePrimIdentity resolvePackage dependencyExports =
  fromMaybe (PackageId "aihc-prim") $
    if packageName resolvePackage == "aihc-prim"
      then Just (packageId resolvePackage)
      else
        listToMaybe
          [ dependencyIdentity
          | ModuleKey (Package dependencyName dependencyIdentity) _ <- Map.keys dependencyExports,
            dependencyName == "aihc-prim"
          ]

-- | The directory name and unit identity of a package in the store.
--
-- The fingerprint is a function of the plan: the package name and version,
-- the compiler, the target, and the identities of the dependencies. It does
-- not read the sources, so a consumer computes it without them. A Hackage
-- release never changes, and a core library is identified by the compiler
-- it ships with.
storePackageIdentity :: ModuleCompileConfig -> [InstalledPackage] -> PackageInputs -> IO (FilePath, Text)
storePackageIdentity config dependencies inputs = do
  let (unitIdentity, packageNameText, packageVersionText) = packageUnitIdentity inputs
      cInputs = inputCCompileInfo inputs
  cSysrootArguments <-
    if compileNoCode config || null (HackageCabal.cCompileSources cInputs)
      then pure []
      else wasmSysrootIncludeArguments (compileTarget config)
  let fingerprint =
        stableHash
          ( map
              TE.encodeUtf8
              ( packageArtifactFormatVersion
                  : T.pack (packageOptionsKey config)
                  : T.pack (show cSysrootArguments)
                  : packageNameText
                  : packageVersionText
                  : sortOn id (map installedIdentity dependencies)
              )
          )
  pure (T.unpack unitIdentity <> "-" <> take 16 fingerprint, unitIdentity)

-- | The directory name and unit identity of a package in a build directory.
localPackageIdentity :: PackageInputs -> (FilePath, Text)
localPackageIdentity inputs =
  let (unitIdentity, _, _) = packageUnitIdentity inputs
   in (T.unpack unitIdentity, unitIdentity)

packageUnitIdentity :: PackageInputs -> (Text, Text, Text)
packageUnitIdentity inputs =
  let packageId = package (packageDescription (inputDescription inputs))
      packageNameText = T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId))
      packageVersionText = T.pack (prettyShow (CabalPackage.packageVersion packageId))
   in (packageNameText <> "-" <> packageVersionText, packageNameText, packageVersionText)

-- | What the package archive depends on besides the module objects.
archiveInputsHash :: ModuleCompileConfig -> FilePath -> [InstalledPackage] -> PackageInputs -> IO String
archiveInputsHash config root dependencies inputs = do
  let cInputs = inputCCompileInfo inputs
  sourceHash <- sourceFilesHash root (inputCabalFile inputs : HackageCabal.cCompileSources cInputs)
  cSysrootArguments <-
    if null (HackageCabal.cCompileSources cInputs)
      then pure []
      else wasmSysrootIncludeArguments (compileTarget config)
  pure
    ( stableHash
        ( map
            TE.encodeUtf8
            ( T.pack (backendOptionsKey config)
                : T.pack sourceHash
                : T.pack (show cSysrootArguments)
                : sortOn id (map installedIdentity dependencies)
            )
        )
    )

buildEnvironmentIdentity :: NativeTarget -> IO String
buildEnvironmentIdentity target = do
  (compiler, arguments) <- backendCompiler target
  archiver <- backendArchiver target
  compilerHash <- executableIdentity compiler
  archiverHash <- executableIdentity archiver
  headers <-
    mapM
      getDataFileName
      [ "compiler/native/runtime/include/HsFFI.h",
        "compiler/native/runtime/include/MachDeps.h",
        "compiler/native/runtime/include/ghcplatform.h"
      ]
  headerHash <- stableHash <$> mapM BS.readFile headers
  pure (stableHash (map BS8.pack [compilerBuildIdentity, compilerHash, archiverHash, headerHash, show arguments]))

-- | The part of the configuration that changes what a package is: the
-- compiler and the target. Flags that add or drop outputs, such as
-- @--keep-core@, or that only check, such as @--lint@, are recorded in the
-- manifest instead.
packageOptionsKey :: ModuleCompileConfig -> String
packageOptionsKey config =
  stableHash
    [ BS8.pack (compileBuildIdentity config),
      TE.encodeUtf8 packageArtifactFormatVersion,
      BS8.pack (show (compileTarget config))
    ]

-- | The part of the configuration the type interfaces depend on.
frontendOptionsKey :: ModuleCompileConfig -> String
frontendOptionsKey = packageOptionsKey

-- | The part of the configuration the backend outputs depend on.
backendOptionsKey :: ModuleCompileConfig -> String
backendOptionsKey config =
  stableHash
    [ BS8.pack (compileBuildIdentity config),
      TE.encodeUtf8 packageArtifactFormatVersion,
      BS8.pack (show (compileTarget config, compileKeepCore config, compileKeepGrin config, compileKeepNative config, compileLint config))
    ]

createTemporaryStoreRoot :: FilePath -> FilePath -> IO FilePath
createTemporaryStoreRoot storeRoot packageDirectory = do
  (path, handle) <- openBinaryTempFile storeRoot (".tmp-" <> packageDirectory <> "-")
  hClose handle
  removeFile path
  createDirectory path
  pure path

removeTemporaryStoreRoot :: FilePath -> IO ()
removeTemporaryStoreRoot path = do
  exists <- doesDirectoryExist path
  when exists (removeDirectoryRecursive path)

setInstalledStorePath :: FilePath -> InstalledPackage -> InstalledPackage
setInstalledStorePath storePath installed =
  installed
    { installedResult =
        (installedResult installed)
          { installStorePath = storePath
          }
    }

loadRequiredDependencies :: [SourceModule] -> [InstalledPackage] -> IO [InstalledPackage]
loadRequiredDependencies sources = mapM loadDependency
  where
    requirements = requiredDependencyModules sources
    loadDependency dependency = loadInstalledPackage requirements (installedImmutable dependency) (installStorePath (installedResult dependency))

requiredDependencyModules :: [SourceModule] -> Set.Set (Maybe Text, Text)
requiredDependencyModules sources =
  Set.fromList
    ( [ (importDeclPackage importDecl, importDeclModule importDecl)
      | source <- sources,
        importDecl <- Syntax.moduleImports (sourceModuleAst source),
        not (localImport importDecl)
      ]
        <> [(Nothing, "Prelude") | any moduleUsesImplicitPrelude sources]
        <> [(Nothing, name) | name <- wiredInterfaceModules]
    )
  where
    localNames = Set.fromList (map sourceName sources)
    localImport importDecl =
      importDeclPackage importDecl == Just "this"
        || (isNothing (importDeclPackage importDecl) && importDeclModule importDecl `Set.member` localNames)

loadInstalledPackage :: Set.Set (Maybe Text, Text) -> Bool -> FilePath -> IO InstalledPackage
loadInstalledPackage requirements immutable storePath = do
  manifestResult <- readPackageManifest (packageManifestPath storePath)
  manifest <- either (ioError . userError . ("Invalid installed package manifest: " <>)) pure manifestResult
  digests <-
    readStamp (packageDigestsPath storePath)
      >>= maybe (ioError (userError ("The installed package has no digests: " <> storePath))) pure
  let selectedModules = filter (moduleRequired manifest) (packageManifestModules manifest)
  entries <- mapM loadModule selectedModules
  (instanceFacts', instanceProviders) <-
    if null selectedModules
      then pure (mempty, Map.empty)
      else loadPackageInstances selectedModules
  let package = Package (packageManifestName manifest) (PackageId (packageManifestUnitId manifest))
      exports = Map.fromList [(ModuleKey package name, scope) | (name, scope, _) <- entries]
      types = LazyMap.fromList [(name, interface) | (name, _, interface) <- entries]
      exposed = Map.restrictKeys (packageDigestsModules digests) (Set.fromList (packageManifestModules manifest))
      scopeHashes = Map.map moduleScopeDigest exposed
      typeHashes = Map.map moduleTypeDigest exposed
  pure
    InstalledPackage
      { installedResult = InstallResult storePath [] (packageManifestModules manifest),
        installedName = packageManifestName manifest,
        installedVersion = packageManifestVersion manifest,
        installedIdentity = packageManifestIdentity manifest,
        installedImmutable = immutable,
        installedManifest = manifest,
        installedExports = exports,
        installedTypes = types,
        installedScopeHashes = scopeHashes,
        installedTypeHashes = typeHashes,
        installedInstanceDigest = packageDigestsInstances digests,
        installedInstanceFacts = instanceFacts',
        installedInstanceProviders = instanceProviders
      }
  where
    moduleRequired manifest name =
      any
        (\(packageName', moduleName') -> moduleName' == name && maybe True (== packageManifestName manifest) packageName')
        (Set.toList requirements)

    loadModule name = do
      let root = storePath </> moduleNameDirectory name
          resolvePath = root </> "resolve.cbor"
          typePath = root </> "type.cbor"
      resolveBytes <- BS.readFile resolvePath
      resolveArtifact <- either (ioError . userError . (("Invalid resolve artifact " <> resolvePath <> ": ") <>)) pure (decodeResolveArtifact resolveBytes)
      typeBytes <- BL.readFile typePath
      let typeArtifact = decodeTypeArtifact typeBytes
      unless (resolveArtifactModuleName resolveArtifact == name) (ioError (userError ("Resolve artifact module name does not match " <> resolvePath)))
      unless (typeArtifactModuleName typeArtifact == name) (ioError (userError ("Type artifact module name does not match " <> typePath)))
      pure (name, resolveArtifactScope resolveArtifact, typeArtifactInterface typeArtifact)

    loadPackageInstances selected = do
      let path = storePath </> "instances.cbor"
      exists <- doesFileExist path
      if not exists
        then pure (mempty, Map.empty)
        else do
          bytes <- BL.readFile path
          let artifact = decodeTypeArtifact bytes
          unless (typeArtifactModuleName artifact == "$package-instances") (ioError (userError ("Package instance artifact name does not match " <> path)))
          let providers = Map.restrictKeys (Map.map Set.fromList (typeArtifactInstanceProviders artifact)) (Set.fromList selected)
              visibleProviders = Set.unions (Map.elems providers)
          pure (selectInstanceProviders (typeArtifactInterface artifact) visibleProviders, providers)

parseSource :: FilePath -> DependencyVersions -> HackageCabal.FileInfo -> IO SourceModule
parseSource root versions fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  ParsedInterfaceFile path modu sourceLines parseDiagnostics _ extensions _ <- parseInterfaceFile root versions fileInfo
  -- The type checker reads the language pragmas of the module. Give it the
  -- effective extensions, which include the cabal default extensions and
  -- the language edition. The type checker turns MonoLocalBinds on by
  -- default, so turn it off when the effective extensions do not have it.
  let monoLocalBinds = [Syntax.DisableExtension Syntax.MonoLocalBinds | Syntax.MonoLocalBinds `notElem` extensions]
      modu' = modu {Syntax.moduleLanguagePragmas = monoLocalBinds <> map Syntax.EnableExtension extensions <> Syntax.moduleLanguagePragmas modu}
  pure (SourceModule path (BS.length bytes) (T.pack (stableHash [bytes, BS8.pack (show modu'), BS8.pack (show extensions)])) modu' extensions sourceLines parseDiagnostics)

loadSourceModules :: Int -> FilePath -> DependencyVersions -> [HackageCabal.FileInfo] -> IO ([SourceModule], [TaskTiming])
loadSourceModules workers root versions files = do
  results <- mapM (const newEmptyTMVarIO) files
  let tasks = zipWith3 loadTask [0 ..] files results
  timings <- runTaskGraph workers tasks
  sources <- mapM (atomically . readTMVar) results
  pure (sources, timings)
  where
    loadTask order fileInfo result =
      Task
        { taskId = TaskId ("imports:" <> HackageCabal.fileInfoPath fileInfo),
          taskKind = TaskParse,
          taskOrder = order,
          taskDependencies = Set.empty,
          taskAction = do
            source <- parseSource root versions fileInfo
            let ast = sourceModuleAst source
                imports = map importDeclModule (Syntax.moduleImports ast)
            _ <- evaluate (rnf (moduleName ast, imports))
            atomically (putTMVar result source)
        }

sourceModuleUnits :: [SourceModule] -> [SourceUnit]
sourceModuleUnits sources = zipWith makeUnit [0 ..] orderedComponents
  where
    node source = (source, sourceName source, moduleDependencies source)
    moduleDependencies source =
      nub (filter (/= sourceName source) wiredTypeModules <> sourceDependencyNames source)
    flatten (AcyclicSCC value) = [value]
    flatten (CyclicSCC values) = values
    components = map (sortOn sourceName . flatten) (stronglyConnComp (map node sources))
    componentNames = Map.fromList [(sourceName source, index) | (index, component) <- zip [0 ..] components, source <- component]
    dependenciesFor component =
      Set.toAscList $
        Set.fromList
          [ dependencyIndex
          | source <- component,
            dependency <- moduleDependencies source,
            Just dependencyIndex <- [Map.lookup dependency componentNames],
            dependencyIndex /= fromMaybe (-1) (Map.lookup (sourceName source) componentNames)
          ]
    componentDependencies = Map.fromList [(index, dependenciesFor component) | (index, component) <- zip [0 ..] components]
    componentLabel component = minimum (map sourceName component)
    orderedIndices = canonicalTopologicalOrder components componentDependencies componentLabel
    orderedComponents = [components !! index | index <- orderedIndices]
    orderedIdByOldIndex = Map.fromList [(oldIndex, UnitId order) | (order, oldIndex) <- zip [0 ..] orderedIndices]
    makeUnit order component =
      let oldIndex =
            fromMaybe (error "missing source component") $
              listToMaybe component >>= (\source -> Map.lookup (sourceName source) componentNames)
       in SourceUnit
            { sourceUnitId = UnitId order,
              sourceUnitOrder = order,
              sourceUnitSources = component,
              sourceUnitDependencies =
                sortOn
                  id
                  [ dependencyId
                  | dependencyIndex <- Map.findWithDefault [] oldIndex componentDependencies,
                    Just dependencyId <- [Map.lookup dependencyIndex orderedIdByOldIndex]
                  ]
            }

canonicalTopologicalOrder :: [[SourceModule]] -> Map.Map Int [Int] -> ([SourceModule] -> Text) -> [Int]
canonicalTopologicalOrder components dependencies label = go Set.empty []
  where
    componentCount = length components
    go complete ordered
      | Set.size complete == componentCount = reverse ordered
      | otherwise =
          case sortOn
            (label . (components !!))
            [ index
            | index <- [0 .. componentCount - 1],
              index `Set.notMember` complete,
              all (`Set.member` complete) (Map.findWithDefault [] index dependencies)
            ] of
            [] -> error "source component graph is cyclic"
            index : _ -> go (Set.insert index complete) (index : ordered)

renderResolveErrors :: [SourceModule] -> [ResolveError] -> String
renderResolveErrors sources errors =
  "Name resolution failed:\n"
    <> intercalate "\n\n" (map (renderResolveError sourceLines) errors)
    <> "\n"
  where
    sourceLines = Map.unions (map sourceModuleSourceLines sources)

renderResolveError :: Map.Map FilePath (Map.Map Int Text) -> ResolveError -> String
renderResolveError sourceLines resolveError =
  case resolveError of
    ResolveResolutionError sourceSpan name namespace message ->
      renderResolveLocation sourceSpan
        <> ": error: "
        <> renderResolveMessage message name namespace
        <> renderResolveExcerpt sourceLines sourceSpan
    ResolveNotImplemented message -> "error: not implemented: " <> message

renderResolveLocation :: SourceSpan -> String
renderResolveLocation sourceSpan =
  case sourceSpan of
    NoSourceSpan -> "<unknown location>"
    SourceSpan sourcePath startLine startColumn _ _ _ _ ->
      sourcePath <> ":" <> show startLine <> ":" <> show startColumn

renderResolveMessage :: String -> Text -> ResolutionNamespace -> String
renderResolveMessage message name namespace
  | message == "unbound" = "unbound " <> renderedNamespace <> " name ‘" <> T.unpack name <> "’"
  | message == "not found" = renderedNamespace <> " ‘" <> T.unpack name <> "’ not found"
  | otherwise = message <> ": " <> renderedNamespace <> " name ‘" <> T.unpack name <> "’"
  where
    renderedNamespace =
      case namespace of
        ResolutionNamespaceTerm -> "term"
        ResolutionNamespaceType -> "type"
        ResolutionNamespaceModule -> "module"

renderResolveExcerpt :: Map.Map FilePath (Map.Map Int Text) -> SourceSpan -> String
renderResolveExcerpt sourceLines sourceSpan =
  case sourceSpan of
    NoSourceSpan -> ""
    SourceSpan sourcePath startLine startColumn endLine endColumn _ _ ->
      case Map.lookup sourcePath sourceLines >>= Map.lookup startLine of
        Nothing -> ""
        Just sourceLine ->
          let lineNumber = show startLine
              gutterWidth = length lineNumber
              caretStart = max 0 (startColumn - 1)
              caretWidth
                | startLine == endLine = max 1 (endColumn - startColumn)
                | otherwise = max 1 (T.length sourceLine - caretStart)
           in "\n  "
                <> lineNumber
                <> " | "
                <> T.unpack sourceLine
                <> "\n  "
                <> replicate gutterWidth ' '
                <> " | "
                <> replicate caretStart ' '
                <> replicate caretWidth '^'

renderFrontendFailure :: [SourceModule] -> [Value] -> [ResolveError] -> [(Text, TcDiagnostic)] -> String
renderFrontendFailure sources parseDiagnostics resolveDiagnostics typeDiagnostics =
  case sections of
    [] -> ""
    _ -> intercalate "\n\n" (map dropFinalNewlines sections) <> "\n"
  where
    sections =
      [renderParseDiagnostics parseDiagnostics | not (null parseDiagnostics)]
        <> [renderResolveErrors sources resolveDiagnostics | not (null resolveDiagnostics)]
        <> [renderTypeErrors sources typeDiagnostics | not (null typeDiagnostics)]

    dropFinalNewlines = reverse . dropWhile (== '\n') . reverse

renderParseDiagnostics :: [Value] -> String
renderParseDiagnostics diagnostics =
  "Parse failed:\n" <> intercalate "\n" (map (renderHumanDiagnostic "parse") diagnostics)

renderTypeErrors :: [SourceModule] -> [(Text, TcDiagnostic)] -> String
renderTypeErrors sources diagnostics =
  "Type check failed:\n"
    <> intercalate "\n\n" (map renderTypeError diagnostics)
    <> "\n"
  where
    sourceLines = Map.unions (map sourceModuleSourceLines sources)
    renderTypeError (label, diagnostic) =
      case diagLoc diagnostic of
        Nothing -> "<unknown location in " <> T.unpack label <> ">: error: " <> renderTypeErrorKind (diagKind diagnostic)
        Just sourceSpan ->
          renderResolveLocation sourceSpan
            <> ": error: "
            <> renderTypeErrorKind (diagKind diagnostic)
            <> renderResolveExcerpt sourceLines sourceSpan

renderTypeErrorKind :: TcErrorKind -> String
renderTypeErrorKind kind =
  case kind of
    UnificationError left right _ _ ->
      "could not match " <> renderTcType left <> " with " <> renderTcType right
    OccursCheckError variable ty ->
      "occurs check failed: " <> renderTcType variable <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderTcType expected <> ", got " <> renderTcType actual
    UnsolvedWanted pred' _ ->
      "unsolved constraint " <> renderPred pred'
    TopLevelUnliftedBinding name ty ->
      "top-level binding " <> T.unpack name <> " has unlifted type " <> renderTcType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      "function argument " <> T.unpack name <> " has type " <> renderTcType ty <> " without a fixed runtime representation"
    OtherError message ->
      message

runPackageTasks :: PackageTaskContext -> Int -> [SourceUnit] -> IO ([UnitRuntime], [TaskTiming])
runPackageTasks context workers units = do
  runtimes <-
    forM units $ \unit ->
      UnitRuntime unit <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  let runtimeMap = Map.fromList [(sourceUnitId (runtimeUnit runtime), runtime) | runtime <- runtimes]
      tasks = concatMap (unitTasks runtimeMap) runtimes
  timings <- runTaskGraph workers tasks
  pure (runtimes, timings)
  where
    unitTasks runtimeMap runtime =
      map (parseTask runtime) (sourceUnitSources unit)
        <> [resolveTask runtimeMap runtime, typeTask runtimeMap runtime]
        <> [backendTask runtime | not (compileNoCode config)]
      where
        unit = runtimeUnit runtime

    parseTask runtime source =
      Task
        { taskId = parseTaskId source,
          taskKind = TaskParse,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies = Set.empty,
          taskAction = evaluate (rnf (sourceModuleAst source, sourceModuleParseDiagnostics source))
        }

    resolveTask runtimeMap runtime =
      Task
        { taskId = resolveTaskId (runtimeUnit runtime),
          taskKind = TaskResolve,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies =
            Set.fromList
              ( map parseTaskId (sourceUnitSources (runtimeUnit runtime))
                  <> map (resolveTaskId . runtimeUnit . lookupRuntime runtimeMap) (sourceUnitDependencies (runtimeUnit runtime))
              ),
          taskAction =
            runResolveUnit
              context
              runtimeMap
              runtime
        }

    typeTask runtimeMap runtime =
      Task
        { taskId = typeTaskId (runtimeUnit runtime),
          taskKind = TaskTypeCheck,
          taskOrder = sourceUnitOrder (runtimeUnit runtime),
          taskDependencies =
            Set.fromList
              ( resolveTaskId (runtimeUnit runtime)
                  : map (typeTaskId . runtimeUnit . lookupRuntime runtimeMap) (sourceUnitDependencies (runtimeUnit runtime))
              ),
          taskAction =
            runTypeUnit
              context
              runtimeMap
              runtime
        }

    backendTask runtime =
      Task
        { taskId = backendTaskId (runtimeUnit runtime),
          taskKind = TaskBackend,
          taskOrder = negate (sum (map sourceModuleSize (sourceUnitSources (runtimeUnit runtime)))),
          taskDependencies = Set.singleton (typeTaskId (runtimeUnit runtime)),
          taskAction = runBackendUnit context runtime
        }
    config = taskModuleCompileConfig context

parseTaskId :: SourceModule -> TaskId
parseTaskId = TaskId . ("parse:" <>) . sourceModulePath

resolveTaskId :: SourceUnit -> TaskId
resolveTaskId = TaskId . ("resolve:" <>) . T.unpack . unitLabel

typeTaskId :: SourceUnit -> TaskId
typeTaskId = TaskId . ("type-check:" <>) . T.unpack . unitLabel

backendTaskId :: SourceUnit -> TaskId
backendTaskId = TaskId . ("backend:" <>) . T.unpack . unitLabel

unitLabel :: SourceUnit -> Text
unitLabel = T.intercalate "+" . map sourceName . sourceUnitSources

sourceName :: SourceModule -> Text
sourceName = fromMaybe "Main" . moduleName . sourceModuleAst

sourceDependencyNames :: SourceModule -> [Text]
sourceDependencyNames source =
  map importDeclModule (Syntax.moduleImports modu)
    <> ["Prelude" | moduleUsesImplicitPrelude source]
  where
    modu = sourceModuleAst source

moduleUsesImplicitPrelude :: SourceModule -> Bool
moduleUsesImplicitPrelude = elem ImplicitPrelude . sourceModuleExtensions

lookupRuntime :: Map.Map UnitId UnitRuntime -> UnitId -> UnitRuntime
lookupRuntime runtimes identifier =
  fromMaybe (error "missing unit runtime") (Map.lookup identifier runtimes)

readDependencyResults :: (UnitRuntime -> TMVar value) -> Map.Map UnitId UnitRuntime -> [UnitId] -> IO [value]
readDependencyResults select runtimes =
  mapM (atomically . readTMVar . select . lookupRuntime runtimes)

runResolveUnit :: PackageTaskContext -> Map.Map UnitId UnitRuntime -> UnitRuntime -> IO ()
runResolveUnit context runtimes runtime = do
  dependencyResults <- readDependencyResults runtimeResolveResult runtimes (sourceUnitDependencies unit)
  let storePath = taskStorePath context
      resolvePackage = taskResolvePackage context
      root = taskPackageRoot context
      dependencyExports = taskDependencyExports context
      dependencyScopeHashes = taskDependencyScopeHashes context
      verbose = compileVerbose config
      sources = sourceUnitSources unit
      packageModules = modulesInPackage resolvePackage (map sourceModuleAst sources)
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredInterfaceModules)
      availableExports = Map.unions (map resolveUnitExports dependencyResults) `Map.union` dependencyExports
      availableScopeHashes = Map.unions (map resolveUnitScopeHashes dependencyResults) `Map.union` dependencyScopeHashes
      scopeInputs = [("scope:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableScopeHashes]]
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      inputs = sortOn fst (sourceHashes <> scopeInputs)
      resolvePath source = moduleDirectory (sourceModuleAst source) </> "resolve.cbor"
      stampPath = storePath </> unitResolveStampPath unit
      parseSuccess = all (null . sourceModuleParseDiagnostics) sources
      dependenciesSucceeded = all resolveUnitSuccess dependencyResults
  reused <-
    if parseSuccess && dependenciesSucceeded
      then reuseResolveUnit storePath stampPath inputs resolvePackage (map resolvePath sources)
      else pure Nothing
  result <- case reused of
    Just (unitExports, scopeHashes) -> do
      verbose ("Reuse resolve context: " <> T.unpack (unitLabel unit))
      pure
        ResolveUnitResult
          { resolveUnitExports = unitExports,
            resolveUnitScopeHashes = scopeHashes,
            resolveUnitResolved = Nothing,
            resolveUnitErrors = [],
            resolveUnitSuccess = True
          }
    Nothing -> do
      let builtinScope = builtinFunctionScope resolvePackage availableExports packageModules
          resolved = resolveWithDeps builtinScope availableExports packageModules
          errors = resolveErrors resolved
          unitExports = extractInterfaceWithDeps availableExports resolved
          success = parseSuccess && dependenciesSucceeded && null errors
      scopeHashes <-
        if success
          then do
            digests <- forM sources $ \source -> writeArtifact verbose inputs unitExports resolvePackage (storePath </> resolvePath source) source
            files <- stampFiles storePath (map resolvePath sources)
            writeStamp stampPath ResolveStamp {resolveStampInputs = inputs, resolveStampScopes = Map.fromList digests, resolveStampFiles = files}
            pure (Map.fromList digests)
          else pure Map.empty
      pure
        ResolveUnitResult
          { resolveUnitExports = unitExports,
            resolveUnitScopeHashes = scopeHashes,
            resolveUnitResolved = Just resolved,
            resolveUnitErrors = errors,
            resolveUnitSuccess = success
          }
  atomically (putTMVar (runtimeResolveResult runtime) result)
  where
    config = taskModuleCompileConfig context
    unit = runtimeUnit runtime

-- | The exports and scope digests of a unit whose resolve artifacts were
-- built from the same inputs, if the artifacts are the ones the stamp
-- recorded.
reuseResolveUnit :: FilePath -> FilePath -> [(Text, Text)] -> Package -> [FilePath] -> IO (Maybe (ModuleExports, Map.Map Text Text))
reuseResolveUnit storePath stampPath inputs resolvePackage artifactPaths = do
  stamp <- readStamp stampPath
  case stamp of
    Just recorded | resolveStampInputs recorded == inputs -> do
      current <- filesMatchStamps storePath (resolveStampFiles recorded)
      if not current
        then pure Nothing
        else do
          decoded <- forM artifactPaths $ \path -> decodeResolveArtifact <$> BS.readFile (storePath </> path)
          case sequence decoded of
            Left _ -> pure Nothing
            Right artifacts ->
              pure
                ( Just
                    ( Map.fromList [(ModuleKey resolvePackage (resolveArtifactModuleName artifact), resolveArtifactScope artifact) | artifact <- artifacts],
                      resolveStampScopes recorded
                    )
                )
    _ -> pure Nothing

runTypeUnit :: PackageTaskContext -> Map.Map UnitId UnitRuntime -> UnitRuntime -> IO ()
runTypeUnit context runtimes runtime = do
  resolvedOutput <- atomically (readTMVar (runtimeResolveResult runtime))
  dependencyResults <- readDependencyResults runtimeTypeResult runtimes (sourceUnitDependencies unit)
  dependencyResolveResults <- readDependencyResults runtimeResolveResult runtimes (sourceUnitDependencies unit)
  let storePath = taskStorePath context
      resolvePackage = taskResolvePackage context
      primIdentity = taskPrimIdentity context
      root = taskPackageRoot context
      dependencyExports = taskDependencyExports context
      dependencyScopeHashes = taskDependencyScopeHashes context
      dependencyTypes = taskDependencyTypes context
      dependencyTypeHashes = taskDependencyTypeHashes context
      dependencyInstanceFacts = taskDependencyInstanceFacts context
      dependencyInstanceProviders = taskDependencyInstanceProviders context
      verbose = compileVerbose config
      sources = sourceUnitSources unit
      unitNames = map sourceName sources
      importedNames = nub (concatMap sourceDependencyNames sources)
      dependencyNames = nub (importedNames <> wiredInterfaceModules)
      availableTypes = LazyMap.unions (map typeUnitTypes dependencyResults) `LazyMap.union` dependencyTypes
      availableTypeHashes = LazyMap.unions (map typeUnitHashes dependencyResults) `LazyMap.union` dependencyTypeHashes
      availableExports = Map.unions (map resolveUnitExports dependencyResolveResults) `Map.union` dependencyExports
      availableScopeHashes = Map.unions (map resolveUnitScopeHashes dependencyResolveResults) `Map.union` dependencyScopeHashes
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- sources]
      scopeInputs =
        [("scope:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableScopeHashes]]
      typeInputs =
        [("type:" <> name, digest) | name <- dependencyNames, name `notElem` unitNames, Just digest <- [Map.lookup name availableTypeHashes]]
      -- The instances a unit sees come from every unit below it and from
      -- the dependency packages that supply an imported module. A facts
      -- digest covers the facts of the units below the one it names.
      factsInputs =
        [ ("facts:" <> unitLabel (runtimeUnit (lookupRuntime runtimes dependency)), typeUnitFactsDigest result)
        | (dependency, result) <- zip (sourceUnitDependencies unit) dependencyResults
        ]
      packageInputs =
        [ ("package:" <> name, digest)
        | (name, digest, modules) <- taskDependencyPackages context,
          any (`Set.member` modules) dependencyNames
        ]
      inputs =
        sortOn fst $
          sourceHashes
            <> scopeInputs
            <> typeInputs
            <> factsInputs
            <> packageInputs
            <> [ ("options:frontend", T.pack (frontendOptionsKey config)),
                 ("options:extensions", T.pack (show (map sourceModuleExtensions sources)))
               ]
      typePath source = moduleDirectory (sourceModuleAst source) </> "type.cbor"
      factsPath = unitFactsPath unit
      stampPath = storePath </> unitStampPath unit
      frontendFiles = factsPath : map typePath sources
      externalInstanceProviders =
        Set.unions
          [ Map.findWithDefault Set.empty name dependencyInstanceProviders
          | name <- dependencyNames,
            name `notElem` unitNames
          ]
      externalInstanceInterface = selectInstanceProviders dependencyInstanceFacts externalInstanceProviders
      -- Each dependency carries the instance closure of its own dependencies,
      -- so the closures agree wherever they overlap.
      importedInstanceInterface =
        unionTcInterfaces
          (externalInstanceInterface : map typeUnitInstanceInterface dependencyResults)
      importedTypes =
        mergeTcInterfaces
          ( importedInstanceInterface
              : [ interface
                | name <- dependencyNames,
                  name `notElem` unitNames,
                  Just interface <- [Map.lookup name availableTypes]
                ]
          )
      checkUnit = do
        resolved <-
          case resolveUnitResolved resolvedOutput of
            Just result -> pure result
            Nothing ->
              let packageModules = modulesInPackage resolvePackage (map sourceModuleAst sources)
                  builtinScope = builtinFunctionScope resolvePackage availableExports packageModules
               in pure (resolveWithDeps builtinScope availableExports packageModules)
        let checked =
              typecheckModuleSccWithInterface
                (primTcConfig primIdentity)
                importedTypes
                (map snd (resolvedModules resolved))
            checkedDiagnostics = concatMap tcModuleDiagnostics (fst checked)
        _ <- evaluate (length checkedDiagnostics)
        pure (checked, checkedDiagnostics)
      dependencySuccess = all typeUnitSuccess dependencyResults
      resolveSuccess = resolveUnitSuccess resolvedOutput
  reused <-
    if resolveSuccess && dependencySuccess
      then reuseTypeUnit config storePath stampPath inputs
      else pure Nothing
  case reused of
    Just recorded -> do
      artifacts <- mapM (fmap decodeTypeArtifact . BL.readFile . (storePath </>) . typePath) sources
      ownFacts <- typeArtifactInterface . decodeTypeArtifact <$> BL.readFile (storePath </> factsPath)
      let interfaces = map typeArtifactInterface artifacts
          complete = mergeTcInterfaces (importedTypes : ownFacts : interfaces)
      verbose ("Reuse type and backend artifacts: " <> T.unpack (unitLabel unit))
      atomically $
        putTMVar
          (runtimeTypeResult runtime)
          TypeUnitResult
            { typeUnitTypes = Map.fromList (zip unitNames interfaces),
              typeUnitHashes = unitStampTypes recorded,
              typeUnitOwnInstanceInterface = ownFacts,
              typeUnitFactsDigest = unitStampFacts recorded,
              typeUnitInstanceInterface = unionTcInterfaces [importedInstanceInterface, ownFacts],
              typeUnitDiagnostics = [],
              typeUnitWritten = Set.empty,
              typeUnitReused = Set.fromList unitNames,
              typeUnitPendingStamp = Nothing,
              typeUnitPendingCompile = Nothing,
              typeUnitDesugarInterface = complete,
              typeUnitSuccess = True
            }
    Nothing -> do
      (initialChecked@(_, checkedInterface), diagnostics) <- checkUnit
      let completeInterface = mergeTcInterfaces [importedTypes, checkedInterface]
          unitTypes = map (moduleTypeInterface (resolveUnitExports resolvedOutput) resolvePackage completeInterface) sources
          ownInstanceInterface = addReferencedFacts completeInterface (instanceFacts checkedInterface)
          completeInstanceInterface = unionTcInterfaces [importedInstanceInterface, ownInstanceInterface]
          typeSuccess = not (any ((== TcError) . diagSeverity) diagnostics)
          success = resolveSuccess && dependencySuccess && typeSuccess
      (ownTypeHashes, factsDigest) <-
        if success
          then do
            typeHashes <- Map.fromList <$> zipWithM (writeTypeArtifact verbose inputs ((storePath </>) . typePath)) sources unitTypes
            -- The facts artifact records the facts digests it was built
            -- above, so its own digest changes with any of them.
            let factsBytes = encodeTypeArtifact (TypeArtifact "$unit" (sortOn fst (factsInputs <> packageInputs)) Map.empty ownInstanceInterface)
            createDirectoryIfMissing True (takeDirectory (storePath </> factsPath))
            BL.writeFile (storePath </> factsPath) factsBytes
            pure (typeHashes, T.pack (stableHash [BL.toStrict factsBytes]))
          else pure (Map.empty, "")
      pendingCompile <-
        if compileNoCode config || not success
          then pure Nothing
          else do
            let (checkedModules, _) = initialChecked
                desugarConfigs =
                  Map.fromList
                    [ (name, Fc.moduleDesugarConfig (primKinds primIdentity) primIdentity resolvePackage name (resolveUnitExports resolvedOutput))
                    | name <- unitNames
                    ]
            pure (Just (PendingCompile checkedModules desugarConfigs))
      let unitSet = Set.fromList unitNames
          -- A unit with warnings is not stamped, so the next build reports
          -- them again.
          pendingStamp
            | success && null diagnostics =
                Just
                  PendingStamp
                    { pendingStampPath = stampPath,
                      pendingStampInputs = inputs,
                      pendingStampTypes = ownTypeHashes,
                      pendingStampFacts = factsDigest,
                      pendingStampFrontendFiles = frontendFiles
                    }
            | otherwise = Nothing
      -- Force the type result before this type-check task ends.
      typeResult <-
        evaluate
          TypeUnitResult
            { typeUnitTypes = Map.fromList (zip unitNames unitTypes),
              typeUnitHashes = ownTypeHashes,
              typeUnitOwnInstanceInterface = ownInstanceInterface,
              typeUnitFactsDigest = factsDigest,
              typeUnitInstanceInterface = completeInstanceInterface,
              typeUnitDiagnostics = diagnostics,
              typeUnitWritten = unitSet,
              typeUnitReused = Set.empty,
              typeUnitPendingStamp = pendingStamp,
              typeUnitPendingCompile = pendingCompile,
              typeUnitDesugarInterface = completeInterface,
              typeUnitSuccess = success
            }
      when (compileNoCode config) $
        forM_ pendingStamp $
          \pending -> writeUnitStamp storePath pending Nothing
      atomically (putTMVar (runtimeTypeResult runtime) typeResult)
  where
    config = taskModuleCompileConfig context
    unit = runtimeUnit runtime

-- | The stamp of a unit whose type artifacts, and objects when code is
-- wanted, were built from the same inputs and are still the recorded files.
reuseTypeUnit :: ModuleCompileConfig -> FilePath -> FilePath -> [(Text, Text)] -> IO (Maybe UnitStamp)
reuseTypeUnit config storePath stampPath inputs = do
  stamp <- readStamp stampPath
  case stamp of
    Just recorded | unitStampInputs recorded == inputs -> do
      frontendCurrent <- filesMatchStamps storePath (unitStampFiles recorded)
      backendCurrent <-
        if compileNoCode config
          then pure True
          else case unitStampBackend recorded of
            Just backend | backendStampOptions backend == T.pack (backendOptionsKey config) -> filesMatchStamps storePath (backendStampFiles backend)
            _ -> pure False
      pure (if frontendCurrent && backendCurrent then Just recorded else Nothing)
    _ -> pure Nothing

writeUnitStamp :: FilePath -> PendingStamp -> Maybe (Text, [FilePath]) -> IO ()
writeUnitStamp storePath pending backend = do
  files <- stampFiles storePath (pendingStampFrontendFiles pending)
  backendStamp <- forM backend $ \(options, paths) -> BackendStamp options <$> stampFiles storePath paths
  writeStamp
    (pendingStampPath pending)
    UnitStamp
      { unitStampInputs = pendingStampInputs pending,
        unitStampTypes = pendingStampTypes pending,
        unitStampFacts = pendingStampFacts pending,
        unitStampFiles = files,
        unitStampBackend = backendStamp
      }

runBackendUnit :: PackageTaskContext -> UnitRuntime -> IO ()
runBackendUnit context runtime = do
  started <- getMonotonicTimeNSec
  result <- atomically (readTMVar (runtimeTypeResult runtime))
  case typeUnitPendingCompile result of
    Just pending | typeUnitSuccess result -> do
      let config = taskModuleCompileConfig context
          storePath = taskStorePath context
      phaseTimings <-
        compileCheckedModules
          config
          (compileVerbose config)
          (taskPrimIdentity context)
          (typeUnitDesugarInterface result)
          (moduleOutputPaths storePath (compileTarget config))
          (pendingDesugarConfigs pending)
          (pendingModules pending)
      forM_ (typeUnitPendingStamp result) $ \stamp ->
        writeUnitStamp storePath stamp (Just (T.pack (backendOptionsKey config), unitBackendPaths config (runtimeUnit runtime)))
      ended <- getMonotonicTimeNSec
      atomicModifyIORef' (taskBackendPhaseTimings context) (\total -> (total <> withOtherTime started ended phaseTimings, ()))
    _ -> do
      ended <- getMonotonicTimeNSec
      atomicModifyIORef' (taskBackendPhaseTimings context) (\total -> (total <> withOtherTime started ended mempty, ()))

unitStampDirectory :: FilePath
unitStampDirectory = ".units"

unitStampBase :: SourceUnit -> FilePath
unitStampBase unit = unitStampDirectory </> stableHash [TE.encodeUtf8 (unitLabel unit)]

unitFactsPath :: SourceUnit -> FilePath
unitFactsPath unit = unitStampBase unit <.> "cbor"

unitResolveStampPath :: SourceUnit -> FilePath
unitResolveStampPath unit = unitStampBase unit <.> "resolve.json"

unitStampPath :: SourceUnit -> FilePath
unitStampPath unit = unitStampBase unit <.> "unit.json"

-- | The backend outputs of a unit, relative to the package.
unitBackendPaths :: ModuleCompileConfig -> SourceUnit -> [FilePath]
unitBackendPaths config unit = concatMap paths (sourceUnitSources unit)
  where
    paths source =
      let name = sourceName source
          output = moduleOutputPaths "" (compileTarget config) name
       in [outputObjectPath output]
            <> [outputFcPath output | compileKeepCore config]
            <> concat [[outputGrinPath output, outputCpsGrinPath output, outputGcGrinPath output] | compileKeepGrin config]
            <> [outputNativePath output | compileKeepNative config]

instanceFacts :: TcInterface -> TcInterface
instanceFacts interface =
  mempty
    { tcInterfaceInstanceMap = tcInterfaceInstanceMap interface,
      tcInterfaceDataFamilyInstanceMap = tcInterfaceDataFamilyInstanceMap interface,
      tcInterfaceTypeFamilyInstanceMap = tcInterfaceTypeFamilyInstanceMap interface
    }

interfaceInstanceProviders :: TcInterface -> Set.Set InstanceProvider
interfaceInstanceProviders interface =
  Set.fromList
    ( map (first PackageId . iiDictOrigin) (tcInterfaceInstances interface)
        <> map (tyConOrigin . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstances interface)
        <> map tfiiOrigin (tcInterfaceTypeFamilyInstances interface)
    )
  where
    first transform (left, right) = (transform left, right)
    tyConOrigin tyCon = (tyConPackageId tyCon, tyConModuleName tyCon)

selectInstanceProviders :: TcInterface -> Set.Set InstanceProvider -> TcInterface
selectInstanceProviders complete providers
  | Set.null providers = mempty
  | otherwise =
      addReferencedFacts
        complete
        mempty
          { tcInterfaceInstanceMap = Map.filter ((`Set.member` providers) . first PackageId . iiDictOrigin) (tcInterfaceInstanceMap complete),
            tcInterfaceDataFamilyInstanceMap = Map.filter ((`Set.member` providers) . tyConOrigin . dfiiRepresentationTyCon) (tcInterfaceDataFamilyInstanceMap complete),
            tcInterfaceTypeFamilyInstanceMap = Map.filter ((`Set.member` providers) . tfiiOrigin) (tcInterfaceTypeFamilyInstanceMap complete)
          }
  where
    first transform (left, right) = (transform left, right)
    tyConOrigin tyCon = (tyConPackageId tyCon, tyConModuleName tyCon)

writePackageInstanceArtifact :: (String -> IO ()) -> FilePath -> Map.Map Text Text -> Map.Map Text (Set.Set InstanceProvider) -> TcInterface -> IO Text
writePackageInstanceArtifact verbose storePath typeHashes providers interface = do
  let path = storePath </> "instances.cbor"
      hashes = sortOn fst [("type:" <> name, digest) | (name, digest) <- Map.toList typeHashes]
      bytes = encodeTypeArtifact (TypeArtifact "$package-instances" hashes (Map.map Set.toAscList providers) interface)
  createDirectoryIfMissing True storePath
  BL.writeFile path bytes
  verbose ("Write package instances: " <> path)
  pure (T.pack (stableHash [BL.toStrict bytes]))

wiredTypeModules :: [Text]
wiredTypeModules = ["GHC.CString", "GHC.Classes", "GHC.Prim", "GHC.Prim.Base", "GHC.Prim.Enum", "GHC.Prim.Num", "GHC.Prim.Real", "GHC.Prim.String", "GHC.Tuple", "GHC.Types"]

-- | Modules whose names generated code refers to, but whose order the
-- dependency graph must not fix: a derived @Read@ instance calls the reader
-- of the primitive package, and a module that derives @Read@ does not
-- import it. The primitive package itself compiles this module in its own
-- import order.
wiredDerivingModules :: [Text]
wiredDerivingModules = ["GHC.Prim.Read"]

-- | Every module whose type interface a compilation needs without an
-- import.
wiredInterfaceModules :: [Text]
wiredInterfaceModules = wiredTypeModules <> wiredDerivingModules

builtinFunctionScope :: Package -> ModuleExports -> [(Package, Module)] -> Scope
builtinFunctionScope currentPackage dependencyExports packageModules =
  foldr (unionScope . lookupBuiltin) emptyScope builtinFunctionModules
  where
    allExports = collectModuleExportsWithDeps dependencyExports packageModules `Map.union` dependencyExports
    lookupBuiltin name = lookupImportedModule currentPackage Nothing name allExports
    builtinFunctionModules = ["GHC.Classes", "GHC.Prim", "GHC.Prim.Base", "GHC.Prim.Enum", "GHC.Prim.Num", "GHC.Prim.Real", "GHC.Prim.String"]

measureTime :: IO a -> IO (a, Word64)
measureTime action = do
  start <- getMonotonicTimeNSec
  value <- action
  end <- getMonotonicTimeNSec
  pure (value, end - start)

withOtherTime :: Word64 -> Word64 -> BackendPhaseTimings -> BackendPhaseTimings
withOtherTime started ended timings =
  timings
    { backendOtherNs = extra
    }
  where
    accounted = backendDesugarNs timings + backendGrinNs timings + backendNativeNs timings
    elapsed = ended - started
    extra
      | elapsed > accounted = elapsed - accounted
      | otherwise = 0

renderBackendPhaseTotals :: BackendPhaseTimings -> String
renderBackendPhaseTotals timings =
  unlines
    [ "desugar total: " <> renderDuration (backendDesugarNs timings),
      "grin total: " <> renderDuration (backendGrinNs timings),
      "native total: " <> renderDuration (backendNativeNs timings),
      "other total: " <> renderDuration (backendOtherNs timings)
    ]

compileCheckedModules :: ModuleCompileConfig -> (String -> IO ()) -> PackageId -> TcInterface -> (Text -> ModuleOutputPaths) -> Map.Map Text DesugarConfig -> [Module] -> IO BackendPhaseTimings
compileCheckedModules config verbose primIdentity interface outputPaths desugarConfigs checkedModules = do
  (splitModules, desugarNs) <- measureTime $ do
    let kinds = primKinds primIdentity
        bindings = concatMap (tcModuleBindings kinds) checkedModules
        moduleNames = map (fromMaybe "Main" . moduleName) checkedModules
        -- A module the resolver did not report on keeps every name public.
        desugarConfig name =
          Map.findWithDefault (Fc.allPublicDesugarConfig kinds primIdentity) name desugarConfigs
        desugarResults =
          [ Fc.desugarModuleFc (desugarConfig name) bindings interface checked
          | (name, checked) <- zip moduleNames checkedModules
          ]
        desugarErrors =
          [ T.unpack name <> ": " <> err
          | (name, result) <- zip moduleNames desugarResults,
            err <- dsErrors result
          ]
    unless (all dsSuccess desugarResults) (ioError (userError ("FC generation failed: " <> unlines desugarErrors)))
    let fcModules = zipWith FcModule moduleNames (map dsProgram desugarResults)
    fcErrors <-
      fmap concat $
        forM fcModules $ \fcModule -> do
          when lint (verbose ("Lint FC: " <> T.unpack (fcModuleName fcModule)))
          let errors = [(fcModuleName fcModule, err) | err <- Fc.lintProgram (fcProgram fcModule)]
          when lint (void (evaluate (length errors)))
          pure errors
    let fcReport = ["    " <> T.unpack name <> ": " <> show err | (name, err) <- fcErrors]
    when lint $
      unless (null fcErrors) $
        ioError
          ( userError
              ( unlines
                  ( ["FC lint failed:"]
                      <> fcReport
                  )
              )
          )
    when keepCore (mapM_ writeFcModule fcModules)
    pure (spanEmptyModules fcModules)
  let (emptyFcModules, nonemptyFcModules) = splitModules
  (grinModules, grinNs) <- measureTime $ do
    grinModules <- mapM lowerGrinModule nonemptyFcModules
    when keepGrin (mapM_ writeGrinModule grinModules)
    pure grinModules
  (_, nativeNs) <- measureTime $ do
    mapM_ writeEmptyModule emptyFcModules
    nativeModules <- mapM (generateNativeModule target) grinModules
    mapM_ writeNativeSourceFile nativeModules
    mapM_ compileNativeSourceFile nativeModules
    unless keepNative (mapM_ removeNativeSourceFile nativeModules)
  pure
    BackendPhaseTimings
      { backendDesugarNs = desugarNs,
        backendGrinNs = grinNs,
        backendNativeNs = nativeNs,
        backendOtherNs = 0
      }
  where
    keepCore = compileKeepCore config
    keepGrin = compileKeepGrin config
    keepNative = compileKeepNative config
    lint = compileLint config
    target = compileTarget config
    spanEmptyModules = foldr split ([], [])
      where
        split fcModule (emptyModules, nonemptyModules)
          | null (Fc.programDecls (fcProgram fcModule)) = (fcModule : emptyModules, nonemptyModules)
          | otherwise = (emptyModules, fcModule : nonemptyModules)

    writeEmptyModule fcModule = do
      let name = fcModuleName fcModule
          paths = outputPaths name
      createDirectoryIfMissing True (takeDirectory (outputObjectPath paths))
      BS.writeFile (outputObjectPath paths) ""
      when (compileKeepGrin config) $ do
        writeFile (outputGrinPath paths) ""
        writeFile (outputCpsGrinPath paths) ""
        writeFile (outputGcGrinPath paths) ""
      when (compileKeepNative config) (writeFile (outputNativePath paths) "")
      verbose ("Write empty object: " <> T.unpack name)

    writeFcModule fcModule = do
      let name = fcModuleName fcModule
          path = outputFcPath (outputPaths name)
      writeFcFile path (fcProgram fcModule)
      verbose ("Write FC: " <> T.unpack name)

    writeFcFile path program = do
      let rendered = Fc.renderProgram program
          output = if "\n" `T.isSuffixOf` rendered then rendered else rendered <> "\n"
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path output

    lowerGrinModule fcModule = do
      verbose ("Lower GRIN: " <> T.unpack (fcModuleName fcModule))
      plainProgram <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram (fcProgram fcModule))
      when (compileLint config) $ do
        let plainErrors = Grin.lintProgram plainProgram
        unless (null plainErrors) (ioError (userError ("GRIN lint failed: " <> show plainErrors)))
      cpsProgram <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin plainProgram)
      let gcProgram = Grin.lowerGc cpsProgram
      when (compileLint config) $ do
        let gcErrors = Grin.lintGcProgram gcProgram
        unless (null gcErrors) (ioError (userError ("GC-GRIN lint failed: " <> show gcErrors)))
      pure
        GrinModule
          { grinModuleName = fcModuleName fcModule,
            plainGrinProgram = plainProgram,
            cpsGrinProgram = cpsProgram,
            gcGrinProgram = gcProgram
          }

    writeGrinModule grinModule = do
      let name = grinModuleName grinModule
          paths = outputPaths name
      writeGrinFile (outputGrinPath paths) (plainGrinProgram grinModule)
      verbose ("Write GRIN: " <> T.unpack name)
      writeGrinFile (outputCpsGrinPath paths) (Grin.cpsGrinProgram (cpsGrinProgram grinModule))
      verbose ("Write CPS-GRIN: " <> T.unpack name)
      writeGrinFile (outputGcGrinPath paths) (Grin.gcGrinProgram (gcGrinProgram grinModule))
      verbose ("Write GC-GRIN: " <> T.unpack name)

    writeGrinFile path program = do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (withFinalNewline (renderString (layoutPretty defaultLayoutOptions (Grin.prettyProgram program))))

    -- Every target goes through Lir. An object target keeps the Lir text
    -- as its source; a text target keeps the backend output that the
    -- compiler driver consumes.
    generateNativeModule selectedTarget grinModule = do
      let name = grinModuleName grinModule
          gcProgram = gcGrinProgram grinModule
      lirModule <- either (ioError . userError . ("Lir generation failed: " <>) . show) pure (Lir.lowerModule (lowerTargetFor selectedTarget) gcProgram)
      output <- either (ioError . userError . ("Lir backend failed: " <>)) pure (compileLir selectedTarget lirModule)
      pure $ case output of
        BackendObject object -> NativeModule name (if keepNative then Just (Lir.renderModule lirModule) else Nothing) (Just object)
        BackendSource source -> NativeModule name (Just source) Nothing

    writeNativeSourceFile nativeModule = do
      case nativeSource nativeModule of
        Nothing -> pure ()
        Just source -> do
          let name = nativeModuleName nativeModule
              path = outputNativePath (outputPaths name)
          createDirectoryIfMissing True (takeDirectory path)
          TIO.writeFile path source
          verbose ("Write native source: " <> T.unpack name)

    compileNativeSourceFile nativeModule = do
      let name = nativeModuleName nativeModule
          paths = outputPaths name
      case nativeObject nativeModule of
        Just object -> BL.writeFile (outputObjectPath paths) object
        Nothing -> do
          (compiler, compilerArguments) <- backendCompiler (compileTarget config)
          runTool compiler (compilerArguments <> ["-c", outputNativePath paths, "-o", outputObjectPath paths])
      verbose ("Write object: " <> T.unpack name)

    removeNativeSourceFile nativeModule =
      case nativeSource nativeModule of
        Nothing -> pure ()
        Just _ -> removeFile (outputNativePath (outputPaths (nativeModuleName nativeModule)))

moduleOutputPaths :: FilePath -> NativeTarget -> Text -> ModuleOutputPaths
moduleOutputPaths storePath target name =
  ModuleOutputPaths
    { outputFcPath = directory </> "core",
      outputGrinPath = directory </> "grin",
      outputCpsGrinPath = directory </> "cps.grin",
      outputGcGrinPath = directory </> "gc.grin",
      outputNativePath = objectPath <> nativeSourceExtension target,
      outputObjectPath = objectPath
    }
  where
    directory = storePath </> moduleNameDirectory name
    objectPath = directory </> T.unpack name <> ".o"

withFinalNewline :: String -> String
withFinalNewline rendered
  | "\n" `isSuffixOf` rendered = rendered
  | otherwise = rendered <> "\n"

cabalPlatformForTarget :: NativeTarget -> (OS, Arch)
cabalPlatformForTarget target =
  case target of
    AppleArm64 -> (OSX, AArch64)
    LinuxAmd64 -> (Linux, X86_64)
    Llvm -> (buildOS, buildArch)
    Wasm32Wasip3 -> (Wasi, Wasm32)

compilePackageCFiles :: NativeTarget -> (String -> IO ()) -> FilePath -> FilePath -> HackageCabal.CCompileInfo -> IO [FilePath]
compilePackageCFiles target verbose packageRoot storePath info
  | null (HackageCabal.cCompileSources info) = pure []
  | otherwise = do
      (compiler, targetArguments) <- backendCompiler target
      ffiHeader <- getDataFileName "compiler/native/runtime/include/HsFFI.h"
      sysrootIncludes <- wasmSysrootIncludeArguments target
      let ffiIncludeDir = takeDirectory ffiHeader
          includeArguments =
            sysrootIncludes
              <> ["-I" <> directory | directory <- HackageCabal.cCompileIncludeDirs info]
              <> ["-I" <> ffiIncludeDir]
          objectRoot = storePath </> "cbits"
      createDirectoryIfMissing True objectRoot
      forM (HackageCabal.cCompileSources info) $ \source -> do
        exists <- doesFileExist source
        unless exists (ioError (userError ("C source is absent: " <> source)))
        let object = objectRoot </> cObjectFileName (makeRelative packageRoot source)
        verbose ("Compile C source: " <> source)
        runTool
          compiler
          ( targetArguments
              <> HackageCabal.cCompileCcOptions info
              <> includeArguments
              <> ["-c", source, "-o", object]
          )
        pure object

wasmSysrootIncludeArguments :: NativeTarget -> IO [String]
wasmSysrootIncludeArguments target =
  case target of
    Wasm32Wasip3 -> do
      sysroot <- wasmSysroot
      pure ["-isystem" <> wasmSysrootInclude sysroot]
    _ -> pure []

cObjectFileName :: FilePath -> FilePath
cObjectFileName source =
  map replaceSeparator (dropExtension source) <.> "o"
  where
    replaceSeparator character =
      if character == '/' || character == '\\'
        then '_'
        else character

buildLibraryArchive :: NativeTarget -> (String -> IO ()) -> FilePath -> [FilePath] -> IO ()
buildLibraryArchive target verbose archive moduleObjects = do
  createDirectoryIfMissing True (takeDirectory archive)
  archiveExists <- doesFileExist archive
  when archiveExists (removeFile archive)
  archiver <- backendArchiver target
  nonemptyObjects <- filterM (fmap (> 0) . getFileSize) moduleObjects
  -- BSD ar refuses to create an archive with no members, and a package whose
  -- modules are all empty standins (aihc-internal) has none. Every archive
  -- format begins with the same global header, and an archive that stops
  -- there is a valid empty archive for ld64, GNU ld, lld and wasm-ld alike.
  if null nonemptyObjects
    then BS.writeFile archive emptyArchive
    else do
      environment <- getEnvironment
      -- Set archive timestamps only in the child process environment.
      let archiveEnvironment = ("ZERO_AR_DATE", "1") : filter ((/= "ZERO_AR_DATE") . fst) environment
      runToolWithEnvironment (Just archiveEnvironment) archiver (["rcs", archive] <> nonemptyObjects)
  verbose ("Write archive: " <> archive)
  where
    emptyArchive = BS8.pack "!<arch>\n"

runTool :: FilePath -> [String] -> IO ()
runTool = runToolWithEnvironment Nothing

runToolWithEnvironment :: Maybe [(String, String)] -> FilePath -> [String] -> IO ()
runToolWithEnvironment environment executable arguments = do
  (status, output, errors) <- readCreateProcessWithExitCode (proc executable arguments) {env = environment} ""
  case status of
    ExitSuccess -> pure ()
    ExitFailure code ->
      ioError
        ( userError
            ( executable
                <> " failed with exit code "
                <> show code
                <> ":\n"
                <> if null errors then output else errors
            )
        )

moduleTypeInterface :: ModuleExports -> Package -> TcInterface -> SourceModule -> TcInterface
moduleTypeInterface exports package interface source =
  addReferencedFacts
    interface
    interface
      { tcInterfaceTermMap = Map.filterWithKey (\key _ -> visibleTerm key) (tcInterfaceTermMap interface),
        tcInterfaceTyConMap = Map.filter visibleTyCon (tcInterfaceTyConMap interface),
        tcInterfaceDataTypeMap = Map.filterWithKey (\key _ -> visibleTypeIdentity key) (tcInterfaceDataTypeMap interface),
        tcInterfaceClassMap = Map.filter visibleClass (tcInterfaceClassMap interface),
        tcInterfaceInstanceMap = Map.filter visibleInstance (tcInterfaceInstanceMap interface),
        tcInterfaceDataFamilyInstanceMap = Map.filter visibleDataFamilyInstance (tcInterfaceDataFamilyInstanceMap interface),
        tcInterfaceTypeFamilyInstanceMap = Map.filter visibleTypeFamilyInstance (tcInterfaceTypeFamilyInstanceMap interface),
        tcInterfacePatSynMap = Map.filterWithKey (\key _ -> visibleTerm key) (tcInterfacePatSynMap interface),
        tcInterfaceForeignImportMap = Map.filterWithKey (\key _ -> visibleTerm key) (tcInterfaceForeignImportMap interface)
      }
  where
    name = fromMaybe "Main" (moduleName (sourceModuleAst source))
    scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
    termIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTerms scope)))
    typeIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTypes scope)))
    localIdentity identifier = (packageId package, name, identifier)
    localTyCon tyCon = tyConPackageId tyCon == packageId package && tyConModuleName tyCon == name
    visibleTerm (TcTermGlobal packageId' moduleName' identifier) =
      visibleTermIdentity (packageId', moduleName', identifier)
        || any (visibleTermIdentity . (packageId',moduleName',)) (patSynHelperBase identifier)
    visibleTerm (TcTermLocal {}) = False
    visibleTermIdentity identity@(_, _, identifier) =
      Map.member identifier (scopeTerms scope) || identity `Set.member` termIdentities || identity == localIdentity identifier
    -- The matcher and the builder of a visible pattern synonym are visible.
    patSynHelperBase identifier = mapMaybe (`T.stripPrefix` identifier) ["$m", "$b"]
    visibleTyCon info =
      let tyCon = tciTyCon info
          identity = (tyConPackageId tyCon, tyConModuleName tyCon, tciName info)
          (namespaceScope, namespaceIdentities) =
            case tyConNamespace tyCon of
              ResolutionNamespaceTerm -> (scopeTerms scope, termIdentities)
              ResolutionNamespaceType -> (scopeTypes scope, typeIdentities)
              ResolutionNamespaceModule -> (Map.empty, Set.empty)
       in Map.member (tciName info) namespaceScope || identity `Set.member` namespaceIdentities || identity == localIdentity (tciName info)
    visibleTypeIdentity (packageId', moduleName', namespace, identifier) =
      let identity = (packageId', moduleName', identifier)
       in namespace == ResolutionNamespaceType
            && (Map.member identifier (scopeTypes scope) || identity `Set.member` typeIdentities || identity == localIdentity identifier)
    visibleClass info =
      case ciOrigin info of
        Just (packageIdText, moduleName') ->
          let identity = (PackageId packageIdText, moduleName', ciName info)
           in Map.member (ciName info) (scopeTypes scope) || identity `Set.member` typeIdentities || identity == localIdentity (ciName info)
        Nothing -> False
    visibleInstance info = iiDictOrigin info == (packageIdText (packageId package), name)
    visibleDataFamilyInstance = localTyCon . dfiiRepresentationTyCon
    visibleTypeFamilyInstance info = any localTyCon (typeTyCons (tfiiLeft info) <> typeTyCons (tfiiRight info))
    resolvedIdentity resolved = case resolved of
      ResolvedTopLevel packageId' resolvedName -> Just (packageId', fromMaybe name (nameQualifier resolvedName), nameText resolvedName)
      _ -> Nothing

addReferencedFacts :: TcInterface -> TcInterface -> TcInterface
addReferencedFacts complete interface =
  interface
    { tcInterfaceTermMap = tcInterfaceTermMap interface <> Map.fromList callStackSupportTerms,
      tcInterfaceTyConMap = tcInterfaceTyConMap interface <> supportTyCons,
      tcInterfaceDataTypeMap = tcInterfaceDataTypeMap interface <> supportDataTypes,
      tcInterfaceClassMap = tcInterfaceClassMap interface <> supportClasses
    }
  where
    availableTyCons = tcInterfaceTyConMap complete
    availableDataTypes = tcInterfaceDataTypeMap complete
    availableClasses = tcInterfaceClassMap complete
    -- A use of a function with a HasCallStack constraint desugars to calls
    -- of the call-stack helpers, even when the module does not import them.
    callStackModules =
      Set.fromList
        [ (tyConPackageId tyCon, tyConModuleName tyCon)
        | tyCon <- concatMap (typeSchemeTyCons . snd) (tcInterfaceTerms interface),
          tyConName tyCon == "CallStack"
        ]
    callStackSupportTerms =
      [ (key, scheme)
      | (package', moduleName') <- Set.toList callStackModules,
        identifier <- ["pushCallStack", "emptyCallStack"],
        let key = TcTermGlobal package' moduleName' identifier,
        key `Map.notMember` tcInterfaceTermMap interface,
        Just scheme <- [Map.lookup key (tcInterfaceTermMap complete)]
      ]
    callStackSupportTyCons
      | Set.null callStackModules = []
      | otherwise =
          [ tyCon
          | info <- Map.elems availableTyCons,
            let tyCon = tciTyCon info,
            (tyConPackageId tyCon, tyConModuleName tyCon) `Set.member` callStackModules,
            tyConName tyCon `elem` ["SrcLoc", "CallStack"]
          ]
    referenced =
      Set.fromList
        ( concatMap (typeSchemeTyCons . snd) (tcInterfaceTerms interface)
            <> concatMap (typeSchemeTyCons . snd) callStackSupportTerms
            <> callStackSupportTyCons
            <> concatMap tyConInfoTyCons (tcInterfaceTyCons interface)
            <> concatMap dataTypeInfoTyCons (tcInterfaceDataTypes interface)
            <> concatMap classInfoTyCons (tcInterfaceClasses interface)
            <> concatMap instanceInfoTyCons (tcInterfaceInstances interface)
            <> concatMap dataFamilyInstanceInfoTyCons (tcInterfaceDataFamilyInstances interface)
            <> concatMap typeFamilyInstanceInfoTyCons (tcInterfaceTypeFamilyInstances interface)
            <> concatMap (foreignImportInfoTyCons . snd) (tcInterfaceForeignImports interface)
        )
    reachable = closeTyCons Set.empty referenced
    reachableKeys = Set.map tyConKey reachable
    supportTyCons = Map.restrictKeys availableTyCons (reachableKeys `Set.difference` Map.keysSet (tcInterfaceTyConMap interface))
    supportDataTypes = Map.restrictKeys availableDataTypes (reachableKeys `Set.difference` Map.keysSet (tcInterfaceDataTypeMap interface))
    supportClasses = Map.restrictKeys availableClasses (reachableKeys `Set.difference` Map.keysSet (tcInterfaceClassMap interface))
    closeTyCons found pending
      | Set.null pending = found
      | otherwise =
          let (tyCon, pending') = Set.deleteFindMin pending
              key = tyConKey tyCon
              dependencies =
                Set.fromList
                  ( maybe [] tyConInfoTyCons (Map.lookup key availableTyCons)
                      <> maybe [] dataTypeInfoTyCons (Map.lookup key availableDataTypes)
                      <> maybe [] classInfoTyCons (Map.lookup key availableClasses)
                  )
              found' = Set.insert tyCon found
           in closeTyCons found' (pending' <> (dependencies `Set.difference` found'))

-- | The type constructors that a foreign call marshals through.
foreignImportInfoTyCons :: TcForeignImportInfo -> [TyCon]
foreignImportInfoTyCons info =
  case info of
    TcForeignPrimImport -> []
    TcForeignCCallImport _ plan ->
      concatMap marshalTyCons (tcForeignArguments plan <> [tcForeignResult plan])
  where
    marshalTyCons marshal = typeTyCons (tcForeignSourceType marshal) <> typeTyCons (tcForeignPrimitiveType marshal)

tyConInfoTyCons :: TyConInfo -> [TyCon]
tyConInfoTyCons info =
  typeSchemeTyCons (tciKindScheme info)
    <> maybe [] (maybe [] typeTyCons . tsiBody) (tciTypeSynonym info)

dataTypeInfoTyCons :: DataTypeInfo -> [TyCon]
dataTypeInfoTyCons info =
  dtiTyCon info
    : typeTyCons (dtiResultKind info)
      <> concatMap dataConInfoTyCons (dtiConstructors info)

dataConInfoTyCons :: DataConInfo -> [TyCon]
dataConInfoTyCons info =
  concatMap predTyCons (dciTheta info)
    <> concatMap (typeTyCons . dcfiType) (dciFields info)
    <> typeTyCons (dciResTy info)

classInfoTyCons :: ClassInfo -> [TyCon]
classInfoTyCons info =
  ciTyCon info
    : concatMap (typeTyCons . TcTyVar) (ciKindTyVars info)
      <> concatMap typeTyCons (ciSuperClassTypes info)
      <> concatMap (typeSchemeTyCons . snd) (ciMethods info)
      <> concatMap (typeSchemeTyCons . snd) (ciDefaultSignatures info)
      <> map atiTyCon (ciAssociatedTypes info)
      <> concatMap typeFamilyInstanceInfoTyCons (mapMaybe atiDefault (ciAssociatedTypes info))

instanceInfoTyCons :: InstanceInfo -> [TyCon]
instanceInfoTyCons info =
  typeTyCons (iiDictType info)
    <> concatMap predTyCons (iiContext info)
    <> concatMap typeTyCons (iiHead info)

dataFamilyInstanceInfoTyCons :: DataFamilyInstanceInfo -> [TyCon]
dataFamilyInstanceInfoTyCons info =
  dfiiRepresentationTyCon info : typeTyCons (dfiiFamilyType info)

typeFamilyInstanceInfoTyCons :: TypeFamilyInstanceInfo -> [TyCon]
typeFamilyInstanceInfoTyCons info = typeTyCons (tfiiLeft info) <> typeTyCons (tfiiRight info)

typeSchemeTyCons :: TypeScheme -> [TyCon]
typeSchemeTyCons (ForAll _ predicates body) = concatMap predTyCons predicates <> typeTyCons body

predTyCons :: Pred -> [TyCon]
predTyCons predicate = case predicate of
  ClassPred tyCon arguments -> tyCon : concatMap typeTyCons arguments
  EqPred left right -> typeTyCons left <> typeTyCons right
  IParamPred _ payload -> typeTyCons payload
  QuantifiedPred variables antecedents consequent ->
    concatMap (typeTyCons . tvKind) variables <> concatMap predTyCons antecedents <> predTyCons consequent

typeTyCons :: TcType -> [TyCon]
typeTyCons ty = case ty of
  TcTyVar {} -> []
  TcMetaTv {} -> []
  TcArrowTy -> []
  TcTyCon tyCon arguments -> tyCon : concatMap typeTyCons arguments
  TcFunTy argument result -> typeTyCons argument <> typeTyCons result
  TcForAllTy _ body -> typeTyCons body
  TcQualTy predicates body -> concatMap predTyCons predicates <> typeTyCons body
  TcAppTy function argument -> typeTyCons function <> typeTyCons argument

writeTypeArtifact :: (String -> IO ()) -> [(Text, Text)] -> (SourceModule -> FilePath) -> SourceModule -> TcInterface -> IO (Text, Text)
writeTypeArtifact verbose hashes artifactPath source interface = do
  let path = artifactPath source
      name = fromMaybe "Main" (moduleName (sourceModuleAst source))
      (artifactBytes, interfaceBytes) = encodeTypeArtifactParts (TypeArtifact name hashes Map.empty interface)
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path artifactBytes
  verbose ("Write type interface: " <> T.unpack name)
  pure (name, T.pack (stableHash [BL.toStrict interfaceBytes]))

moduleDirectory :: Module -> FilePath
moduleDirectory = moduleNameDirectory . fromMaybe "Main" . moduleName

moduleNameDirectory :: Text -> FilePath
moduleNameDirectory = foldl' (</>) "" . map T.unpack . T.splitOn "."

-- | Write the resolve artifact of a module and return the digest of the
-- scope inside it, taken from the bytes as written.
writeArtifact :: (String -> IO ()) -> [(Text, Text)] -> ModuleExports -> Package -> FilePath -> SourceModule -> IO (Text, Text)
writeArtifact verbose hashes exports package path source = do
  createDirectoryIfMissing True (takeDirectory path)
  let name = fromMaybe "Main" (moduleName (sourceModuleAst source))
      scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
      (artifactBytes, scopeBytes) = encodeResolveArtifactParts (ResolveArtifact name hashes scope)
  BL.writeFile path artifactBytes
  verbose ("Write resolve context: " <> T.unpack name)
  pure (name, T.pack (stableHash [BL.toStrict scopeBytes]))

stableHash :: [BS.ByteString] -> String
stableHash = hashChunks

packageArtifactFormatVersion :: Text
packageArtifactFormatVersion = "aihc-artifacts-18"
