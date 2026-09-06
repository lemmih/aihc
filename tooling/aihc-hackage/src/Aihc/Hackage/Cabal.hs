{-# LANGUAGE PatternSynonyms #-}

-- | Cabal-file parsing utilities: condition evaluation, component file discovery.
module Aihc.Hackage.Cabal
  ( -- * File info
    FileInfo (..),
    CCompileInfo (..),

    -- * Component file discovery
    collectComponentFiles,
    collectLibraryCCompileInfo,
    collectLibraryCCompileInfoFor,
    collectLibraryExposedModules,
    collectLibraryFiles,
    collectLibraryFilesFor,

    -- * Condition evaluation
    conditionEvaluator,
    conditionEvaluatorFor,
    collectCondTreeData,
    collectMergedBuildInfo,

    -- * Build tool dependency extraction
    buildToolDependencyNames,
    packageUsesCustomPreprocessor,

    -- * Extension / language extraction
    extractExtensions,
    extractLanguage,
    extractDependencies,
    packageDefaultsToHaskell98,
  )
where

import Aihc.Hackage.Cpp (emulatedGhcVersion)
import Aihc.Hackage.Util (existingPaths, moduleFilesForBuildInfo, sourceDirs)
import Data.List (isPrefixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Compat.Graph qualified as Graph
import Distribution.Compiler (CompilerFlavor (..), CompilerId (..))
import Distribution.Compiler qualified as Compiler
import Distribution.ModuleName qualified as ModuleName
import Distribution.Package (packageName, unPackageName)
import Distribution.PackageDescription
  ( BuildInfo,
    Executable,
    FlagName,
    Library,
    PackageDescription,
    autogenModules,
    benchmarkBuildInfo,
    buildInfo,
    buildToolDepends,
    buildTools,
    buildable,
    cSources,
    ccOptions,
    condBenchmarks,
    condExecutables,
    condForeignLibs,
    condLibrary,
    condSubLibraries,
    condTestSuites,
    cppOptions,
    defaultExtensions,
    defaultLanguage,
    exeModules,
    exposedModules,
    flagDefault,
    flagName,
    includeDirs,
    libBuildInfo,
    modulePath,
    oldExtensions,
    options,
    otherModules,
    package,
    packageDescription,
    testBuildInfo,
  )
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Build.PathsModule (generatePathsModule)
import Distribution.Simple.BuildPaths (autogenPathsModuleName)
import Distribution.Simple.Compiler
  ( AbiTag (..),
    Compiler (..),
    DebugInfoLevel (..),
    OptimisationLevel (..),
    PackageDBX (..),
    ProfDetailLevel (..),
  )
import Distribution.Simple.InstallDirs (defaultInstallDirs)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Simple.Setup (defaultConfigFlags)
import Distribution.System (Arch, OS, buildArch, buildOS, buildPlatform)
import Distribution.Types.BuildInfo (targetBuildDepends)
import Distribution.Types.ComponentId (mkComponentId)
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo (..))
import Distribution.Types.ComponentName (ComponentName (..), componentNameString, pattern CBenchName, pattern CExeName, pattern CFLibName, pattern CTestName)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Types.CondTree
  ( CondBranch (CondBranch),
    CondTree (condTreeComponents, condTreeData),
  )
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.ForeignLib (foreignLibBuildInfo)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, genPackageFlags)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Types.MungedPackageName (MungedPackageName (..))
import Distribution.Types.UnitId (mkUnitId)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version (mkVersion, withinRange)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))

-- | Information about a Haskell source file discovered via a @.cabal@ file.
data FileInfo = FileInfo
  { fileInfoPath :: FilePath,
    -- | Extension names as strings (e.g. @\"OverloadedStrings\"@, @\"NoImplicitPrelude\"@).
    fileInfoExtensions :: [String],
    -- | CPP options from the @cpp-options@ field.
    fileInfoCppOptions :: [String],
    -- | Include search directories from the @include-dirs@ field.
    fileInfoIncludeDirs :: [FilePath],
    -- | Default language from the @default-language@ field.
    fileInfoLanguage :: Maybe String,
    -- | Build dependency package names.
    fileInfoDependencies :: [Text]
  }
  deriving (Show)

-- | C compile inputs from the active library @c-sources@, @include-dirs@, and
-- @cc-options@ fields.
data CCompileInfo = CCompileInfo
  { cCompileSources :: [FilePath],
    cCompileIncludeDirs :: [FilePath],
    cCompileCcOptions :: [String]
  }
  deriving (Eq, Show)

-- | Collect all source files from a parsed @GenericPackageDescription@.
--
-- Returns deduplicated 'FileInfo' records for every library and executable
-- component whose @buildable@ flag is true.
collectComponentFiles :: GenericPackageDescription -> FilePath -> IO [FileInfo]
collectComponentFiles gpd packageRoot = do
  libraryFiles <- collectLibraryFiles gpd packageRoot
  executableFiles <- collectExecutableFiles gpd packageRoot
  pure (dedupeFiles (libraryFiles <> executableFiles))

-- | Collect source files from buildable library components only.
collectLibraryFiles :: GenericPackageDescription -> FilePath -> IO [FileInfo]
collectLibraryFiles = collectLibraryFilesFor buildOS buildArch

-- | Collect source files from buildable library components for one platform.
collectLibraryFilesFor :: OS -> Arch -> GenericPackageDescription -> FilePath -> IO [FileInfo]
collectLibraryFilesFor os arch gpd packageRoot = do
  let evalCond = conditionEvaluatorFor gpd os arch
      pkgDescr = packageDescription gpd
      libraryTrees = maybe [] (pure . (LMainLibName,)) (condLibrary gpd) <> map (first LSubLibName) (condSubLibraries gpd)

  libraryFiles <- fmap concat (mapM (uncurry (libraryFilesFor pkgDescr evalCond packageRoot)) libraryTrees)
  pure (dedupeFiles libraryFiles)

-- | Collect C compile inputs from buildable library components for the host.
collectLibraryCCompileInfo :: GenericPackageDescription -> FilePath -> CCompileInfo
collectLibraryCCompileInfo = collectLibraryCCompileInfoFor buildOS buildArch

-- | Collect C compile inputs from buildable library components for one platform.
collectLibraryCCompileInfoFor :: OS -> Arch -> GenericPackageDescription -> FilePath -> CCompileInfo
collectLibraryCCompileInfoFor os arch gpd packageRoot =
  mergeCCompileInfo
    [ cCompileInfoFromBuild packageRoot build
    | tree <- libraryTrees,
      let build = collectMergedBuildInfo evalCond libBuildInfo tree,
      buildable build
    ]
  where
    evalCond = conditionEvaluatorFor gpd os arch
    libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)

cCompileInfoFromBuild :: FilePath -> BuildInfo -> CCompileInfo
cCompileInfoFromBuild packageRoot build =
  CCompileInfo
    { cCompileSources = extractCSources packageRoot build,
      cCompileIncludeDirs = extractIncludeDirs packageRoot build,
      cCompileCcOptions = ccOptions build
    }

mergeCCompileInfo :: [CCompileInfo] -> CCompileInfo
mergeCCompileInfo items =
  CCompileInfo
    { cCompileSources = nub (concatMap cCompileSources items),
      cCompileIncludeDirs = nub (concatMap cCompileIncludeDirs items),
      cCompileCcOptions = concatMap cCompileCcOptions items
    }

-- | Collect the public module interface selected by active Cabal conditions.
-- Private @other-modules@ are intentionally absent even though
-- 'collectLibraryFiles' includes their source files for compilation.
collectLibraryExposedModules :: GenericPackageDescription -> [Text]
collectLibraryExposedModules gpd =
  nub
    [ T.pack (prettyShow moduleName)
    | tree <- libraryTrees,
      let build = collectMergedBuildInfo evalCond libBuildInfo tree,
      buildable build,
      library <- collectCondTreeData evalCond tree,
      moduleName <- exposedModules library
    ]
  where
    evalCond = conditionEvaluator gpd
    libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)

collectExecutableFiles :: GenericPackageDescription -> FilePath -> IO [FileInfo]
collectExecutableFiles gpd packageRoot = do
  let evalCond = conditionEvaluator gpd
      pkgDescr = packageDescription gpd
      executableTrees = condExecutables gpd

  executableFiles <- fmap concat (mapM (uncurry (executableFilesFor pkgDescr evalCond packageRoot)) executableTrees)
  pure (dedupeFiles executableFiles)

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

dedupeFiles :: [FileInfo] -> [FileInfo]
dedupeFiles [] = []
dedupeFiles (f : fs) = f : dedupeFiles (filter (\x -> fileInfoPath x /= fileInfoPath f) fs)

libraryFilesFor :: PackageDescription -> (Condition ConfVar -> Bool) -> FilePath -> LibraryName -> CondTree ConfVar c Library -> IO [FileInfo]
libraryFilesFor pkgDescr evalCond packageRoot libName tree = do
  let libraries = collectCondTreeData evalCond tree
      build = collectMergedBuildInfo evalCond libBuildInfo tree
      moduleNames = nub (concatMap exposedModules libraries <> otherModules build <> autogenModules build)
      exts = extractExtensions build
      cppOpts = cppOptions build
      includeSearchDirs = extractIncludeDirs packageRoot build
      lang = extractLanguage build
      deps = extractDependencies build
  if not (buildable build)
    then pure []
    else do
      paths <- moduleFilesForBuildInfo packageRoot build moduleNames
      generatedPaths <- generatedPathsFiles packageRoot pkgDescr (libraryComponentName libName) moduleNames
      pure $
        [FileInfo path exts cppOpts includeSearchDirs lang deps | path <- paths]
          <> [generatedPathsFileInfo path | path <- generatedPaths]

executableFilesFor :: PackageDescription -> (Condition ConfVar -> Bool) -> FilePath -> UnqualComponentName -> CondTree ConfVar c Executable -> IO [FileInfo]
executableFilesFor pkgDescr evalCond packageRoot exeName tree = do
  let executable = condTreeData tree
      build = collectMergedBuildInfo evalCond buildInfo tree
      moduleNames = otherModules build <> exeModules executable <> autogenModules build
      mainPath = getSymbolicPath (modulePath executable)
      exts = extractExtensions build
      cppOpts = cppOptions build
      includeSearchDirs = extractIncludeDirs packageRoot build
      lang = extractLanguage build
      deps = extractDependencies build
  if not (buildable build)
    then pure []
    else do
      moduleFiles <- moduleFilesForBuildInfo packageRoot build moduleNames
      mainFiles <- existingPaths [dir </> mainPath | dir <- sourceDirs packageRoot build]
      generatedPaths <- generatedPathsFiles packageRoot pkgDescr (CExeName exeName) moduleNames
      pure $
        [FileInfo path exts cppOpts includeSearchDirs lang deps | path <- moduleFiles <> mainFiles]
          <> [generatedPathsFileInfo path | path <- generatedPaths]

libraryComponentName :: LibraryName -> ComponentName
libraryComponentName = CLibName

-- | Collect package and executable names referenced by active build-tool fields.
buildToolDependencyNames :: GenericPackageDescription -> [Text]
buildToolDependencyNames gpd =
  nub $
    concatMap buildInfoToolNames (activeComponentBuildInfos gpd)

-- | Return whether any active source component uses Cabal's Haskell98 default.
--
-- Cabal treats a missing @default-language@ as Haskell98. AIHC does not support
-- Haskell98 as a package language target, so progress tooling filters these
-- packages before parsing their files.
packageDefaultsToHaskell98 :: GenericPackageDescription -> Bool
packageDefaultsToHaskell98 =
  any buildInfoDefaultsToHaskell98 . activeSourceComponentBuildInfos

buildInfoDefaultsToHaskell98 :: BuildInfo -> Bool
buildInfoDefaultsToHaskell98 bi =
  case defaultLanguage bi of
    Nothing -> True
    Just lang -> prettyShow lang == "Haskell98"

activeSourceComponentBuildInfos :: GenericPackageDescription -> [BuildInfo]
activeSourceComponentBuildInfos gpd =
  let evalCond = conditionEvaluator gpd
      merged = collectMergedBuildInfo evalCond
   in maybe [] (pure . merged libBuildInfo) (condLibrary gpd)
        <> map (merged libBuildInfo . snd) (condSubLibraries gpd)
        <> map (merged buildInfo . snd) (condExecutables gpd)

-- | Return whether any active component requests a custom GHC preprocessor.
packageUsesCustomPreprocessor :: GenericPackageDescription -> Bool
packageUsesCustomPreprocessor gpd =
  any buildInfoUsesCustomPreprocessor (activeComponentBuildInfos gpd)

activeComponentBuildInfos :: GenericPackageDescription -> [BuildInfo]
activeComponentBuildInfos gpd =
  let evalCond = conditionEvaluator gpd
      merged = collectMergedBuildInfo evalCond
   in maybe [] (pure . merged libBuildInfo) (condLibrary gpd)
        <> map (merged libBuildInfo . snd) (condSubLibraries gpd)
        <> map (merged foreignLibBuildInfo . snd) (condForeignLibs gpd)
        <> map (merged buildInfo . snd) (condExecutables gpd)
        <> map (merged testBuildInfo . snd) (condTestSuites gpd)
        <> map (merged benchmarkBuildInfo . snd) (condBenchmarks gpd)

buildInfoToolNames :: BuildInfo -> [Text]
buildInfoToolNames bi =
  concatMap exeDependencyNames (buildToolDepends bi)
    <> concatMap legacyExeDependencyNames (buildTools bi)

exeDependencyNames :: ExeDependency -> [Text]
exeDependencyNames (ExeDependency pkgName exeName _) =
  [ T.pack (unPackageName pkgName),
    T.pack (unUnqualComponentName exeName)
  ]

legacyExeDependencyNames :: LegacyExeDependency -> [Text]
legacyExeDependencyNames (LegacyExeDependency toolName _) =
  [T.pack toolName]

buildInfoUsesCustomPreprocessor :: BuildInfo -> Bool
buildInfoUsesCustomPreprocessor bi =
  ghcOptionsUseCustomPreprocessor (concat ghcOptions)
  where
    ghcOptions =
      [ opts
      | (GHC, opts) <- Compiler.perCompilerFlavorToList (options bi)
      ]

ghcOptionsUseCustomPreprocessor :: [String] -> Bool
ghcOptionsUseCustomPreprocessor opts =
  "-F" `elem` opts && any isPgmFOption opts
  where
    isPgmFOption opt = "-pgmF" `isPrefixOf` opt

generatedPathsFileInfo :: FilePath -> FileInfo
generatedPathsFileInfo path =
  FileInfo
    { fileInfoPath = path,
      fileInfoExtensions = [],
      fileInfoCppOptions = [],
      fileInfoIncludeDirs = [],
      fileInfoLanguage = Nothing,
      fileInfoDependencies = [T.pack "base"]
    }

generatedPathsFiles :: FilePath -> PackageDescription -> ComponentName -> [ModuleName.ModuleName] -> IO [FilePath]
generatedPathsFiles packageRoot pkgDescr componentName moduleNames
  | autogenPathsModuleName pkgDescr `notElem` moduleNames = pure []
  | otherwise = do
      let path = generatedPathsModulePath packageRoot pkgDescr
      createDirectoryIfMissing True (takeDirectory path)
      lbi <- syntheticLocalBuildInfo pkgDescr
      let clbi = syntheticComponentLocalBuildInfo pkgDescr componentName
      writeFile path (generatePathsModule pkgDescr lbi clbi)
      pure [path]

generatedPathsModulePath :: FilePath -> PackageDescription -> FilePath
generatedPathsModulePath packageRoot pkgDescr =
  packageRoot </> ".aihc-autogen" </> ModuleName.toFilePath (autogenPathsModuleName pkgDescr) <.> "hs"

syntheticLocalBuildInfo :: PackageDescription -> IO LocalBuildInfo
syntheticLocalBuildInfo pkgDescr = do
  dirs <- defaultInstallDirs GHC False False
  let comp =
        Compiler
          (CompilerId GHC (mkVersion emulatedGhcVersion))
          NoAbiTag
          [CompilerId GHC (mkVersion emulatedGhcVersion)]
          []
          []
          Map.empty
  pure $
    LocalBuildInfo
      { configFlags = defaultConfigFlags emptyProgramDb,
        flagAssignment = mempty,
        componentEnabledSpec = ComponentRequestedSpec False False,
        extraConfigArgs = [],
        installDirTemplates = dirs,
        compiler = comp,
        hostPlatform = buildPlatform,
        pkgDescrFile = Nothing,
        componentGraph = Graph.empty,
        componentNameMap = Map.empty,
        promisedPkgs = Map.empty,
        installedPkgs = mempty,
        localPkgDescr = pkgDescr,
        withPrograms = emptyProgramDb,
        withPackageDB = [GlobalPackageDB],
        withVanillaLib = True,
        withProfLib = False,
        withProfLibShared = False,
        withSharedLib = False,
        withStaticLib = False,
        withDynExe = False,
        withFullyStaticExe = False,
        withProfExe = False,
        withProfLibDetail = ProfDetailNone,
        withProfExeDetail = ProfDetailNone,
        withOptimization = NoOptimisation,
        withDebugInfo = NoDebugInfo,
        withGHCiLib = False,
        splitSections = False,
        splitObjs = False,
        stripExes = False,
        stripLibs = False,
        exeCoverage = False,
        libCoverage = False,
        extraCoverageFor = [],
        relocatable = False
      }

syntheticComponentLocalBuildInfo :: PackageDescription -> ComponentName -> ComponentLocalBuildInfo
syntheticComponentLocalBuildInfo pkgDescr componentName =
  case componentName of
    CLibName libName ->
      LibComponentLocalBuildInfo
        { componentLocalName = componentName,
          componentComponentId = componentId,
          componentUnitId = unitId,
          componentIsIndefinite_ = False,
          componentInstantiatedWith = [],
          componentPackageDeps = [],
          componentIncludes = [],
          componentExeDeps = [],
          componentInternalDeps = [],
          componentCompatPackageKey = unitIdText,
          componentCompatPackageName = MungedPackageName (packageName pkgDescr) libName,
          componentExposedModules = [],
          componentIsPublic = True
        }
    CExeName _ ->
      ExeComponentLocalBuildInfo
        { componentLocalName = componentName,
          componentComponentId = componentId,
          componentUnitId = unitId,
          componentPackageDeps = [],
          componentIncludes = [],
          componentExeDeps = [],
          componentInternalDeps = []
        }
    CTestName _ ->
      TestComponentLocalBuildInfo
        { componentLocalName = componentName,
          componentComponentId = componentId,
          componentUnitId = unitId,
          componentPackageDeps = [],
          componentIncludes = [],
          componentExeDeps = [],
          componentInternalDeps = []
        }
    CBenchName _ ->
      BenchComponentLocalBuildInfo
        { componentLocalName = componentName,
          componentComponentId = componentId,
          componentUnitId = unitId,
          componentPackageDeps = [],
          componentIncludes = [],
          componentExeDeps = [],
          componentInternalDeps = []
        }
    CFLibName _ ->
      FLibComponentLocalBuildInfo
        { componentLocalName = componentName,
          componentComponentId = componentId,
          componentUnitId = unitId,
          componentPackageDeps = [],
          componentIncludes = [],
          componentExeDeps = [],
          componentInternalDeps = []
        }
  where
    unitIdText = prettyShow (package pkgDescr) <> componentSuffix componentName
    componentId = mkComponentId unitIdText
    unitId = mkUnitId unitIdText

componentSuffix :: ComponentName -> String
componentSuffix componentName =
  case componentName of
    CLibName LMainLibName -> ""
    CLibName (LSubLibName name) -> "-lib-" <> prettyShow name
    CNotLibName _ -> "-exe-" <> maybe "unnamed" prettyShow (componentNameString componentName)

-- | Evaluate cabal conditions using the emulated compiler and default flag values.
conditionEvaluator :: GenericPackageDescription -> Condition ConfVar -> Bool
conditionEvaluator gpd = conditionEvaluatorFor gpd buildOS buildArch

-- | Evaluate cabal conditions for one OS and architecture.
conditionEvaluatorFor :: GenericPackageDescription -> OS -> Arch -> Condition ConfVar -> Bool
conditionEvaluatorFor gpd os arch = eval
  where
    defaultFlags :: Map.Map FlagName Bool
    defaultFlags =
      Map.fromList [(flagName flag, flagDefault flag) | flag <- genPackageFlags gpd]

    -- aihc presents itself as GHC so that packages take the branch matching
    -- the CPP macros in "Aihc.Hackage.Cpp"; the host compiler is irrelevant.
    compilerVer = mkVersion emulatedGhcVersion

    eval (Var confVar) =
      case confVar of
        OS wanted -> wanted == os
        Arch wanted -> wanted == arch
        PackageFlag flag -> Map.findWithDefault False flag defaultFlags
        Impl flavor range -> flavor == GHC && withinRange compilerVer range
    eval (Lit b) = b
    eval (CNot c) = not (eval c)
    eval (COr a b) = eval a || eval b
    eval (CAnd a b) = eval a && eval b

-- | Collect all data nodes from a 'CondTree', evaluating conditions.
collectCondTreeData :: (Condition v -> Bool) -> CondTree v c a -> [a]
collectCondTreeData evalCond tree =
  condTreeData tree : concatMap collectBranch (condTreeComponents tree)
  where
    collectBranch (CondBranch cond thenTree elseTree) =
      if evalCond cond
        then collectCondTreeData evalCond thenTree
        else maybe [] (collectCondTreeData evalCond) elseTree

-- | Merge 'BuildInfo' from all active branches of a 'CondTree'.
collectMergedBuildInfo :: (Monoid b) => (Condition v -> Bool) -> (a -> b) -> CondTree v c a -> b
collectMergedBuildInfo evalCond toBuildInfo =
  mconcat . map toBuildInfo . collectCondTreeData evalCond

-- | Extract extension names as strings from a 'BuildInfo'.
extractExtensions :: BuildInfo -> [String]
extractExtensions bi = nub (map prettyShow (defaultExtensions bi <> oldExtensions bi))

-- | Extract the default language as a string from a 'BuildInfo'.
extractLanguage :: BuildInfo -> Maybe String
extractLanguage bi =
  case defaultLanguage bi of
    Just lang -> Just (prettyShow lang)
    Nothing -> Nothing

-- | Extract include search directories from a 'BuildInfo'.
extractIncludeDirs :: FilePath -> BuildInfo -> [FilePath]
extractIncludeDirs packageRoot bi =
  nub [packageRoot </> getSymbolicPath dir | dir <- includeDirs bi]

-- | Extract C source paths from a 'BuildInfo'.
extractCSources :: FilePath -> BuildInfo -> [FilePath]
extractCSources packageRoot bi =
  nub [packageRoot </> getSymbolicPath path | path <- cSources bi]

-- | Extract build dependency package names from a 'BuildInfo'.
extractDependencies :: BuildInfo -> [Text]
extractDependencies bi =
  map (T.pack . unPackageName . depPkgName) (targetBuildDepends bi)
