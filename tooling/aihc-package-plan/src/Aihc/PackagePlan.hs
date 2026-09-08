-- |
-- Module      : Aihc.PackagePlan
-- Description : Dependency resolution shared by the aihc tools
--
-- Resolves a package and its transitive library dependencies to source
-- directories. Core libraries (@base@, @ghc-prim@, @ghc-internal@,
-- @template-haskell@) are redirected to the standins under @core-libs@; every
-- other package is resolved through a caller-supplied 'DependencyResolver'.
--
-- The compiler and the documentation tool share this module so that both see
-- the same dependency graph for a package.
module Aihc.PackagePlan
  ( DependencyResolver (..),
    PackagePlan (..),
    buildPackagePlanWithResolver,
    DependencyVersions,
    dependencyVersionsFromManifests,
    coreProviders,
    coreProviderSourcePath,
    CoreProvider (..),
    localDependencyResolverWithFallback,
    packageSpecFromSource,
  )
where

import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (DependencyVersions)
import Aihc.Hackage.Release (BootLibrary (..), emulatedGhc, lookupBootLibraryByStandin, showVersionBranch)
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Hackage.Util qualified as HackageUtil
import Data.ByteString qualified as BS
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (buildable, condLibrary, condSubLibraries, libBuildInfo, package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath (normalise, takeDirectory, (</>))

data PackagePlan = PackagePlan
  { planSourcePath :: !FilePath,
    planDependencyPlans :: ![PackagePlan]
  }
  deriving (Eq, Show)

data DependencyResolver = DependencyResolver
  { resolverResolveVersion :: String -> IO String,
    resolverSourcePath :: PackageSpec -> IO FilePath
  }

data CoreProvider = CoreProvider
  { coreProviderName :: !String,
    coreProviderVersion :: !String,
    coreProviderSourceRel :: !FilePath
  }

localDependencyResolverWithFallback :: DependencyResolver -> FilePath -> DependencyResolver
localDependencyResolverWithFallback fallback rootSource =
  DependencyResolver
    { resolverResolveVersion = \name -> do
        local <- localPackage name
        maybe (resolverResolveVersion fallback name) (pure . pkgVersion . fst) local,
      resolverSourcePath = \spec -> do
        local <- localPackage (pkgName spec)
        case local of
          Just (localSpec, path)
            | pkgVersion localSpec == pkgVersion spec -> pure path
          _ -> resolverSourcePath fallback spec
    }
  where
    workspace = takeDirectory (normalise rootSource)
    localPackage name = do
      rootSpec <- packageSpecFromSource rootSource
      if pkgName rootSpec == name
        then pure (Just (rootSpec, rootSource))
        else do
          let candidate = workspace </> name
          exists <- doesDirectoryExist candidate
          if exists
            then do
              spec <- packageSpecFromSource candidate
              pure (Just (spec, candidate))
            else pure Nothing

packageSpecFromSource :: FilePath -> IO PackageSpec
packageSpecFromSource sourcePath = do
  cabalFiles <- HackageUtil.findCabalFiles sourcePath
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
      files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
  cabalBytes <- BS.readFile cabalFile
  gpd <-
    case runParseResult (parseGenericPackageDescription cabalBytes) of
      (_, Right parsed) -> pure parsed
      (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errs))
  let packageId = package (packageDescription gpd)
  pure
    PackageSpec
      { pkgName = CabalPackage.unPackageName (CabalPackage.packageName packageId),
        pkgVersion = prettyShow (CabalPackage.packageVersion packageId)
      }

buildPackagePlanWithResolver :: DependencyResolver -> PackageSpec -> IO PackagePlan
buildPackagePlanWithResolver resolver = buildPackagePlanRecursive resolver []

buildPackagePlanRecursive :: DependencyResolver -> [PackageSpec] -> PackageSpec -> IO PackagePlan
buildPackagePlanRecursive resolver stack rawSpec
  | packageSpecIdentity spec `elem` map packageSpecIdentity stack =
      ioError (userError ("Cyclic dependency while installing " <> formatPackage spec))
  | otherwise = do
      sourcePath <- sourcePathForSpec resolver spec
      dependencyNames <- packageDependencyNamesFromSource sourcePath
      dependencySpecs <- mapM resolveDependencySpec (withImplicitPrimDependency spec dependencyNames)
      dependencyPlans <- mapM (buildPackagePlanRecursive resolver (spec : stack)) dependencySpecs
      pure
        PackagePlan
          { planSourcePath = sourcePath,
            planDependencyPlans = dependencyPlans
          }
  where
    spec = canonicalPackageSpec rawSpec
    resolveDependencySpec dependencyName = do
      version <- resolveVersionForDependency dependencyName
      pure (canonicalPackageSpec (PackageSpec dependencyName version))

    resolveVersionForDependency dependencyName =
      case lookupCoreProvider dependencyName of
        Just provider -> pure (coreProviderVersion provider)
        Nothing -> resolverResolveVersion resolver dependencyName

withImplicitPrimDependency :: PackageSpec -> [String] -> [String]
withImplicitPrimDependency spec dependencies
  | pkgName spec == "aihc-prim" = dependencies
  | any isPrimDependency dependencies = dependencies
  | otherwise = "aihc-prim" : dependencies
  where
    isPrimDependency name = name == "aihc-prim" || name == "ghc-prim"

sourcePathForSpec :: DependencyResolver -> PackageSpec -> IO FilePath
sourcePathForSpec resolver spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> coreProviderSourcePath provider
    Nothing -> resolverSourcePath resolver spec

packageDependencyNamesFromSource :: FilePath -> IO [String]
packageDependencyNamesFromSource sourcePath = do
  cabalFiles <- HackageUtil.findCabalFiles sourcePath
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
      files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
  cabalBytes <- BS.readFile cabalFile
  case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right gpd) -> pure (packageDependencyNames gpd)
    (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errs))

lookupCoreProvider :: String -> Maybe CoreProvider
lookupCoreProvider name =
  case name of
    "base" -> Just aihcBaseProvider
    "aihc-base" -> Just aihcBaseProvider
    "ghc-prim" -> Just aihcPrimProvider
    "aihc-prim" -> Just aihcPrimProvider
    "ghc-internal" -> Just aihcInternalProvider
    "aihc-internal" -> Just aihcInternalProvider
    "template-haskell" -> Just aihcTemplateHaskellProvider
    "aihc-template-haskell" -> Just aihcTemplateHaskellProvider
    "system-cxx-std-lib" -> Just systemCxxStdLibProvider
    _ -> Nothing

canonicalPackageSpec :: PackageSpec -> PackageSpec
canonicalPackageSpec spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> PackageSpec (coreProviderName provider) (coreProviderVersion provider)
    Nothing -> spec

-- | Every standin under @core-libs@, with the version of the boot library it
-- replaces. The versions come from the emulated GHC release so that a
-- package sees the same @base@ version in its @MIN_VERSION_base@ macro, in
-- its resolved dependencies and in the standin's own @.cabal@ file.
coreProviders :: [CoreProvider]
coreProviders = map (uncurry coreProvider) coreProviderSources
  where
    coreProviderSources =
      [ ("aihc-base", "core-libs" </> "aihc-base"),
        ("aihc-prim", "core-libs" </> "aihc-prim"),
        ("aihc-internal", "core-libs" </> "aihc-internal"),
        ("aihc-template-haskell", "core-libs" </> "aihc-template-haskell"),
        ("system-cxx-std-lib", "core-libs" </> "system-cxx-std-lib")
      ]
    coreProvider name sourceRel =
      CoreProvider
        { coreProviderName = name,
          coreProviderVersion =
            maybe
              (error ("core-libs package " <> name <> " is not a boot library of the emulated GHC release"))
              (showVersionBranch . bootLibraryVersion)
              (lookupBootLibraryByStandin name emulatedGhc),
          coreProviderSourceRel = sourceRel
        }

namedCoreProvider :: String -> CoreProvider
namedCoreProvider name =
  case [provider | provider <- coreProviders, coreProviderName provider == name] of
    provider : _ -> provider
    [] -> error ("unknown core provider " <> name)

aihcBaseProvider :: CoreProvider
aihcBaseProvider = namedCoreProvider "aihc-base"

aihcPrimProvider :: CoreProvider
aihcPrimProvider = namedCoreProvider "aihc-prim"

aihcInternalProvider :: CoreProvider
aihcInternalProvider = namedCoreProvider "aihc-internal"

aihcTemplateHaskellProvider :: CoreProvider
aihcTemplateHaskellProvider = namedCoreProvider "aihc-template-haskell"

systemCxxStdLibProvider :: CoreProvider
systemCxxStdLibProvider = namedCoreProvider "system-cxx-std-lib"

-- | The versions a file's @MIN_VERSION_*@ macros report, from the manifests
-- of the packages it is compiled against. A standin is reachable under both
-- its own name and the name of the boot library it replaces, because a
-- Hackage package writes @MIN_VERSION_base@ while the installed package is
-- called @aihc-base@.
dependencyVersionsFromManifests :: [(Text, Text)] -> DependencyVersions
dependencyVersionsFromManifests manifests =
  Map.fromList (concatMap entries manifests)
  where
    entries (name, versionText) =
      case mapM readComponent (T.splitOn "." versionText) of
        Just version ->
          (name, version)
            : [ (T.pack (bootLibraryName library), version)
              | Just library <- [lookupBootLibraryByStandin (T.unpack name) emulatedGhc]
              ]
        Nothing -> []
    readComponent component =
      case reads (T.unpack component) of
        [(value, "")] -> Just value
        _ -> Nothing

coreProviderSourcePath :: CoreProvider -> IO FilePath
coreProviderSourcePath provider = do
  override <- lookupEnv "AIHC_CORE_LIBS_ROOT"
  case override of
    Just root -> pure (root </> coreProviderSourceRel provider)
    Nothing -> do
      cwd <- getCurrentDirectory
      findAncestorContaining providerMarker cwd
  where
    providerRel = coreProviderSourceRel provider
    providerMarker = providerRel </> coreProviderName provider <> ".cabal"

    findAncestorContaining marker dir = do
      exists <- doesFileExist (dir </> marker)
      if exists
        then pure (dir </> providerRel)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then ioError (userError ("Could not find local core library " <> providerRel <> " from current directory"))
            else findAncestorContaining marker parent

packageSpecIdentity :: PackageSpec -> (String, String)
packageSpecIdentity spec =
  (pkgName spec, pkgVersion spec)

packageDependencyNames :: GenericPackageDescription -> [String]
packageDependencyNames gpd =
  (sort . nub . map T.unpack)
    ( concatMap
        (filter (/= currentPackageName) . libraryDependencies)
        libraryTrees
    )
  where
    evalCond = HackageCabal.conditionEvaluator gpd
    currentPackageName = T.pack . CabalPackage.unPackageName . CabalPackage.packageName . package $ packageDescription gpd
    libraryTrees =
      maybe [] pure (condLibrary gpd)
        <> map snd (condSubLibraries gpd)

    libraryDependencies tree =
      let build = HackageCabal.collectMergedBuildInfo evalCond libBuildInfo tree
       in if buildable build
            then HackageCabal.extractDependencies build
            else []
