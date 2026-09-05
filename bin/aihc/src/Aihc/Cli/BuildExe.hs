{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cli.BuildExe
  ( runBuildExe,
  )
where

import Aihc.Cli.Install
  ( ModuleCompileConfig (..),
    ModuleCompileRequest (..),
    ModuleCompileResult (..),
    compileModules,
  )
import Aihc.Cli.Options (BuildExeOptions (..), GarbageCollector)
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest)
import Aihc.Cli.Runtime (prepareEntryArchive, prepareRuntimeArchive, readWasmClangProcessWithExitCode, runtimeGarbageCollector)
import Aihc.Cli.Store (defaultStoreRoot, installedEntryArchivePath, installedRuntimeArchivePath)
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Native (NativeTarget (..), WasmSysroot (..), backendCompiler, nativeTargetStoreDirectory, wasmSysroot)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    ImportDecl (..),
    LanguageEdition (Haskell98Edition),
    Module,
    SourceSpan,
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    moduleName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (Package (..), PackageId (..))
import Control.Exception (bracket)
import Control.Monad (filterM, foldM, forM, unless, when)
import Data.List (find, isInfixOf, isPrefixOf, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Distribution.Package (unPackageName)
import Distribution.Parsec (simpleParsec)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Version (Version, VersionRange, withinRange)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

data InstalledPackage = InstalledPackage
  { installedManifest :: !PackageManifest,
    installedRoot :: !FilePath,
    installedVersion :: !Version
  }

data PackageConstraint = PackageConstraint
  { constraintName :: !Text,
    constraintRange :: !VersionRange
  }

data SourceModule = SourceModule
  { sourcePath :: !FilePath,
    sourceModuleName :: !Text,
    sourceDependencies :: ![SourceDependency],
    sourceParseResult :: ([(SourceSpan, Text)], Module)
  }

data SourceDependency = SourceDependency
  { sourceDependencyPackage :: !(Maybe Text),
    sourceDependencyModule :: !Text
  }
  deriving (Eq, Ord, Show)

data InstalledModule = InstalledModule
  { installedModulePackage :: !InstalledPackage,
    installedModuleName :: !Text
  }

type InstalledModuleIndex = Map.Map Text [InstalledModule]

runBuildExe :: BuildExeOptions -> IO ()
runBuildExe options = do
  storeRoot <- maybe defaultStoreRoot pure (buildExeStoreRoot options)
  currentDirectory <- getCurrentDirectory
  let target = buildExeTarget options
      targetStoreRoot = storeRoot </> nativeTargetStoreDirectory target
      localBuildRoot = fromMaybe (currentDirectory </> ".aihc-cache") (buildExeBuildRoot options)
      buildRoot = localBuildRoot </> nativeTargetStoreDirectory target
      sourceDirectories = case buildExeSourceDirectories options of [] -> ["."]; values -> values
      output = fromMaybe (dropExtension (buildExeSourceFile options)) (buildExeOutputFile options)
  available <- readInstalledPackages targetStoreRoot
  constraints <- mapM parsePackageConstraint (buildExePackageConstraints options)
  selected <- resolvePackages available (constraints <> map implicitConstraint ["aihc-base", "aihc-prim"])
  mapM_ requirePackageArchive selected
  let moduleIndex = buildInstalledModuleIndex selected
  sources <- discoverSources sourceDirectories moduleIndex (buildExeSourceFile options)
  validateInstalledDependencies moduleIndex sources
  sourceFiles <- materializeSourceFiles buildRoot selected sources
  runtime <- ensureRuntime storeRoot target (buildExeGarbageCollector options)
  entry <- ensureEntry storeRoot target
  let compileConfig =
        ModuleCompileConfig
          { compileKeepGrin = False,
            compileKeepNative = False,
            compileLint = buildExeLint options,
            compileNoCode = False,
            compileTarget = target,
            compileVerbose = const (pure ()),
            compilePrintTimings = const (pure ()),
            compileUseColor = False
          }
      compileRequest =
        ModuleCompileRequest
          { compileOutputRoot = buildRoot,
            compilePackageRoot = currentDirectory,
            compilePackage = Package "exe" (PackageId "exe"),
            compileSourceFiles = sourceFiles,
            compileDependencyRoots = map installedRoot selected
          }
  compiled <- compileModules compileConfig compileRequest
  createDirectoryIfMissing True (takeDirectory output)
  linkExecutable target output (compileObjectPaths compiled) (map packageArchive selected) entry runtime

implicitConstraint :: Text -> PackageConstraint
implicitConstraint name =
  case simpleParsec (T.unpack name) of
    Just (Dependency _ versionRange _) -> PackageConstraint name versionRange
    Nothing -> error "invalid implicit package constraint"

parsePackageConstraint :: String -> IO PackageConstraint
parsePackageConstraint input =
  case simpleParsec input of
    Just (Dependency name versionRange _) -> pure (PackageConstraint (T.pack (unPackageName name)) versionRange)
    Nothing -> ioError (userError ("Invalid package constraint: " <> input))

readInstalledPackages :: FilePath -> IO [InstalledPackage]
readInstalledPackages targetRoot = do
  exists <- doesDirectoryExist targetRoot
  unless exists (ioError (userError ("No libraries are compiled for the target in " <> targetRoot)))
  entries <- listDirectory targetRoot
  fmap concat . forM entries $ \entry -> do
    let root = targetRoot </> entry
        path = packageManifestPath root
    existsManifest <- doesFileExist path
    if not existsManifest
      then pure []
      else do
        decoded <- readPackageManifest path
        manifest <- either (ioError . userError . (("Invalid package manifest " <> path <> ": ") <>)) pure decoded
        version <-
          maybe
            (ioError (userError ("Invalid installed package version: " <> T.unpack (packageManifestVersion manifest))))
            pure
            (simpleParsec (T.unpack (packageManifestVersion manifest)))
        pure [InstalledPackage manifest root version]

resolvePackages :: [InstalledPackage] -> [PackageConstraint] -> IO [InstalledPackage]
resolvePackages available constraints = do
  roots <- mapM select grouped
  closure <- foldM addPackage [] roots
  validateSelectedPackageNames closure
  validateSelectedConstraints closure grouped
  pure closure
  where
    grouped =
      [ (name, Map.findWithDefault [] name rangesByName)
      | name <- nub (map constraintName constraints)
      ]
    rangesByName =
      Map.fromListWith
        (<>)
        [(constraintName constraint, [constraintRange constraint]) | constraint <- constraints]
    select (name, ranges) =
      case sortOn installedVersion (filter (matches name ranges) available) of
        [] -> ioError (userError ("No compiled library fulfills the constraint for " <> T.unpack name))
        matches' ->
          case filter ((== installedVersion (last matches')) . installedVersion) matches' of
            [package] -> pure package
            _ -> ioError (userError ("More than one compiled build fulfills the constraint for " <> T.unpack name))
    matches name ranges package =
      packageManifestName (installedManifest package) == name
        && all (installedVersion package `withinRange`) ranges
    addPackage selected package
      | identity package `elem` map identity selected = pure selected
      | otherwise = do
          dependencies <- mapM requireIdentity (packageManifestDependencies (installedManifest package))
          foldM addPackage (selected <> [package]) dependencies
    requireIdentity wanted =
      maybe
        (ioError (userError ("A required compiled library is absent: " <> T.unpack wanted)))
        pure
        (find ((== wanted) . identity) available)
    identity = packageManifestIdentity . installedManifest
    validateSelectedPackageNames selected =
      mapM_ validateName (Map.toList packagesByName)
      where
        packagesByName =
          Map.fromListWith
            (<>)
            [ (packageManifestName (installedManifest package), [package])
            | package <- selected
            ]
        validateName (_, [_]) = pure ()
        validateName (name, _) = ioError (userError ("The dependency plan selects more than one build of " <> T.unpack name))
    validateSelectedConstraints selected =
      mapM_ $ \(name, ranges) ->
        case filter ((== name) . packageManifestName . installedManifest) selected of
          [package]
            | all (installedVersion package `withinRange`) ranges -> pure ()
            | otherwise -> conflict name
          [] -> conflict name
          _ -> ioError (userError ("The dependency plan selects more than one version of " <> T.unpack name))
    conflict name = ioError (userError ("The installed dependency plan does not fulfill the constraint for " <> T.unpack name))

packageArchive :: InstalledPackage -> FilePath
packageArchive package =
  installedRoot package
    </> "lib"
    </> "lib"
      <> T.unpack (packageManifestName (installedManifest package))
      <> ".a"

requirePackageArchive :: InstalledPackage -> IO ()
requirePackageArchive package = do
  let archive = packageArchive package
  exists <- doesFileExist archive
  unless exists $
    ioError
      ( userError
          ( "The library "
              <> T.unpack (packageManifestName (installedManifest package))
              <> " is not compiled for the target: "
              <> archive
          )
      )

buildInstalledModuleIndex :: [InstalledPackage] -> InstalledModuleIndex
buildInstalledModuleIndex packages =
  Map.fromListWith (<>) [(installedModuleName entry, [entry]) | entry <- entries]
  where
    entries =
      [ InstalledModule package name
      | package <- packages,
        name <- packageManifestModules (installedManifest package)
      ]

discoverSources :: [FilePath] -> InstalledModuleIndex -> FilePath -> IO [SourceModule]
discoverSources sourceDirectories moduleIndex mainPath = do
  mainSource <- parseSource mainPath
  unless (sourceModuleName mainSource == "Main") (ioError (userError ("The input file does not define module Main: " <> mainPath)))
  discovered <- visit Map.empty mainSource
  when (Map.member "Aihc.Entry" discovered) (ioError (userError "Source module conflicts with generated module Aihc.Entry"))
  entrySource <- parseSourceText "<aihc-entry>" generatedEntryText
  pure (Map.elems discovered <> [entrySource])
  where
    visit found source = do
      let name = sourceModuleName source
      case Map.lookup name found of
        Just previous
          | sourcePath previous == sourcePath source -> pure found
          | otherwise -> ioError (userError ("More than one source file defines module " <> T.unpack name))
        Nothing -> do
          let found' = Map.insert name source found
          foldM visitImport found' (sourceDependencies source)
    visitImport found dependency
      | not (isLocalSourceDependency dependency), Map.member name moduleIndex = pure found
      | isNothing (sourceDependencyPackage dependency), Map.member name moduleIndex = pure found
      | Map.member name found = pure found
      | not (isLocalSourceDependency dependency) = pure found
      | otherwise = do
          path <- findSourceFile sourceDirectories name
          parseSource path >>= visit found
      where
        name = sourceDependencyModule dependency

generatedEntryText :: Text
generatedEntryText =
  T.unlines
    [ "{-# LANGUAGE NoImplicitPrelude #-}",
      "module Aihc.Entry where",
      "import qualified Main",
      "import GHC.TopHandler (runMainIO)",
      "entry = runMainIO Main.main"
    ]

validateInstalledDependencies :: InstalledModuleIndex -> [SourceModule] -> IO ()
validateInstalledDependencies moduleIndex sources = mapM_ validateDependency externalDependencies
  where
    localNames = Set.fromList (map sourceModuleName sources)
    externalDependencies =
      nub
        [ dependency
        | source <- sources,
          dependency <- sourceDependencies source,
          not (isLocalSourceDependency dependency)
            || sourceDependencyModule dependency `Set.notMember` localNames
        ]
    validateDependency dependency =
      case matchingModules dependency of
        [] ->
          ioError
            ( userError
                ( "Required installed module not found: "
                    <> maybe "" ((<> ":") . T.unpack) (sourceDependencyPackage dependency)
                    <> T.unpack (sourceDependencyModule dependency)
                )
            )
        [_] -> pure ()
        _ -> ioError (userError ("Ambiguous installed module: " <> T.unpack (sourceDependencyModule dependency)))
    matchingModules dependency =
      case sourceDependencyPackage dependency of
        Nothing -> candidates
        Just packageName' ->
          filter
            ((== packageName') . packageManifestName . installedManifest . installedModulePackage)
            candidates
      where
        candidates = Map.findWithDefault [] (sourceDependencyModule dependency) moduleIndex

materializeSourceFiles :: FilePath -> [InstalledPackage] -> [SourceModule] -> IO [HackageCabal.FileInfo]
materializeSourceFiles buildRoot packages sources = do
  let generatedPath = buildRoot </> "generated" </> "Aihc" </> "Entry.hs"
      dependencyNames = map (packageManifestName . installedManifest) packages
  createDirectoryIfMissing True (takeDirectory generatedPath)
  TIO.writeFile generatedPath generatedEntryText
  pure (map (sourceFileInfo generatedPath dependencyNames) sources)

sourceFileInfo :: FilePath -> [Text] -> SourceModule -> HackageCabal.FileInfo
sourceFileInfo generatedPath dependencyNames source =
  HackageCabal.FileInfo
    { HackageCabal.fileInfoPath = if sourcePath source == "<aihc-entry>" then generatedPath else sourcePath source,
      HackageCabal.fileInfoExtensions = [],
      HackageCabal.fileInfoCppOptions = [],
      HackageCabal.fileInfoIncludeDirs = [],
      HackageCabal.fileInfoLanguage = Nothing,
      HackageCabal.fileInfoDependencies = dependencyNames
    }

findSourceFile :: [FilePath] -> Text -> IO FilePath
findSourceFile directories name = do
  let relative = foldl (</>) "" (map T.unpack (T.splitOn "." name)) <> ".hs"
      candidates = map (</> relative) directories
  matches <- filterM doesFileExist candidates
  case matches of
    [path] -> pure path
    [] -> ioError (userError ("Source module not found: " <> T.unpack name))
    _ -> ioError (userError ("More than one source file provides module " <> T.unpack name))

parseSource :: FilePath -> IO SourceModule
parseSource path = TIO.readFile path >>= parseSourceText path

parseSourceText :: FilePath -> Text -> IO SourceModule
parseSourceText path source = do
  let extensions = sourceExtensions source
      parsed = parseModule (parserConfig path source) source
      modu = snd parsed
      name = fromMaybe "Main" (moduleName modu)
      dependencies =
        nub
          ( map importDependency (Syntax.moduleImports modu)
              <> implicitSourceDependencies "exe" extensions
          )
  pure
    SourceModule
      { sourcePath = path,
        sourceModuleName = name,
        sourceDependencies = dependencies,
        sourceParseResult = parsed
      }

importDependency :: ImportDecl -> SourceDependency
importDependency importDecl =
  SourceDependency
    { sourceDependencyPackage = importDeclPackage importDecl,
      sourceDependencyModule = importDeclModule importDecl
    }

implicitSourceDependencies :: Text -> [Extension] -> [SourceDependency]
implicitSourceDependencies currentPackage extensions =
  compilerDependencies
    <> [ SourceDependency (Just "aihc-base") "Prelude"
       | currentPackage /= "aihc-base",
         ImplicitPrelude `elem` extensions
       ]

compilerDependencies :: [SourceDependency]
compilerDependencies =
  [ SourceDependency (Just "aihc-prim") "GHC.Types",
    SourceDependency (Just "aihc-prim") "GHC.Prim.Base",
    SourceDependency (Just "aihc-prim") "GHC.Classes",
    SourceDependency (Just "aihc-prim") "GHC.Prim.Num"
  ]

isLocalSourceDependency :: SourceDependency -> Bool
isLocalSourceDependency dependency =
  isNothing (sourceDependencyPackage dependency)
    || sourceDependencyPackage dependency == Just "this"

parserConfig :: FilePath -> Text -> ParserConfig
parserConfig path source =
  defaultConfig
    { parserSourceName = path,
      parserExtensions = sourceExtensions source
    }

sourceExtensions :: Text -> [Extension]
sourceExtensions source = effectiveExtensions language (headerExtensionSettings header)
  where
    header = readModuleHeaderPragmas source
    language = fromMaybe Haskell98Edition (headerLanguageEdition header)

ensureEntry :: FilePath -> NativeTarget -> IO FilePath
ensureEntry storeRoot target = do
  let entry = installedEntryArchivePath storeRoot target
  exists <- doesFileExist entry
  if exists then pure entry else prepareEntryArchive storeRoot target

ensureRuntime :: FilePath -> NativeTarget -> GarbageCollector -> IO FilePath
ensureRuntime storeRoot target garbageCollector = do
  let runtime = installedRuntimeArchivePath storeRoot target (runtimeGarbageCollector garbageCollector)
  exists <- doesFileExist runtime
  if exists then pure runtime else prepareRuntimeArchive storeRoot target garbageCollector

linkExecutable :: NativeTarget -> FilePath -> [FilePath] -> [FilePath] -> FilePath -> FilePath -> IO ()
linkExecutable Wasm32Wasip3 output objects archives entry runtime =
  withTemporaryDirectory "aihc-wasm-link" $ \directory -> do
    sysroot <- wasmSysroot
    let coreModule = directory </> "program.wasm"
    -- The libc archive follows every other input. A linker takes only the
    -- members that resolve a symbol it has already seen, so this pulls the
    -- allocator, the memory routines, and the math functions the runtime
    -- leaves undefined, and nothing else.
    runTool
      "wasm-ld"
      ( ["--no-entry", "--export-memory", "--allow-undefined"]
          <> objects
          <> archives
          <> ["--whole-archive", entry, runtime, "--no-whole-archive"]
          <> [wasmSysrootLibc sysroot, "-o", coreModule]
      )
    buildComponent coreModule output
    runTool "wasm-tools" ["validate", output]
linkExecutable target output objects archives entry runtime = do
  (compiler, arguments) <- backendCompiler target
  runTool compiler (arguments <> objects <> archives <> [entry, runtime, "-o", output])

-- | Encode the linked core module as a component. The component model has no
-- way to describe a WASI preview 1 import, so a runtime unit that reaches a
-- libc function needing one fails here rather than at run time. The notice
-- names that cause, which the encoder reports only as an unresolved import.
buildComponent :: FilePath -> FilePath -> IO ()
buildComponent coreModule output = do
  result <- readProcessWithExitCode "wasm-tools" ["component", "new", coreModule, "-o", output] ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (exitCode, stdout, stderr) -> do
      let reported = if null stderr then stdout else stderr
          notice
            | "wasi_snapshot_preview1" `isInfixOf` reported =
                reported
                  <> "\n\nAIHC notice: the program imports WASI preview 1. The runtime reaches\n\
                     \WASI through the preview 3 bindings only, so this comes from a libc\n\
                     \function that needs the host, such as one of the stdio, exit, or clock\n\
                     \families. Implement it in the P3 IO backend instead.\n"
            | otherwise = reported
      ioError (userError ("wasm-tools failed (" <> show exitCode <> "): " <> notice))

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  result <-
    if tool == "clang" && any ("--target=wasm32" `isPrefixOf`) arguments
      then readWasmClangProcessWithExitCode tool arguments
      else readProcessWithExitCode tool arguments ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (exitCode, stdout, stderr) -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> if null stderr then stdout else stderr))

withTemporaryDirectory :: String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary template
      hClose handle
      removeFile path
      createDirectory path
      pure path
