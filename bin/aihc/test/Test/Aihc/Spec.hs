{-# LANGUAGE OverloadedStrings #-}

module Test.Aihc.Spec (tests) where

import Aihc.Cli.BuildExe (LinkBundle (..), linkBundleManifestPath, runBuildExe, runLinkExe)
import Aihc.Cli.Install (InstallResult (..), install, installWith, parsePackageTarget)
import Aihc.Cli.Options (BuildExeOptions (..), GarbageCollector (GcSemispace), InstallOptions (..), LinkExeOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest, writePackageManifest)
import Aihc.Cli.PackagePlan (CoreProvider (..), coreProviderSourcePath, coreProviders)
import Aihc.Cli.Store (installedEntryArchivePath)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Fc qualified as Fc
import Aihc.Hackage.Release (BootLibrary (..), emulatedGhc, lookupBootLibrary)
import Aihc.Native (NativeTarget (..), nativeTargetStoreDirectory)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc (tcInterfaceTerms, tcTermKeyIdentifier)
import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM, forM_, void)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getFileSize,
    getModificationTime,
    getPermissions,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    setModificationTime,
    setOwnerWritable,
    setPermissions,
    withCurrentDirectory,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import System.IO (IOMode (WriteMode), hClose, openTempFile, withFile)
import System.IO.Error (ioeGetErrorString)
import System.Process (readProcess, readProcessWithExitCode)
import Test.Aihc.SeedStore
  ( Sandbox (..),
    SeedStore,
    acquireCoreStore,
    acquirePrimStore,
    buildExeHostTarget,
    installTestTargets,
    releaseSeedStore,
    seededPackagePath,
    withSandbox,
  )
import Test.Tasty (DependencyType (AllFinish), TestTree, dependentTestGroup, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

-- | The core libraries are seeded once for the whole group rather than by each
-- test; see "Test.Aihc.SeedStore". aihc-base is a separate, nested resource so
-- that running only the @install@ tests never installs it.
tests :: TestTree
tests =
  withResource acquirePrimStore releaseSeedStore $ \primStore ->
    testGroup
      "aihc"
      [ withResource (acquireCoreStore primStore) releaseSeedStore $ \coreStore ->
          -- These run one at a time: build-exe resolves its cache directory
          -- against the working directory, and the process has only one.
          dependentTestGroup
            "build-exe"
            AllFinish
            [ testCase "builds imported source modules and runs the executable" (test_buildExeSourceDirectories coreStore),
              testCase "reports the ambiguous installed module" (test_buildExeAmbiguousModule coreStore),
              testCase "reports ambiguous package builds" (test_buildExeAmbiguousPackage coreStore),
              testCase "reports conflicting dependency builds" (test_buildExeConflictingDependencies coreStore),
              testCase "reports the generated entry collision" (test_buildExeEntryCollision coreStore),
              testCase "writes a link bundle that link-exe turns into the executable" (test_buildExeLinkBundle coreStore)
            ],
        testGroup
          "install"
          [ testCase "reuses unchanged fixture modules" (test_installIncremental primStore),
            testCase "writes Core files and reuses an installed package" (test_installResolveArtifacts primStore),
            testCase "accepts type-check warnings" (test_installTypeWarning primStore),
            testCase "loads the implicit Prelude type interface" (test_installImplicitPrelude primStore),
            testCase "duplicates re-exported term signatures in type interfaces" (test_installTypeReexports primStore),
            testCase "limits instances to the transitive import graph" (test_installInstanceVisibility primStore),
            testCase "installs direct local dependencies" (test_installLocalDependencies primStore),
            testCase "prints timings independently from verbose output" (test_installTimingOutput primStore),
            testCase "reports all frontend errors in stable dependency order" (test_installResolveError primStore),
            testCase "writes Core for a ccall import" (test_installFcCcall primStore),
            testCase "compiles Cabal c-sources into the library archive" (test_installCSources primStore),
            testCase "writes an empty archive for a package with no code" (test_installEmptyArchive primStore),
            testCase "defines MIN_VERSION macros from the installed dependency versions" (test_installMinVersionMacros primStore),
            testCase "core-libs versions match the emulated GHC release" test_coreLibsMatchRelease,
            testCase "selects Cabal source dirs by target architecture" (test_installArchSourceDirs primStore),
            testCase "retains Core and GRIN only with keep-core and keep-grin" (test_installKeepGrin primStore),
            testCase "writes target-specific objects and library archives" (test_installTargetArchives primStore),
            -- This one installs aihc-prim into an empty store on purpose: it is
            -- the test that covers the install the seed store performs.
            testCase "install writes core for aihc-prim and lints stored programs" test_installAihcPrim,
            testCase "parses Hackage package targets" test_parsePackageTarget
          ]
      ]

test_parsePackageTarget :: Assertion
test_parsePackageTarget = do
  assertEqual "bare name" (Just ("nats", Nothing)) (parsePackageTarget "nats")
  assertEqual "hyphenated name" (Just ("aihc-base", Nothing)) (parsePackageTarget "aihc-base")
  assertEqual "name and version" (Just ("nats", Just "1.1.2")) (parsePackageTarget "nats-1.1.2")
  assertEqual "hyphenated name and version" (Just ("aihc-base", Just "4.21.2.0")) (parsePackageTarget "aihc-base-4.21.2.0")
  assertEqual "path" Nothing (parsePackageTarget "core-libs/aihc-base")
  assertEqual "spaces" Nothing (parsePackageTarget "not a package")

-- | Give a @build-exe@ test a sandbox holding a seeded store, the fixture that
-- the default options compile, and those options.
withBuildExeSandbox ::
  IO SeedStore ->
  String ->
  (Sandbox -> FilePath -> FilePath -> BuildExeOptions -> Assertion) ->
  Assertion
withBuildExeSandbox getStore prefix action = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build-exe/source-directories"
  withSandbox getStore prefix $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let options =
          BuildExeOptions
            { buildExeSourceFile = fixtureRoot </> "Main.hs",
              buildExeSourceDirectories = [fixtureRoot],
              buildExePackageConstraints = ["aihc-base == 4.21.2.0"],
              buildExeTarget = buildExeHostTarget,
              buildExeGarbageCollector = GcSemispace,
              buildExeStoreRoot = Just storeRoot,
              buildExeBuildRoot = Nothing,
              buildExeLint = False,
              buildExeNoLink = False,
              buildExeOutputFile = Just (sandboxRoot sandbox </> "program")
            }
    action sandbox fixtureRoot storeRoot options

-- | Run @build-exe@ and return the error it reports, failing the test when it
-- succeeds instead.
buildExeError :: FilePath -> BuildExeOptions -> String -> IO String
buildExeError workingDirectory options expectation = do
  result <-
    try (withCurrentDirectory workingDirectory (runBuildExe options)) ::
      IO (Either IOException ())
  case result of
    Left err -> pure (ioeGetErrorString err)
    Right () -> assertFailure expectation

test_buildExeSourceDirectories :: IO SeedStore -> Assertion
test_buildExeSourceDirectories getStore =
  withBuildExeSandbox getStore "aihc-build-exe" $ \sandbox fixtureRoot storeRoot options -> do
    let root = sandboxRoot sandbox
        output = sandboxRoot sandbox </> "program"
        target = buildExeTarget options
    basePackage <- seededPackagePath storeRoot target "aihc-base"
    manifestResult <- readPackageManifest (packageManifestPath basePackage)
    manifest <- either assertFailure pure manifestResult
    assertBool "package manifest contains Prelude" ("Prelude" `elem` packageManifestModules manifest)
    let unusedResolve = basePackage </> "Data" </> "Bool" </> "resolve.cbor"
        unusedType = basePackage </> "Data" </> "Bool" </> "type.cbor"
    resolveBytes <- BS.readFile unusedResolve
    BS.writeFile unusedResolve "invalid unused resolve interface"
    let activeRoot = storeRoot </> ".active" </> nativeTargetStoreDirectory target
    createDirectoryIfMissing True activeRoot
    writeFile (activeRoot </> T.unpack (packageManifestName manifest <> "-" <> packageManifestVersion manifest)) "absent-cached-build"
    let storeCache = storeRoot </> ".build-cache"
    createDirectoryIfMissing True storeCache
    writePackageManifest (packageManifestPath storeCache) manifest {packageManifestVersion = "9999"}
    writeFile (basePackage </> "build-inputs.hash") "invalid-build-fingerprint"
    bracket (getPermissions storeCache) (setPermissions storeCache) $ \permissions -> do
      setPermissions storeCache (setOwnerWritable False permissions)
      withCurrentDirectory root (runBuildExe options)
    assertFileExists (root </> ".aihc-cache" </> nativeTargetStoreDirectory target </> "Main" </> "Main.o")
    assertFileDoesNotExist (root </> ".aihc-cache" </> nativeTargetStoreDirectory target </> "GHC" </> "Base" </> "GHC.Base.o")
    removeDirectoryRecursive storeCache
    removeFile (basePackage </> "build-inputs.hash")
    let customBuildRoot = root </> "custom-build-root"
    withCurrentDirectory fixtureRoot (runBuildExe options {buildExeBuildRoot = Just customBuildRoot})
    assertFileExists (customBuildRoot </> nativeTargetStoreDirectory target </> "Main" </> "Main.o")
    BS.writeFile unusedResolve resolveBytes
    typeBytes <- BS.readFile unusedType
    BS.writeFile unusedType "invalid unused type interface"
    withCurrentDirectory root (runBuildExe options)
    BS.writeFile unusedType typeBytes
    withCurrentDirectory root (runBuildExe options {buildExeLint = True})
    entryExists <- doesFileExist (installedEntryArchivePath storeRoot target)
    assertBool "target entry archive exists" entryExists
    (status, stdout, stderr) <- readProcessWithExitCode output [] ""
    assertEqual "executable exit status" ExitSuccess status
    assertEqual "executable stdout" "build-exe works\n" stdout
    assertEqual "executable stderr" "" stderr
    (rtsStatus, rtsStdout, rtsStderr) <-
      readProcessWithExitCode output ["first", "+RTS", "-M1G", "-RTS", "second"] ""
    assertEqual "RTS executable exit status" ExitSuccess rtsStatus
    assertEqual "RTS options are absent from program arguments" "first\nsecond\n" rtsStdout
    assertEqual "RTS executable stderr" "" rtsStderr
    (plainStatus, plainStdout, plainStderr) <-
      readProcessWithExitCode output ["-M1G", "second"] ""
    assertEqual "plain option executable exit status" ExitSuccess plainStatus
    assertEqual "plain option remains a program argument" "-M1G\nsecond\n" plainStdout
    assertEqual "plain option executable stderr" "" plainStderr
    (limitStatus, limitStdout, limitStderr) <-
      readProcessWithExitCode output ["+RTS", "-M1", "-RTS"] ""
    assertBool "heap limit terminates the executable" (limitStatus /= ExitSuccess)
    assertEqual "heap limit stdout" "" limitStdout
    assertEqual "heap limit diagnostic" "aihc runtime: heap limit exceeded\n" limitStderr
    (invalidStatus, invalidStdout, invalidStderr) <-
      readProcessWithExitCode output ["+RTS", "-M1X", "-RTS"] ""
    assertBool "invalid heap size terminates the executable" (invalidStatus /= ExitSuccess)
    assertEqual "invalid heap size stdout" "" invalidStdout
    assertEqual "invalid heap size diagnostic" "aihc runtime: invalid size for RTS option -M\n" invalidStderr

-- | @--no-link@ leaves no executable behind. The bundle it writes instead is
-- self-contained: linking it from another directory, with the store gone,
-- still produces the program.
test_buildExeLinkBundle :: IO SeedStore -> Assertion
test_buildExeLinkBundle getStore =
  withBuildExeSandbox getStore "aihc-link-bundle" $ \sandbox _fixtureRoot storeRoot options -> do
    let root = sandboxRoot sandbox
        bundle = root </> "bundle"
        output = root </> "linked" </> "program"
    withCurrentDirectory root (runBuildExe options {buildExeNoLink = True, buildExeOutputFile = Just bundle})
    assertFileDoesNotExist (root </> "program")
    assertFileExists (linkBundleManifestPath bundle)
    decoded <- Aeson.eitherDecode <$> BL.readFile (linkBundleManifestPath bundle)
    manifest <- either assertFailure pure decoded
    assertEqual "bundle target" (buildExeTarget options) (linkBundleTarget manifest)
    assertBool "bundle lists the main object" (any ("Main.o" `isSuffixOf`) (linkBundleObjects manifest))
    assertBool "bundle lists the base archive" (any ("libaihc-base.a" `isSuffixOf`) (linkBundleArchives manifest))
    forM_ (linkBundleObjects manifest <> linkBundleArchives manifest <> [linkBundleEntry manifest, linkBundleRuntime manifest]) $ \input -> do
      assertBool ("bundle input is relative: " <> input) ("inputs/" `isPrefixOf` input)
      assertFileExists (bundle </> input)
    removeDirectoryRecursive storeRoot
    withCurrentDirectory root $
      runLinkExe LinkExeOptions {linkExeBundle = bundle, linkExeOutputFile = output}
    (status, stdout, stderr) <- readProcessWithExitCode output [] ""
    assertEqual "linked executable exit status" ExitSuccess status
    assertEqual "linked executable stdout" "build-exe works\n" stdout
    assertEqual "linked executable stderr" "" stderr

test_buildExeAmbiguousModule :: IO SeedStore -> Assertion
test_buildExeAmbiguousModule getStore =
  withBuildExeSandbox getStore "aihc-build-exe-ambiguous-module" $ \sandbox _ storeRoot options -> do
    writeCachedPackage storeRoot (buildExeTarget options) "duplicate-1.0.0-a" "duplicate" "1.0.0" [] ["System.IO"]
    err <-
      buildExeError
        (sandboxRoot sandbox)
        options {buildExePackageConstraints = buildExePackageConstraints options <> ["duplicate == 1.0.0"]}
        "expected the installed module import to be ambiguous"
    assertBool "reports the ambiguous installed module" ("Ambiguous installed module: System.IO" `isInfixOf` err)

test_buildExeAmbiguousPackage :: IO SeedStore -> Assertion
test_buildExeAmbiguousPackage getStore =
  withBuildExeSandbox getStore "aihc-build-exe-ambiguous-package" $ \sandbox _ storeRoot options -> do
    let target = buildExeTarget options
    writeCachedPackage storeRoot target "duplicate-1.0.0-a" "duplicate" "1.0.0" [] ["System.IO"]
    writeCachedPackage storeRoot target "duplicate-1.0.0-b" "duplicate" "1.0.0" [] []
    err <-
      buildExeError
        (sandboxRoot sandbox)
        options {buildExePackageConstraints = buildExePackageConstraints options <> ["duplicate == 1.0.0"]}
        "expected the compiled package build to be ambiguous"
    assertBool
      "reports ambiguous package builds"
      ("More than one compiled build fulfills the constraint for duplicate" `isInfixOf` err)

test_buildExeConflictingDependencies :: IO SeedStore -> Assertion
test_buildExeConflictingDependencies getStore =
  withBuildExeSandbox getStore "aihc-build-exe-conflicting-dependencies" $ \sandbox _ storeRoot options -> do
    let target = buildExeTarget options
    writeCachedPackage storeRoot target "shared-1.0.0-a" "shared" "1.0.0" [] []
    writeCachedPackage storeRoot target "shared-1.0.0-b" "shared" "1.0.0" [] []
    writeCachedPackage storeRoot target "root-a-1.0.0" "root-a" "1.0.0" ["shared-1.0.0-a"] []
    writeCachedPackage storeRoot target "root-b-1.0.0" "root-b" "1.0.0" ["shared-1.0.0-b"] []
    err <-
      buildExeError
        (sandboxRoot sandbox)
        options
          { buildExePackageConstraints =
              buildExePackageConstraints options <> ["root-a == 1.0.0", "root-b == 1.0.0"]
          }
        "expected the dependency builds to conflict"
    assertBool
      "reports conflicting dependency builds"
      ("The dependency plan selects more than one build of shared" `isInfixOf` err)

test_buildExeEntryCollision :: IO SeedStore -> Assertion
test_buildExeEntryCollision getStore =
  withBuildExeSandbox getStore "aihc-build-exe-entry-collision" $ \sandbox _ _ options -> do
    entryCollisionRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build-exe/generated-entry-collision"
    err <-
      buildExeError
        (sandboxRoot sandbox)
        options
          { buildExeSourceFile = entryCollisionRoot </> "Main.hs",
            buildExeSourceDirectories = [entryCollisionRoot]
          }
        "expected the generated entry module to conflict"
    assertBool
      "reports the generated entry collision"
      ("Source module conflicts with generated module Aihc.Entry" `isInfixOf` err)

writeCachedPackage :: FilePath -> NativeTarget -> FilePath -> Text -> Text -> [Text] -> [Text] -> IO ()
writeCachedPackage storeRoot target identity name version dependencies modules = do
  let packageRoot = storeRoot </> nativeTargetStoreDirectory target </> identity
      archive = packageRoot </> "lib" </> "lib" <> T.unpack name <> ".a"
  createDirectoryIfMissing True (takeDirectory archive)
  writePackageManifest
    (packageManifestPath packageRoot)
    PackageManifest
      { packageManifestName = name,
        packageManifestVersion = version,
        packageManifestIdentity = T.pack identity,
        packageManifestUnitId = T.pack identity,
        packageManifestDependencies = dependencies,
        packageManifestModules = modules
      }
  BS.writeFile archive ""

test_installIncremental :: IO SeedStore -> Assertion
test_installIncremental getStore = do
  fixture <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/incremental"
  withSandbox getStore "aihc-incremental" $ \sandbox -> do
    store <- sandboxStore sandbox "store"
    let root = sandboxRoot sandbox </> "source"
        options = InstallOptions root (Just store) True False False False False False False False AppleArm64
    createDirectoryIfMissing True (root </> "src")
    let dependencyRoot = sandboxRoot sandbox </> "dep"
    createDirectoryIfMissing True (dependencyRoot </> "src")
    copyFile (fixture </> "dep" </> "dep.cabal") (dependencyRoot </> "dep.cabal")
    copyFile (fixture </> "dep" </> "src" </> "Dep.hs") (dependencyRoot </> "src" </> "Dep.hs")
    copyFile (fixture </> "demo.cabal") (root </> "demo.cabal")
    forM_ ["A.hs", "B.hs", "C.hs"] $ \name ->
      copyFile (fixture </> "src" </> name) (root </> "src" </> name)
    first <- install options
    initialManifest <- readPackageManifest (packageManifestPath (installStorePath first)) >>= either assertFailure pure
    assertEqual "initial modules" ["A", "B", "C"] (sort (installWrittenModules first))
    unchanged <- install options
    assertEqual "unchanged package" (installStorePath first) (installStorePath unchanged)
    copyFile (fixture </> "Extra.hs") (root </> "src" </> "Extra.hs")
    unrelated <- install options
    assertEqual "unselected source does not change the package" (installStorePath first) (installStorePath unrelated)
    removeFile (root </> "src" </> "Extra.hs")
    withoutUnrelated <- install options
    assertEqual "unselected source removal does not change the package" (installStorePath first) (installStorePath withoutUnrelated)
    originalTime <- getModificationTime (root </> "src" </> "A.hs")
    originalSize <- getFileSize (root </> "src" </> "A.hs")
    copyFile (fixture </> "implementation/A.hs") (root </> "src" </> "A.hs")
    setModificationTime (root </> "src" </> "A.hs") originalTime
    changedSize <- getFileSize (root </> "src" </> "A.hs")
    assertEqual "fixture preserves file size" originalSize changedSize
    changed <- install options
    assertEqual "source changes preserve package selection" (installStorePath first) (installStorePath changed)
    assertEqual "changed module" ["A"] (installWrittenModules changed)
    assertEqual "unchanged modules" ["B", "C"] (sort (installReusedModules changed))
    copyFile (fixture </> "interface/A.hs") (root </> "src" </> "A.hs")
    typeChanged <- install options
    assertEqual "type change reaches the dependent module" ["A", "B"] (sort (installWrittenModules typeChanged))
    assertEqual "independent module" ["C"] (installReusedModules typeChanged)
    assertFileExists (installStorePath first </> "A" </> "type.cbor")
    copyFile (fixture </> "dependency" </> "Dep.hs") (dependencyRoot </> "src" </> "Dep.hs")
    dependencyChanged <- install options
    assertEqual "dependency changes preserve package selection" (installStorePath typeChanged) (installStorePath dependencyChanged)
    assertEqual "dependency implementation preserves module artifacts" ["A", "B", "C"] (sort (installReusedModules dependencyChanged))
    noCode <- install options {installNoCode = True}
    assertEqual "code mode preserves package selection" (installStorePath dependencyChanged) (installStorePath noCode)
    assertFileDoesNotExist (installStorePath noCode </> "A" </> "A.o")
    withCode <- install options
    assertEqual "code artifacts remain available" (installStorePath dependencyChanged) (installStorePath withCode)
    assertFileExists (installStorePath withCode </> "A" </> "A.o")
    cachedObjects <- listNamedFiles (store </> ".build-cache") "C.o"
    assertBool "module cache contains the independent object" (not (null cachedObjects))
    forM_ cachedObjects $ \path -> BS.writeFile path "invalid object"
    removeDirectoryRecursive (installStorePath withCode)
    repaired <- install options
    assertEqual "corrupt module cache rebuilds the module" ["C"] (installWrittenModules repaired)
    assertEqual "valid module cache remains available" ["A", "B"] (sort (installReusedModules repaired))
    forced <- install options {installReinstall = True}
    assertEqual "forced modules" ["A", "B", "C"] (sort (installWrittenModules forced))
    removeFile (installStorePath forced </> "build-inputs.hash")
    withoutFingerprint <- install options
    assertEqual "missing build metadata preserves package selection" (installStorePath first) (installStorePath withoutFingerprint)
    removeDirectoryRecursive (store </> ".build-cache")
    withoutCache <- install options
    finalManifest <- readPackageManifest (packageManifestPath (installStorePath withoutCache)) >>= either assertFailure pure
    assertEqual "cache removal preserves package identity" (packageManifestIdentity initialManifest) (packageManifestIdentity finalManifest)
    assertEqual "cache removal preserves dependency selection" (packageManifestDependencies initialManifest) (packageManifestDependencies finalManifest)

test_installResolveArtifacts :: IO SeedStore -> Assertion
test_installResolveArtifacts getStore =
  withSandbox getStore "aihc-install" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallOptions sourceRoot (Just storeRoot) True False False False False False False False AppleArm64
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo.A, Demo.B",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010"
          ]
      )
    writeFile (sourceDir </> "A.hs") "module Demo.A where\nimport Demo.B\na x = x\n"
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nimport Demo.A\nb x = x\n"
    first <- install options
    assertEqual "written modules" ["Demo.A", "Demo.B"] (sort (installWrittenModules first))
    assertFileExists (installStorePath first </> "Demo" </> "A" </> "resolve.cbor")
    assertFileExists (installStorePath first </> "Demo" </> "B" </> "resolve.cbor")
    assertFileExists (installStorePath first </> "Demo" </> "A" </> "type.cbor")
    assertFileExists (installStorePath first </> "Demo" </> "B" </> "type.cbor")
    assertCoreFile (installStorePath first </> "Demo" </> "A" </> "core")
    assertCoreFile (installStorePath first </> "Demo" </> "B" </> "core")
    second <- install options
    assertEqual "reused modules" ["Demo.A", "Demo.B"] (sort (installReusedModules second))
    assertEqual "stable package directory" (installStorePath first) (installStorePath second)
    assertCoreFile (installStorePath second </> "Demo" </> "A" </> "core")
    assertCoreFile (installStorePath second </> "Demo" </> "B" </> "core")
    BS.writeFile (installStorePath second </> "Demo" </> "A" </> "resolve.cbor") "invalid resolve artifact"
    BS.writeFile (installStorePath second </> "Demo" </> "B" </> "type.cbor") "invalid type artifact"
    reinstalled <- install options {installReinstall = True}
    assertEqual "reinstall rebuilds all modules" ["Demo.A", "Demo.B"] (sort (installWrittenModules reinstalled))
    assertEqual "reinstall reuses no modules" [] (installReusedModules reinstalled)
    removeFile (installStorePath first </> "Demo" </> "A" </> "core")
    coreRepaired <- install options {installReinstall = True}
    assertEqual "repairs the complete SCC when core is absent" ["Demo.A", "Demo.B"] (sort (installWrittenModules coreRepaired))
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nimport Demo.A\nb x = (x)\n"
    changed <- install options {installReinstall = True}
    assertEqual "source changes preserve the package directory" (installStorePath first) (installStorePath changed)
    assertEqual "source changes rebuild the complete SCC" ["Demo.A", "Demo.B"] (sort (installWrittenModules changed))
    let artifact = installStorePath first </> "Demo" </> "A" </> "resolve.cbor"
    artifactBytes <- BS.readFile artifact
    BS.writeFile artifact (BS.init artifactBytes)
    repaired <- install options {installReinstall = True}
    assertEqual "repairs the complete corrupt SCC" ["Demo.A", "Demo.B"] (sort (installWrittenModules repaired))
    assertEqual "does not reuse a corrupt SCC" [] (installReusedModules repaired)

test_installTimingOutput :: IO SeedStore -> Assertion
test_installTimingOutput getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/keep-grin"
  withSandbox getStore "aihc-install-timings" $ \sandbox -> do
    verboseStore <- sandboxStore sandbox "verbose"
    timingStore <- sandboxStore sandbox "timings"
    let baseOptions = InstallOptions fixtureRoot Nothing False False False False False True False False AppleArm64
    verboseOutput <-
      captureInstallOutput baseOptions {installStoreRoot = Just verboseStore, installVerbose = True}
    timingOutput <-
      captureInstallOutput baseOptions {installStoreRoot = Just timingStore, installPrintTimings = True}
    assertBool "verbose output contains an installation step" ("Read Cabal package:" `isInfixOf` verboseOutput)
    assertBool "verbose output does not contain timings" (not ("Compile time:" `isInfixOf` verboseOutput))
    assertBool
      "timing output contains the stage symbols"
      ("▁=parse ▂=resolve ▄=type-check █=backend .=idle" `isInfixOf` timingOutput)
    assertBool "timing output contains frontend time" ("Frontend time:" `isInfixOf` timingOutput)
    assertBool "parse total includes a span" (hasStageSpan "▁ total:" timingOutput)
    assertBool "resolve total includes a span" (hasStageSpan "▂ total:" timingOutput)
    assertBool "type-check total includes a span" (hasStageSpan "▄ total:" timingOutput)
    assertBool "backend total includes a span" (hasStageSpan "█ total:" timingOutput)
    assertBool "timing output contains desugar total" ("desugar total:" `isInfixOf` timingOutput)
    assertBool "timing output contains grin total" ("grin total:" `isInfixOf` timingOutput)
    assertBool "timing output contains native total" ("native total:" `isInfixOf` timingOutput)
    assertBool "timing output contains other total" ("other total:" `isInfixOf` timingOutput)
    assertBool "timing output does not contain verbose output" (not ("Read Cabal package:" `isInfixOf` timingOutput))
    assertBool "redirected timing output does not contain colors" ('\ESC' `notElem` timingOutput)

hasStageSpan :: String -> String -> Bool
hasStageSpan label output =
  any (\line -> label `isInfixOf` line && ", spanning " `isInfixOf` line) (lines output)

-- | Run @install@ with its messages written to a file and return them.
--
-- The messages go through 'installWith' and not through a redirect of the
-- process stdout. The tests run in parallel, and the test runner writes its
-- progress to stdout while this test runs. On a terminal that progress
-- contains escape sequences, so a redirect would capture them at random.
captureInstallOutput :: InstallOptions -> IO String
captureInstallOutput options =
  withTempDir "aihc-capture-install" $ \root -> do
    let outputPath = root </> "output"
    withFile outputPath WriteMode $ \outputHandle ->
      void (installWith outputHandle options)
    T.unpack <$> TIO.readFile outputPath

test_installResolveError :: IO SeedStore -> Assertion
test_installResolveError getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/resolve-error"
  expected <- readFile (fixtureRoot </> "expected.txt")
  withSandbox getStore "aihc-install-resolve-error" $ \sandbox -> do
    actual <-
      bracket getNumCapabilities setNumCapabilities $ \_ ->
        forM [1, 2, 4] $ \workers -> do
          setNumCapabilities workers
          storeRoot <- sandboxStore sandbox ("store-" <> show workers)
          let options = InstallOptions fixtureRoot (Just storeRoot) False False False False False False False False AppleArm64
          result <- try (install options) :: IO (Either IOException InstallResult)
          case result of
            Right _ -> assertFailure "expected frontend compilation to fail"
            Left err -> do
              storeEntries <- listDirectory (storeRoot </> nativeTargetStoreDirectory AppleArm64)
              assertBool "failed install leaves no temporary entry" (not (any (".tmp-" `isPrefixOf`) storeEntries))
              assertBool "failed install leaves no package entry" (not (any ("demo-" `isPrefixOf`) storeEntries))
              pure (T.unpack (T.replace (T.pack fixtureRoot) "<PACKAGE>" (T.pack (ioeGetErrorString err))))
    mapM_ (assertEqual "formatted frontend errors" expected) actual

findFixtureRoot :: FilePath -> IO FilePath
findFixtureRoot fixture = do
  configuredRoot <- lookupEnv "AIHC_TEST_ROOT"
  case configuredRoot of
    Just root -> validate (root </> fixture)
    Nothing -> getCurrentDirectory >>= findUp
  where
    validate candidate = do
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else assertFailure ("could not find fixture " <> candidate)
    findUp directory = do
      let candidate = directory </> fixture
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then assertFailure ("could not find fixture " <> fixture)
            else findUp parent

test_installKeepGrin :: IO SeedStore -> Assertion
test_installKeepGrin getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/keep-grin"
  withSandbox getStore "aihc-install-keep-grin" $ \sandbox -> do
    withoutStore <- sandboxStore sandbox "without"
    withStore <- sandboxStore sandbox "with"
    noCodeStore <- sandboxStore sandbox "no-code"
    withoutGrin <- install (InstallOptions fixtureRoot (Just withoutStore) False False False False False False False False AppleArm64)
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "core")
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "grin")
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "cps.grin")
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "gc.grin")
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "Demo.o.lir")
    retained <- install (InstallOptions fixtureRoot (Just withStore) True True False False False False False False AppleArm64)
    let corePath = installStorePath retained </> "Demo" </> "core"
        grinPath = installStorePath retained </> "Demo" </> "grin"
        cpsGrinPath = installStorePath retained </> "Demo" </> "cps.grin"
        gcGrinPath = installStorePath retained </> "Demo" </> "gc.grin"
    assertFileExists grinPath
    assertFileExists cpsGrinPath
    assertFileExists gcGrinPath
    originalCore <- readFile corePath
    removeFile cpsGrinPath
    removeFile gcGrinPath
    repaired <- install (InstallOptions fixtureRoot (Just withStore) True True False False True False False False AppleArm64)
    assertFileExists grinPath
    assertFileExists cpsGrinPath
    assertFileExists gcGrinPath
    repairedCore <- readFile corePath
    assertEqual "GRIN repair keeps Core" originalCore repairedCore
    assertEqual "GRIN repair writes the module" ["Demo"] (installWrittenModules repaired)
    noCode <-
      install
        (InstallOptions fixtureRoot (Just noCodeStore) True True True True False True False False AppleArm64)
    let noCodeRoot = installStorePath noCode
    assertFileExists (noCodeRoot </> "Demo" </> "resolve.cbor")
    assertFileExists (noCodeRoot </> "Demo" </> "type.cbor")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "core")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "grin")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "cps.grin")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "gc.grin")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "Demo.o")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "Demo.o.lir")
    assertFileDoesNotExist (noCodeRoot </> "lib" </> "libdemo.a")

-- | The suffix the backend adds beside the object file it emits.
nativeArtifactExtension :: NativeTarget -> FilePath
nativeArtifactExtension target =
  case target of
    AppleArm64 -> ".lir"
    LinuxAmd64 -> ".lir"
    Llvm -> ".ll"
    Wasm32Wasip3 -> ".s"

test_installTargetArchives :: IO SeedStore -> Assertion
test_installTargetArchives getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/keep-grin"
  -- The seed store is populated for exactly these targets.
  targets <- installTestTargets
  withSandbox getStore "aihc-install-targets" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    results <- forM targets $ \target -> do
      let directory = nativeTargetStoreDirectory target
          nativeExtension = nativeArtifactExtension target
      result <- install (InstallOptions fixtureRoot (Just storeRoot) True False True False False False False False target)
      let objectPath = installStorePath result </> "Demo" </> "Demo.o"
          nativePath = objectPath <> nativeExtension
          corePath = installStorePath result </> "Demo" </> "core"
          archivePath = installStorePath result </> "lib" </> "libdemo.a"
      assertEqual "target store directory" directory (takeFileName (takeDirectory (installStorePath result)))
      assertFileExists objectPath
      assertFileExists nativePath
      assertFileExists archivePath
      objectHeader <- BS.take 4 <$> BS.readFile objectPath
      case target of
        AppleArm64 -> do
          assertEqual "Mach-O object header" (BS.pack [0xcf, 0xfa, 0xed, 0xfe]) objectHeader
          assertFileDoesNotExist (objectPath <> ".s")
        LinuxAmd64 -> do
          assertEqual "ELF object header" (BS.pack [0x7f, 0x45, 0x4c, 0x46]) objectHeader
          assertFileDoesNotExist (objectPath <> ".s")
        _ -> pure ()
      members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", archivePath] ""
      assertEqual ("archive members for " <> show target) ["Demo.o"] members
      originalCore <- readFile corePath
      removeFile nativePath
      repaired <- install (InstallOptions fixtureRoot (Just storeRoot) True False True False True False False False target)
      assertFileExists nativePath
      repairedCore <- readFile corePath
      assertEqual "native output repair keeps Core" originalCore repairedCore
      assertEqual "native output repair writes the module" ["Demo"] (installWrittenModules repaired)
      pure result
    case results of
      [] -> assertFailure "no target results"
      first : rest -> do
        assertBool
          "targets preserve package identity"
          (all ((== takeFileName (installStorePath first)) . takeFileName . installStorePath) rest)
        firstFingerprint <- BS.readFile (installStorePath first </> "build-inputs.hash")
        forM_ rest $ \result -> do
          fingerprint <- BS.readFile (installStorePath result </> "build-inputs.hash")
          assertBool "targets have separate build fingerprints" (firstFingerprint /= fingerprint)

assertCoreFile :: FilePath -> Assertion
assertCoreFile path = do
  assertFileExists path
  core <- TIO.readFile path
  case Fc.parseProgram core of
    Left parseError -> assertFailure ("invalid Core file " <> path <> ": " <> Fc.renderParseError parseError)
    Right _ -> pure ()

test_installArchSourceDirs :: IO SeedStore -> Assertion
test_installArchSourceDirs getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/arch-source-dirs"
  targets <- installTestTargets
  withSandbox getStore "aihc-install-arch-source-dirs" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    forM_ targets $ \target -> do
      result <- install (InstallOptions fixtureRoot (Just storeRoot) True False False False False False False False target)
      core <- readFile (installStorePath result </> "Payload" </> "core")
      let expected = archSourceDirPayload target
          unexpected = if expected == "32#" then "64#" else "32#"
      assertBool
        ("Core for " <> show target <> " contains " <> expected)
        (expected `isInfixOf` core)
      assertBool
        ("Core for " <> show target <> " does not contain " <> unexpected)
        (not (unexpected `isInfixOf` core))

archSourceDirPayload :: NativeTarget -> String
archSourceDirPayload target =
  case target of
    Wasm32Wasip3 -> "32#"
    _ -> "64#"

test_installCSources :: IO SeedStore -> Assertion
test_installCSources getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/c-sources"
  withSandbox getStore "aihc-install-c-sources" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    result <- install (InstallOptions fixtureRoot (Just storeRoot) False False False False False False False False AppleArm64)
    let archivePath = installStorePath result </> "lib" </> "libdemo.a"
    assertFileExists archivePath
    members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", archivePath] ""
    assertEqual "archive members" ["Demo.o", "cbits_helper.o"] (sort members)
    symbols <- readProcess "nm" [archivePath] ""
    assertBool "archive defines the C symbol" ("aihc_c_add" `isInfixOf` symbols)

-- An API standin such as aihc-internal has only empty modules, so nothing
-- goes into its archive. BSD ar refuses to create an archive with no
-- members, so the install writes the bare archive header instead.
test_installEmptyArchive :: IO SeedStore -> Assertion
test_installEmptyArchive getStore =
  withSandbox getStore "aihc-install-empty-archive" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src"
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010"
          ]
      )
    writeFile (sourceDir </> "Demo.hs") "module Demo () where\n"
    result <- install (InstallOptions sourceRoot (Just storeRoot) False False False False False False False False AppleArm64)
    let archivePath = installStorePath result </> "lib" </> "libdemo.a"
    assertFileExists archivePath
    members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", archivePath] ""
    assertEqual "archive members" [] members

-- A file that guards code on @MIN_VERSION_base@ must see the version of
-- the aihc-base it is compiled against, not an unconditional yes. The wrong
-- branch here is not Haskell, so the install only succeeds when the macro
-- compares honestly.
test_installMinVersionMacros :: IO SeedStore -> Assertion
test_installMinVersionMacros getStore =
  withSandbox getStore "aihc-install-min-version-macros" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src"
        baseVersion = maybe [] bootLibraryVersion (lookupBootLibrary "base" emulatedGhc)
        (major, minor) = case baseVersion of
          a : b : _ -> (a, b)
          _ -> error "the emulated release has no base version"
        guardOn a b = "MIN_VERSION_base(" <> show a <> "," <> show b <> ",0)"
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo",
            "  hs-source-dirs: src",
            "  build-depends: base",
            "  default-language: Haskell2010",
            "  default-extensions: CPP"
          ]
      )
    writeFile
      (sourceDir </> "Demo.hs")
      ( unlines
          [ "module Demo (current) where",
            "#if " <> guardOn major minor <> " && !" <> guardOn major (minor + 1) <> " && !MIN_VERSION_GLASGOW_HASKELL(99,0,0,0)",
            "current = ()",
            "#else",
            "this is not haskell (",
            "#endif"
          ]
      )
    result <- install (InstallOptions sourceRoot (Just storeRoot) False False False False False False False False AppleArm64)
    assertEqual "written modules" ["Demo"] (installWrittenModules result)

-- Every standin under core-libs claims the version of the boot library it
-- replaces, and the emulated release is the single source of that version.
test_coreLibsMatchRelease :: Assertion
test_coreLibsMatchRelease =
  forM_ coreProviders $ \provider -> do
    sourcePath <- coreProviderSourcePath provider
    cabalFiles <- filter ((== ".cabal") . takeExtension) <$> listDirectory sourcePath
    cabalFile <- case cabalFiles of
      [file] -> pure (sourcePath </> file)
      _ -> assertFailure ("expected one .cabal file under " <> sourcePath)
    contents <- readFile cabalFile
    let declared = [dropWhile isSpace rest | line <- lines contents, Just rest <- [stripPrefix "version:" line]]
    assertEqual (coreProviderName provider <> " version") [coreProviderVersion provider] declared

test_installFcCcall :: IO SeedStore -> Assertion
test_installFcCcall getStore =
  withSandbox getStore "aihc-install-fc-ccall" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src"
        options = InstallOptions sourceRoot (Just storeRoot) True False False False False False False False AppleArm64
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010",
            "  default-extensions: ForeignFunctionInterface, MagicHash"
          ]
      )
    writeFile
      (sourceDir </> "Demo.hs")
      "module Demo where\nimport GHC.Prim (Int#)\ndata Int = I# Int#\nforeign import ccall unsafe \"foo\" foo :: Int -> Int\n"
    result <- install options
    assertCoreFile (installStorePath result </> "Demo" </> "core")

test_installAihcPrim :: Assertion
test_installAihcPrim = do
  aihcPrimRoot <- findAihcPrimRoot
  withTempDir "aihc-install-aihc-prim" $ \root -> do
    let storeRoot = root </> "store"
        targetStoreRoot = storeRoot </> nativeTargetStoreDirectory AppleArm64
        options = InstallOptions aihcPrimRoot (Just storeRoot) True True False True False False False False AppleArm64
    createDirectoryIfMissing True storeRoot
    caught <- try (install options) :: IO (Either IOException InstallResult)
    result <- case caught of
      Left err -> assertFailure ("install aihc-prim failed: " <> show err)
      Right value -> pure value
    manifest <- readPackageManifest (packageManifestPath (installStorePath result)) >>= either assertFailure pure
    let packageDir = installStorePath result
        packageId = PackageId (packageManifestUnitId manifest)
        loader requested =
          Fc.storeModuleLoader
            targetStoreRoot
            (if requested == packageId then PackageId (T.pack (takeFileName packageDir)) else requested)
    assertEqual "installed artifacts use the selected package identity" (packageManifestIdentity manifest) (packageManifestUnitId manifest)
    mapM_ (assertTypeArtifactSize packageDir) ["GHC.Tuple", "GHC.Types"]
    mapM_ (assertModuleCore packageDir) aihcPrimLibraryModules
    coreFiles <- listNamedFiles packageDir "core"
    mapM_ assertCoreFile coreFiles
    grinFiles <- listNamedFiles packageDir "grin"
    assertEqual "one GRIN file for each Core file" (length coreFiles) (length grinFiles)
    types <- loadStoredFc loader packageId "GHC.Types"
    prim <- loadStoredFc loader packageId "GHC.Prim"
    assertEqual "GHC.Types lint errors" [] (Fc.lintProgram types)
    assertEqual "GHC.Prim lint errors" [] (Fc.lintProgram prim)
    mapM_ (assertModuleClosureLints loader packageId) (filter (`notElem` ["GHC.Types", "GHC.Prim"]) aihcPrimLibraryModules)

assertTypeArtifactSize :: FilePath -> Text -> Assertion
assertTypeArtifactSize packageDir name = do
  let path = foldl (</>) packageDir (map T.unpack (T.splitOn "." name) ++ ["type.cbor"])
  size <- getFileSize path
  assertBool ("type artifact is less than 1 MiB: " <> path) (size < 1024 * 1024)

aihcPrimLibraryModules :: [Text]
aihcPrimLibraryModules =
  [ "GHC.CString",
    "GHC.Classes",
    "GHC.Debug",
    "GHC.Magic",
    "GHC.Magic.Dict",
    "GHC.Prim",
    "GHC.Prim.Exception",
    "GHC.Prim.Ext",
    "GHC.Prim.Panic",
    "GHC.Prim.PtrEq",
    "GHC.Prim.Unicode",
    "GHC.PrimopWrappers",
    "GHC.Tuple",
    "GHC.Types"
  ]

findAihcPrimRoot :: IO FilePath
findAihcPrimRoot = do
  envRoot <- lookupEnv "AIHC_PRIM_SRC"
  case envRoot of
    Just root -> do
      cabalExists <- doesFileExist (root </> "aihc-prim.cabal")
      if cabalExists
        then pure root
        else assertFailure ("AIHC_PRIM_SRC has no aihc-prim.cabal: " <> root)
    Nothing -> do
      cwd <- getCurrentDirectory
      findUp cwd
  where
    findUp dir = do
      let candidate = dir </> "core-libs" </> "aihc-prim"
      cabalExists <- doesFileExist (candidate </> "aihc-prim.cabal")
      if cabalExists
        then pure candidate
        else do
          let parent = takeDirectory dir
          if parent == dir
            then assertFailure ("could not find core-libs/aihc-prim from " <> dir)
            else findUp parent

moduleCorePath :: FilePath -> Text -> FilePath
moduleCorePath packageDir moduleName =
  foldl (</>) packageDir (map T.unpack (T.splitOn "." moduleName) ++ ["core"])

assertModuleCore :: FilePath -> Text -> Assertion
assertModuleCore packageDir moduleName =
  assertFileExists (moduleCorePath packageDir moduleName)

loadStoredFc :: Fc.ModuleLoader -> PackageId -> Text -> IO Fc.Program
loadStoredFc loader packageId moduleName = do
  loaded <- loader packageId moduleName
  case loaded of
    Nothing -> assertFailure ("store loader did not find " <> T.unpack moduleName)
    Just program -> pure program

assertModuleClosureLints :: Fc.ModuleLoader -> PackageId -> Text -> Assertion
assertModuleClosureLints loader packageId moduleName = do
  program <- loadStoredFc loader packageId moduleName
  assertEqual
    (T.unpack moduleName <> " lint errors")
    []
    (Fc.lintProgram program)

listNamedFiles :: FilePath -> FilePath -> IO [FilePath]
listNamedFiles root name = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- listDirectory root
      concat <$> mapM (go . (root </>)) entries
  where
    go path = do
      isDir <- doesDirectoryExist path
      if isDir
        then listNamedFiles path name
        else
          if takeFileName path == name
            then pure [path]
            else pure []

test_installTypeWarning :: IO SeedStore -> Assertion
test_installTypeWarning getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/type-warning"
  withSandbox getStore "aihc-install-type-warning" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let options = InstallOptions fixtureRoot (Just storeRoot) False False False False False True False False AppleArm64
    result <- install options
    assertEqual "warning does not prevent installation" ["Demo"] (installWrittenModules result)

test_installImplicitPrelude :: IO SeedStore -> Assertion
test_installImplicitPrelude getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/implicit-prelude"
  withSandbox getStore "aihc-install-implicit-prelude" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = fixtureRoot </> "demo"
        options = InstallOptions sourceRoot (Just storeRoot) False False False False False True False False AppleArm64
    result <- install options
    assertEqual "implicit Prelude user" ["Demo"] (installWrittenModules result)

test_installTypeReexports :: IO SeedStore -> Assertion
test_installTypeReexports getStore =
  withSandbox getStore "aihc-install-type-reexports" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallOptions sourceRoot (Just storeRoot) False False False False False False False False AppleArm64
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo.A, Demo.B",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010"
          ]
      )
    writeFile
      (sourceDir </> "A.hs")
      "module Demo.A where\ndata Box a = Box a\nclass Identity a where\n  identity :: a -> a\nfn x = x\n"
    writeFile (sourceDir </> "B.hs") "module Demo.B (module Demo.A) where\nimport Demo.A\n"
    result <- install options
    bytes <- BL.readFile (installStorePath result </> "Demo" </> "B" </> "type.cbor")
    let artifact = decodeTypeArtifact bytes
    let termNames = mapMaybe (tcTermKeyIdentifier . fst) (tcInterfaceTerms (typeArtifactInterface artifact))
    assertBool "re-exported signature" ("fn" `elem` termNames)

test_installLocalDependencies :: IO SeedStore -> Assertion
test_installLocalDependencies getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/local-dependencies"
  withSandbox getStore "aihc-install-local-dependencies" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = fixtureRoot </> "demo"
        options = InstallOptions sourceRoot (Just storeRoot) False False False False False False False False AppleArm64
    _ <- install options
    let targetStoreRoot = storeRoot </> nativeTargetStoreDirectory AppleArm64
    storeEntries <- listDirectory targetStoreRoot
    assertBool "temporary store directories are absent" (not (any (".tmp-" `isPrefixOf`) storeEntries))
    let dependencyStores = filter (== "dep-1.0.0") storeEntries
    case dependencyStores of
      [dependencyStore] -> do
        let dependencyStoreRoot = targetStoreRoot </> dependencyStore
            unusedTypePath = dependencyStoreRoot </> "Dep" </> "Unused" </> "type.cbor"
            sentinelPath = dependencyStoreRoot </> "reinstall-sentinel"
        assertFileExists (dependencyStoreRoot </> "Dep" </> "resolve.cbor")
        assertFileExists (dependencyStoreRoot </> "Dep" </> "type.cbor")
        assertFileExists unusedTypePath
        BS.writeFile unusedTypePath "invalid unused type artifact"
        writeFile sentinelPath "dependency was not reinstalled"
        reinstalled <- install options {installReinstall = True}
        assertEqual "reinstall writes the specified package" ["Demo"] (installWrittenModules reinstalled)
        assertFileExists sentinelPath
        unusedTypeBytes <- BS.readFile unusedTypePath
        assertEqual "reinstall does not read or replace the unused module" "invalid unused type artifact" unusedTypeBytes
      _ -> assertFailure ("expected one installed dependency, got " <> show dependencyStores)

test_installInstanceVisibility :: IO SeedStore -> Assertion
test_installInstanceVisibility getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/instance-visibility"
  withSandbox getStore "aihc-install-instance-visibility" $ \sandbox -> do
    let installFixture source store =
          install
            (InstallOptions (fixtureRoot </> source) (Just store) False False False False False True False False AppleArm64)
    withoutStore <- sandboxStore sandbox "without-store"
    withStore <- sandboxStore sandbox "with-store"
    withoutResult <- try (installFixture "without" withoutStore) :: IO (Either IOException InstallResult)
    case withoutResult of
      Left _ -> pure ()
      Right _ -> assertFailure "an unrelated module supplied an instance"
    _ <- installFixture "with" withStore
    pure ()

assertFileExists :: FilePath -> Assertion
assertFileExists path = do
  exists <- doesFileExist path
  assertBool ("expected file to exist: " <> path) exists

assertFileDoesNotExist :: FilePath -> Assertion
assertFileDoesNotExist path = do
  exists <- doesFileExist path
  assertBool ("expected file not to exist: " <> path) (not exists)

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix <> "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket
    (pure tempFile)
    removeDirectoryRecursive
    action
