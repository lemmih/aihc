module Main (main) where

import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Cpp (builtinCppMacros, injectSyntheticCppMacros)
import Aihc.Hackage.Index (parseHackageIndex, parseHackageIndexUpdatedSince)
import Aihc.Hackage.Release (GhcRelease (..), emulatedGhc, showVersionBranch)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.VersionResolver (parsePreferredVersions)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Hedgehog (Property, property, success)
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain . testGroup "aihc-hackage" $
    [ testCase "parses latest package versions from Hackage index tarball" $ do
        parseHackageIndex testHackageIndex
          @?= Right
            [ PackageSpec "alpha" "1.2.0",
              PackageSpec "beta" "0.1"
            ],
      testCase "filters Hackage index packages by latest upload time" $ do
        parseHackageIndexUpdatedSince 100 testHackageIndex
          @?= Right
            [ PackageSpec "alpha" "1.2.0"
            ],
      testCase "ignores deprecated versions when reading Hackage preferred versions" $ do
        assertEqual
          "preferred versions"
          ["1.5.2.0", "1.5.1.0"]
          (map prettyShow (parsePreferredVersions deprecatedPreferredVersions)),
      testCase "reads preferred versions when nothing is deprecated" $ do
        assertEqual
          "preferred versions"
          ["0.8.11", "0.8.10"]
          (map prettyShow (parsePreferredVersions plainPreferredVersions)),
      testCase "treats unparseable preferred version metadata as unknown" $ do
        assertEqual "preferred versions" [] (map prettyShow (parsePreferredVersions (BSC.pack "not json"))),
      testCase "generates Cabal Paths module as a normal source file" test_generatesPathsModule,
      testCase "collects exposed modules from active conditional library branches" test_collectsConditionalExposedModules,
      testCase "evaluates impl(ghc) conditions against the emulated compiler" test_evaluatesImplConditions,
      testCase "defines MIN_VERSION macros from resolved dependency versions" test_minVersionMacros,
      testCase "extracts active build tool dependency names" test_extractsBuildToolDependencyNames,
      testCase "detects packages that default to Haskell98" test_detectsHaskell98DefaultLanguage,
      testCase "ignores inactive Haskell98 default-language branches" test_ignoresInactiveHaskell98DefaultLanguage,
      testCase "detects active custom preprocessor options" test_detectsCustomPreprocessorOptions,
      testCase "collects C sources and compile options from library cabal files" test_collectsCSources,
      testProperty "Hedgehog options" prop_dummy
    ]

-- | Keep the repository Hedgehog options accepted by this test suite.
prop_dummy :: Property
prop_dummy = property success

(@?=) :: (Eq a, Show a) => Either String a -> Either String a -> Assertion
actual @?= expected =
  if actual == expected
    then pure ()
    else assertFailure ("expected: " <> show expected <> "\n but got: " <> show actual)

testHackageIndex :: LBS.ByteString
testHackageIndex =
  GZip.compress $
    Tar.write
      [ cabalEntryAt "alpha/1.0.0/alpha.cabal" 20,
        cabalEntryAt "alpha/1.2.0/alpha.cabal" 100,
        cabalEntryAt "alpha/1.1.0/alpha.cabal" 200,
        cabalEntryAt "beta/0.1/beta.cabal" 99,
        cabalEntry "beta/0.2/not-beta.cabal",
        cabalEntry "preferred-versions"
      ]
  where
    cabalEntry path =
      cabalEntryAt path 0

    cabalEntryAt path uploadedAt =
      case Tar.toTarPath False path of
        Left err -> error ("invalid test tar path: " <> show err)
        Right tarPath ->
          let contents = LBS.fromStrict (BSC.pack "name: ignored\n")
           in (Tar.simpleEntry tarPath (Tar.NormalFile contents (LBS.length contents))) {Tar.entryTime = uploadedAt}

test_generatesPathsModule :: Assertion
test_generatesPathsModule =
  withTempDir "aihc-hackage-paths" $ \root -> do
    let cabalFile = root </> "paths-demo.cabal"
        srcDir = root </> "src"
        sourceFile = srcDir </> "PathsUser.hs"
    createDirectoryIfMissing True srcDir
    writeFile cabalFile pathsDemoCabal
    writeFile sourceFile pathsUserSource

    cabalBytes <- BS.readFile cabalFile
    gpd <-
      case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
        Right parsed -> pure parsed
        Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

    files <- HC.collectComponentFiles gpd root
    let paths = map HC.fileInfoPath files
        generated = root </> ".aihc-autogen" </> "Paths_paths_demo.hs"
    assertBool "expected package source module to be selected" (any ("src/PathsUser.hs" `isSuffixOf`) paths)
    assertBool "expected generated Paths module to be selected" (generated `elem` paths)

    generatedSource <- readFile generated
    assertBool "expected Cabal module header" ("module Paths_paths_demo" `isInfixOf` generatedSource)
    assertBool "expected version export" ("version :: Version" `isInfixOf` generatedSource)
    assertBool "expected getDataDir export" ("getDataDir" `isInfixOf` generatedSource)
    assertBool "expected getDataFileName export" ("getDataFileName :: FilePath -> IO FilePath" `isInfixOf` generatedSource)

    case filter ((== generated) . HC.fileInfoPath) files of
      [info] -> assertEqual "expected generated module to depend on base" [T.pack "base"] (HC.fileInfoDependencies info)
      _ -> assertFailure "expected exactly one FileInfo for generated Paths module"

test_collectsConditionalExposedModules :: Assertion
test_collectsConditionalExposedModules =
  withTempDir "aihc-hackage-conditional-exposed" $ \root -> do
    let cabalFile = root </> "conditional-exposed.cabal"
        srcDir = root </> "src" </> "Control" </> "Category"
        sourceFile = srcDir </> "Unicode.hs"
    createDirectoryIfMissing True srcDir
    writeFile cabalFile conditionalExposedCabal
    writeFile sourceFile "module Control.Category.Unicode where\n"

    cabalBytes <- BS.readFile cabalFile
    gpd <-
      case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
        Right parsed -> pure parsed
        Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

    files <- HC.collectComponentFiles gpd root
    let paths = map HC.fileInfoPath files
    assertBool "expected conditionally exposed module to be selected" (any ("src/Control/Category/Unicode.hs" `isSuffixOf`) paths)

-- A package that guards a @build-depends@ on @impl(ghc >= X)@ must take the
-- same branch as its @#if __GLASGOW_HASKELL__@ does, whatever compiler built
-- aihc itself. Both read the emulated release.
test_evaluatesImplConditions :: Assertion
test_evaluatesImplConditions = do
  let compilerVersion = releaseCompilerVersion emulatedGhc
      major = take 2 compilerVersion
      next = init major ++ [last major + 2]
      cabalSource = implConditionalCabal (showVersionBranch major) (showVersionBranch next)
  gpd <- parseTestCabal cabalSource
  assertEqual "exposed modules" ["Current.Branch"] (map T.unpack (HC.collectLibraryExposedModules gpd))
  let macro name = T.unpack <$> Map.lookup (T.pack name) builtinCppMacros
  assertEqual "__GLASGOW_HASKELL__" (Just (show (head major * 100 + major !! 1))) (macro "__GLASGOW_HASKELL__")
  assertEqual "__GLASGOW_HASKELL_FULL_VERSION__" (Just (show (showVersionBranch compilerVersion))) (macro "__GLASGOW_HASKELL_FULL_VERSION__")

-- The synthetic @MIN_VERSION_*@ macros compare against the resolved
-- dependency versions the way Cabal's @cabal_macros.h@ does.
test_minVersionMacros :: Assertion
test_minVersionMacros = do
  let versions = Map.fromList [(T.pack "base", [4, 21, 2, 0])]
      injected = injectSyntheticCppMacros [] versions (map T.pack ["base", "unknown-pkg"]) (T.pack "body\n")
      macroLines = T.lines injected
      hasLine expected = assertBool expected (T.pack expected `elem` macroLines)
  hasLine "#define VERSION_base \"4.21.2.0\""
  hasLine "#define MIN_VERSION_base(major1,major2,minor) ((major1) < 4 || (major1) == 4 && (major2) < 21 || (major1) == 4 && (major2) == 21 && (minor) <= 2)"
  hasLine "#define MIN_VERSION_unknown_pkg(major1,major2,minor) 1"
  let compilerVersion = releaseCompilerVersion emulatedGhc
  hasLine ("#define MIN_VERSION_ghc(major1,major2,minor) " <> atLeastThree compilerVersion)
  assertBool "MIN_VERSION_GLASGOW_HASKELL is a comparison" (any (T.pack "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) (" `T.isPrefixOf`) macroLines)
  assertEqual "source follows the macros" (Just (T.pack "body")) (Just (last macroLines))
  where
    atLeastThree version =
      let (a, b, c) = case take 3 (version ++ repeat 0) of
            [x, y, z] -> (show x, show y, show z)
            _ -> error "unreachable"
       in "((major1) < " <> a <> " || (major1) == " <> a <> " && (major2) < " <> b <> " || (major1) == " <> a <> " && (major2) == " <> b <> " && (minor) <= " <> c <> ")"

test_extractsBuildToolDependencyNames :: Assertion
test_extractsBuildToolDependencyNames = do
  gpd <- parseTestCabal buildToolDependsCabal
  assertEqual
    "expected active modern and legacy build tools"
    (sort ["alex", "genprimopcode"])
    (sort (map T.unpack (HC.buildToolDependencyNames gpd)))

test_detectsHaskell98DefaultLanguage :: Assertion
test_detectsHaskell98DefaultLanguage = do
  explicit <- parseTestCabal haskell98DefaultLanguageCabal
  missing <- parseTestCabal missingDefaultLanguageCabal
  supported <- parseTestCabal haskell2010DefaultLanguageCabal
  assertBool "explicit Haskell98 default-language is unsupported" (HC.packageDefaultsToHaskell98 explicit)
  assertBool "missing default-language falls back to Haskell98" (HC.packageDefaultsToHaskell98 missing)
  assertBool "Haskell2010 default-language is supported" (not (HC.packageDefaultsToHaskell98 supported))

test_ignoresInactiveHaskell98DefaultLanguage :: Assertion
test_ignoresInactiveHaskell98DefaultLanguage = do
  gpd <- parseTestCabal inactiveHaskell98DefaultLanguageCabal
  assertBool "inactive Haskell98 branch should not filter the package" (not (HC.packageDefaultsToHaskell98 gpd))

test_collectsCSources :: Assertion
test_collectsCSources = do
  gpd <- parseTestCabal cSourcesCabal
  let info = HC.collectLibraryCCompileInfo gpd "/pkg"
  assertEqual
    "expected C sources from the cabal file"
    ["/pkg/cbits/helper.c"]
    (HC.cCompileSources info)
  assertEqual
    "expected include directories from the cabal file"
    ["/pkg/cbits"]
    (HC.cCompileIncludeDirs info)
  assertEqual
    "expected cc-options from the cabal file"
    ["-std=c11"]
    (HC.cCompileCcOptions info)
  assertBool
    "inactive javascript C source is not selected"
    (not (any ("js.c" `isSuffixOf`) (HC.cCompileSources info)))

test_detectsCustomPreprocessorOptions :: Assertion
test_detectsCustomPreprocessorOptions = do
  hsp <- parseTestCabal customPreprocessorCabal
  inactive <- parseTestCabal inactiveCustomPreprocessorCabal
  pgmFOnly <- parseTestCabal pgmFWithoutFPreprocessorCabal
  assertBool "expected -F with -pgmF to require filtering" (HC.packageUsesCustomPreprocessor hsp)
  assertBool "expected inactive custom preprocessor options to be ignored" (not (HC.packageUsesCustomPreprocessor inactive))
  assertBool "expected -pgmF without -F not to enable preprocessing" (not (HC.packageUsesCustomPreprocessor pgmFOnly))

parseTestCabal :: String -> IO GenericPackageDescription
parseTestCabal source =
  case snd (runParseResult (parseGenericPackageDescription (BSC.pack source))) of
    Right parsed -> pure parsed
    Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

pathsDemoCabal :: String
pathsDemoCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: paths-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: PathsUser",
      "  autogen-modules: Paths_paths_demo",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010"
    ]

pathsUserSource :: String
pathsUserSource =
  unlines
    [ "module PathsUser where",
      "import Paths_paths_demo (version, getDataDir, getDataFileName)",
      "pathsVersion = version",
      "pathsDataDir = getDataDir",
      "pathsDataFileName = getDataFileName"
    ]

implConditionalCabal :: String -> String -> String
implConditionalCabal current next =
  unlines
    [ "cabal-version: 3.0",
      "name: impl-conditional",
      "version: 0.1.0.0",
      "",
      "library",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if impl(ghc >= " <> next <> ")",
      "    exposed-modules: Future.Branch",
      "  elif impl(ghc >= " <> current <> ")",
      "    exposed-modules: Current.Branch",
      "  else",
      "    exposed-modules: Old.Branch"
    ]

conditionalExposedCabal :: String
conditionalExposedCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: conditional-exposed",
      "version: 0.1.0.0",
      "",
      "flag old-base",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if flag(old-base)",
      "    build-depends: base >= 3.0 && < 3.0.3.1",
      "  else",
      "    exposed-modules: Control.Category.Unicode",
      "    build-depends: base >= 3.0.3.1 && < 5"
    ]

buildToolDependsCabal :: String
buildToolDependsCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: build-tool-demo",
      "version: 0.1.0.0",
      "",
      "flag generated",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: BuildToolDemo",
      "  hs-source-dirs: src",
      "  build-tool-depends: genprimopcode:genprimopcode >= 0",
      "  build-tools: alex >= 3",
      "  default-language: Haskell2010",
      "  if flag(generated)",
      "    build-tool-depends: inactive-tool:inactive-tool >= 0"
    ]

haskell98DefaultLanguageCabal :: String
haskell98DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: haskell98-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Haskell98Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell98"
    ]

missingDefaultLanguageCabal :: String
missingDefaultLanguageCabal =
  unlines
    [ "cabal-version: 1.10",
      "name: missing-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: MissingLanguage",
      "  hs-source-dirs: src"
    ]

haskell2010DefaultLanguageCabal :: String
haskell2010DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: haskell2010-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Haskell2010Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010"
    ]

inactiveHaskell98DefaultLanguageCabal :: String
inactiveHaskell98DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: inactive-haskell98-language",
      "version: 0.1.0.0",
      "",
      "flag legacy",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: InactiveHaskell98Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if flag(legacy)",
      "    default-language: Haskell98"
    ]

cSourcesCabal :: String
cSourcesCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: c-sources-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: CSourcesDemo",
      "  hs-source-dirs: src",
      "  c-sources: cbits/helper.c",
      "  include-dirs: cbits",
      "  cc-options: -std=c11",
      "  default-language: Haskell2010",
      "  if arch(javascript)",
      "    c-sources: cbits/js.c"
    ]

customPreprocessorCabal :: String
customPreprocessorCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: custom-preprocessor-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: CustomPreprocessorDemo",
      "  ghc-options: -F -pgmFtrhsx -Wall",
      "  build-depends: base",
      "  default-language: Haskell2010"
    ]

inactiveCustomPreprocessorCabal :: String
inactiveCustomPreprocessorCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: inactive-custom-preprocessor-demo",
      "version: 0.1.0.0",
      "",
      "flag generated",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: InactiveCustomPreprocessorDemo",
      "  build-depends: base",
      "  default-language: Haskell2010",
      "  if flag(generated)",
      "    ghc-options: -F -pgmFtrhsx"
    ]

pgmFWithoutFPreprocessorCabal :: String
pgmFWithoutFPreprocessorCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: pgmf-without-f-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: PgmFWithoutFDemo",
      "  ghc-options: -pgmFtrhsx -Wall",
      "  build-depends: base",
      "  default-language: Haskell2010"
    ]

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix ++ "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket
    (pure tempFile)
    removeDirectoryRecursive
    action

-- | Hackage preferred-version metadata where the newest upload is deprecated.
deprecatedPreferredVersions :: BS.ByteString
deprecatedPreferredVersions =
  BSC.pack "{\"deprecated-version\":[\"1.6.0.0\"],\"normal-version\":[\"1.5.2.0\",\"1.5.1.0\"]}"

-- | Hackage preferred-version metadata without any deprecated versions.
plainPreferredVersions :: BS.ByteString
plainPreferredVersions =
  BSC.pack "{\"normal-version\":[\"0.8.11\",\"0.8.10\"]}"
