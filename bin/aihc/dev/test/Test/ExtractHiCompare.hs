{-# LANGUAGE OverloadedStrings #-}

module Test.ExtractHiCompare
  ( extractHiCompareTests,
    localPackageSubsetTests,
  )
where

import Aihc.Dev.ExtractHi (extractPackage, extractSourcePackage)
import Aihc.Dev.ExtractHi.Compare
  ( CompatibilityReport (..),
    CoreLibProgressReport (..),
    InterfaceMismatch (..),
    comparePackageCompatibility,
    comparePackageSubset,
    coreLibApiDivergences,
    renderCoreLibProgressReports,
    runCoreLibApiDivergences,
  )
import Aihc.Dev.ExtractHi.Types
import Control.Exception (IOException, bracket, try)
import Data.List qualified as List
import Data.Text qualified as T
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

extractHiCompareTests :: TestTree
extractHiCompareTests =
  testGroup
    "extract-hi compare"
    [ testCase "accepts empty candidate module exports" test_acceptsEmptyCandidateModuleExports,
      testCase "rejects candidate module missing from oracle" test_rejectsMissingModule,
      testCase "rejects changed value type" test_rejectsChangedValueType,
      testGroup
        "core-libs-progress"
        [ testCase "counts matching exported interface facts" test_coreLibProgressCountsMatches,
          testCase "extracts source package exports" test_coreLibProgressExtractsSourcePackage,
          testCase "rejects exported source values without signatures" test_coreLibProgressRejectsMissingSourceSignature,
          testCase "resolves base re-exported value types" test_coreLibProgressResolvesReexportedTypes,
          testCase "counts missing candidate exports as failures" test_coreLibProgressCountsMissingExports,
          testCase "rejects changed value types and type kinds" test_coreLibProgressRejectsChangedSignatures,
          testCase "requires exports to come from the same module" test_coreLibProgressRequiresSameModule,
          testCase "counts candidate-only exports separately" test_coreLibProgressCountsExtrasSeparately,
          testCase "renders stable command output" test_coreLibProgressRendersStableOutput
        ],
      testGroup
        "core-libs-api"
        [ testCase "allows candidate-only modules to export anything" test_apiDivergenceAllowsNewModules,
          testCase "rejects names that the oracle module does not export" test_apiDivergenceRejectsExtraNames,
          testCase "compares names across namespaces and signatures" test_apiDivergenceIgnoresNamespaces,
          testCase "rejects fixities the oracle does not declare identically" test_apiDivergenceRejectsFixities,
          testCase "skips oracle modules without extracted exports" test_apiDivergenceSkipsEmptyOracleModules,
          testCase "aihc-prim and aihc-base only have the known divergences" test_coreLibsHaveOnlyKnownDivergences
        ]
    ]

-- | Subset checks against packages that must be registered with @ghc-pkg@.
localPackageSubsetTests :: TestTree
localPackageSubsetTests =
  testGroup
    "extract-hi local package subsets"
    [ testCase "aihc-internal is a subset of ghc-internal" test_aihcInternalSubset,
      testCase "aihc-template-haskell is a subset of template-haskell" test_aihcTemplateHaskellSubset
    ]

test_acceptsEmptyCandidateModuleExports :: Assertion
test_acceptsEmptyCandidateModuleExports =
  assertEqual
    "mismatches"
    []
    (comparePackageSubset (pkg [emptyModule "GHC.Tuple"]) (pkg [moduleWithValue "GHC.Tuple" "Solo" "Solo a"]))

test_rejectsMissingModule :: Assertion
test_rejectsMissingModule =
  assertBool "expected missing module mismatch" $
    not (null (comparePackageSubset (pkg [emptyModule "Missing"]) (pkg [emptyModule "GHC.Tuple"])))

test_rejectsChangedValueType :: Assertion
test_rejectsChangedValueType =
  assertBool "expected changed type mismatch" $
    not (null (comparePackageSubset (pkg [moduleWithValue "GHC.Tuple" "Solo" "Int"]) (pkg [moduleWithValue "GHC.Tuple" "Solo" "Solo a"])))

test_coreLibProgressCountsMatches :: Assertion
test_coreLibProgressCountsMatches =
  assertEqual
    "report"
    (CompatibilityReport 6 6 0 [])
    (comparePackageCompatibility (pkg [fullModule "A"]) (pkg [fullModule "A"]))

test_coreLibProgressExtractsSourcePackage :: Assertion
test_coreLibProgressExtractsSourcePackage =
  withTempDir "core-libs-progress-source" $ \root -> do
    let srcDir = root </> "src" </> "Data"
    createDirectoryIfMissing True srcDir
    writeFile (root </> "demo-base.cabal") demoBaseCabal
    writeFile (srcDir </> "Bool.hs") demoBoolSource
    iface <- extractSourcePackage root "demo-base"
    case piModules iface of
      [modIface] -> do
        assertEqual "module" "Data.Bool" (miModule modIface)
        assertEqual "values" [ExportedValue "&&" "Bool -> Bool -> Bool", ExportedValue "not" "Bool -> Bool"] (miValues modIface)
        assertEqual "types" [ExportedType "Bool" "<unspecified-source-kind>" ["False", "True"]] (miTypes modIface)
        assertEqual "fixities" [FixityInfo "&&" InfixR 3] (miFixities modIface)
      _ -> assertFailure ("expected one source module, got " <> show (piModules iface))

test_coreLibProgressRejectsMissingSourceSignature :: Assertion
test_coreLibProgressRejectsMissingSourceSignature =
  withTempDir "core-libs-progress-source-missing-signature" $ \root -> do
    let srcDir = root </> "src" </> "Data"
    createDirectoryIfMissing True srcDir
    writeFile (root </> "demo-base.cabal") demoBaseCabal
    writeFile (srcDir </> "Bool.hs") demoBoolMissingSignatureSource
    result <- try (extractSourcePackage root "demo-base") :: IO (Either IOException PackageInterface)
    case result of
      Left err ->
        assertBool "mentions missing type signature" ("without type signatures: not" `List.isInfixOf` show err)
      Right iface ->
        assertFailure ("expected missing source signature failure, got " <> show iface)

test_coreLibProgressResolvesReexportedTypes :: Assertion
test_coreLibProgressResolvesReexportedTypes = do
  baseIface <- extractPackage "base"
  dataEither <- findModule "Data.Either" baseIface
  assertEqual
    "Data.Either value types"
    [ ExportedValue "either" "(a -> c) -> (b -> c) -> Either a b -> c",
      ExportedValue "fromLeft" "a -> Either a b -> a",
      ExportedValue "fromRight" "b -> Either a b -> b",
      ExportedValue "isLeft" "Either a b -> Bool",
      ExportedValue "isRight" "Either a b -> Bool",
      ExportedValue "lefts" "[Either a b] -> [a]",
      ExportedValue "partitionEithers" "[Either a b] -> ([a], [b])",
      ExportedValue "rights" "[Either a b] -> [b]"
    ]
    (miValues dataEither)

test_coreLibProgressCountsMissingExports :: Assertion
test_coreLibProgressCountsMissingExports = do
  let report = comparePackageCompatibility (pkg [emptyModule "A"]) (pkg [moduleWithValue "A" "f" "Int"])
  assertEqual "matched" 0 (crMatched report)
  assertEqual "total" 1 (crTotal report)
  assertEqual "extras" 0 (crExtra report)
  assertEqual "mismatches" 1 (length (crMismatches report))

test_coreLibProgressRejectsChangedSignatures :: Assertion
test_coreLibProgressRejectsChangedSignatures = do
  let candidate =
        (emptyModule "A")
          { miValues = [ExportedValue "f" "Bool"],
            miTypes = [ExportedType "T" "Bool" []]
          }
      oracle =
        (emptyModule "A")
          { miValues = [ExportedValue "f" "Int"],
            miTypes = [ExportedType "T" "Type" []]
          }
      report = comparePackageCompatibility (pkg [candidate]) (pkg [oracle])
  assertEqual "matched" 0 (crMatched report)
  assertEqual "total" 2 (crTotal report)
  assertEqual "mismatches" 2 (length (crMismatches report))

test_coreLibProgressRequiresSameModule :: Assertion
test_coreLibProgressRequiresSameModule = do
  let report =
        comparePackageCompatibility
          (pkg [moduleWithValue "B" "f" "Int"])
          (pkg [moduleWithValue "A" "f" "Int"])
  assertEqual "matched" 0 (crMatched report)
  assertEqual "total" 1 (crTotal report)
  assertEqual "extras" 1 (crExtra report)

test_coreLibProgressCountsExtrasSeparately :: Assertion
test_coreLibProgressCountsExtrasSeparately = do
  let candidate =
        (moduleWithValue "A" "f" "Int")
          { miValues =
              [ ExportedValue "f" "Int",
                ExportedValue "extra" "Int"
              ]
          }
      report = comparePackageCompatibility (pkg [candidate]) (pkg [moduleWithValue "A" "f" "Int"])
  assertEqual "matched" 1 (crMatched report)
  assertEqual "total" 1 (crTotal report)
  assertEqual "extras" 1 (crExtra report)
  assertEqual "mismatches" 0 (length (crMismatches report))

test_coreLibProgressRendersStableOutput :: Assertion
test_coreLibProgressRendersStableOutput =
  assertEqual
    "output"
    "GHC_PRIM 1 4 25.00\nBASE 2 5 40.00\nEXTRA ghc-prim 2\nEXTRA base 3\n"
    ( renderCoreLibProgressReports
        [ CoreLibProgressReport "GHC_PRIM" "ghc-prim" (CompatibilityReport 1 4 2 []),
          CoreLibProgressReport "BASE" "base" (CompatibilityReport 2 5 3 [])
        ]
    )

test_apiDivergenceAllowsNewModules :: Assertion
test_apiDivergenceAllowsNewModules =
  assertEqual
    "divergences"
    []
    (coreLibApiDivergences [pkg [fullModule "A"]] (pkg [fullModule "Aihc.Only", moduleWithValue "Aihc.Extra" "g" "Int"]))

test_apiDivergenceRejectsExtraNames :: Assertion
test_apiDivergenceRejectsExtraNames =
  assertEqual
    "divergences"
    [ InterfaceMismatch "A.MkU" "not exported by pkg",
      InterfaceMismatch "A.U" "not exported by pkg",
      InterfaceMismatch "A.extra" "not exported by pkg"
    ]
    (coreLibApiDivergences [pkg [fullModule "A"]] (pkg [candidate]))
  where
    candidate =
      (fullModule "A")
        { miValues = [ExportedValue "f" "Int", ExportedValue "extra" "Int"],
          miTypes = [ExportedType "T" "Type" ["MkT"], ExportedType "U" "Type" ["MkU"]]
        }

test_apiDivergenceIgnoresNamespaces :: Assertion
test_apiDivergenceIgnoresNamespaces =
  assertEqual
    "divergences"
    []
    (coreLibApiDivergences [pkg [oracle]] (pkg [candidate]))
  where
    -- A method exported as a plain function, a type without its kind, and a
    -- value with a differently rendered type are all still the same names.
    oracle =
      (emptyModule "A")
        { miValues = [ExportedValue "f" "Applicative f => f a"],
          miTypes = [ExportedType "T" "Type -> Type" ["MkT"]],
          miClasses = [ExportedClass "C" [ClassMethod "method" "Int"]]
        }
    candidate =
      (emptyModule "A")
        { miValues = [ExportedValue "f" "(Applicative f) => f a", ExportedValue "method" "Int"],
          miTypes = [ExportedType "T" "<unspecified-source-kind>" ["MkT"]],
          miClasses = [ExportedClass "C" []]
        }

test_apiDivergenceRejectsFixities :: Assertion
test_apiDivergenceRejectsFixities =
  assertEqual
    "divergences"
    [ InterfaceMismatch "A.fixity:+" ("fixity differs from pkg: " <> T.pack (show oracleFixity)),
      InterfaceMismatch "A.fixity:*" "fixity is not declared by pkg"
    ]
    (coreLibApiDivergences [pkg [oracle]] (pkg [candidate]))
  where
    oracleFixity = FixityInfo "+" InfixL 6
    operators = [ExportedValue "+" "Int", ExportedValue "*" "Int"]
    oracle = (emptyModule "A") {miValues = operators, miFixities = [oracleFixity]}
    candidate = (emptyModule "A") {miValues = operators, miFixities = [FixityInfo "+" InfixR 6, FixityInfo "*" InfixL 7]}

test_apiDivergenceSkipsEmptyOracleModules :: Assertion
test_apiDivergenceSkipsEmptyOracleModules =
  assertEqual
    "divergences"
    []
    (coreLibApiDivergences [pkg [emptyModule "GHC.Prim"]] (pkg [moduleWithValue "GHC.Prim" "+#" "Int# -> Int# -> Int#"]))

-- | Exports of modules shared with @ghc-prim@ or @base@ that GHC does not
-- provide. Every entry is an incompatibility that should be fixed; remove it
-- from this list once the export is gone. New entries must not be added.
knownCoreLibApiDivergences :: [T.Text]
knownCoreLibApiDivergences =
  [ "Foreign.Marshal.unsafeLocalState",
    "GHC.Char.ord",
    "GHC.Char.unsafeChr",
    "GHC.Environment.setFullArgs",
    "GHC.Event.awaitIO",
    "GHC.Foreign.openIOHandle",
    "GHC.Foreign.openUtf8FilePath",
    "GHC.Generics.P",
    "GHC.Generics.Rep",
    "GHC.Generics.Rep1",
    "GHC.IO.FD.copyAddrToByteArray",
    "GHC.IO.FD.dEFAULT_BUFFER_SIZE",
    "GHC.IO.FD.readIntoBuffer",
    "GHC.IO.FD.readIntoPtr",
    "GHC.IO.FD.writeFromBuffer",
    "GHC.IO.FD.writeFromPtr",
    "GHC.IO.Handle.Text.hLookAhead",
    "GHC.IO.IOMode.ioModeNumber",
    "GHC.IO.IOMode.isReadableMode",
    "GHC.IO.IOMode.isWritableMode",
    "GHC.Show.showListWith",
    "GHC.Stack.Types.appendCallStack",
    "GHC.Stack.Types.popCallStack",
    "GHC.Stack.Types.prettyCallStack",
    "GHC.Stack.Types.prettyCallStackLines",
    "GHC.Stack.Types.prettySrcLoc",
    "GHC.Stack.Types.pushCallSite",
    "GHC.Unicode.isLetter",
    "GHC.Unicode.isMark",
    "GHC.Unicode.isNumber",
    "GHC.Unicode.isSeparator",
    "Prelude.+++",
    "Prelude.<++",
    "Prelude.choice",
    "Prelude.get",
    "Prelude.look",
    "Prelude.minPrec",
    "Prelude.pfail",
    "Prelude.prec",
    "Prelude.readPrec_to_S",
    "Prelude.readS_to_Prec",
    "Prelude.reset",
    "Prelude.step",
    "Prelude.Prec",
    "Prelude.ReadPrec",
    "Prelude.fixity:+++",
    "Prelude.fixity:<++",
    "Type.Reflection.typeRepArgs"
  ]

test_coreLibsHaveOnlyKnownDivergences :: Assertion
test_coreLibsHaveOnlyKnownDivergences = do
  divergences <- runCoreLibApiDivergences
  let unexpected = filter ((`notElem` knownCoreLibApiDivergences) . mismatchPath) divergences
      fixed = filter (`notElem` map mismatchPath divergences) knownCoreLibApiDivergences
  assertEqual
    "exports that GHC's ghc-prim or base do not provide"
    []
    (map renderMismatch unexpected)
  assertEqual
    "known divergences that no longer occur; remove them from knownCoreLibApiDivergences"
    []
    fixed
  where
    renderMismatch item = mismatchPath item <> ": " <> mismatchMessage item

test_aihcInternalSubset :: Assertion
test_aihcInternalSubset = do
  candidate <- extractPackage "aihc-internal"
  oracle <- extractPackage "ghc-internal"
  assertEqual "aihc-internal mismatches" [] (comparePackageSubset candidate oracle)

-- A source fixture cannot compare all exported interface facts in all public modules.
test_aihcTemplateHaskellSubset :: Assertion
test_aihcTemplateHaskellSubset = do
  candidate <- extractPackage "aihc-template-haskell"
  oracle <- extractPackage "template-haskell"
  assertEqual "aihc-template-haskell mismatches" [] (comparePackageSubset candidate oracle)

pkg :: [ModuleInterface] -> PackageInterface
pkg modules =
  PackageInterface
    { piPackage = "pkg-0",
      piModules = modules
    }

emptyModule :: String -> ModuleInterface
emptyModule name =
  ModuleInterface
    { miModule = fromString name,
      miTypes = [],
      miValues = [],
      miClasses = [],
      miFixities = []
    }

moduleWithValue :: String -> String -> String -> ModuleInterface
moduleWithValue moduleName valueName valueType =
  (emptyModule moduleName)
    { miValues =
        [ ExportedValue
            { evName = fromString valueName,
              evType = fromString valueType
            }
        ]
    }

fullModule :: String -> ModuleInterface
fullModule name =
  (emptyModule name)
    { miValues = [ExportedValue "f" "Int"],
      miTypes = [ExportedType "T" "Type" ["MkT"]],
      miClasses = [ExportedClass "C" [ClassMethod "method" "Int"]],
      miFixities = [FixityInfo "+" InfixL 6]
    }

findModule :: T.Text -> PackageInterface -> IO ModuleInterface
findModule name iface =
  case List.find ((== name) . miModule) (piModules iface) of
    Just modu -> pure modu
    Nothing -> assertFailure ("module not found: " <> T.unpack name)

demoBaseCabal :: String
demoBaseCabal =
  unlines
    [ "cabal-version: 3.8",
      "name: demo-base",
      "version: 0.1.0.0",
      "build-type: Simple",
      "library",
      "  exposed-modules: Data.Bool",
      "  hs-source-dirs: src",
      "  default-language: GHC2021"
    ]

demoBoolSource :: String
demoBoolSource =
  unlines
    [ "module Data.Bool",
      "  ( Bool(False, True),",
      "    not,",
      "    (&&),",
      "  )",
      "where",
      "data Bool = False | True",
      "infixr 3 &&",
      "not :: Bool -> Bool",
      "not False = True",
      "not True = False",
      "(&&) :: Bool -> Bool -> Bool",
      "False && _ = False",
      "True && x = x"
    ]

demoBoolMissingSignatureSource :: String
demoBoolMissingSignatureSource =
  unlines
    [ "module Data.Bool",
      "  ( Bool(False, True),",
      "    not,",
      "  )",
      "where",
      "data Bool = False | True",
      "not False = True",
      "not True = False"
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

fromString :: String -> T.Text
fromString = T.pack
