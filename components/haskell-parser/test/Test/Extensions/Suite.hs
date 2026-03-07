{-# LANGUAGE OverloadedStrings #-}

module Test.Extensions.Suite
  ( extensionTests,
  )
where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.LanguageExtensions.Type (Extension (ParallelListComp))
import qualified Parser
import Parser.Ast (Module)
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Oracle
  ( oracleModuleAstFingerprintWithExtensions,
    oracleParsesModuleWithExtensions,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data ExtensionSpec = ExtensionSpec
  { extName :: !String,
    extFixtureDir :: !FilePath,
    extNotes :: !String
  }
  deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExpected :: !Expected,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"

registryPath :: FilePath
registryPath = fixtureRoot </> "extensions.tsv"

extensionTests :: IO TestTree
extensionTests = do
  specs <- loadRegistry
  groups <- mapM extensionGroup specs
  summary <- summaryTest specs
  pure (testGroup "extensions-oracle" (groups <> [summary]))

extensionGroup :: ExtensionSpec -> IO TestTree
extensionGroup spec = do
  let manifest = manifestPathFor spec
  exists <- doesFileExist manifest
  if not exists
    then pure (testGroup (extName spec) [])
    else do
      exts <- resolveOracleExtensions spec
      cases <- loadManifest spec
      checks <- mapM (mkCaseTest spec exts) cases
      summary <- extensionSummaryTest spec exts cases
      pure (testGroup (extName spec) (checks <> [summary]))

mkCaseTest :: ExtensionSpec -> [Extension] -> CaseMeta -> IO TestTree
mkCaseTest spec exts meta = do
  source <- TIO.readFile (fixtureDirFor spec </> casePath meta)
  pure $ testCase (caseId meta) (assertCase spec exts meta source)

assertCase :: ExtensionSpec -> [Extension] -> CaseMeta -> Text -> Assertion
assertCase spec exts meta source = do
  (_, outcome, details) <- evaluateCase spec exts meta source
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "Regression in extension case "
            <> caseId meta
            <> " ("
            <> extName spec
            <> ":"
            <> caseCategory meta
            <> ") expected "
            <> show (caseExpected meta)
            <> " reason="
            <> caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

summaryTest :: [ExtensionSpec] -> IO TestTree
summaryTest specs = do
  outcomes <- concat <$> mapM evaluateExtension specs
  pure $ testCase "summary" (assertNoRegressions outcomes)

extensionSummaryTest :: ExtensionSpec -> [Extension] -> [CaseMeta] -> IO TestTree
extensionSummaryTest spec exts cases = do
  outcomes <- mapM (evaluateCaseFromFile spec exts) cases
  pure $ testCase "summary" (assertNoRegressions outcomes)

assertNoRegressions :: [(CaseMeta, Outcome, String)] -> Assertion
assertNoRegressions outcomes = do
  let failN = length [() | (_, OutcomeFail, _) <- outcomes]
  when (failN > 0) $ assertFailure ("extension suite contains " <> show failN <> " regression(s)")

evaluateExtension :: ExtensionSpec -> IO [(CaseMeta, Outcome, String)]
evaluateExtension spec = do
  let manifest = manifestPathFor spec
  exists <- doesFileExist manifest
  if not exists
    then pure []
    else do
      exts <- resolveOracleExtensions spec
      cases <- loadManifest spec
      mapM (evaluateCaseFromFile spec exts) cases

evaluateCaseFromFile :: ExtensionSpec -> [Extension] -> CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCaseFromFile spec exts meta = do
  source <- TIO.readFile (fixtureDirFor spec </> casePath meta)
  evaluateCase spec exts meta source

evaluateCase :: ExtensionSpec -> [Extension] -> CaseMeta -> Text -> IO (CaseMeta, Outcome, String)
evaluateCase _spec exts meta source = do
  let parsed = Parser.parseModule Parser.defaultConfig source
      oracleOk = oracleParsesModuleWithExtensions exts source
      roundtripOk = moduleRoundtripsViaGhc exts source parsed
      (outcome, details) = classify (caseExpected meta) oracleOk roundtripOk
  pure (meta, outcome, details)

classify :: Expected -> Bool -> Bool -> (Outcome, String)
classify expected oracleOk roundtripOk =
  case expected of
    ExpectPass
      | not oracleOk -> (OutcomeFail, "oracle rejected pass case")
      | not roundtripOk -> (OutcomeFail, "roundtrip mismatch against oracle AST")
      | otherwise -> (OutcomePass, "")
    ExpectXFail
      | not oracleOk ->
          ( OutcomeFail,
            "oracle rejected xfail case (fixture invalid or missing oracle extension mapping)"
          )
      | roundtripOk -> (OutcomeXPass, "case now passes oracle and roundtrip checks")
      | otherwise -> (OutcomeXFail, "")

moduleRoundtripsViaGhc :: [Extension] -> Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc exts source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = prettyModule parsed
       in case (oracleModuleAstFingerprintWithExtensions exts source, oracleModuleAstFingerprintWithExtensions exts rendered) of
            (Right sourceAst, Right renderedAst) -> sourceAst == renderedAst
            _ -> False

loadRegistry :: IO [ExtensionSpec]
loadRegistry = do
  raw <- TIO.readFile registryPath
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM parseRegistryRow rows

parseRegistryRow :: Text -> IO ExtensionSpec
parseRegistryRow row =
  case T.splitOn "\t" row of
    [nameTxt, dirTxt] ->
      pure
        ExtensionSpec
          { extName = T.unpack (T.strip nameTxt),
            extFixtureDir = T.unpack (T.strip dirTxt),
            extNotes = ""
          }
    [nameTxt, dirTxt, notesTxt] ->
      pure
        ExtensionSpec
          { extName = T.unpack (T.strip nameTxt),
            extFixtureDir = T.unpack (T.strip dirTxt),
            extNotes = T.unpack (T.strip notesTxt)
          }
    _ -> fail ("Invalid extension registry row (expected 2 or 3 tab-separated columns): " <> T.unpack row)

resolveOracleExtensions :: ExtensionSpec -> IO [Extension]
resolveOracleExtensions spec =
  case extName spec of
    "ParallelListComp" -> pure [ParallelListComp]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)

fixtureDirFor :: ExtensionSpec -> FilePath
fixtureDirFor spec = fixtureRoot </> extFixtureDir spec

manifestPathFor :: ExtensionSpec -> FilePath
manifestPathFor spec = fixtureDirFor spec </> "manifest.tsv"

loadManifest :: ExtensionSpec -> IO [CaseMeta]
loadManifest spec = do
  raw <- TIO.readFile (manifestPathFor spec)
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM (parseManifestRow spec) rows

parseManifestRow :: ExtensionSpec -> Text -> IO CaseMeta
parseManifestRow spec row =
  case T.splitOn "\t" row of
    [cid, cat, pathTxt, expectedTxt] ->
      parseManifestRowWithReason spec cid cat pathTxt expectedTxt ""
    [cid, cat, pathTxt, expectedTxt, reasonTxt] ->
      parseManifestRowWithReason spec cid cat pathTxt expectedTxt reasonTxt
    _ -> fail ("Invalid manifest row (expected 4 or 5 tab-separated columns): " <> T.unpack row)

parseManifestRowWithReason :: ExtensionSpec -> Text -> Text -> Text -> Text -> Text -> IO CaseMeta
parseManifestRowWithReason spec cid cat pathTxt expectedTxt reasonTxt = do
  let path = T.unpack pathTxt
  exists <- doesFileExist (fixtureDirFor spec </> path)
  if not exists
    then fail ("Manifest references missing case file: " <> path)
    else do
      expected <-
        case expectedTxt of
          "pass" -> pure ExpectPass
          "xfail" -> pure ExpectXFail
          _ -> fail ("Unknown expected value in manifest: " <> T.unpack expectedTxt)
      let reason = trim (T.unpack reasonTxt)
      case expected of
        ExpectXFail | null reason -> fail ("xfail case requires a reason: " <> T.unpack cid)
        _ -> pure ()
      pure
        CaseMeta
          { caseId = T.unpack cid,
            caseCategory = T.unpack cat,
            casePath = path,
            caseExpected = expected,
            caseReason = reason
          }

stripComment :: Text -> Text
stripComment line =
  let core = fst (T.breakOn "#" line)
   in T.strip core

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
