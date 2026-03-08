{-# LANGUAGE OverloadedStrings #-}

module Test.Extensions.Suite
  ( extensionTests,
  )
where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import ExtensionSupport
import GHC.LanguageExtensions.Type (Extension (ParallelListComp, TypeApplications))
import qualified Parser
import Parser.Ast (Module)
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.FilePath ((</>))
import Test.Oracle
  ( oracleModuleAstFingerprintWithExtensions,
    oracleParsesModuleWithExtensions,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

extensionTests :: IO TestTree
extensionTests = do
  specs <- loadRegistry
  groups <- mapM extensionGroup specs
  summary <- summaryTest specs
  pure (testGroup "extensions-oracle" (groups <> [summary]))

extensionGroup :: ExtensionSpec -> IO TestTree
extensionGroup spec = do
  exists <- hasManifest spec
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
  exists <- hasManifest spec
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
  pure (finalizeOutcome meta oracleOk roundtripOk)

moduleRoundtripsViaGhc :: [Extension] -> Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc exts source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = prettyModule parsed
       in case (oracleModuleAstFingerprintWithExtensions exts source, oracleModuleAstFingerprintWithExtensions exts rendered) of
            (Right sourceAst, Right renderedAst) -> sourceAst == renderedAst
            _ -> False

resolveOracleExtensions :: ExtensionSpec -> IO [Extension]
resolveOracleExtensions spec =
  case extName spec of
    "ParallelListComp" -> pure [ParallelListComp]
    "TypeApplications" -> pure [TypeApplications]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)
