{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Suite
  ( parserGoldenTests,
  )
where

import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified ParserGolden as PG
import System.FilePath (takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

parserGoldenTests :: IO TestTree
parserGoldenTests = do
  exprCases <- PG.loadExprCases
  moduleCases <- PG.loadModuleCases

  exprChecks <- mapM mkExprCaseTest exprCases
  moduleChecks <- mapM mkModuleCaseTest moduleCases

  exprSummary <- mkSummaryTest "expr" PG.evaluateExprCase exprCases
  moduleSummary <- mkSummaryTest "module" PG.evaluateModuleCase moduleCases
  combinedSummary <- mkCombinedSummary exprCases moduleCases

  pure
    ( testGroup
        "parser-golden"
        [ fixtureValidationTests,
          testGroup "expr" (exprChecks <> [exprSummary]),
          testGroup "module" (moduleChecks <> [moduleSummary]),
          combinedSummary
        ]
    )

mkExprCaseTest :: PG.ParserCase -> IO TestTree
mkExprCaseTest meta = pure $ testCase (PG.caseId meta) (assertExprCase meta)

mkModuleCaseTest :: PG.ParserCase -> IO TestTree
mkModuleCaseTest meta = pure $ testCase (PG.caseId meta) (assertModuleCase meta)

mkSummaryTest :: String -> (PG.ParserCase -> (PG.Outcome, String)) -> [PG.ParserCase] -> IO TestTree
mkSummaryTest label evaluateCase cases = do
  let outcomes = map (evaluate evaluateCase) cases
  pure $ testCase (label <> " summary") (assertNoRegressions label outcomes)

mkCombinedSummary :: [PG.ParserCase] -> [PG.ParserCase] -> IO TestTree
mkCombinedSummary exprCases moduleCases = do
  let exprOutcomes = map (evaluate PG.evaluateExprCase) exprCases
      moduleOutcomes = map (evaluate PG.evaluateModuleCase) moduleCases
      outcomes = exprOutcomes <> moduleOutcomes
  pure $ testCase "summary" (assertNoRegressions "parser golden" outcomes)

assertExprCase :: PG.ParserCase -> Assertion
assertExprCase = assertCaseWith PG.evaluateExprCase

assertModuleCase :: PG.ParserCase -> Assertion
assertModuleCase = assertCaseWith PG.evaluateModuleCase

assertCaseWith :: (PG.ParserCase -> (PG.Outcome, String)) -> PG.ParserCase -> Assertion
assertCaseWith evaluateCase meta =
  case evaluateCase meta of
    (PG.OutcomeFail, details) ->
      assertFailure
        ( "Regression in parser case "
            <> PG.caseId meta
            <> " ("
            <> PG.caseCategory meta
            <> ") expected "
            <> show (PG.caseStatus meta)
            <> " reason="
            <> PG.caseReason meta
            <> " details="
            <> details
        )
    (PG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xpass parser case "
            <> PG.caseId meta
            <> " reason="
            <> PG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

assertNoRegressions :: String -> [(PG.ParserCase, PG.Outcome, String)] -> Assertion
assertNoRegressions label outcomes = do
  let (passN, xfailN, xpassN, failN) = PG.progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  when (failN > 0 || xpassN > 0) $
    assertFailure
      ( label
          <> " regressions found. "
          <> "pass="
          <> show passN
          <> " xfail="
          <> show xfailN
          <> " xpass="
          <> show xpassN
          <> " fail="
          <> show failN
          <> " completion="
          <> show completion
          <> "%"
      )

evaluate :: (PG.ParserCase -> (PG.Outcome, String)) -> PG.ParserCase -> (PG.ParserCase, PG.Outcome, String)
evaluate evaluateCase meta =
  let (outcome, details) = evaluateCase meta
   in (meta, outcome, details)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

fixtureValidationTests :: TestTree
fixtureValidationTests =
  testGroup
    "fixture-parse"
    [ testCase "rejects missing required keys" $
        case PG.parseParserCaseText PG.CaseExpr "missing.yaml" "extensions: []\n" of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for missing required YAML keys",
      testCase "requires reason for xfail" $
        case PG.parseParserCaseText PG.CaseExpr "xfail.yaml" validXFailMissingReason of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure when xfail reason is missing",
      testCase "requires ast for pass" $
        case PG.parseParserCaseText PG.CaseExpr "pass.yaml" validPassMissingAst of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure when pass ast is missing",
      testCase "accepts xpass with reason and ast" $
        case PG.parseParserCaseText PG.CaseExpr "xpass.yaml" validXPassFixture of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right parsed ->
            if PG.caseStatus parsed == PG.StatusXPass
              then pure ()
              else assertFailure "expected xpass status",
      testCase "only YAML fixtures are loaded" $ do
        exprCases <- PG.loadExprCases
        moduleCases <- PG.loadModuleCases
        let cases = exprCases <> moduleCases
        mapM_
          ( \meta ->
              unless (takeExtension (PG.casePath meta) `elem` [".yaml", ".yml"]) $
                assertFailure ("unexpected non-parser fixture loaded: " <> PG.casePath meta)
          )
          cases
    ]

validXFailMissingReason :: T.Text
validXFailMissingReason =
  T.unlines
    [ "extensions: []",
      "input: bad",
      "status: xfail"
    ]

validPassMissingAst :: T.Text
validPassMissingAst =
  T.unlines
    [ "extensions: []",
      "input: x",
      "status: pass"
    ]

validXPassFixture :: T.Text
validXPassFixture =
  T.unlines
    [ "extensions: []",
      "input: x",
      "ast: EVar \"x\"",
      "status: xpass",
      "reason: known bug"
    ]
