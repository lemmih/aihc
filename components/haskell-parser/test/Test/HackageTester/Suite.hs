{-# LANGUAGE OverloadedStrings #-}

module Test.HackageTester.Suite
  ( hackageTesterTests,
  )
where

import qualified Data.Text as T
import GhcOracle (oracleDetailedParsesModuleWithNamesAt)
import HackageTester.CLI (Options (..), parseOptionsPure)
import HackageTester.Model (FileResult (..), Outcome (..), Summary (..), shouldFailSummary, summarizeResults)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

hackageTesterTests :: TestTree
hackageTesterTests =
  testGroup
    "hackage-tester"
    [ testGroup
        "cli"
        [ testCase "parses required package argument" test_cliParsesPackage,
          testCase "parses optional flags" test_cliParsesOptionalFlags,
          testCase "rejects missing package" test_cliRejectsMissingPackage,
          testCase "rejects invalid jobs" test_cliRejectsInvalidJobs
        ],
      testGroup
        "summary"
        [ testCase "counts outcomes correctly" test_summaryCountsOutcomes,
          testCase "fails when no files were processed" test_zeroFilesFails
        ],
      testGroup
        "oracle"
        [ testCase "accepts No-prefixed LANGUAGE pragmas" test_oracleAcceptsNoPrefixedLanguagePragma
        ]
    ]

test_cliParsesPackage :: Assertion
test_cliParsesPackage =
  assertEqual
    "expected defaults with required package"
    (Right (Options "transformers" Nothing Nothing False))
    (parseOptionsPure ["transformers"])

test_cliParsesOptionalFlags :: Assertion
test_cliParsesOptionalFlags =
  assertEqual
    "expected all optional flags to parse"
    (Right (Options "text" (Just "2.0.2") (Just 4) True))
    (parseOptionsPure ["text", "--version", "2.0.2", "--jobs", "4", "--json"])

test_cliRejectsMissingPackage :: Assertion
test_cliRejectsMissingPackage =
  assertLeftContaining "Missing: PACKAGE" (parseOptionsPure [])

test_cliRejectsInvalidJobs :: Assertion
test_cliRejectsInvalidJobs =
  assertLeftContaining "must be a positive integer" (parseOptionsPure ["bytestring", "--jobs", "0"])

test_summaryCountsOutcomes :: Assertion
test_summaryCountsOutcomes = do
  let results =
        [ FileResult "A.hs" OutcomeSuccess [] Nothing,
          FileResult "B.hs" OutcomeGhcError [] Nothing,
          FileResult "C.hs" OutcomeParseError [] Nothing,
          FileResult "D.hs" OutcomeRoundtripFail [] Nothing
        ]
      summary = summarizeResults results
  assertEqual "total files" 4 (totalFiles summary)
  assertEqual "successes" 1 (successCount summary)
  assertEqual "failures" 3 (failureCount summary)
  assertEqual "ghc errors" 1 (ghcErrors summary)
  assertEqual "parse errors" 1 (parseErrors summary)
  assertEqual "roundtrip fails" 1 (roundtripFails summary)

test_zeroFilesFails :: Assertion
test_zeroFilesFails =
  assertBool "expected empty run to fail" (shouldFailSummary (summarizeResults []))

test_oracleAcceptsNoPrefixedLanguagePragma :: Assertion
test_oracleAcceptsNoPrefixedLanguagePragma =
  case oracleDetailedParsesModuleWithNamesAt "hackage-tester" [] Nothing source of
    Left err ->
      assertBool
        ("expected NoMonomorphismRestriction pragma to be accepted, got: " <> T.unpack err)
        False
    Right () -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE NoMonomorphismRestriction #-}",
          "module A where",
          "x = 1"
        ]

assertLeftContaining :: String -> Either String a -> Assertion
assertLeftContaining needle result =
  case result of
    Left err ->
      assertBool
        ("expected parse error to contain " ++ show needle ++ ", got: " ++ err)
        (needle `contains` err)
    Right _ ->
      assertBool "expected parse failure but got success" False

contains :: String -> String -> Bool
contains needle haystack = any (needle `prefixOf`) (tails haystack)

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : rest) = xs : tails rest
