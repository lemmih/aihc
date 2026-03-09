{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cpp (Config (..), Result (..), Step (..), preprocess)
import qualified Data.Text as T
import Test.Progress (CaseMeta (..), Outcome (..), evaluateCase, loadManifest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

main :: IO ()
main = do
  cases <- loadManifest
  checks <- mapM mkCase cases
  defaultMain (testGroup "cpp-oracle" (checks <> [linePragmaTest]))

mkCase :: CaseMeta -> IO TestTree
mkCase meta =
  pure $ testCase (caseId meta) (assertCase meta)

assertCase :: CaseMeta -> Assertion
assertCase meta = do
  (_, outcome, details) <- evaluateCase meta
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "cpp regression in "
            <> caseId meta
            <> " ["
            <> caseCategory meta
            <> "]: "
            <> details
        )
    _ -> pure ()

linePragmaTest :: TestTree
linePragmaTest =
  testCase "include emits line pragmas" $
    case preprocess Config {configInputFile = "root.hs"} "before\n#include \"nested.inc\"\nafter" of
      NeedInclude _ k ->
        case k (Just "inside") of
          Done result -> do
            let out = T.lines (resultOutput result)
            if "#line 1 \"nested.inc\"" `elem` out && "#line 3 \"root.hs\"" `elem` out
              then pure ()
              else assertFailure "expected include line pragmas in output"
          NeedInclude {} -> assertFailure "unexpected nested include in line pragma test"
      Done _ -> assertFailure "expected include continuation step"
