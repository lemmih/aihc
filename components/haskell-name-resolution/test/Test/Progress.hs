{-# LANGUAGE OverloadedStrings #-}

module Test.Progress
  ( progressTests,
  )
where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser (defaultConfig, parseModule)
import Parser.Types (ParseResult (..))
import Resolver (defaultResolveConfig, resolveModule)
import Resolver.Ast
import Resolver.Types (diagCode, diagnostics, resolved)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Oracle.Resolve (oracleResolveFacts)
import Test.ResolveFacts (ResolveFacts (..), VarFact (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExpected :: !Expected,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/progress"

manifestPath :: FilePath
manifestPath = fixtureRoot </> "manifest.tsv"

progressTests :: IO TestTree
progressTests = do
  cases <- loadManifest
  individual <- mapM caseTest cases
  summary <- summaryTest cases
  pure (testGroup "resolver-progress" (individual <> [summary]))

caseTest :: CaseMeta -> IO TestTree
caseTest meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  pure $ testCase (caseId meta) (assertOutcome meta source)

assertOutcome :: CaseMeta -> T.Text -> Assertion
assertOutcome meta source = do
  (outcome, details) <- evaluateCaseText meta source
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "resolver progress regression in "
            <> caseId meta
            <> " ["
            <> caseCategory meta
            <> "]: "
            <> details
        )
    _ -> pure ()

summaryTest :: [CaseMeta] -> IO TestTree
summaryTest cases = do
  outcomes <- mapM evaluateCase cases
  pure $ testCase "summary" (assertNoFailures outcomes)

assertNoFailures :: [(CaseMeta, Outcome, String)] -> Assertion
assertNoFailures outcomes = do
  let failN = length [() | (_, OutcomeFail, _) <- outcomes]
  when (failN > 0) $
    assertFailure ("resolver progress summary contains " <> show failN <> " failure(s)")

evaluateCase :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  (outcome, details) <- evaluateCaseText meta source
  pure (meta, outcome, details)

evaluateCaseText :: CaseMeta -> T.Text -> IO (Outcome, String)
evaluateCaseText meta source =
  case oracleResolveFacts defaultResolveConfig source of
    Left oracleErr -> pure (OutcomeFail, "oracle failed: " <> T.unpack oracleErr)
    Right oracleFacts -> do
      let oursFacts = resolveFacts source
      pure $ classify (caseExpected meta) oursFacts oracleFacts

classify :: Expected -> Either String ResolveFacts -> ResolveFacts -> (Outcome, String)
classify expected oursEither oracleFacts =
  case expected of
    ExpectPass ->
      case oursEither of
        Left err -> (OutcomeFail, "expected pass but parser/resolver failed: " <> err)
        Right oursFacts
          | oursFacts == oracleFacts -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "facts differ from oracle"
              )
    ExpectXFail ->
      case oursEither of
        Left _ -> (OutcomeXFail, "")
        Right oursFacts
          | oursFacts == oracleFacts -> (OutcomeXPass, "expected xfail but now matches oracle")
          | otherwise -> (OutcomeXFail, "")

resolveFacts :: T.Text -> Either String ResolveFacts
resolveFacts input =
  case parseModule defaultConfig input of
    ParseErr err -> Left (show err)
    ParseOk modu ->
      let rr = resolveModule defaultResolveConfig modu
          resolvedMod = resolved rr
          decls = resolvedDecls resolvedMod
          vars = concatMap collectVars decls
       in Right
            ResolveFacts
              { rfModuleName = resolvedModuleName resolvedMod,
                rfDeclNames = map resolvedDeclName decls,
                rfVars = vars,
                rfDiagnosticCodes = map diagCode (diagnostics rr)
              }
  where
    collectVars decl = collectExpr (resolvedDeclExpr decl)
    collectExpr expr =
      case expr of
        RInt _ -> []
        RApp f x -> collectExpr f <> collectExpr x
        RVar name ->
          [ VarFact
              { vfName = rnText name,
                vfBinding = fmap (const (rnText name)) (rnId name),
                vfClass = rnClass name
              }
          ]

loadManifest :: IO [CaseMeta]
loadManifest = do
  raw <- TIO.readFile manifestPath
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM parseRow rows

stripComment :: T.Text -> T.Text
stripComment line =
  let core = fst (T.breakOn "#" line)
   in T.strip core

parseRow :: T.Text -> IO CaseMeta
parseRow row =
  case T.splitOn "\t" row of
    [cid, cat, pathTxt, expectedTxt] -> parseRowWithReason cid cat pathTxt expectedTxt ""
    [cid, cat, pathTxt, expectedTxt, reasonTxt] -> parseRowWithReason cid cat pathTxt expectedTxt reasonTxt
    _ -> fail ("Invalid manifest row (expected 4 or 5 tab-separated columns): " <> T.unpack row)

parseRowWithReason :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> IO CaseMeta
parseRowWithReason cid cat pathTxt expectedTxt reasonTxt = do
  let path = T.unpack pathTxt
  exists <- doesFileExist (fixtureRoot </> path)
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
        ExpectXFail | null reason -> fail ("xfail case requires reason: " <> T.unpack cid)
        _ -> pure ()
      pure
        CaseMeta
          { caseId = T.unpack cid,
            caseCategory = T.unpack cat,
            casePath = path,
            caseExpected = expected,
            caseReason = reason
          }

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
