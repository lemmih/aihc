{-# LANGUAGE OverloadedStrings #-}

module Test.Progress
  ( CaseMeta (..),
    Outcome (..),
    evaluateCase,
    loadManifest,
    progressSummary,
  )
where

import qualified Control.Exception as E
import Cpp (Config (..), Diagnostic (..), IncludeRequest (..), Result (..), Severity (..), Step (..), preprocess)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

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

progressSummary :: [(CaseMeta, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

evaluateCase :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase meta = do
  let sourcePath = fixtureRoot </> casePath meta
  source <- TIO.readFile sourcePath
  ours <- runOurs sourcePath source
  oracle <- runOracle sourcePath
  let (outcome, details) = classify (caseExpected meta) ours oracle
  pure (meta, outcome, details)

classify :: Expected -> Either String Text -> Either String Text -> (Outcome, String)
classify expected ours oracle =
  case expected of
    ExpectPass ->
      case (ours, oracle) of
        (Right oursOut, Right oracleOut)
          | oursOut == oracleOut -> (OutcomePass, "")
          | otherwise -> (OutcomeFail, "preprocessed output differs from cpphs oracle")
        (Left _, Left _) -> (OutcomePass, "")
        (Left oursErr, _) -> (OutcomeFail, "ours failed: " <> oursErr)
        (_, Left oracleErr) -> (OutcomeFail, "oracle failed: " <> oracleErr)
    ExpectXFail ->
      case (ours, oracle) of
        (Right oursOut, Right oracleOut)
          | oursOut == oracleOut -> (OutcomeXPass, "expected xfail but now matches cpphs")
          | otherwise -> (OutcomeXFail, "")
        (Left _, Left _) -> (OutcomeXPass, "expected xfail but now matches cpphs")
        _ -> (OutcomeXFail, "")

runOurs :: FilePath -> Text -> IO (Either String Text)
runOurs sourcePath source = do
  result <- drive (preprocess Config {configInputFile = takeFileName sourcePath} source)
  let errors = [diagMessage d | d <- resultDiagnostics result, diagSeverity d == Error]
  case errors of
    [] -> pure (Right (resultOutput result))
    (msg : _) -> pure (Left (T.unpack msg))
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = do
      let includeAbsPath = resolveIncludePath sourcePath req
      exists <- doesFileExist includeAbsPath
      content <- if exists then Just <$> TIO.readFile includeAbsPath else pure Nothing
      drive (k content)

resolveIncludePath :: FilePath -> IncludeRequest -> FilePath
resolveIncludePath rootPath req =
  takeDirectory rootPath </> takeDirectory (includeFrom req) </> includePath req

runOracle :: FilePath -> IO (Either String Text)
runOracle sourcePath = do
  source <- TIO.readFile sourcePath
  oracleOut <-
    (E.try (runCpphs defaultCpphsOptions sourcePath (T.unpack source)) :: IO (Either E.SomeException String))
  pure $
    case oracleOut of
      Right out -> Right (T.pack out)
      Left err -> Left ("cpphs failed: " <> show err)

loadManifest :: IO [CaseMeta]
loadManifest = do
  raw <- TIO.readFile manifestPath
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM parseRow rows

stripComment :: Text -> Text
stripComment line =
  let core = fst (T.breakOn "#" line)
   in T.strip core

parseRow :: Text -> IO CaseMeta
parseRow row =
  case T.splitOn "\t" row of
    [cid, cat, pathTxt, expectedTxt] -> parseRowWithReason cid cat pathTxt expectedTxt ""
    [cid, cat, pathTxt, expectedTxt, reasonTxt] -> parseRowWithReason cid cat pathTxt expectedTxt reasonTxt
    _ -> fail ("Invalid manifest row (expected 4 or 5 tab-separated columns): " <> T.unpack row)

parseRowWithReason :: Text -> Text -> Text -> Text -> Text -> IO CaseMeta
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
