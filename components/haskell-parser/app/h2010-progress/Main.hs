{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cpp (resultOutput)
import CppSupport (preprocessForParserWithoutIncludes)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GhcOracle
  ( oracleModuleAstFingerprintWithExtensionsAt,
    oracleParsesModuleWithExtensionsAt,
  )
import qualified Parser
import Parser.Ast (Module)
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

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
fixtureRoot = "test/Test/Fixtures/haskell2010"

manifestPath :: FilePath
manifestPath = fixtureRoot </> "manifest.tsv"

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
  cases <- loadManifest
  outcomes <- mapM evaluateCase cases
  let passN = length [() | (_, OutcomePass, _) <- outcomes]
      xfailN = length [() | (_, OutcomeXFail, _) <- outcomes]
      xpassN = length [() | (_, OutcomeXPass, _) <- outcomes]
      failN = length [() | (_, OutcomeFail, _) <- outcomes]
      totalN = passN + xfailN + xpassN + failN
      completion = pct (passN + xpassN) totalN
  putStrLn "Haskell2010 parser progress"
  putStrLn "=========================="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  let regressions = [(meta, details) | (meta, OutcomeFail, details) <- outcomes]
      xpasses = [(meta, details) | (meta, OutcomeXPass, details) <- outcomes]

  mapM_ printRegression regressions
  mapM_ printXPass xpasses

  if null regressions && (not strict || null xpasses)
    then exitSuccess
    else exitFailure

printRegression :: (CaseMeta, String) -> IO ()
printRegression (meta, details) =
  putStrLn
    ( "FAIL "
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

printXPass :: (CaseMeta, String) -> IO ()
printXPass (meta, details) =
  putStrLn
    ( "XPASS "
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

evaluateCase :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  let source' = resultOutput (preprocessForParserWithoutIncludes (casePath meta) source)
      parsed = Parser.parseModule Parser.defaultConfig source'
      oracleOk = oracleParsesModule source'
      roundtripOk = moduleRoundtripsViaGhc source' parsed
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
      | oracleOk && roundtripOk -> (OutcomeXPass, "case now passes oracle and roundtrip checks")
      | otherwise -> (OutcomeXFail, "")

moduleRoundtripsViaGhc :: Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = prettyModule parsed
       in case (oracleModuleAstFingerprint source, oracleModuleAstFingerprint rendered) of
            (Right sourceAst, Right renderedAst) -> sourceAst == renderedAst
            _ -> False

oracleParsesModule :: Text -> Bool
oracleParsesModule = oracleParsesModuleWithExtensionsAt "h2010-progress" []

oracleModuleAstFingerprint :: Text -> Either Text Text
oracleModuleAstFingerprint = oracleModuleAstFingerprintWithExtensionsAt "h2010-progress" []

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
    [cid, cat, pathTxt, expectedTxt] ->
      parseRowWithReason cid cat pathTxt expectedTxt ""
    [cid, cat, pathTxt, expectedTxt, reasonTxt] ->
      parseRowWithReason cid cat pathTxt expectedTxt reasonTxt
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

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
