{-# LANGUAGE OverloadedStrings #-}

module Test.H2010.Suite
  ( h2010Tests
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import GHC.LanguageExtensions.Type (Extension (ForeignFunctionInterface))
import qualified GHC.Parser as GHCParser
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts)
import qualified Parser
import Parser.Types (ParseResult (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String
  , caseCategory :: !String
  , casePath :: !FilePath
  , caseExpected :: !Expected
  , caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/haskell2010"

manifestPath :: FilePath
manifestPath = fixtureRoot </> "manifest.tsv"

h2010Tests :: IO TestTree
h2010Tests = do
  cases <- loadManifest
  checks <- mapM mkCaseTest cases
  summary <- mkSummaryTest cases
  pure (testGroup "haskell2010-oracle" (checks <> [summary]))

mkCaseTest :: CaseMeta -> IO TestTree
mkCaseTest meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  pure $ testCase (caseId meta) (assertCase meta source)

mkSummaryTest :: [CaseMeta] -> IO TestTree
mkSummaryTest cases = do
  outcomes <- mapM evaluateCase cases
  pure $
    testCase "summary" $ do
      let (passN, xfailN, xpassN, failN) = foldr countOutcome (0, 0, 0, 0) outcomes
          totalN = passN + xfailN + xpassN + failN
          completion = pct (passN + xpassN) totalN
      if failN > 0
        then
          assertFailure
            ( "Haskell2010 regressions found. "
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
        else pure ()

countOutcome :: Outcome -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
countOutcome outcome (passN, xfailN, xpassN, failN) =
  case outcome of
    OutcomePass -> (passN + 1, xfailN, xpassN, failN)
    OutcomeXFail -> (passN, xfailN + 1, xpassN, failN)
    OutcomeXPass -> (passN, xfailN, xpassN + 1, failN)
    OutcomeFail -> (passN, xfailN, xpassN, failN + 1)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

assertCase :: CaseMeta -> Text -> Assertion
assertCase meta source = do
  outcome <- evaluateCaseText meta source
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "Regression in case "
            <> caseId meta
            <> " ("
            <> caseCategory meta
            <> ") expected "
            <> show (caseExpected meta)
            <> " reason="
            <> caseReason meta
        )
    _ -> pure ()

evaluateCase :: CaseMeta -> IO Outcome
evaluateCase meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  evaluateCaseText meta source

evaluateCaseText :: CaseMeta -> Text -> IO Outcome
evaluateCaseText meta source = do
  let oracleOk = oracleParsesModule source
      oursOk = parserParsesModule source
  pure $ classify (caseExpected meta) oracleOk oursOk

classify :: Expected -> Bool -> Bool -> Outcome
classify expected oracleOk oursOk =
  case expected of
    ExpectPass
      | not oracleOk -> OutcomeFail
      | oursOk -> OutcomePass
      | otherwise -> OutcomeFail
    ExpectXFail
      | not oracleOk -> OutcomeFail
      | oursOk -> OutcomeXPass
      | otherwise -> OutcomeXFail

parserParsesModule :: Text -> Bool
parserParsesModule input =
  case Parser.parseModule Parser.defaultConfig input of
    ParseOk _ -> True
    ParseErr _ -> False

oracleParsesModule :: Text -> Bool
oracleParsesModule input =
  case parseWithGhc input of
    Right _ -> True
    Left _ -> False

parseWithGhc :: Text -> Either String (HsModule GhcPs)
parseWithGhc input =
  let exts = EnumSet.fromList [ForeignFunctionInterface] :: EnumSet.EnumSet Extension
      opts = Lexer.mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<h2010-oracle>") 1 1
   in case Lexer.unP GHCParser.parseModule (Lexer.initParserState opts buffer start) of
        Lexer.POk _ modu -> Right (unLoc modu)
        Lexer.PFailed _ -> Left "oracle parse failed"

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
              { caseId = T.unpack cid
              , caseCategory = T.unpack cat
              , casePath = path
              , caseExpected = expected
              , caseReason = reason
              }

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
