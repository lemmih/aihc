{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import GHC.LanguageExtensions.Type (Extension (ForeignFunctionInterface, ParallelListComp))
import qualified GHC.Parser as GHCParser
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
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

data SupportStatus = Supported | InProgress | Planned deriving (Eq, Show)

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

data ExtensionResult = ExtensionResult
  { erSpec :: !ExtensionSpec,
    erStatus :: !SupportStatus,
    erPassN :: !Int,
    erXFailN :: !Int,
    erXPassN :: !Int,
    erFailN :: !Int,
    erTotalN :: !Int,
    erOutcomes :: ![(CaseMeta, Outcome, String)]
  }

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"

registryPath :: FilePath
registryPath = fixtureRoot </> "extensions.tsv"

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
      markdown = "--markdown" `elem` args
  specs <- loadRegistry
  results <- mapM evaluateExtension specs

  if markdown
    then do
      now <- getCurrentTime
      putStrLn (renderMarkdown (formatTime defaultTimeLocale "%Y-%m-%d" now) results)
    else printTextSummary results

  let failN = sum [erFailN result | result <- results]
      xpassN = sum [erXPassN result | result <- results]

  if failN == 0 && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

printTextSummary :: [ExtensionResult] -> IO ()
printTextSummary results = do
  let supportedN = length [() | result <- results, erStatus result == Supported]
      inProgressN = length [() | result <- results, erStatus result == InProgress]
      plannedN = length [() | result <- results, erStatus result == Planned]
      totalN = length results
  putStrLn "Haskell parser extension support progress"
  putStrLn "================================="
  putStrLn ("SUPPORTED    " <> show supportedN)
  putStrLn ("IN_PROGRESS  " <> show inProgressN)
  putStrLn ("PLANNED      " <> show plannedN)
  putStrLn ("TOTAL        " <> show totalN)
  putStrLn ""
  mapM_ printExtensionLine results

  let regressions =
        [ (erSpec result, meta, details)
        | result <- results,
          (meta, OutcomeFail, details) <- erOutcomes result
        ]
      xpasses =
        [ (erSpec result, meta, details)
        | result <- results,
          (meta, OutcomeXPass, details) <- erOutcomes result
        ]

  mapM_ printRegression regressions
  mapM_ printXPass xpasses

printExtensionLine :: ExtensionResult -> IO ()
printExtensionLine result =
  putStrLn
    ( extName (erSpec result)
        <> "\t"
        <> statusText (erStatus result)
        <> "\tPASS="
        <> show (erPassN result)
        <> " XFAIL="
        <> show (erXFailN result)
        <> " XPASS="
        <> show (erXPassN result)
        <> " FAIL="
        <> show (erFailN result)
    )

printRegression :: (ExtensionSpec, CaseMeta, String) -> IO ()
printRegression (spec, meta, details) =
  putStrLn
    ( "FAIL "
        <> extName spec
        <> "/"
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

printXPass :: (ExtensionSpec, CaseMeta, String) -> IO ()
printXPass (spec, meta, details) =
  putStrLn
    ( "XPASS "
        <> extName spec
        <> "/"
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

renderMarkdown :: String -> [ExtensionResult] -> String
renderMarkdown generatedDate results =
  unlines
    ( [ "# Haskell Parser Extension Support Status",
        "",
        "**Generated**: " <> generatedDate,
        "",
        "## Summary",
        "",
        "- Total Extensions: " <> show totalN,
        "- Supported: " <> show supportedN,
        "- In Progress: " <> show inProgressN,
        "- Planned: " <> show plannedN,
        "",
        "## Extension Status",
        "",
        "| Extension | Status | Tests Passing | Notes |",
        "|-----------|--------|---------------|-------|"
      ]
        <> map renderResultRow results
    )
  where
    supportedN = length [() | result <- results, erStatus result == Supported]
    inProgressN = length [() | result <- results, erStatus result == InProgress]
    plannedN = length [() | result <- results, erStatus result == Planned]
    totalN = length results

renderResultRow :: ExtensionResult -> String
renderResultRow result =
  "| "
    <> extName spec
    <> " | "
    <> statusText (erStatus result)
    <> " | "
    <> testsPassingText
    <> " | "
    <> sanitizeCell (extNotes spec)
    <> " |"
  where
    spec = erSpec result
    testsPassingText =
      case erStatus result of
        Planned -> "-"
        _ -> show (erPassN result) <> "/" <> show (erTotalN result)

sanitizeCell :: String -> String
sanitizeCell = concatMap escapePipe
  where
    escapePipe '|' = "\\|"
    escapePipe c = [c]

statusText :: SupportStatus -> String
statusText status =
  case status of
    Supported -> "Supported"
    InProgress -> "In Progress"
    Planned -> "Planned"

evaluateExtension :: ExtensionSpec -> IO ExtensionResult
evaluateExtension spec = do
  let manifest = manifestPathFor spec
  exists <- doesFileExist manifest
  if not exists
    then
      pure
        ExtensionResult
          { erSpec = spec,
            erStatus = Planned,
            erPassN = 0,
            erXFailN = 0,
            erXPassN = 0,
            erFailN = 0,
            erTotalN = 0,
            erOutcomes = []
          }
    else do
      exts <- resolveOracleExtensions spec
      cases <- loadManifest spec
      outcomes <- mapM (evaluateCase spec exts) cases
      let passN = countOutcome OutcomePass outcomes
          xfailN = countOutcome OutcomeXFail outcomes
          xpassN = countOutcome OutcomeXPass outcomes
          failN = countOutcome OutcomeFail outcomes
          totalN = passN + xfailN + xpassN + failN
          status
            | totalN > 0 && failN == 0 && xfailN == 0 && xpassN == 0 = Supported
            | otherwise = InProgress
      pure
        ExtensionResult
          { erSpec = spec,
            erStatus = status,
            erPassN = passN,
            erXFailN = xfailN,
            erXPassN = xpassN,
            erFailN = failN,
            erTotalN = totalN,
            erOutcomes = outcomes
          }

countOutcome :: Outcome -> [(CaseMeta, Outcome, String)] -> Int
countOutcome target = length . filter (\(_, outcome, _) -> outcome == target)

evaluateCase :: ExtensionSpec -> [Extension] -> CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase spec exts meta = do
  source <- TIO.readFile (fixtureDirFor spec </> casePath meta)
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

oracleParsesModuleWithExtensions :: [Extension] -> Text -> Bool
oracleParsesModuleWithExtensions exts input =
  case parseWithGhcWithExtensions exts input of
    Right _ -> True
    Left _ -> False

oracleModuleAstFingerprintWithExtensions :: [Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensions exts input = do
  parsed <- parseWithGhcWithExtensions exts input
  pure (T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhcWithExtensions :: [Extension] -> Text -> Either Text (HsModule GhcPs)
parseWithGhcWithExtensions extraExts input =
  let allExts = nub (ForeignFunctionInterface : extraExts)
      exts = EnumSet.fromList allExts :: EnumSet.EnumSet Extension
      opts = Lexer.mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<extension-progress>") 1 1
   in case Lexer.unP GHCParser.parseModule (Lexer.initParserState opts buffer start) of
        Lexer.POk _ modu -> Right (unLoc modu)
        Lexer.PFailed _ -> Left "oracle parse failed"

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
