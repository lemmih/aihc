{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import ExtensionSupport
import GHC.LanguageExtensions.Type (Extension (ParallelListComp))
import GhcOracle
  ( oracleModuleAstFingerprintWithExtensionsAt,
    oracleParsesModuleWithExtensionsAt,
  )
import qualified Parser
import Parser.Ast (Module)
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

data SupportStatus = Supported | InProgress | Planned deriving (Eq, Show)

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
  exists <- hasManifest spec
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
      oracleOk = oracleParsesModuleWithExtensionsAt "extension-progress" exts source
      roundtripOk = moduleRoundtripsViaGhc exts source parsed
  pure (finalizeOutcome meta oracleOk roundtripOk)

moduleRoundtripsViaGhc :: [Extension] -> Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc exts source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = prettyModule parsed
       in case ( oracleModuleAstFingerprintWithExtensionsAt "extension-progress" exts source,
                 oracleModuleAstFingerprintWithExtensionsAt "extension-progress" exts rendered
               ) of
            (Right sourceAst, Right renderedAst) -> sourceAst == renderedAst
            _ -> False

resolveOracleExtensions :: ExtensionSpec -> IO [Extension]
resolveOracleExtensions spec =
  case extName spec of
    "ParallelListComp" -> pure [ParallelListComp]
    _ -> fail ("Unsupported extension fixture without oracle mapping: " <> extName spec)
