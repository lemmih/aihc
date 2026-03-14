{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import CppSupport (preprocessForParser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GhcOracle
import HackageSupport (FileInfo (..), diagToText, downloadPackage, findTargetFilesFromCabal, prefixCppErrors, resolveIncludeBestEffort)
import ParserValidation (ValidationError (..), ValidationErrorKind (..), validateParserDetailed)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [packageName] -> testPackage packageName
    _ -> do
      putStrLn "Usage: cabal run hackage-tester -- <package-name>"
      putStrLn "Example: cabal run hackage-tester -- transformers"
      exitFailure

testPackage :: String -> IO ()
testPackage packageName = do
  putStrLn ("Testing package: " ++ packageName)
  version <- getLatestVersion packageName
  case version of
    Nothing -> do
      putStrLn ("Failed to resolve version for: " ++ packageName)
      exitFailure
    Just ver -> do
      srcDir <- downloadPackage packageName ver
      files <- findTargetFilesFromCabal srcDir
      putStrLn ("Found " ++ show (length files) ++ " Haskell source files")
      results <- processFiles srcDir files
      printSummary results
      let failed = filter (\r -> ghcError r || parseError r || roundtripFail r) results
      if null failed then exitSuccess else exitFailure

getLatestVersion :: String -> IO (Maybe String)
getLatestVersion packageName = do
  let url = "https://hackage.haskell.org/package/" ++ packageName ++ "/" ++ packageName ++ ".cabal"
  result <- readProcess "curl" ["-s", "-f", url] ""
  let cabalContent = T.pack result
      lines' = T.lines cabalContent
      versionLines = filter (T.isPrefixOf "version:") lines'
  pure $ case versionLines of
    [] -> Nothing
    (vline : _) ->
      let ver = T.strip (T.drop (T.length "version:") vline)
       in Just (T.unpack ver)

data FileResult = FileResult
  { filePath :: FilePath,
    ghcError :: Bool,
    parseError :: Bool,
    roundtripFail :: Bool,
    ghcErrorMsg :: Maybe Text,
    parseErrorMsg :: Maybe Text,
    roundtripErrorMsg :: Maybe Text
  }

processFiles :: FilePath -> [FileInfo] -> IO [FileResult]
processFiles packageRoot files =
  forM files $ \info -> do
    let file = fileInfoPath info
    result <- processFile packageRoot info
    case (ghcError result, parseError result, roundtripFail result) of
      (True, _, _) -> putStrLn ("GHC_ERROR:     " ++ file)
      (_, True, _) -> putStrLn ("PARSE_ERROR:   " ++ file)
      (_, _, True) -> putStrLn ("ROUNDTRIP_FAIL: " ++ file)
      _ -> pure ()
    pure result

processFile :: FilePath -> FileInfo -> IO FileResult
processFile packageRoot info = do
  let file = fileInfoPath info
  source <- TIO.readFile file
  preprocessed <- preprocessForParser file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrors = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      cppErrorMsg =
        if null cppErrors
          then Nothing
          else Just (T.intercalate "\n" cppErrors)

  let ghcResult = GhcOracle.oracleDetailedParsesModuleWithNamesAt file (fileInfoExtensions info) (fileInfoLanguage info) source'
  case ghcResult of
    Left err ->
      pure
        FileResult
          { filePath = file,
            ghcError = True,
            parseError = False,
            roundtripFail = False,
            ghcErrorMsg = Just (prefixCppErrors cppErrorMsg err),
            parseErrorMsg = Nothing,
            roundtripErrorMsg = Nothing
          }
    Right () ->
      case validateParserDetailed source' of
        Nothing ->
          pure
            FileResult
              { filePath = file,
                ghcError = False,
                parseError = False,
                roundtripFail = False,
                ghcErrorMsg = Nothing,
                parseErrorMsg = cppErrorMsg,
                roundtripErrorMsg = Nothing
              }
        Just err ->
          case validationErrorKind err of
            ValidationParseError ->
              pure
                FileResult
                  { filePath = file,
                    ghcError = False,
                    parseError = True,
                    roundtripFail = False,
                    ghcErrorMsg = Nothing,
                    parseErrorMsg = Just (prefixCppErrors cppErrorMsg (T.pack (validationErrorMessage err))),
                    roundtripErrorMsg = Nothing
                  }
            ValidationRoundtripError ->
              pure
                FileResult
                  { filePath = file,
                    ghcError = False,
                    parseError = False,
                    roundtripFail = True,
                    ghcErrorMsg = Nothing,
                    parseErrorMsg = cppErrorMsg,
                    roundtripErrorMsg = Just (prefixCppErrors cppErrorMsg (T.pack (validationErrorMessage err)))
                  }

printSummary :: [FileResult] -> IO ()
printSummary results = do
  let total = length results
      ghcErrs = length (filter ghcError results)
      parseErrs = length (filter parseError results)
      roundtripFails = length (filter roundtripFail results)
      successCount = total - ghcErrs - parseErrs - roundtripFails
      successRate :: Double
      successRate = if total > 0 then fromIntegral successCount * 100.0 / fromIntegral total else 0.0

  putStrLn ""
  putStrLn "Summary:"
  putStrLn ("  Total files:     " ++ show total)
  putStrLn ("  GHC errors:      " ++ show ghcErrs)
  putStrLn ("  Parse errors:    " ++ show parseErrs)
  putStrLn ("  Roundtrip fails: " ++ show roundtripFails)
  putStrLn ("  Success rate:    " ++ show (round successRate :: Int) ++ "%")
