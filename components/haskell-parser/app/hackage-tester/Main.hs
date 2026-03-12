{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import CppSupport (preprocessForParser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HackageSupport (diagToText, downloadPackage, findTargetFilesFromCabal, prefixCppErrors, resolveIncludeBestEffort)
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
      let failed = filter (\r -> parseError r || roundtripFail r) results
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
    parseError :: Bool,
    roundtripFail :: Bool,
    parseErrorMsg :: Maybe Text,
    roundtripErrorMsg :: Maybe Text
  }

processFiles :: FilePath -> [FilePath] -> IO [FileResult]
processFiles packageRoot files =
  forM files $ \file -> do
    result <- processFile packageRoot file
    case (parseError result, roundtripFail result) of
      (True, _) -> putStrLn ("PARSE_ERROR: " ++ file)
      (_, True) -> putStrLn ("ROUNDTRIP_FAIL: " ++ file)
      _ -> pure ()
    pure result

processFile :: FilePath -> FilePath -> IO FileResult
processFile packageRoot file = do
  source <- TIO.readFile file
  preprocessed <- preprocessForParser file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrors = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      cppErrorMsg =
        if null cppErrors
          then Nothing
          else Just (T.intercalate "\n" cppErrors)
  case validateParserDetailed source' of
    Nothing ->
      pure
        FileResult
          { filePath = file,
            parseError = False,
            roundtripFail = False,
            parseErrorMsg = cppErrorMsg,
            roundtripErrorMsg = Nothing
          }
    Just err ->
      case validationErrorKind err of
        ValidationParseError ->
          pure
            FileResult
              { filePath = file,
                parseError = True,
                roundtripFail = False,
                parseErrorMsg = Just (prefixCppErrors cppErrorMsg (T.pack (validationErrorMessage err))),
                roundtripErrorMsg = Nothing
              }
        ValidationRoundtripError ->
          pure
            FileResult
              { filePath = file,
                parseError = False,
                roundtripFail = True,
                parseErrorMsg = cppErrorMsg,
                roundtripErrorMsg = Just (prefixCppErrors cppErrorMsg (T.pack (validationErrorMessage err)))
              }

printSummary :: [FileResult] -> IO ()
printSummary results = do
  let total = length results
      parseErrs = length (filter parseError results)
      roundtripFails = length (filter roundtripFail results)
      successCount = total - parseErrs - roundtripFails
      successRate :: Double
      successRate = if total > 0 then fromIntegral successCount * 100.0 / fromIntegral total else 0.0

  putStrLn ""
  putStrLn "Summary:"
  putStrLn ("  Total files:     " ++ show total)
  putStrLn ("  Parse errors:    " ++ show parseErrs)
  putStrLn ("  Roundtrip fails: " ++ show roundtripFails)
  putStrLn ("  Success rate:    " ++ show (round successRate :: Int) ++ "%")
