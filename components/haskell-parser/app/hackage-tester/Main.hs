{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, foldM, when)
import Data.List (isPrefixOf, isSuffixOf)
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
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import qualified Parser
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    getHomeDirectory,
    listDirectory,
    removeFile,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

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
      files <- findHaskellFiles srcDir
      putStrLn ("Found " ++ show (length files) ++ " Haskell source files")
      results <- processFiles files
      printSummary results
      let failed = filter (\r -> parseError r || roundtripFail r) results
      if null failed
        then exitSuccess
        else exitFailure

getLatestVersion :: String -> IO (Maybe String)
getLatestVersion packageName = do
  let url = "https://hackage.haskell.org/package/" ++ packageName ++ "/" ++ packageName ++ ".cabal"
  (exitCode, stdout, _) <- readProcessWithExitCode "curl" ["-sL", url] ""
  if exitCode == ExitSuccess
    then
      let cabalContent = T.pack stdout
          lines' = T.lines cabalContent
          versionLines = filter (T.isPrefixOf "version:") lines'
       in case versionLines of
            [] -> pure Nothing
            (vline : _) ->
              let ver = T.strip (T.drop (T.length "version:") vline)
               in pure (Just (T.unpack ver))
    else pure Nothing

downloadPackage :: String -> String -> IO FilePath
downloadPackage packageName version = do
  cacheDir <- getCacheDir
  let pkgDir = cacheDir </> packageName ++ "-" ++ version
  exists <- doesDirectoryExist pkgDir
  if exists
    then do
      putStrLn ("Cache hit: " ++ packageName ++ "-" ++ version)
      pure pkgDir
    else do
      createDirectoryIfMissing True cacheDir
      putStrLn ("Downloading " ++ packageName ++ "-" ++ version ++ " from Hackage...")
      let url = "https://hackage.haskell.org/package/" ++ packageName ++ "-" ++ version ++ "/" ++ packageName ++ "-" ++ version ++ ".tar.gz"
      let tarball = cacheDir </> packageName ++ "-" ++ version ++ ".tar.gz"
      downloadFile url tarball
      extractTarball tarball cacheDir (packageName ++ "-" ++ version)
      removeFile tarball
      pure pkgDir

downloadFile :: String -> FilePath -> IO ()
downloadFile url dest = do
  (exitCode, _, stderr) <- readProcessWithExitCode "curl" ["-sL", "-o", dest, url] ""
  when (exitCode /= ExitSuccess) $
    error ("Failed to download: " ++ url ++ "\n" ++ stderr)

extractTarball :: FilePath -> FilePath -> String -> IO ()
extractTarball tarball destDir packageDir = do
  let extractDir = destDir </> packageDir
  createDirectoryIfMissing True extractDir
  (exitCode, _, stderr) <- readProcessWithExitCode "tar" ["-xzf", tarball, "-C", extractDir, "--strip-components=1"] ""
  when (exitCode /= ExitSuccess) $
    error ("Failed to extract tarball: " ++ stderr)

getCacheDir :: IO FilePath
getCacheDir = do
  home <- getHomeDirectory
  pure (home </> ".cache" </> "aihc" </> "hackage")

findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = go [] ""
  where
    go acc subdir = do
      let fullDir = dir </> subdir
      entries <- listDirectory fullDir
      foldM
        ( \acc' entry -> do
            let entryPath = subdir </> entry
            let fullPath = fullDir </> entry
            isDir <- doesDirectoryExist fullPath
            if isDir
              then
                if ".git" `isPrefixOf` entry
                  then pure acc'
                  else go acc' entryPath
              else
                if ".hs" `isSuffixOf` entry || ".lhs" `isSuffixOf` entry
                  then pure (fullPath : acc')
                  else pure acc'
        )
        acc
        entries

data FileResult = FileResult
  { filePath :: FilePath,
    parseError :: Bool,
    roundtripFail :: Bool,
    parseErrorMsg :: Maybe Text,
    roundtripErrorMsg :: Maybe Text
  }

processFiles :: [FilePath] -> IO [FileResult]
processFiles files = do
  results <- forM files $ \file -> do
    result <- processFile file
    case (parseError result, roundtripFail result) of
      (True, _) -> putStrLn ("PARSE_ERROR: " ++ file)
      (_, True) -> putStrLn ("ROUNDTRIP_FAIL: " ++ file)
      _ -> pure ()
    pure result
  pure results

processFile :: FilePath -> IO FileResult
processFile file = do
  source <- TIO.readFile file
  let oursResult = Parser.parseModule Parser.defaultConfig source
  case oursResult of
    ParseErr err ->
      pure
        FileResult
          { filePath = file,
            parseError = True,
            roundtripFail = False,
            parseErrorMsg = Just (T.pack (show err)),
            roundtripErrorMsg = Nothing
          }
    ParseOk parsed -> do
      let rendered = prettyModule parsed
      let sourceAst = oracleModuleAstFingerprint source
      let renderedAst = oracleModuleAstFingerprint rendered
      case (sourceAst, renderedAst) of
        (Right sa, Right ra) ->
          if sa == ra
            then
              pure
                FileResult
                  { filePath = file,
                    parseError = False,
                    roundtripFail = False,
                    parseErrorMsg = Nothing,
                    roundtripErrorMsg = Nothing
                  }
            else
              pure
                FileResult
                  { filePath = file,
                    parseError = False,
                    roundtripFail = True,
                    parseErrorMsg = Nothing,
                    roundtripErrorMsg = Just "AST fingerprint mismatch"
                  }
        _ ->
          pure
            FileResult
              { filePath = file,
                parseError = False,
                roundtripFail = True,
                parseErrorMsg = Nothing,
                roundtripErrorMsg = Just "Failed to get AST fingerprint"
              }

oracleModuleAstFingerprint :: Text -> Either Text Text
oracleModuleAstFingerprint input = do
  parsed <- parseWithGhc input
  pure (T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhc :: Text -> Either Text (HsModule GhcPs)
parseWithGhc input =
  let exts = EnumSet.fromList [ForeignFunctionInterface] :: EnumSet.EnumSet Extension
      opts = Lexer.mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<hackage-tester>") 1 1
   in case Lexer.unP GHCParser.parseModule (Lexer.initParserState opts buffer start) of
         Lexer.POk _ modu -> Right (unLoc modu)
         Lexer.PFailed _ -> Left "ghc parse failed"

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
