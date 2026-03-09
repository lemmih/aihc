{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (foldM, forM, when)
import Cpp
  ( Diagnostic (..),
    IncludeKind (..),
    IncludeRequest (..),
    Severity (..),
    resultDiagnostics,
    resultOutput,
  )
import CppSupport (preprocessForParser)
import Data.List (isPrefixOf, isSuffixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import GHC.LanguageExtensions.Type (Extension (ForeignFunctionInterface))
import qualified GHC.Parser as GHCParser
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import qualified Parser
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameDirectory,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, (</>))
import System.Process (callCommand, readProcess)

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
      results <- processFiles srcDir files
      printSummary results
      let failed = filter (\r -> parseError r || roundtripFail r) results
      if null failed
        then exitSuccess
        else exitFailure

getLatestVersion :: String -> IO (Maybe String)
getLatestVersion packageName = do
  let url = "https://hackage.haskell.org/package/" ++ packageName ++ "/" ++ packageName ++ ".cabal"
  result <- readProcess "curl" ["-s", "-f", url] ""
  let cabalContent = T.pack result
      lines' = T.lines cabalContent
      versionLines = filter (T.isPrefixOf "version:") lines'
   in case versionLines of
        [] -> pure Nothing
        (vline : _) ->
          let ver = T.strip (T.drop (T.length "version:") vline)
           in pure (Just (T.unpack ver))

downloadPackage :: String -> String -> IO FilePath
downloadPackage packageName version = do
  cacheDir <- getCacheDir
  let pkgDir = cacheDir </> packageName ++ "-" ++ version
      markerFile = pkgDir </> ".complete"
  markerExists <- doesFileExist markerFile
  if markerExists
    then do
      putStrLn ("Cache hit: " ++ packageName ++ "-" ++ version)
      pure pkgDir
    else do
      createDirectoryIfMissing True cacheDir
      putStrLn ("Downloading " ++ packageName ++ "-" ++ version ++ " from Hackage...")
      let url = "https://hackage.haskell.org/package/" ++ packageName ++ "-" ++ version ++ "/" ++ packageName ++ "-" ++ version ++ ".tar.gz"
      let tarball = cacheDir </> packageName ++ "-" ++ version ++ ".tar.gz"
      let tempDir = cacheDir </> packageName ++ "-" ++ version ++ ".tmp"
      downloadFile url tarball
      extractTarball tarball tempDir
      removeFile tarball
      -- Clean up on success before renaming
      dirExists <- doesDirectoryExist pkgDir
      when dirExists $ removeDirectoryRecursive pkgDir
      renameDirectory tempDir pkgDir
      writeFile markerFile ""
      pure pkgDir

downloadFile :: String -> FilePath -> IO ()
downloadFile url dest = do
  callCommand ("curl -s -f -o " ++ show dest ++ " " ++ show url)

-- Extract gzip-compressed tarball using system tar command
extractTarball :: FilePath -> FilePath -> IO ()
extractTarball tarball destDir = do
  createDirectoryIfMissing True destDir
  -- Use system tar to extract gzip-compressed tarball
  -- tar automatically detects compression format
  callCommand ("tar -xzf " ++ show tarball ++ " -C " ++ show destDir)

getCacheDir :: IO FilePath
getCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  pure (cacheBase </> "hackage")

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
      oursResult = Parser.parseModule Parser.defaultConfig source'
  case oursResult of
    ParseErr err ->
      pure
        FileResult
          { filePath = file,
            parseError = True,
            roundtripFail = False,
            parseErrorMsg = Just (prefixCppErrors cppErrorMsg (T.pack (show err))),
            roundtripErrorMsg = Nothing
          }
    ParseOk parsed -> do
      let rendered = prettyModule parsed
      let sourceAst = oracleModuleAstFingerprint source'
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
                    parseErrorMsg = cppErrorMsg,
                    roundtripErrorMsg = Nothing
                  }
            else
              pure
                FileResult
                  { filePath = file,
                    parseError = False,
                    roundtripFail = True,
                    parseErrorMsg = cppErrorMsg,
                    roundtripErrorMsg = Just (prefixCppErrors cppErrorMsg "AST fingerprint mismatch")
                  }
        _ ->
          pure
            FileResult
              { filePath = file,
                parseError = False,
                roundtripFail = True,
                parseErrorMsg = cppErrorMsg,
                roundtripErrorMsg = Just (prefixCppErrors cppErrorMsg "Failed to get AST fingerprint")
              }

resolveIncludeBestEffort :: FilePath -> FilePath -> IncludeRequest -> IO (Maybe Text)
resolveIncludeBestEffort packageRoot currentFile req = do
  firstExisting <- firstExistingPath (includeCandidates packageRoot currentFile req)
  case firstExisting of
    Nothing -> pure Nothing
    Just includeFile -> Just <$> TIO.readFile includeFile

includeCandidates :: FilePath -> FilePath -> IncludeRequest -> [FilePath]
includeCandidates packageRoot currentFile req =
  map normalise $ nub [dir </> includePath req | dir <- searchDirs]
  where
    includeDir = takeDirectory (includeFrom req)
    sourceRelDir = takeDirectory (makeRelative packageRoot currentFile)
    packageAncestors = ancestorDirs sourceRelDir
    localRoots =
      [ takeDirectory currentFile,
        packageRoot </> sourceRelDir,
        packageRoot </> includeDir
      ]
    systemRoots =
      [ packageRoot </> "include",
        packageRoot </> "includes",
        packageRoot </> "cbits",
        packageRoot
      ]
    searchDirs =
      case includeKind req of
        IncludeLocal -> localRoots <> map (packageRoot </>) packageAncestors <> systemRoots
        IncludeSystem -> systemRoots <> localRoots <> map (packageRoot </>) packageAncestors

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  case filter (not . null) (splitDirectories path) of
    [] -> []
    parts ->
      [ foldl (</>) "." (take n parts)
      | n <- [length parts, length parts - 1 .. 1]
      ]

firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (candidate : rest) = do
  let path = if isAbsolute candidate then candidate else normalise candidate
  exists <- doesFileExist path
  if exists
    then pure (Just path)
    else firstExistingPath rest

diagToText :: Diagnostic -> Text
diagToText diag =
  T.pack (diagFile diag)
    <> ":"
    <> T.pack (show (diagLine diag))
    <> ": "
    <> sev
    <> ": "
    <> diagMessage diag
  where
    sev =
      case diagSeverity diag of
        Warning -> "warning"
        Error -> "error"

prefixCppErrors :: Maybe Text -> Text -> Text
prefixCppErrors cppMsg msg =
  case cppMsg of
    Nothing -> msg
    Just cppText -> "cpp diagnostics:\n" <> cppText <> "\n" <> msg

oracleModuleAstFingerprint :: Text -> Either Text Text
oracleModuleAstFingerprint input = do
  parsed <- parseWithGhc input
  pure (T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhc :: Text -> Either Text (HsModule GhcPs)
parseWithGhc input =
  let exts = EnumSet.fromList [ForeignFunctionInterface] :: EnumSet.EnumSet Extension
      opts = mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<hackage-tester>") 1 1
   in case unP GHCParser.parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

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
