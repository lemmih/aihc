-- | Shared file-system utilities for Hackage package processing.
module Aihc.Hackage.Util
  ( readTextFileLenient,
    existingPaths,
    dedupeExistingFiles,
    findCabalFiles,
    chooseBestCabalFile,
    moduleFilesForBuildInfo,
    sourceDirs,
  )
where

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription (BuildInfo, hsSourceDirs)
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (makeRelative, normalise, splitDirectories, takeFileName, (<.>), (</>))

-- | Read a file as 'Text' with lenient UTF-8 decoding.
readTextFileLenient :: FilePath -> IO Text
readTextFileLenient filePath = do
  bytes <- BS.readFile filePath
  pure (decodeUtf8With lenientDecode bytes)

-- | Return only the paths that exist on disk, normalised.
existingPaths :: [FilePath] -> IO [FilePath]
existingPaths candidates = do
  existing <- forM candidates $ \candidate -> do
    fileExists <- doesFileExist candidate
    pure (if fileExists then Just (normalise candidate) else Nothing)
  pure (catMaybes existing)

-- | Deduplicate and filter to existing files.
dedupeExistingFiles :: [FilePath] -> IO [FilePath]
dedupeExistingFiles files = fmap nub (existingPaths files)

-- | Find all @.cabal@ files under a directory (recursive, skips @.git@).
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
  entries <- listDirectory dir
  paths <- fmap concat $
    forM entries $ \entry -> do
      let fullPath = dir </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then
          if ".git" `isPrefixOf` entry
            then pure []
            else findCabalFiles fullPath
        else
          if ".cabal" `isSuffixOf` entry
            then pure [fullPath]
            else pure []
  pure (nub (map normalise paths))

-- | When multiple @.cabal@ files are found, pick the \"best\" one.
--
-- Heuristic: prefer files closer to the root and outside test\/example
-- directories.
chooseBestCabalFile :: FilePath -> [FilePath] -> FilePath
chooseBestCabalFile extractedRoot files =
  case sortOn rank files of
    best : _ -> best
    [] -> error ("chooseBestCabalFile: no .cabal files found under " ++ extractedRoot)
  where
    rank file =
      let rel = splitDirectories (makeRelative extractedRoot file)
          dirParts = case reverse rel of
            _fileName : restRev -> reverse restRev
            [] -> []
          lowerDirParts = map (map toLower) dirParts
          isLikelyFixtureDir = any (`elem` fixtureDirNames) lowerDirParts
       in ( if isLikelyFixtureDir then (1 :: Int) else 0,
            length rel,
            length dirParts,
            scoreByFileName (map toLower (takeFileName file)),
            file
          )

    scoreByFileName fileNameLower
      | "test-" `isPrefixOf` fileNameLower = 1 :: Int
      | "example-" `isPrefixOf` fileNameLower = 1
      | otherwise = 0

    fixtureDirNames =
      [ "test",
        "tests",
        "testing",
        "example",
        "examples",
        "benchmark",
        "benchmarks"
      ]

-- | Resolve module names to existing source files for a 'BuildInfo'.
--
-- Each module uses the first file that exists in @hs-source-dirs@ order.
moduleFilesForBuildInfo :: FilePath -> BuildInfo -> [ModuleName] -> IO [FilePath]
moduleFilesForBuildInfo packageRoot build modules = do
  let dirs = sourceDirs packageRoot build
  fmap catMaybes (mapM (firstExistingModule dirs) modules)

firstExistingModule :: [FilePath] -> ModuleName -> IO (Maybe FilePath)
firstExistingModule dirs modu =
  firstExisting
    [ dir </> toFilePath modu <.> ext
    | dir <- dirs,
      ext <- ["hs", "lhs"]
    ]

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (candidate : rest) = do
  exists <- doesFileExist candidate
  if exists
    then pure (Just (normalise candidate))
    else firstExisting rest

-- | Compute source directories from a 'BuildInfo'.
sourceDirs :: FilePath -> BuildInfo -> [FilePath]
sourceDirs packageRoot build =
  case map getSymbolicPath (hsSourceDirs build) of
    [] -> [packageRoot]
    dirs -> [packageRoot </> dir | dir <- dirs]
