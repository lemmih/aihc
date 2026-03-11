{-# LANGUAGE OverloadedStrings #-}

module HackageSupport
  ( downloadPackage,
    findTargetFilesFromCabal,
    resolveIncludeBestEffort,
    diagToText,
    prefixCppErrors,
  )
where

import Control.Monad (forM, when)
import Cpp (Diagnostic (..), IncludeKind (..), IncludeRequest (..), Severity (..))
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, isSuffixOf, nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription
  ( BuildInfo,
    Executable,
    Library,
    autogenModules,
    buildInfo,
    condExecutables,
    condLibrary,
    condSubLibraries,
    exeModules,
    exposedModules,
    hsSourceDirs,
    libBuildInfo,
    modulePath,
    otherModules,
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.CondTree
  ( CondBranch,
    CondTree,
    condBranchIfFalse,
    condBranchIfTrue,
    condTreeComponents,
    condTreeData,
  )
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Utils.Path (getSymbolicPath)
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
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, (<.>), (</>))
import System.Process (callCommand)

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
      dirExists <- doesDirectoryExist pkgDir
      when dirExists $ removeDirectoryRecursive pkgDir
      renameDirectory tempDir pkgDir
      writeFile markerFile ""
      pure pkgDir

downloadFile :: String -> FilePath -> IO ()
downloadFile url dest =
  callCommand ("curl -s -f -o " ++ show dest ++ " " ++ show url)

extractTarball :: FilePath -> FilePath -> IO ()
extractTarball tarball destDir = do
  createDirectoryIfMissing True destDir
  callCommand ("tar -xzf " ++ show tarball ++ " -C " ++ show destDir)

getCacheDir :: IO FilePath
getCacheDir = do
  cacheBase <- getXdgDirectory XdgCache "aihc"
  pure (cacheBase </> "hackage")

findTargetFilesFromCabal :: FilePath -> IO [FilePath]
findTargetFilesFromCabal extractedRoot = do
  cabalFiles <- findCabalFiles extractedRoot
  cabalFile <-
    case cabalFiles of
      [file] -> pure file
      [] ->
        ioError
          ( userError
              ("No .cabal file found under extracted package root: " ++ extractedRoot)
          )
      _ ->
        ioError
          ( userError
              ("Multiple .cabal files found under extracted package root: " ++ show cabalFiles)
          )
  cabalBytes <- BS.readFile cabalFile
  let (_, parseResult) = runParseResult (parseGenericPackageDescription cabalBytes)
  gpd <-
    case parseResult of
      Right parsed -> pure parsed
      Left (_, errs) ->
        ioError
          ( userError
              ("Failed to parse cabal file " ++ cabalFile ++ ": " ++ show errs)
          )
  collectComponentFiles gpd cabalFile

collectComponentFiles :: GenericPackageDescription -> FilePath -> IO [FilePath]
collectComponentFiles gpd cabalFile = do
  let packageRoot = takeDirectory cabalFile
      libraryTrees = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd)
      executableTrees = map snd (condExecutables gpd)
      libraries = concatMap flattenCondTree libraryTrees
      executables = concatMap flattenCondTree executableTrees
  libraryFiles <- fmap concat (forM libraries (libraryFilesFor packageRoot))
  executableFiles <- fmap concat (forM executables (executableFilesFor packageRoot))
  dedupeExistingFiles (libraryFiles <> executableFiles)

libraryFilesFor :: FilePath -> Library -> IO [FilePath]
libraryFilesFor packageRoot library =
  moduleFilesForBuildInfo packageRoot build moduleNames
  where
    build = libBuildInfo library
    moduleNames = exposedModules library <> otherModules build <> autogenModules build

executableFilesFor :: FilePath -> Executable -> IO [FilePath]
executableFilesFor packageRoot executable = do
  moduleFiles <- moduleFilesForBuildInfo packageRoot build moduleNames
  mainFiles <- existingPaths [dir </> mainPath | dir <- sourceDirs packageRoot build]
  pure (moduleFiles <> mainFiles)
  where
    build = buildInfo executable
    moduleNames = otherModules build <> exeModules executable <> autogenModules build
    mainPath = modulePath executable

moduleFilesForBuildInfo :: FilePath -> BuildInfo -> [ModuleName] -> IO [FilePath]
moduleFilesForBuildInfo packageRoot build modules = do
  let dirs = sourceDirs packageRoot build
      moduleCandidates =
        [ dir </> toFilePath modu <.> ext
        | dir <- dirs,
          modu <- modules,
          ext <- ["hs", "lhs"]
        ]
  dedupeExistingFiles moduleCandidates

sourceDirs :: FilePath -> BuildInfo -> [FilePath]
sourceDirs packageRoot build =
  case map getSymbolicPath (hsSourceDirs build) of
    [] -> [packageRoot]
    dirs -> [packageRoot </> dir | dir <- dirs]

flattenCondTree :: CondTree v c a -> [a]
flattenCondTree tree =
  condTreeData tree : concatMap flattenBranch (condTreeComponents tree)

flattenBranch :: CondBranch v c a -> [a]
flattenBranch branch =
  flattenCondTree (condBranchIfTrue branch)
    <> maybe [] flattenCondTree (condBranchIfFalse branch)

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

existingPaths :: [FilePath] -> IO [FilePath]
existingPaths candidates = do
  existing <- forM candidates $ \candidate -> do
    fileExists <- doesFileExist candidate
    pure (if fileExists then Just (normalise candidate) else Nothing)
  pure (catMaybes existing)

dedupeExistingFiles :: [FilePath] -> IO [FilePath]
dedupeExistingFiles files = fmap nub (existingPaths files)

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
