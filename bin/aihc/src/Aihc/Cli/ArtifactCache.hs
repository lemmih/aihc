module Aihc.Cli.ArtifactCache
  ( hashChunks,
    compilerBuildIdentity,
    executableIdentity,
    sourceTreeHash,
    restoreArtifacts,
    publishArtifacts,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (IOException, bracket, evaluate, try)
import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Numeric (showHex)
import System.Directory (canonicalizePath, copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, findExecutable, listDirectory, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Unsafe (unsafePerformIO)

-- Each field has a length prefix to prevent ambiguous concatenation.
hashChunks :: [BS.ByteString] -> String
hashChunks = concatMap hex . BS.unpack . SHA256.hashlazy . BL.fromChunks . concatMap field
  where
    field bytes = [BS8.pack (show (BS.length bytes) <> ":"), bytes]
    hex byte = let value = showHex byte "" in replicate (2 - length value) '0' <> value

-- The executable cannot change during this process.
{-# NOINLINE compilerBuildIdentity #-}
compilerBuildIdentity :: String
compilerBuildIdentity = unsafePerformIO (getExecutablePath >>= executableIdentity)

{-# NOINLINE executableHashes #-}
executableHashes :: MVar (Map.Map FilePath String)
executableHashes = unsafePerformIO (newMVar Map.empty)

executableIdentity :: FilePath -> IO String
executableIdentity command = do
  found <- findExecutable command
  path <- maybe (ioError (userError ("Compiler tool is absent: " <> command))) canonicalizePath found
  modifyMVar executableHashes $ \hashes -> case Map.lookup path hashes of
    Just digest -> pure (hashes, digest)
    Nothing -> do
      digest <-
        if "/nix/store/" `isPrefixOf` path
          then pure (hashChunks [BS8.pack path])
          else concatMap hex . BS.unpack . SHA256.hashlazy <$> BL.readFile path
      pure (Map.insert path digest hashes, digest)
  where
    hex byte = let value = showHex byte "" in replicate (2 - length value) '0' <> value

sourceTreeHash :: [FilePath] -> FilePath -> IO String
sourceTreeHash excluded root = do
  excludedPaths <- mapM canonicalizePath excluded
  files <- walk excludedPaths Set.empty root
  chunks <- forM files $ \path -> do
    bytes <- BS.readFile path
    digest <- evaluate (SHA256.hash bytes)
    pure [BS8.pack (makeRelative root path), digest]
  pure (hashChunks (concat chunks))
  where
    walk excludedPaths parents directory = do
      canonical <- canonicalizePath directory
      if canonical `elem` excludedPaths
        then pure []
        else do
          unless (canonical `Set.notMember` parents) (ioError (userError "Cyclic source directory"))
          names <- sort <$> listDirectory directory
          fmap concat . forM names $ \name -> do
            let path = directory </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then
                if name `elem` [".git", "dist", "dist-newstyle", ".stack-work", ".aihc-cache"]
                  then pure []
                  else walk excludedPaths (Set.insert canonical parents) path
              else pure [path]

-- Check every output before a cache entry supplies artifacts.
restoreArtifacts :: FilePath -> FilePath -> [FilePath] -> IO Bool
restoreArtifacts cache output expectedPaths = do
  result <- try $ do
    manifest <- Aeson.eitherDecode <$> BL.readFile (cache </> "outputs.json")
    entries <- either (ioError . userError) pure manifest :: IO [(FilePath, String)]
    unless (sort (map fst entries) == sort expectedPaths) (ioError (userError "Incomplete cached artifacts"))
    forM_ entries $ \(path, expected) -> do
      bytes <- BS.readFile (cache </> path)
      unless (hashChunks [bytes] == expected) (ioError (userError "Invalid cached artifact"))
    forM_ entries $ \(path, _) -> do
      createDirectoryIfMissing True (takeDirectory (output </> path))
      copyFile (cache </> path) (output </> path)
  pure (case result :: Either IOException () of Right () -> True; Left _ -> False)

publishArtifacts :: FilePath -> FilePath -> [FilePath] -> IO ()
publishArtifacts cache source paths = do
  createDirectoryIfMissing True (takeDirectory cache)
  bracket temporary cleanup $ \staging -> do
    entries <- forM paths $ \path -> do
      bytes <- BS.readFile (source </> path)
      createDirectoryIfMissing True (takeDirectory (staging </> path))
      BS.writeFile (staging </> path) bytes
      pure (path, hashChunks [bytes])
    BL.writeFile (staging </> "outputs.json") (Aeson.encode entries)
    result <- try (renameDirectory staging cache)
    case result of
      Right () -> pure ()
      Left err -> do
        exists <- doesDirectoryExist cache
        unless exists (ioError (err :: IOException))
  where
    cleanup path = do
      exists <- doesDirectoryExist path
      when exists (removeDirectoryRecursive path)
    temporary = do
      (path, handle) <- openBinaryTempFile (takeDirectory cache) ".tmp-"
      hClose handle
      removeFile path
      createDirectory path
      pure path
