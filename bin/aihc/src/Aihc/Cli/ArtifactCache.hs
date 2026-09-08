module Aihc.Cli.ArtifactCache
  ( hashChunks,
    compilerBuildIdentity,
    executableIdentity,
    sourceFilesHash,
    restoreArtifacts,
    publishArtifacts,
  )
where

import Aihc.CompilerBuildIdentity (compilerBuildIdentity)
import Control.Exception (IOException, bracket, evaluate, try)
import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (showHex)
import System.Directory (canonicalizePath, copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, findExecutable, removeDirectoryRecursive, removeFile, renameDirectory)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (hClose, openBinaryTempFile)

-- Each field has a length prefix to prevent ambiguous concatenation.
hashChunks :: [BS.ByteString] -> String
hashChunks = concatMap hex . BS.unpack . SHA256.hashlazy . BL.fromChunks . concatMap field
  where
    field bytes = [BS8.pack (show (BS.length bytes) <> ":"), bytes]
    hex byte = let value = showHex byte "" in replicate (2 - length value) '0' <> value

executableIdentity :: FilePath -> IO String
executableIdentity command = do
  found <- findExecutable command
  path <- maybe (ioError (userError ("Compiler tool is absent: " <> command))) canonicalizePath found
  pure (hashChunks [TE.encodeUtf8 (T.pack path)])

sourceFilesHash :: FilePath -> [FilePath] -> IO String
sourceFilesHash root files = do
  chunks <- forM (sort (nub files)) $ \path -> do
    bytes <- BS.readFile path
    digest <- evaluate (SHA256.hash bytes)
    pure [TE.encodeUtf8 (T.pack (makeRelative root path)), digest]
  pure (hashChunks (concat chunks))

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
