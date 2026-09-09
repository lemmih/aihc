module Aihc.Cli.ArtifactCache
  ( hashChunks,
    compilerBuildIdentity,
    executableIdentity,
    sourceFilesHash,
  )
where

import Aihc.CompilerBuildIdentity (compilerBuildIdentity)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Numeric (showHex)
import System.Directory (canonicalizePath, findExecutable)
import System.FilePath (makeRelative)

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
