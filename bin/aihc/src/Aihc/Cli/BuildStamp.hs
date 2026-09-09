{-# LANGUAGE OverloadedStrings #-}

-- | Records of what a build directory holds, so that the next build can
-- reuse an artifact without reading it.
--
-- A local package builds in place. Every unit of the package writes a stamp
-- beside its artifacts: the digests of the inputs it was built from, the
-- digests of the artifacts it wrote, and the size and modification time of
-- each artifact file. A later build compares the recorded inputs with the
-- digests it has in hand for the same unit, checks that the artifact files
-- are the ones the stamp describes, and reuses them on a match. Nothing is
-- hashed for the comparison: source digests come from parsing, and artifact
-- digests were taken from the bytes as they were written.
--
-- A package writes one 'PackageDigests' record next to its manifest so that a
-- consumer learns the digests of its interfaces without encoding them again.
module Aihc.Cli.BuildStamp
  ( FileStamp (..),
    ResolveStamp (..),
    UnitStamp (..),
    BackendStamp (..),
    PackageDigests (..),
    ModuleDigests (..),
    packageDigestsPath,
    readStamp,
    writeStamp,
    stampFiles,
    filesMatchStamps,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime, renameFile)
import System.FilePath (takeDirectory, (</>))

-- | The identity of an artifact file as the stamp saw it.
data FileStamp = FileStamp
  { fileStampPath :: !FilePath,
    fileStampSize :: !Integer,
    fileStampModified :: !String
  }
  deriving (Eq, Show)

instance Aeson.ToJSON FileStamp where
  toJSON stamp =
    Aeson.object
      [ "path" .= fileStampPath stamp,
        "size" .= fileStampSize stamp,
        "modified" .= fileStampModified stamp
      ]

instance Aeson.FromJSON FileStamp where
  parseJSON = Aeson.withObject "FileStamp" $ \object ->
    FileStamp <$> object .: "path" <*> object .: "size" <*> object .: "modified"

-- | What the resolve phase of a unit was built from and produced.
data ResolveStamp = ResolveStamp
  { resolveStampInputs :: ![(Text, Text)],
    -- | The scope digest of each module of the unit.
    resolveStampScopes :: !(Map.Map Text Text),
    resolveStampFiles :: ![FileStamp]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ResolveStamp where
  toJSON stamp =
    Aeson.object
      [ "schemaVersion" .= (1 :: Int),
        "inputs" .= resolveStampInputs stamp,
        "scopes" .= resolveStampScopes stamp,
        "files" .= resolveStampFiles stamp
      ]

instance Aeson.FromJSON ResolveStamp where
  parseJSON = Aeson.withObject "ResolveStamp" $ \object -> do
    schemaVersion <- object .: "schemaVersion"
    case schemaVersion :: Int of
      1 -> ResolveStamp <$> object .: "inputs" <*> object .: "scopes" <*> object .: "files"
      _ -> fail "unsupported resolve stamp schema"

-- | What the type-check and backend phases of a unit were built from and
-- produced.
data UnitStamp = UnitStamp
  { unitStampInputs :: ![(Text, Text)],
    -- | The interface digest of each module of the unit.
    unitStampTypes :: !(Map.Map Text Text),
    -- | The digest of the instance facts of the unit.
    unitStampFacts :: !Text,
    -- | The type artifacts and the facts artifact.
    unitStampFiles :: ![FileStamp],
    -- | The objects, or nothing when the unit has none.
    unitStampBackend :: !(Maybe BackendStamp)
  }
  deriving (Eq, Show)

-- | The backend outputs of a unit and the options they were built with.
data BackendStamp = BackendStamp
  { backendStampOptions :: !Text,
    backendStampFiles :: ![FileStamp]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON BackendStamp where
  toJSON stamp = Aeson.object ["options" .= backendStampOptions stamp, "files" .= backendStampFiles stamp]

instance Aeson.FromJSON BackendStamp where
  parseJSON = Aeson.withObject "BackendStamp" $ \object ->
    BackendStamp <$> object .: "options" <*> object .: "files"

instance Aeson.ToJSON UnitStamp where
  toJSON stamp =
    Aeson.object
      [ "schemaVersion" .= (1 :: Int),
        "inputs" .= unitStampInputs stamp,
        "types" .= unitStampTypes stamp,
        "facts" .= unitStampFacts stamp,
        "files" .= unitStampFiles stamp,
        "backend" .= unitStampBackend stamp
      ]

instance Aeson.FromJSON UnitStamp where
  parseJSON = Aeson.withObject "UnitStamp" $ \object -> do
    schemaVersion <- object .: "schemaVersion"
    case schemaVersion :: Int of
      1 ->
        UnitStamp
          <$> object .: "inputs"
          <*> object .: "types"
          <*> object .: "facts"
          <*> object .: "files"
          <*> object .: "backend"
      _ -> fail "unsupported unit stamp schema"

-- | The digests of the interfaces of one module of an installed package.
data ModuleDigests = ModuleDigests
  { moduleScopeDigest :: !Text,
    moduleTypeDigest :: !Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ModuleDigests where
  toJSON digests = Aeson.object ["scope" .= moduleScopeDigest digests, "type" .= moduleTypeDigest digests]

instance Aeson.FromJSON ModuleDigests where
  parseJSON = Aeson.withObject "ModuleDigests" $ \object ->
    ModuleDigests <$> object .: "scope" <*> object .: "type"

-- | The digests of everything a consumer of an installed package reads.
data PackageDigests = PackageDigests
  { packageDigestsModules :: !(Map.Map Text ModuleDigests),
    -- | The digest of the package instance artifact.
    packageDigestsInstances :: !Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON PackageDigests where
  toJSON digests =
    Aeson.object
      [ "schemaVersion" .= (1 :: Int),
        "modules" .= packageDigestsModules digests,
        "instances" .= packageDigestsInstances digests
      ]

instance Aeson.FromJSON PackageDigests where
  parseJSON = Aeson.withObject "PackageDigests" $ \object -> do
    schemaVersion <- object .: "schemaVersion"
    case schemaVersion :: Int of
      1 -> PackageDigests <$> object .: "modules" <*> object .: "instances"
      _ -> fail "unsupported package digests schema"

packageDigestsPath :: FilePath -> FilePath
packageDigestsPath packageRoot = packageRoot </> "digests.json"

-- | Read a stamp. An absent or unreadable stamp is no stamp: the artifacts
-- it would describe are rebuilt.
readStamp :: (Aeson.FromJSON stamp) => FilePath -> IO (Maybe stamp)
readStamp path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
      pure (either (const Nothing) (either (const Nothing) Just . Aeson.eitherDecode) result)

-- | Write a stamp after the artifacts it describes, so that a stamp never
-- describes artifacts that are not there.
writeStamp :: (Aeson.ToJSON stamp) => FilePath -> stamp -> IO ()
writeStamp path stamp = do
  createDirectoryIfMissing True (takeDirectory path)
  let temporary = path <> ".tmp"
  BL.writeFile temporary (Aeson.encode stamp)
  renameFile temporary path

-- | Record the artifact files of a unit as they are now.
stampFiles :: FilePath -> [FilePath] -> IO [FileStamp]
stampFiles root = mapM stampFile
  where
    stampFile path = do
      size <- getFileSize (root </> path)
      modified <- getModificationTime (root </> path)
      pure (FileStamp path size (show modified))

-- | Whether every artifact file is the one the stamp recorded. A rewritten
-- file has a new modification time, so a build that stopped between writing
-- an artifact and its stamp does not pass off the new artifact as the old.
filesMatchStamps :: FilePath -> [FileStamp] -> IO Bool
filesMatchStamps root = fmap and . mapM matches
  where
    matches stamp = do
      let path = root </> fileStampPath stamp
      exists <- doesFileExist path
      if not exists
        then pure False
        else do
          size <- getFileSize path
          modified <- getModificationTime path
          pure (size == fileStampSize stamp && show modified == fileStampModified stamp)
