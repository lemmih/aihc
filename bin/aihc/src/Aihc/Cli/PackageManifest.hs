{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cli.PackageManifest
  ( PackageManifest (..),
    packageManifestPath,
    readPackageManifest,
    writePackageManifest,
  )
where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import System.FilePath ((</>))

data PackageManifest = PackageManifest
  { packageManifestName :: !Text,
    packageManifestVersion :: !Text,
    packageManifestIdentity :: !Text,
    packageManifestUnitId :: !Text,
    packageManifestDependencies :: ![Text],
    packageManifestModules :: ![Text],
    -- | The install flags the package was built with, such as @keep-core@
    -- or @no-code@. A store entry never changes, so an install that asks
    -- for an output the entry lacks must rebuild it.
    packageManifestFlags :: ![Text]
  }
  deriving (Eq, Show)

instance Aeson.ToJSON PackageManifest where
  toJSON manifest =
    Aeson.object
      [ "schemaVersion" .= (4 :: Int),
        "name" .= packageManifestName manifest,
        "version" .= packageManifestVersion manifest,
        "identity" .= packageManifestIdentity manifest,
        "unitId" .= packageManifestUnitId manifest,
        "dependencies" .= packageManifestDependencies manifest,
        "modules" .= packageManifestModules manifest,
        "flags" .= packageManifestFlags manifest
      ]

instance Aeson.FromJSON PackageManifest where
  parseJSON = Aeson.withObject "PackageManifest" $ \object -> do
    schemaVersion <- object .: "schemaVersion"
    case schemaVersion :: Int of
      2 -> do
        name <- object .: "name"
        version <- object .: "version"
        identity <- object .: "identity"
        PackageManifest name version identity identity
          <$> object .: "dependencies"
          <*> object .: "modules"
          <*> pure []
      3 ->
        PackageManifest
          <$> object .: "name"
          <*> object .: "version"
          <*> object .: "identity"
          <*> object .: "unitId"
          <*> object .: "dependencies"
          <*> object .: "modules"
          <*> pure []
      4 ->
        PackageManifest
          <$> object .: "name"
          <*> object .: "version"
          <*> object .: "identity"
          <*> object .: "unitId"
          <*> object .: "dependencies"
          <*> object .: "modules"
          <*> object .: "flags"
      _ -> fail "unsupported package manifest schema"

packageManifestPath :: FilePath -> FilePath
packageManifestPath packageRoot = packageRoot </> "package.json"

readPackageManifest :: FilePath -> IO (Either String PackageManifest)
readPackageManifest path = Aeson.eitherDecode <$> BL.readFile path

writePackageManifest :: FilePath -> PackageManifest -> IO ()
writePackageManifest path = BL.writeFile path . Aeson.encode
