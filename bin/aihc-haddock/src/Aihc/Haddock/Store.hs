{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Store
-- Description : The aihc-haddock artifact cache
--
-- A cache separate from the compiler's store. Each package gets one artifact,
-- keyed by the model format version, the package's source files and the keys
-- of its dependencies, so that a change anywhere below a package invalidates
-- it. The artifact holds the documentation model; naming and type information
-- join it once those components are wired in.
module Aihc.Haddock.Store
  ( Store (..),
    defaultStoreRoot,
    ArtifactKey (..),
    packageArtifactKey,
    lookupPackageDoc,
    storePackageDoc,
    documentPlan,
  )
where

import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Haddock.Model
import Aihc.Haddock.Package (loadPackageDoc, packageSpecOf)
import Aihc.PackagePlan (PackagePlan (..))
import Control.Monad (filterM, forM)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
  )
import System.FilePath (makeRelative, takeExtension, takeFileName, (</>))

newtype Store = Store
  { storeRoot :: FilePath
  }

defaultStoreRoot :: IO FilePath
defaultStoreRoot = getXdgDirectory XdgCache "aihc-haddock"

data ArtifactKey = ArtifactKey
  { artifactPackage :: PackageSpec,
    artifactHash :: String
  }
  deriving (Eq, Show)

artifactDirectory :: Store -> ArtifactKey -> FilePath
artifactDirectory store key =
  storeRoot store </> (pkgName (artifactPackage key) <> "-" <> pkgVersion (artifactPackage key) <> "-" <> artifactHash key)

artifactFile :: Store -> ArtifactKey -> FilePath
artifactFile store key = artifactDirectory store key </> "package.json"

-- | Hash the package's source tree and its dependency keys.
packageArtifactKey :: FilePath -> [ArtifactKey] -> IO ArtifactKey
packageArtifactKey root dependencies = do
  spec <- packageSpecOf root
  files <- sort <$> sourceFiles root
  contents <- forM files $ \file -> do
    bytes <- BS.readFile file
    pure (TE.encodeUtf8 (T.pack (makeRelative root file)) <> bytes)
  let chunks =
        TE.encodeUtf8 (T.pack ("aihc-haddock-model-" <> show docModelFormatVersion))
          : map (TE.encodeUtf8 . T.pack . artifactHash) dependencies
            <> contents
  pure (ArtifactKey spec (stableHash chunks))

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles = go
  where
    go dir = do
      entries <- filter (`notElem` ["dist-newstyle", ".git", "dist"]) <$> listDirectory dir
      let paths = map (dir </>) entries
      directories <- filterM doesDirectoryExist paths
      files <- filterM doesFileExist paths
      nested <- mapM go directories
      pure (filter relevant files <> concat nested)
    relevant file =
      takeExtension file `elem` [".hs", ".lhs", ".hs-boot", ".cabal", ".h", ".hsc", ".c"]
        || takeFileName file == "LICENSE"

-- | FNV-1a over the chunks, rendered as sixteen hex digits.
stableHash :: [BS.ByteString] -> String
stableHash chunks = replicate (16 - length rendered) '0' <> rendered
  where
    rendered = showHex (foldl hashChunk (14695981039346656037 :: Word64) chunks) ""
    hashChunk :: Word64 -> BS.ByteString -> Word64
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * 1099511628211)

lookupPackageDoc :: Store -> ArtifactKey -> IO (Maybe PackageDoc)
lookupPackageDoc store key = do
  let file = artifactFile store key
  exists <- doesFileExist file
  if not exists
    then pure Nothing
    else do
      bytes <- BL.readFile file
      pure (either (const Nothing) Just (decodePackageDoc bytes))

storePackageDoc :: Store -> ArtifactKey -> PackageDoc -> IO FilePath
storePackageDoc store key package = do
  createDirectoryIfMissing True (artifactDirectory store key)
  BL.writeFile (artifactFile store key) (encodePackageDoc package)
  pure (artifactFile store key)

-- | Document a package plan: dependencies first, each cached under its key.
-- With @documentDependencies@ off only the root is documented and its
-- dependencies contribute just their versions.
documentPlan :: Store -> Bool -> Bool -> (String -> IO ()) -> PackagePlan -> IO PackageDoc
documentPlan store useCache documentDependencies say plan = do
  dependencyKeys <-
    if documentDependencies
      then forM (planDependencyPlans plan) $ \dependency -> do
        (key, _) <- go dependency
        pure key
      else forM (planDependencyPlans plan) $ \dependency -> do
        spec <- packageSpecOf (planSourcePath dependency)
        pure (ArtifactKey spec "")
  snd <$> documentRoot dependencyKeys (planSourcePath plan)
  where
    go dependency = do
      dependencyKeys <- forM (planDependencyPlans dependency) (fmap fst . go)
      documentRoot dependencyKeys (planSourcePath dependency)

    documentRoot dependencyKeys root = do
      key <- packageArtifactKey root dependencyKeys
      cached <- if useCache then lookupPackageDoc store key else pure Nothing
      case cached of
        Just package -> do
          say ("cached " <> pkgName (artifactPackage key) <> "-" <> pkgVersion (artifactPackage key))
          pure (key, package)
        Nothing -> do
          say ("documenting " <> pkgName (artifactPackage key) <> "-" <> pkgVersion (artifactPackage key))
          package <- loadPackageDoc root (map artifactPackage dependencyKeys)
          _ <- storePackageDoc store key package
          pure (key, package)
