{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (filterM, forM, unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort, sortOn, stripPrefix)
import Distribution.InstalledPackageInfo qualified as Installed
import Distribution.ModuleName qualified as ModuleName
import Distribution.PackageDescription
import Distribution.Pretty (prettyShow)
import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenComponentModulesDir, mkSharedLibName)
import Distribution.Simple.Compiler (compilerId)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex qualified as PackageIndex
import Distribution.Simple.Setup qualified as Setup
import Distribution.Types.UnitId (mkUnitId)
import Distribution.Utils.Path (getSymbolicPath)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \pkg lbi hooks flags -> do
          generateIdentity pkg lbi (Setup.buildProgramArgs flags)
          buildHook simpleUserHooks pkg lbi hooks flags,
        replHook = \pkg lbi hooks flags args -> do
          generateIdentity pkg lbi (Setup.replProgramArgs flags)
          replHook simpleUserHooks pkg lbi hooks flags args,
        haddockHook = \pkg lbi hooks flags -> do
          generateIdentity pkg lbi (Setup.haddockProgramArgs flags)
          haddockHook simpleUserHooks pkg lbi hooks flags
      }

-- Cabal supplies the module set and the dependency closure.
-- The generated module has no runtime filesystem requirements.
generateIdentity :: PackageDescription -> LocalBuildInfo -> [(String, [String])] -> IO ()
generateIdentity pkg lbi arguments = withLibLBI pkg lbi $ \lib clbi -> do
  (paths, options, output) <- identityInputs pkg lbi lib clbi
  writeBuildIdentity (paths, options <> show (Setup.configProgramArgs (configFlags lbi), arguments), output)

identityInputs :: PackageDescription -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo -> IO ([FilePath], String, FilePath)
identityInputs pkg lbi lib clbi = do
  let info = libBuildInfo lib
      generated = ModuleName.fromString "Aihc.CompilerBuildIdentity"
      modules = filter (`notElem` autogenModules info) (allLibModules lib clbi)
  sources <- forM modules $ \name -> do
    let stem = ModuleName.toFilePath name
        candidates = [getSymbolicPath dir </> stem <.> suffix | dir <- hsSourceDirs info, suffix <- ["hs", "lhs", "hsc", "chs"]]
    source <- requireFile (prettyShow name) candidates
    boot <- filterM doesFileExist [source <> "-boot"]
    pure (source : boot)
  cabalFile <- maybe (fail "The Cabal file path is absent.") (pure . getSymbolicPath) (pkgDescrFile lbi)
  dependencies <- case PackageIndex.dependencyClosure (installedPkgs lbi) (map fst (componentPackageDeps clbi)) of
    Left index -> pure (sortOn Installed.installedUnitId (PackageIndex.allPackages index))
    Right _ -> fail "The compiler dependency closure is incomplete."
  libraries <- fmap concat $ forM dependencies $ \dependency ->
    fmap concat $ forM (Installed.hsLibraries dependency) $ \name -> do
      let static = [dir </> "lib" <> name <.> "a" | dir <- nub (Installed.libraryDirsStatic dependency <> Installed.libraryDirs dependency)]
          shared = [dir </> mkSharedLibName (hostPlatform lbi) (compilerId (compiler lbi)) (mkUnitId unit) | Just unit <- [stripPrefix "HS" name], dir <- Installed.libraryDynDirs dependency]
      paths <- filterM doesFileExist (nub (static <> shared))
      if null paths
        then fail ("Build identity input is absent: " <> name)
        else pure paths
  let options = show (compilerId (compiler lbi), hostPlatform lbi, withOptimization lbi, withDebugInfo lbi, flagAssignment lbi, info, map Installed.installedUnitId dependencies)
      output = getSymbolicPath (autogenComponentModulesDir lbi clbi) </> ModuleName.toFilePath generated <.> "hs"
      paths = sort (nub (cabalFile : "Setup.hs" : concat sources <> map getSymbolicPath (cSources info) <> map getSymbolicPath (dataFiles pkg) <> libraries))
  pure (paths, options, output)

writeBuildIdentity :: ([FilePath], String, FilePath) -> IO ()
writeBuildIdentity (paths, options, output) = do
  sourceHashes <- mapM hashFile paths
  let identity = hashChunks (BS8.pack options : sourceHashes)
      content = BS8.pack ("module Aihc.CompilerBuildIdentity (compilerBuildIdentity) where\n\ncompilerBuildIdentity :: String\ncompilerBuildIdentity = " <> show (hex identity) <> "\n")
  exists <- doesFileExist output
  unchanged <- if exists then (== content) <$> BS.readFile output else pure False
  unless unchanged $ do
    createDirectoryIfMissing True (takeDirectory output)
    BS.writeFile output content

requireFile :: String -> [FilePath] -> IO FilePath
requireFile label candidates = do
  matches <- filterM doesFileExist candidates
  case matches of
    path : _ -> pure path
    [] -> fail ("Build identity input is absent: " <> label)

hashFile :: FilePath -> IO BS.ByteString
hashFile path = do
  bytes <- BS.readFile path
  evaluate (hashChunks [BS8.pack path, SHA256.hash bytes])

hashChunks :: [BS.ByteString] -> BS.ByteString
hashChunks = SHA256.hashlazy . BL.fromChunks . concatMap (\bytes -> [BS8.pack (show (BS.length bytes) <> ":"), bytes])

hex :: BS.ByteString -> String
hex = concatMap (\byte -> let value = showHex byte "" in replicate (2 - length value) '0' <> value) . BS.unpack
