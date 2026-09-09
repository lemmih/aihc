{-# LANGUAGE OverloadedStrings #-}

-- | The core libraries are installed once for a whole test run instead of once
-- per test.
--
-- Installing anything into an empty store compiles aihc-prim from source
-- first, and @build-exe@ additionally needs aihc-base. Measured against a cold
-- store, installing a two-module package allocates ~21 GB; against a store
-- that already holds aihc-prim the same install allocates ~5 MB. aihc-base
-- costs a further ~520 GB. Every test used to pay the cold price into its own
-- temporary directory, which accounted for about two thirds of everything the
-- suite allocated.
--
-- Under CI the store is handed to us: a Nix derivation installs the core
-- libraries once and passes the path in @AIHC_PREBUILT_STORE@. Everywhere else
-- the store is populated once by a tasty 'Test.Tasty.withResource'. Either way
-- a test never works on the seeded store directly -- it gets a private
-- writable copy, because several tests corrupt stored artefacts on purpose.
module Test.Aihc.SeedStore
  ( SeedStore,
    Sandbox (..),
    acquirePrimStore,
    acquireCoreStore,
    releaseSeedStore,
    withSandbox,
    seededPackagePath,
    installTestTargets,
    buildExeHostTarget,
    findCoreLibraryRoot,
    prebuiltStoreVariable,
  )
where

import Aihc.Cli.Install (install)
import Aihc.Cli.Options (InstallOptions (..))
import Aihc.Native (NativeTarget (..), hostNativeTarget, nativeTargetStoreDirectory)
import Control.Exception (IOException, bracket, bracketOnError, try)
import Control.Monad (forM_, unless)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
    getPermissions,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    setOwnerWritable,
    setPermissions,
  )
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Info qualified as System
import System.Process (readProcess)

-- | The environment variable through which CI hands us a directory holding
-- both seeded stores, one per subdirectory named below.
prebuiltStoreVariable :: String
prebuiltStoreVariable = "AIHC_PREBUILT_STORE"

-- | The subdirectory of the prebuilt store holding aihc-prim only.
primStoreDirectory :: FilePath
primStoreDirectory = "prim"

-- | The subdirectory of the prebuilt store holding aihc-prim and aihc-base.
coreStoreDirectory :: FilePath
coreStoreDirectory = "core"

-- | A store holding the core libraries, ready to be copied for a single test.
data SeedStore
  = -- | Built by this process; removed again when the resource is released.
    OwnedStore FilePath
  | -- | Handed to us by CI; read-only and not ours to delete.
    BorrowedStore FilePath

seedStoreRoot :: SeedStore -> FilePath
seedStoreRoot store =
  case store of
    OwnedStore root -> root
    BorrowedStore root -> root

-- | A private temporary directory for one test, plus a way to materialise
-- writable copies of the seeded store inside it.
data Sandbox = Sandbox
  { -- | A scratch directory owned by the test.
    sandboxRoot :: FilePath,
    -- | Copy the seeded store to the named entry of 'sandboxRoot' and return
    -- its path. Tests that need several independent stores call this more than
    -- once.
    sandboxStore :: FilePath -> IO FilePath
  }

-- | The targets the @install@ tests exercise, and therefore the targets
-- aihc-prim is seeded for. Seeding the extra targets is cheap because the
-- frontend Core is shared: only the backend runs again.
installTestTargets :: IO [NativeTarget]
installTestTargets = do
  foreignArchives <- arSupportsForeignObjects
  wasm <- clangSupportsWasm
  pure $
    [AppleArm64, Llvm]
      <> [LinuxAmd64 | foreignArchives]
      <> [Wasm32Wasip3 | wasm && foreignArchives]

-- | Every target the seed store holds aihc-prim for: what the install tests
-- ask for, plus whatever @build-exe@ compiles to. Seeding the extra targets is
-- cheap because the frontend Core is shared across them.
primSeedTargets :: IO [NativeTarget]
primSeedTargets = nub . (buildExeHostTarget :) <$> installTestTargets

-- | The target @build-exe@ compiles for, and therefore the target aihc-base is
-- seeded for.
buildExeHostTarget :: NativeTarget
buildExeHostTarget = fromMaybe Llvm hostNativeTarget

-- | Seed aihc-prim for every target the install tests use.
acquirePrimStore :: IO SeedStore
acquirePrimStore =
  withPreparedStore primStoreDirectory $ \root -> do
    primRoot <- findCoreLibraryRoot "aihc-prim"
    targets <- primSeedTargets
    forM_ targets (installCoreLibrary primRoot root)

-- | Seed aihc-prim and aihc-base. Only @build-exe@ needs aihc-base, so this is
-- a separate resource: tasty initialises it only when one of those tests runs,
-- and the @install@ tests never pay for it.
acquireCoreStore :: IO SeedStore -> IO SeedStore
acquireCoreStore getPrimStore =
  withPreparedStore coreStoreDirectory $ \root -> do
    -- Reuse the aihc-prim the sibling resource already installed rather than
    -- compiling it a second time.
    primStore <- getPrimStore
    copyWritable (seedStoreRoot primStore) root
    baseRoot <- findCoreLibraryRoot "aihc-base"
    installCoreLibrary baseRoot root buildExeHostTarget

-- | Use the store CI handed us, or build one in a temporary directory that is
-- cleaned up if the seeding itself fails. The two stores are kept separate in
-- both cases, so a test sees the same contents wherever it runs.
withPreparedStore :: FilePath -> (FilePath -> IO ()) -> IO SeedStore
withPreparedStore subdirectory populate = do
  prebuilt <- lookupEnv prebuiltStoreVariable
  case prebuilt of
    Just prebuiltRoot | not (null prebuiltRoot) -> do
      let root = prebuiltRoot </> subdirectory
      exists <- doesDirectoryExist root
      unless exists (fail (prebuiltStoreVariable <> " has no " <> subdirectory <> " store: " <> root))
      pure (BorrowedStore root)
    _ ->
      bracketOnError
        (createTemporaryDirectory ("aihc-seed-store-" <> subdirectory))
        removeDirectoryRecursive
        (\root -> populate root >> pure (OwnedStore root))

releaseSeedStore :: SeedStore -> IO ()
releaseSeedStore store =
  case store of
    OwnedStore root -> removeDirectoryRecursive root
    BorrowedStore _ -> pure ()

installCoreLibrary :: FilePath -> FilePath -> NativeTarget -> IO ()
installCoreLibrary source storeRoot target = do
  _ <- install (InstallOptions source (Just storeRoot) Nothing True False False False False False False False False target)
  pure ()

-- | Give a test a scratch directory and copies of the seeded store.
withSandbox :: IO SeedStore -> String -> (Sandbox -> IO a) -> IO a
withSandbox getStore prefix action =
  bracket (createTemporaryDirectory prefix) removeDirectoryRecursive $ \root -> do
    store <- getStore
    let newStore name = do
          let destination = root </> name
          copyWritable (seedStoreRoot store) destination
          pure destination
    action Sandbox {sandboxRoot = root, sandboxStore = newStore}

-- | Copy a directory tree, making every entry writable. Both the store CI
-- hands us and, on some systems, the artefacts the compiler writes are
-- read-only, and tests overwrite stored artefacts to exercise repair paths.
copyWritable :: FilePath -> FilePath -> IO ()
copyWritable source destination = do
  createDirectoryIfMissing True destination
  entries <- listDirectory source
  forM_ entries $ \entry -> do
    let from = source </> entry
        to = destination </> entry
    isDirectory <- doesDirectoryExist from
    if isDirectory
      then copyWritable from to
      else do
        copyFile from to
        permissions <- getPermissions to
        setPermissions to (setOwnerWritable True permissions)

-- | The store directory of a package the seed installed, such as @aihc-base@.
seededPackagePath :: FilePath -> NativeTarget -> String -> IO FilePath
seededPackagePath storeRoot target name = do
  let targetRoot = storeRoot </> nativeTargetStoreDirectory target
  entries <- listDirectory targetRoot
  case filter ((name <> "-") `isPrefixOf`) entries of
    [entry] -> pure (targetRoot </> entry)
    matches -> fail ("expected one " <> name <> " package in " <> targetRoot <> ", found " <> show matches)

-- | Locate a core library source tree, preferring the environment variable the
-- Nix checks set.
findCoreLibraryRoot :: FilePath -> IO FilePath
findCoreLibraryRoot name = do
  configured <- lookupEnv (coreLibraryEnvironment name)
  case configured of
    Just root -> validate root
    Nothing -> getCurrentDirectory >>= findUp
  where
    coreLibraryEnvironment library
      | library == "aihc-base" = "AIHC_BASE_SRC"
      | library == "aihc-prim" = "AIHC_PRIM_SRC"
      | otherwise = "AIHC_CORE_LIBRARY_SRC"
    validate root = do
      exists <- doesFileExist (root </> name <> ".cabal")
      if exists
        then pure root
        else fail (coreLibraryEnvironment name <> " has no " <> name <> ".cabal: " <> root)
    findUp directory = do
      let candidate = directory </> "core-libs" </> name
      exists <- doesFileExist (candidate </> name <> ".cabal")
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail ("could not find core-libs/" <> name)
            else findUp parent

createTemporaryDirectory :: String -> IO FilePath
createTemporaryDirectory prefix = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix <> "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  pure tempFile

clangSupportsWasm :: IO Bool
clangSupportsWasm = do
  result <- try (readProcess "clang" ["-print-targets"] "") :: IO (Either IOException String)
  pure $ case result of
    Left _ -> False
    Right targets -> any isWasmTarget (lines targets)
  where
    isWasmTarget line =
      case words line of
        target : _ -> target == "wasm32"
        [] -> False

arSupportsForeignObjects :: IO Bool
arSupportsForeignObjects = do
  archiveTool <- findExecutable "ar"
  pure (System.os /= "darwin" || archiveTool /= Just "/usr/bin/ar")
