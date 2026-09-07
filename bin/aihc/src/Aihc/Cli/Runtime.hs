{-# LANGUAGE NamedFieldPuns #-}

-- | Build support archives once, install them in the aihc store, and let
-- ordinary program links consume those immutable artifacts.
module Aihc.Cli.Runtime
  ( RuntimeBuild (..),
    buildRuntimeArchive,
    prepareEntryArchive,
    prepareRuntimeArchive,
    readWasmClangProcessWithExitCode,
    runPrepareRuntime,
    runtimeGarbageCollector,
    wasmClangCommand,
    wasmOptArguments,
  )
where

import Aihc.Cli.Backend (BackendOutput (..), compileLir, lowerTargetFor, nativeSourceExtension)
import Aihc.Cli.Options (GarbageCollector (..), PrepareRuntimeOptions (..))
import Aihc.Cli.Store (defaultStoreRoot, installedEntryArchivePath, installedRuntimeArchivePath)
import Aihc.Lir.Lower qualified as Lir
import Aihc.Lir.Resolve (loadModule, renderLoadError)
import Aihc.Lir.Syntax (Module)
import Aihc.Native
  ( NativeTarget (..),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    WasmSysroot (..),
    backendArchiver,
    backendCompiler,
    nativeTargetTriple,
    runtimePlan,
    wasmSysroot,
  )
import Aihc.Wasm qualified as Wasm
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectory, createDirectoryIfMissing, removeDirectoryRecursive, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (tryIOError)
import System.Process (readProcessWithExitCode)

prepareEntryArchive :: FilePath -> NativeTarget -> IO FilePath
prepareEntryArchive storeRoot target = do
  let destination = installedEntryArchivePath storeRoot target
      destinationDirectory = takeDirectory destination
  createDirectoryIfMissing True destinationDirectory
  withTemporaryDirectory destinationDirectory "entry-build" $ \directory -> do
    let object = directory </> "entry.o"
        archive = directory </> "entry.a"
    entryModule <- either (ioError . userError . ("Lir entry generation failed: " <>) . show) pure (Lir.lowerEntry (lowerTargetFor target))
    compileLirObject target "entry" entryModule directory object
    archiver <- backendArchiver target
    runTool archiver ["rcs", archive, object]
    renameFile archive destination
  pure destination

runPrepareRuntime :: PrepareRuntimeOptions -> IO ()
runPrepareRuntime options = do
  storeRoot <- maybe defaultStoreRoot pure (prepareRuntimeStoreRoot options)
  entry <- prepareEntryArchive storeRoot (prepareRuntimeTarget options)
  archive <-
    prepareRuntimeArchive
      storeRoot
      (prepareRuntimeTarget options)
      (prepareRuntimeGarbageCollector options)
  putStrLn ("entry: " <> entry)
  putStrLn ("runtime: " <> archive)

prepareRuntimeArchive :: FilePath -> NativeTarget -> GarbageCollector -> IO FilePath
prepareRuntimeArchive storeRoot target garbageCollector = do
  let destination = installedRuntimeArchivePath storeRoot target (runtimeGarbageCollector garbageCollector)
      destinationDirectory = takeDirectory destination
  createDirectoryIfMissing True destinationDirectory
  withTemporaryDirectory destinationDirectory "runtime-build" $ \directory -> do
    archive <-
      case target of
        Wasm32Wasip3 -> do
          objects <- buildWasip3RuntimeObjects garbageCollector directory
          archiveRuntimeObjects target directory objects
        _ ->
          runtimeBuildArchive
            <$> buildRuntimeArchive target (runtimeGarbageCollector garbageCollector) defaultRuntimeCArguments directory
    renameFile archive destination
  pure destination

-- | One runtime archive outside the store, and the include directories a
-- caller needs for the runtime headers.
data RuntimeBuild = RuntimeBuild
  { runtimeBuildArchive :: !FilePath,
    runtimeBuildIncludeDirectories :: ![FilePath]
  }
  deriving (Eq, Show)

-- | The C arguments an ordinary runtime build uses.
defaultRuntimeCArguments :: [String]
defaultRuntimeCArguments = ["-std=c11", "-Wall", "-Wextra", "-Werror"]

-- | Build one runtime archive in @directory@ and return it with the include
-- directories of the runtime headers.
--
-- @extraCArguments@ joins the target arguments of every C source, so a caller
-- can instrument the runtime or resize its semispace. The units written in Lir
-- go through the Lir backend of the target and take no C arguments. A caller
-- links the finished archive and stays independent of which units are C and
-- which are Lir. Place the archive after the objects that reference it: a
-- linker takes only the members that resolve a symbol it has already seen.
buildRuntimeArchive :: NativeTarget -> RuntimeGarbageCollector -> [String] -> FilePath -> IO RuntimeBuild
buildRuntimeArchive target garbageCollector extraCArguments directory = do
  plan@RuntimePlan {runtimeSources, runtimeIncludeDirectories} <- runtimePlan target garbageCollector
  (compiler, targetArguments) <- backendCompiler target
  let commonArguments =
        targetArguments
          <> extraCArguments
          <> ["-I" <> includeDirectory | includeDirectory <- runtimeIncludeDirectories]
  cObjects <- forM (zip [0 :: Int ..] runtimeSources) $ \(index, source) -> do
    let object = directory </> "runtime-" <> show index <> ".o"
    runTool compiler (commonArguments <> ["-c", source, "-o", object])
    pure object
  lirObjects <- buildLirRuntimeObjects target plan directory
  archive <- archiveRuntimeObjects target directory (cObjects <> lirObjects)
  pure RuntimeBuild {runtimeBuildArchive = archive, runtimeBuildIncludeDirectories = runtimeIncludeDirectories}

archiveRuntimeObjects :: NativeTarget -> FilePath -> [FilePath] -> IO FilePath
archiveRuntimeObjects target directory objects = do
  let archive = directory </> "runtime.a"
  archiver <- backendArchiver target
  runTool archiver (["rcs", archive] <> objects)
  pure archive

-- | Compile the runtime units written in Lir with the Lir backend of the
-- target. Their objects join the C objects in the runtime archive.
buildLirRuntimeObjects :: NativeTarget -> RuntimePlan -> FilePath -> IO [FilePath]
buildLirRuntimeObjects target plan directory =
  forM (zip [0 :: Int ..] (runtimeLirSources plan)) $ \(index, source) -> do
    let name = "runtime-lir-" <> show index
        object = directory </> name <> ".o"
    lirModule <- either (ioError . userError . renderLoadError) pure =<< loadModule source
    compileLirObject target name lirModule directory object
    pure object

-- | Compile one Lir module to an object. An object target writes the object
-- directly. A text target writes the backend source next to the object and
-- lets the compiler driver of the target assemble it.
compileLirObject :: NativeTarget -> String -> Module -> FilePath -> FilePath -> IO ()
compileLirObject target name lirModule directory object = do
  output <- either (ioError . userError . ("Lir backend failed: " <>)) pure (compileLir target lirModule)
  case output of
    BackendObject bytes -> BL.writeFile object bytes
    BackendSource source -> do
      let sourcePath = directory </> name <> nativeSourceExtension target
      TIO.writeFile sourcePath source
      (compiler, arguments) <- backendCompiler target
      runTool compiler (arguments <> ["-c", sourcePath, "-o", object])

buildWasip3RuntimeObjects :: GarbageCollector -> FilePath -> IO [FilePath]
buildWasip3RuntimeObjects garbageCollector directory = do
  plan@RuntimePlan {runtimeSources, runtimeIncludeDirectories} <- runtimePlan Wasm32Wasip3 (runtimeGarbageCollector garbageCollector)
  wasmRuntimeSources <- Wasm.wasip3RuntimeSourcePaths
  driver <- Wasm.wasip3RuntimeSourcePath
  world <- Wasm.wasip3WorldPath
  clangOverride <- lookupEnv "AIHC_WASM_CLANG"
  sysroot <- wasmSysroot
  let bindingsSource = directory </> "command.c"
      bindingsObject = directory </> "bindings.o"
      componentTypeObject = directory </> "command_component_type.o"
      (clang, clangTargetArguments) = wasmClangCommand clangOverride
      includeArguments =
        [ "-isystem" <> wasmSysrootInclude sysroot,
          "-I" <> takeDirectory driver,
          "-I" <> directory
        ]
          <> ["-I" <> includeDirectory | includeDirectory <- runtimeIncludeDirectories]
      -- The sysroot supplies libc, so the runtime keeps the declarations of
      -- the host toolchain rather than freestanding ones of its own. The
      -- build stays -nostdlib: wasm-ld takes the libc archive explicitly and
      -- the driver never adds a startup object.
      cArguments =
        [ "-O2",
          "-std=c11",
          "-nostdlib",
          "-Wall",
          "-Wextra",
          "-Werror"
        ]
          <> includeArguments
  runTool "wit-bindgen" ["c", "--world", "command", "--out-dir", directory, world]
  runtimeObjects <-
    forM (zip [0 :: Int ..] (runtimeSources <> wasmRuntimeSources)) $ \(index, source) -> do
      let object = directory </> "runtime-" <> show index <> ".o"
      runWasmClangTool clang (clangTargetArguments <> cArguments <> ["-c", source, "-o", object])
      pure object
  runWasmClangTool clang (clangTargetArguments <> cArguments <> ["-c", bindingsSource, "-o", bindingsObject])
  lirObjects <- buildLirRuntimeObjects Wasm32Wasip3 plan directory
  pure (runtimeObjects <> lirObjects <> [bindingsObject, componentTypeObject])

runtimeGarbageCollector :: GarbageCollector -> RuntimeGarbageCollector
runtimeGarbageCollector garbageCollector =
  case garbageCollector of
    GcSemispace -> RuntimeGcSemispace

-- | Select the ordinary Clang driver used for WebAssembly objects. Nix can
-- override only the executable to bypass its host-target compiler wrapper.
-- The sysroot is not part of this: an assembly input needs no headers, and
-- the C compilations add it themselves.
wasmClangCommand :: Maybe FilePath -> (FilePath, [String])
wasmClangCommand override =
  (fromMaybe "clang" override, ["--target=" <> nativeTargetTriple Wasm32Wasip3])

wasmOptArguments :: FilePath -> FilePath -> [String]
wasmOptArguments input output =
  [input, "-O3", "--enable-tail-call", "--emit-target-features", "-o", output]

-- | Run Clang and, after a WebAssembly compilation failure, inspect its
-- registered targets so a target-limited installation gets an actionable
-- diagnostic without obscuring Clang's original error.
readWasmClangProcessWithExitCode :: FilePath -> [String] -> IO (ExitCode, String, String)
readWasmClangProcessWithExitCode clang arguments = do
  result@(exitCode, stdout, stderr) <- readProcessWithExitCode clang arguments ""
  case exitCode of
    ExitSuccess -> pure result
    ExitFailure _ -> do
      targetsResult <- tryIOError (readProcessWithExitCode clang ["-print-targets"] "")
      pure
        ( exitCode,
          stdout,
          case targetsResult of
            Right (ExitSuccess, targets, _targetsStderr)
              | not (hasWasm32Target targets) -> appendWasm32TargetNotice stderr
            _ -> stderr
        )

hasWasm32Target :: String -> Bool
hasWasm32Target = any lineIsWasm32Target . lines
  where
    lineIsWasm32Target line =
      case words line of
        target : _ -> target == "wasm32"
        [] -> False

appendWasm32TargetNotice :: String -> String
appendWasm32TargetNotice originalError =
  originalError
    <> separator
    <> unlines
      [ "AIHC notice: this Clang installation does not include the wasm32 target.",
        "The default Clang shipped with macOS omits WebAssembly support. Install LLVM Clang",
        "with Homebrew (`brew install llvm`) or Nix",
        "(`nix shell nixpkgs#llvmPackages.clang-unwrapped`), then set AIHC_WASM_CLANG",
        "to that Clang executable."
      ]
  where
    separator
      | null originalError = ""
      | last originalError == '\n' = "\n"
      | otherwise = "\n\n"

runWasmClangTool :: FilePath -> [String] -> IO ()
runWasmClangTool clang arguments = do
  (exitCode, _stdout, stderr) <- readWasmClangProcessWithExitCode clang arguments
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (clang <> " failed (" <> show exitCode <> "): " <> stderr))

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode tool arguments ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> stderr))

withTemporaryDirectory :: FilePath -> String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory parent template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      createDirectoryIfMissing True parent
      (path, handle) <- openTempFile parent template
      hClose handle
      removeFile path
      createDirectory path
      pure path
