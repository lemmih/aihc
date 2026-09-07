{-# LANGUAGE OverloadedStrings #-}

-- | The Lir pipeline on a native backend: the Lir evaluation fixtures, the
-- GRIN heap snapshot fixtures lowered through Lir, and the scheduler
-- programs linked with the C runtime. The backend produces an object or a
-- source file that Clang compiles.
module Test.Lir.NativeSuite
  ( NativeBackend (..),
    tests,
  )
where

import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Cli.Runtime (RuntimeBuild (..))
import Aihc.Grin hiding (renderParseError)
import Aihc.Grin qualified as Grin
import Aihc.Lir
import Aihc.Lir.Lower (LowerTarget, lowerEntry, lowerModule)
import Aihc.Native
  ( NativeTarget,
    RuntimeGarbageCollector (..),
    executableEntryName,
  )
import Aihc.Testing.ExceptionProgram (synchronousExceptionProgram)
import Aihc.Testing.RuntimeArchive (cachedRuntimeArchive)
import Aihc.Testing.SchedulerProgram (blackholeSchedulerProgram, schedulerProgram, stdioSchedulerProgram)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Data.Yaml qualified as Y
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.Directory (createDirectory, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush, hPutStr, openTempFile)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, waitForProcess)
import Test.Lir.Observed (lowerObservedProgram)
import Test.Native.Observed (snapshotSourcePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- | One native backend under test.
data NativeBackend = NativeBackend
  { backendName :: !String,
    backendTarget :: !NativeTarget,
    backendLowerTarget :: !LowerTarget,
    -- | The Clang arguments that select the target.
    backendClangArguments :: ![String],
    -- | Whether this host can run the linked programs.
    backendRuns :: !Bool,
    -- | The allocation count key of the snapshot fixtures.
    backendAllocationKey :: !Text,
    -- | The extension of a source output.
    backendSourceExtension :: !String,
    backendCompile :: !(Module -> Either String BackendOutput)
  }

tests :: NativeBackend -> IO TestTree
tests backend = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir" </> "eval"
      snapshotDirectory = root </> "bin" </> "aihc" </> "compiler" </> "grin" </> "test" </> "Test" </> "Fixtures" </> "grin-snapshot"
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory directory
  snapshots <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory snapshotDirectory
  pure
    ( testGroup
        (backendName backend)
        [ testGroup "Lir evaluation fixtures" (map (fixtureTest backend directory) names),
          testGroup "GRIN heap snapshots through Lir" (map (snapshotTest backend snapshotDirectory) snapshots),
          testGroup
            "programs through Lir"
            [ testGroup
                (collectorName collector)
                [ testCase "runs fork# and yield# with FIFO scheduling" (programTest backend collector "PCAB" schedulerProgram),
                  testCase "catches a synchronous exception" (programTest backend collector "E" synchronousExceptionProgram),
                  testCase "blocks and wakes threads that enter a shared blackhole" (programTest backend collector "TA" blackholeSchedulerProgram),
                  testCase "waits for stdin and resumes an async stdio continuation" (stdioTest backend collector)
                ]
            | collector <- [RuntimeGcSemispace]
            ]
        ]
    )

-- | The backends do not check memory alignment or read-only data, so these
-- interpreter traps have no native counterpart.
uncheckedTraps :: [FilePath]
uncheckedTraps = ["trap-misaligned.lir", "trap-read-only.lir"]

-- | Write the backend output into the directory and return the path that
-- Clang links.
writeUnit :: NativeBackend -> FilePath -> String -> BackendOutput -> IO FilePath
writeUnit backend directory base output =
  case output of
    BackendObject object -> do
      let path = directory </> base <> ".o"
      BL.writeFile path object
      pure path
    BackendSource source -> do
      let path = directory </> base <> backendSourceExtension backend
      TIO.writeFile path source
      pure path

compileUnit :: NativeBackend -> Module -> IO BackendOutput
compileUnit backend lirModule = either (assertFailure . ("backend failed: " <>)) pure (backendCompile backend lirModule)

fixtureTest :: NativeBackend -> FilePath -> FilePath -> TestTree
fixtureTest backend directory name = testCase name $ do
  source <- TIO.readFile (directory </> name)
  lirModule <- either (assertFailure . renderParseError) pure (parseModule source)
  let resultTypes = concat [functionResults function | ItemFunction function <- moduleItems lirModule, functionName function == Symbol "main"]
      wrapped = Module (moduleItems lirModule <> [ItemFunction (testWrapper resultTypes)])
  output <- compileUnit backend wrapped
  when (backendRuns backend && name `notElem` uncheckedTraps) $ do
    (exit, out, err) <- runFixture backend output
    case (headerValues "expect" source, headerValues "expect-trap" source) of
      ([expected], []) -> do
        assertEqual ("exit status, stderr: " <> err) ExitSuccess exit
        words' <- mapM parseWord (lines out)
        let values = zipWith decode resultTypes words'
            actual = T.splitOn ", " (renderValues resultTypes values)
            -- Addresses differ between the interpreter and the native run.
            comparable = [(want, got) | (ty, want, got) <- zip3 resultTypes (T.splitOn ", " expected) actual, ty `notElem` [Ptr, Code]]
        assertEqual "result count" (length resultTypes) (length words')
        assertEqual "results" (map fst comparable) (map snd comparable)
      ([], [expectedTrap]) -> do
        assertBool "trap exit status" (exit /= ExitSuccess)
        assertEqual "trap message" (expectedTrap <> "\n") (T.pack err)
      _ -> assertFailure "fixture has no single expectation"
  where
    parseWord line =
      case reads line of
        [(value, "")] -> pure (value :: Word64)
        _ -> assertFailure ("unexpected driver output: " <> line)

-- | Decode one raw result word with the type of the result.
decode :: Type -> Word64 -> Value
decode ty word =
  case ty of
    F32 -> VF32 (castWord32ToFloat (fromIntegral word))
    F64 -> VF64 (castWord64ToDouble word)
    Ptr -> VPtr word
    Code -> VCode word
    _ -> VInt word

-- | A C function that calls @main@ and stores every result in one word of
-- the output buffer. The driver prints the words.
testWrapper :: [Type] -> Function
testWrapper resultTypes =
  Function
    { functionName = Symbol "aihc_lir_test_main",
      functionLinkage = Export,
      functionParameters = [(Var "out", Ptr)],
      functionResults = [I64],
      functionConvention = CConvention,
      functionBlocks =
        [ Block
            { blockLabel = Label "entry",
              blockParameters = [],
              blockInstructions =
                Instruction results (Call (Symbol "main") [])
                  : [ Instruction [] (Store ty (OperandVar var) (Address (OperandVar (Var "out")) (8 * index)) 1)
                    | (index, var, ty) <- zip3 [0 ..] results resultTypes
                    ],
              blockTerminator = Return [OperandLiteral (LitInt (toInteger (length resultTypes)))]
            }
        ]
    }
  where
    results = [Var ("result" <> T.pack (show index)) | index <- [0 .. length resultTypes - 1]]

headerValues :: Text -> Text -> [Text]
headerValues key source = mapMaybe (T.stripPrefix ("; " <> key <> ": ")) (T.lines source)

driverSource :: String
driverSource =
  unlines
    [ "#include <stdint.h>",
      "#include <stdio.h>",
      "#include <string.h>",
      "extern int64_t aihc_lir_test_main(uint64_t *out);",
      "int main(void) {",
      "  uint64_t out[16];",
      "  memset(out, 0, sizeof out);",
      "  int64_t count = aihc_lir_test_main(out);",
      "  for (int64_t index = 0; index < count; ++index) {",
      "    printf(\"%llu\\n\", (unsigned long long)out[index]);",
      "  }",
      "  return 0;",
      "}"
    ]

runFixture :: NativeBackend -> BackendOutput -> IO (ExitCode, String, String)
runFixture backend output =
  withTempDirectory "aihc-lir-fixture" $ \directory -> do
    unit <- writeUnit backend directory "fixture" output
    let driverPath = directory </> "driver.c"
        executable = directory </> "fixture"
    writeFile driverPath driverSource
    (clangExit, _, clangErr) <-
      -- glibc keeps libm apart from libc, so a fixture that calls one of its
      -- functions needs -lm. The other two links here already carry it.
      readProcessWithExitCode "clang" (backendClangArguments backend <> ["-std=c11", driverPath, unit, "-lm", "-o", executable]) ""
    assertEqual ("clang failed to link the fixture:\n" <> clangErr) ExitSuccess clangExit
    readProcessWithExitCode executable [] ""

-- GRIN heap snapshots

data SnapshotFixture = SnapshotFixture
  { snapshotFixtureEntry :: !Text,
    snapshotFixtureProgram :: !Text,
    snapshotFixtureReturn :: !(Maybe Text),
    snapshotFixtureHeap :: !(Maybe Text),
    snapshotFixtureError :: !(Maybe Text),
    snapshotFixtureAllocations :: !(Maybe (Map.Map Text Word64)),
    snapshotFixtureStatus :: !Text,
    snapshotFixtureLirExterns :: !(Maybe [Text]),
    snapshotFixtureLirFunctions :: !(Maybe [Text]),
    snapshotFixtureLirAbsentFunctions :: !(Maybe [Text])
  }

instance FromJSON SnapshotFixture where
  parseJSON =
    withObject "GRIN snapshot fixture" $ \object ->
      SnapshotFixture
        <$> object .: "entry"
        <*> object .: "program"
        <*> object .:? "return"
        <*> object .:? "heap"
        <*> object .:? "error"
        <*> object .:? "allocations"
        <*> object .: "status"
        <*> object .:? "lir-externs"
        <*> object .:? "lir-functions"
        <*> object .:? "lir-absent-functions"

-- | Lower the fixture program through Lir, check the Lir with the linter,
-- and compare the native heap snapshot with the fixture.
snapshotTest :: NativeBackend -> FilePath -> FilePath -> TestTree
snapshotTest backend directory name = testCase name $ do
  fixture <- either (assertFailure . Y.prettyPrintParseException) pure =<< Y.decodeFileEither (directory </> name)
  assertEqual "fixture status" "pass" (snapshotFixtureStatus fixture)
  program <- either (assertFailure . Grin.renderParseError) pure (parseProgram (snapshotFixtureProgram fixture))
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin program)
  (lirModule, metadata) <- either (assertFailure . show) pure (lowerObservedProgram (backendLowerTarget backend) (FunctionName (snapshotFixtureEntry fixture)) gc)
  assertEqual "Lir lint" [] (map renderLintError (lintModule lirModule))
  let externs = [unSymbol (externFunctionName function) | ItemExternFunction function <- moduleItems lirModule]
      functions = [unSymbol (functionName function) | ItemFunction function <- moduleItems lirModule]
  mapM_ (\name' -> assertBool ("Lir extern: " <> T.unpack name') (name' `elem` externs)) (fromMaybe [] (snapshotFixtureLirExterns fixture))
  mapM_ (\name' -> assertBool ("Lir function: " <> T.unpack name') (name' `elem` functions)) (fromMaybe [] (snapshotFixtureLirFunctions fixture))
  mapM_ (\name' -> assertBool ("Lir function must be absent: " <> T.unpack name') (name' `notElem` functions)) (fromMaybe [] (snapshotFixtureLirAbsentFunctions fixture))
  reparsed <- either (assertFailure . renderParseError) pure (parseModule (renderModule lirModule))
  assertEqual "Lir pretty-printer round-trip" lirModule reparsed
  output <- compileUnit backend lirModule
  when (backendRuns backend) $ do
    native <- runObservedUnit backend output metadata
    case (snapshotFixtureReturn fixture, snapshotFixtureHeap fixture, snapshotFixtureError fixture, native) of
      (Just returnValue, Just heapValue, Nothing, Right snapshot) -> do
        allocations <- maybe (assertFailure ("fixture has no " <> T.unpack (backendAllocationKey backend) <> " allocation count")) pure (snapshotFixtureAllocations fixture >>= Map.lookup (backendAllocationKey backend))
        let heap = T.stripEnd heapValue
            expected
              | heap == "[]" = "return: " <> returnValue <> "\nheap: []"
              | otherwise = "return: " <> returnValue <> "\nheap:\n" <> T.unlines (map ("  " <>) (T.lines heap))
        assertEqual "native snapshot" (T.stripEnd expected <> "\nallocations: " <> T.pack (show allocations)) (T.stripEnd snapshot)
      (Nothing, Nothing, Just err, Left message) -> assertEqual "native error" (T.strip err) message
      (_, _, _, Left message) -> assertFailure ("native snapshot failed: " <> T.unpack message)
      (_, _, _, Right snapshot) -> assertFailure ("native snapshot unexpectedly succeeded:\n" <> T.unpack snapshot)

runObservedUnit :: NativeBackend -> BackendOutput -> Text -> IO (Either Text Text)
runObservedUnit backend output metadata =
  withTempDirectory "aihc-lir-snapshot" $ \directory -> do
    runtimeBuild <- nativeRuntimeBuild backend RuntimeGcSemispace
    snapshotRuntime <- snapshotSourcePath
    unit <- writeUnit backend directory "snapshot" output
    let metadataPath = directory </> "snapshot_metadata.c"
        executablePath = directory </> "snapshot"
    TIO.writeFile metadataPath metadata
    (clangExit, _, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( backendClangArguments backend
            <> ["-std=c11", "-Wall", "-Wextra", "-Werror", "-I", takeDirectory snapshotRuntime]
            <> runtimeIncludeArguments runtimeBuild
            <> [snapshotRuntime, metadataPath, unit, runtimeBuildArchive runtimeBuild, "-lm", "-o", executablePath]
        )
        ""
    assertEqual ("clang failed to link the observed program:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    case programExit of
      ExitSuccess -> do
        assertEqual "native stderr" "" programErr
        pure (Right (T.pack programOut))
      ExitFailure _ -> do
        assertEqual "native stdout" "" programOut
        let message = T.strip (T.pack programErr)
        pure (Left (fromMaybe message (T.stripPrefix "aihc runtime: " message)))

-- Programs

-- | Lower a program as a library module and link it with the Lir entry unit
-- and the C runtime.
compileProgramUnits :: NativeBackend -> GrinProgram -> IO [BackendOutput]
compileProgramUnits backend program = do
  let linkedProgram =
        program
          { grinGlobals =
              [ (if name == "main" then executableEntryName else name, node)
              | (name, node) <- grinGlobals program
              ]
          }
  assertEqual "direct GRIN lint" [] (lintProgram linkedProgram)
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin linkedProgram)
  moduleLir <- either (assertFailure . show) pure (lowerModule (backendLowerTarget backend) gc)
  entryLir <- either (assertFailure . show) pure (lowerEntry (backendLowerTarget backend))
  assertEqual "module Lir lint" [] (map renderLintError (lintModule moduleLir))
  assertEqual "entry Lir lint" [] (map renderLintError (lintModule entryLir))
  moduleUnit <- compileUnit backend moduleLir
  entryUnit <- compileUnit backend entryLir
  pure [moduleUnit, entryUnit]

collectorName :: RuntimeGarbageCollector -> String
collectorName collector =
  case collector of
    RuntimeGcSemispace -> "semispace collector"

programTest :: NativeBackend -> RuntimeGarbageCollector -> String -> GrinProgram -> IO ()
programTest backend collector expected program = do
  units <- compileProgramUnits backend program
  when (backendRuns backend) $
    withProgramExecutable backend collector units $ \executablePath -> do
      (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
      assertEqual ("native stderr: " <> programErr) ExitSuccess programExit
      assertEqual "program stdout" expected programOut

stdioTest :: NativeBackend -> RuntimeGarbageCollector -> IO ()
stdioTest backend collector = do
  units <- compileProgramUnits backend stdioSchedulerProgram
  when (backendRuns backend) $
    withProgramExecutable backend collector units $ \executablePath -> do
      (Just childInput, Just childOutput, Just childError, processHandle) <-
        createProcess (proc executablePath []) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
      threadDelay 50000
      hPutStr childInput "Buffered async IO\n"
      hFlush childInput
      hClose childInput
      programOut <- TIO.hGetContents childOutput
      programErr <- TIO.hGetContents childError
      programExit <- waitForProcess processHandle
      assertEqual ("native stderr: " <> T.unpack programErr) ExitSuccess programExit
      assertEqual "async stdout" "Buffered async IO\n" programOut

withProgramExecutable :: NativeBackend -> RuntimeGarbageCollector -> [BackendOutput] -> (FilePath -> IO ()) -> IO ()
withProgramExecutable backend collector units action =
  withTempDirectory "aihc-lir-program" $ \directory -> do
    runtimeBuild <- nativeRuntimeBuild backend collector
    unitPaths <- forM (zip [0 :: Int ..] units) $ \(index, unit) -> writeUnit backend directory ("program-" <> show index) unit
    let executablePath = directory </> "program"
    (clangExit, _, clangErr) <-
      -- This step links the units against the runtime archive and compiles no
      -- C, so it carries no C compile flags: a toolchain that injects its own
      -- preprocessor flags would report every one of them as unused.
      readProcessWithExitCode "clang" (backendClangArguments backend <> unitPaths <> [runtimeBuildArchive runtimeBuild, "-lm", "-o", executablePath]) ""
    assertEqual ("clang failed to link the program:\n" <> clangErr) ExitSuccess clangExit
    action executablePath

-- | The runtime archive of one link. Every link with the same target and
-- collector shares one archive, and takes the include directories first and
-- the archive last, so the test stays independent of how the runtime is put
-- together.
nativeRuntimeBuild :: NativeBackend -> RuntimeGarbageCollector -> IO RuntimeBuild
nativeRuntimeBuild backend garbageCollector =
  cachedRuntimeArchive (backendTarget backend) garbageCollector ["-std=c11", "-Wall", "-Wextra", "-Werror"]

runtimeIncludeArguments :: RuntimeBuild -> [String]
runtimeIncludeArguments build =
  ["-I" <> include | include <- runtimeBuildIncludeDirectories build]

withTempDirectory :: String -> (FilePath -> IO value) -> IO value
withTempDirectory template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary template
      hClose handle
      removeFile path
      createDirectory path
      pure path
