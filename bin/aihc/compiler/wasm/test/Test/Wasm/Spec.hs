{-# LANGUAGE OverloadedStrings #-}

-- | The Lir pipeline on WebAssembly. Every fixture and program is lowered
-- for the wasm32 target and compiled to assembly. When the WebAssembly
-- toolchain is installed, the Lir evaluation fixtures run as core modules
-- under wasmtime and the scheduler programs run as WASI P3 components.
module Test.Wasm.Spec (tests) where

import Aihc.Cli.Options (GarbageCollector (..))
import Aihc.Cli.Runtime (prepareEntryArchive, prepareRuntimeArchive, wasmClangCommand)
import Aihc.Grin hiding (renderParseError)
import Aihc.Grin qualified as Grin
import Aihc.Lir
import Aihc.Lir.Lower (lowerEntry, lowerModule, wasip3Target)
import Aihc.Native (NativeTarget (Wasm32Wasip3), WasmSysroot (..), backendCompiler, executableEntryName, renderLinkedGlobalSymbol, wasmSysroot)
import Aihc.Testing.ExceptionProgram (synchronousExceptionProgram)
import Aihc.Testing.SchedulerProgram (blackholeSchedulerProgram, schedulerProgram)
import Aihc.Wasm.Lir (compileLirModule)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Data.Yaml qualified as Y
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.Directory (createDirectory, findExecutable, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcess, readProcessWithExitCode)
import Test.Lir.Observed (lowerObservedProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- | The tools that run WebAssembly here, when they are installed.
data WasmTools = WasmTools
  { toolsClang :: !FilePath,
    toolsClangArguments :: ![String],
    toolsComponents :: !Bool
  }

tests :: IO TestTree
tests = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  tools <- findWasmTools
  let directory = root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir" </> "eval"
      snapshotDirectory = root </> "bin" </> "aihc" </> "compiler" </> "grin" </> "test" </> "Test" </> "Fixtures" </> "grin-snapshot"
      lowerDirectory = root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir" </> "lower"
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory directory
  snapshots <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory snapshotDirectory
  lowerFixtures <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory lowerDirectory
  pure
    ( testGroup
        "aihc-wasm"
        [ testGroup "Lir evaluation fixtures" (map (fixtureTest tools directory) names),
          testGroup "GRIN heap snapshots lowered for wasm32" (map (snapshotTest snapshotDirectory) snapshots),
          testGroup "static data fixtures lowered for wasm32" (map (snapshotTest lowerDirectory) lowerFixtures),
          testGroup
            "programs through Lir"
            [ testCase "runs fork# and yield# with FIFO scheduling" (programTest tools "PCAB" schedulerProgram),
              testCase "catches a synchronous exception" (programTest tools "E" synchronousExceptionProgram),
              testCase "blocks and wakes threads that enter a shared blackhole" (programTest tools "TA" blackholeSchedulerProgram)
            ]
        ]
    )

-- | The WebAssembly Clang, the linker, and wasmtime run the fixtures. The
-- component tools also run the programs.
findWasmTools :: IO (Maybe WasmTools)
findWasmTools = do
  override <- lookupEnv "AIHC_WASM_CLANG"
  let (clang, targetArguments) = wasmClangCommand override
  supported <- clangSupportsWasm clang
  linker <- findExecutable "wasm-ld"
  wasmtime <- findExecutable "wasmtime"
  wasmTools <- findExecutable "wasm-tools"
  witBindgen <- findExecutable "wit-bindgen"
  pure $
    if supported && isJust linker && isJust wasmtime
      then Just WasmTools {toolsClang = clang, toolsClangArguments = targetArguments, toolsComponents = isJust wasmTools && isJust witBindgen}
      else Nothing

clangSupportsWasm :: FilePath -> IO Bool
clangSupportsWasm clang = do
  result <- try (readProcess clang ["-print-targets"] "") :: IO (Either IOException String)
  pure $ case result of
    Left _ -> False
    Right targets -> any isWasmTarget (lines targets)
  where
    isWasmTarget line =
      case words line of
        target : _ -> target == "wasm32"
        [] -> False

-- | The backend does not check memory alignment or read-only data, so these
-- interpreter traps have no WebAssembly counterpart. The fixtures that
-- address data objects with 8-byte words do not run on a 32-bit target.
skippedFixtures :: [FilePath]
skippedFixtures = ["trap-misaligned.lir", "trap-read-only.lir", "indirect-call.lir", "info-table.lir"]

compileText :: Module -> IO Text
compileText lirModule = either (assertFailure . ("WebAssembly backend failed: " <>) . show) pure (compileLirModule lirModule)

fixtureTest :: Maybe WasmTools -> FilePath -> FilePath -> TestTree
fixtureTest tools directory name = testCase name $ do
  source <- TIO.readFile (directory </> name)
  parsed <- either (assertFailure . renderParseError) pure (parseModule source)
  lirModule <- either (assertFailure . renderLoadError) pure =<< expandIncludes TIO.readFile (directory </> name) parsed
  let resultTypes = concat [functionResults function | ItemFunction function <- moduleItems lirModule, functionName function == Symbol "main"]
      wrapped = Module (moduleItems lirModule <> [ItemFunction (testWrapper resultTypes)])
  assembly <- compileText wrapped
  assertBool "declares the test entry" (".functype\taihc_lir_test_main (i32) -> (i64)" `T.isInfixOf` assembly)
  case tools of
    Just available | name `notElem` skippedFixtures -> do
      (exit, out, err) <- runFixture available assembly
      case (headerValues "expect" source, headerValues "expect-trap" source) of
        ([expected], []) -> do
          assertEqual ("exit status, stderr: " <> err) ExitSuccess exit
          words' <- mapM parseWord (lines out)
          let values = zipWith decode resultTypes words'
              actual = T.splitOn ", " (renderValues resultTypes values)
              -- Addresses differ between the interpreter and the WebAssembly run.
              comparable = [(want, got) | (ty, want, got) <- zip3 resultTypes (T.splitOn ", " expected) actual, ty `notElem` [Ptr, Code]]
          assertEqual "result count" (length resultTypes) (length words')
          assertEqual "results" (map fst comparable) (map snd comparable)
        ([], [expectedTrap]) -> do
          assertBool "trap exit status" (exit /= ExitSuccess)
          assertEqual "trap message" (expectedTrap <> "\n") (T.pack err)
        _ -> assertFailure "fixture has no single expectation"
    _ -> pure ()
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

-- | A C function that calls @main@ and stores every result in one 8-byte
-- slot of the output buffer. The driver prints the slots.
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

-- | A freestanding driver that prints the result slots with WASI preview 1
-- and reports a trap message on the standard error stream.
driverSource :: String
driverSource =
  unlines
    [ "#include <stdint.h>",
      "typedef struct { const void *buffer; uint32_t length; } aihc_iovec;",
      "__attribute__((import_module(\"wasi_snapshot_preview1\"), import_name(\"fd_write\")))",
      "int32_t aihc_fd_write(int32_t fd, const aihc_iovec *iovs, int32_t count, int32_t *written);",
      "__attribute__((import_module(\"wasi_snapshot_preview1\"), import_name(\"proc_exit\")))",
      "_Noreturn void aihc_proc_exit(int32_t status);",
      "extern int64_t aihc_lir_test_main(uint64_t *out);",
      "static void aihc_put(int32_t fd, const void *bytes, uint32_t length) {",
      "  aihc_iovec iov = {bytes, length};",
      "  int32_t written = 0;",
      "  aihc_fd_write(fd, &iov, 1, &written);",
      "}",
      "_Noreturn void aihc_lir_trap(const uint8_t *message, uint64_t length) {",
      "  aihc_put(2, message, (uint32_t)length);",
      "  aihc_put(2, \"\\n\", 1);",
      "  aihc_proc_exit(1);",
      "}",
      "void _start(void) {",
      "  uint64_t out[16] = {0};",
      "  int64_t count = aihc_lir_test_main(out);",
      "  for (int64_t index = 0; index < count; ++index) {",
      "    char digits[24];",
      "    char line[24];",
      "    int used = 0;",
      "    int length = 0;",
      "    uint64_t value = out[index];",
      "    do {",
      "      digits[used++] = (char)('0' + value % 10);",
      "      value /= 10;",
      "    } while (value != 0);",
      "    while (used > 0) {",
      "      line[length++] = digits[--used];",
      "    }",
      "    line[length++] = '\\n';",
      "    aihc_put(1, line, (uint32_t)length);",
      "  }",
      "}"
    ]

runFixture :: WasmTools -> Text -> IO (ExitCode, String, String)
runFixture tools assembly =
  withTempDirectory "aihc-wasm-fixture" $ \directory -> do
    let assemblyPath = directory </> "fixture.s"
        fixtureObject = directory </> "fixture.o"
        driverPath = directory </> "driver.c"
        driverObject = directory </> "driver.o"
        moduleFile = directory </> "fixture.wasm"
    TIO.writeFile assemblyPath assembly
    writeFile driverPath driverSource
    (_, backendArguments) <- backendCompiler Wasm32Wasip3
    runTool (toolsClang tools) (backendArguments <> ["-c", assemblyPath, "-o", fixtureObject])
    runTool (toolsClang tools) (toolsClangArguments tools <> ["-O1", "-std=c11", "-nostdlib", "-ffreestanding", "-Wall", "-Wextra", "-Werror", "-c", driverPath, "-o", driverObject])
    -- The libc archive follows the objects so a fixture that calls a C
    -- function, such as one of libm, resolves it.
    sysroot <- wasmSysroot
    runTool "wasm-ld" ["--no-entry", "--export=_start", driverObject, fixtureObject, wasmSysrootLibc sysroot, "-o", moduleFile]
    readProcessWithExitCode "wasmtime" ["run", "-C", "cache=n", moduleFile] ""

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  (exitCode, out, err) <- readProcessWithExitCode tool arguments ""
  unless (exitCode == ExitSuccess) $
    assertFailure (tool <> " failed: " <> unwords arguments <> "\n" <> out <> err)

-- GRIN heap snapshots

data SnapshotFixture = SnapshotFixture
  { snapshotFixtureEntry :: !Text,
    snapshotFixtureProgram :: !Text,
    snapshotFixtureStatus :: !Text,
    snapshotFixtureWasmIntegerFields :: ![(Text, [Text])]
  }

instance FromJSON SnapshotFixture where
  parseJSON =
    withObject "GRIN snapshot fixture" $ \object ->
      SnapshotFixture
        <$> object .: "entry"
        <*> object .: "program"
        <*> object .: "status"
        <*> object .:? "wasm-integer-fields" .!= []

-- | Lower the fixture program for wasm32, check the Lir, and compile it.
-- The snapshot runtime needs a C library, so the snapshot itself is not
-- taken on WebAssembly.
snapshotTest :: FilePath -> FilePath -> TestTree
snapshotTest directory name = testCase name $ do
  fixture <- either (assertFailure . Y.prettyPrintParseException) pure =<< Y.decodeFileEither (directory </> name)
  assertEqual "fixture status" "pass" (snapshotFixtureStatus fixture)
  program <- either (assertFailure . Grin.renderParseError) pure (parseProgram (snapshotFixtureProgram fixture))
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin program)
  (lirModule, _) <- either (assertFailure . show) pure (lowerObservedProgram wasip3Target (FunctionName (snapshotFixtureEntry fixture)) gc)
  assertEqual "Lir lint" [] (map renderLintError (lintModule lirModule))
  reparsed <- either (assertFailure . renderParseError) pure (parseModule (renderModule lirModule))
  assertEqual "Lir pretty-printer round-trip" lirModule reparsed
  assembly <- compileText lirModule
  assertBool "info tables have 4-byte words" ("\t.p2align\t2, 0x0" `T.isInfixOf` assembly)
  forM_ (snapshotFixtureWasmIntegerFields fixture) $ \(globalName, expected) -> do
    let fields =
          [ [T.pack (show ty) | DataInt ty _ <- dataFields item]
          | ItemData item <- moduleItems lirModule,
            dataName item == Symbol (renderLinkedGlobalSymbol globalName)
          ]
    assertEqual ("static integer field types: " <> T.unpack globalName) [expected] fields

-- Programs

-- | The test programs print through the C @putchar@, which the WASI P3
-- world does not provide: its standard output is an asynchronous stream.
-- This stub checks the characters against the expected output and traps on
-- the first difference, so a successful exit means the output was a prefix
-- of the expected text.
putcharStub :: String -> String
putcharStub expected =
  unlines
    [ "static const char expected[] = \"" <> expected <> "\";",
      "static unsigned position;",
      "int putchar(int character) {",
      "  if (position >= sizeof expected - 1 || character != expected[position]) {",
      "    __builtin_trap();",
      "  }",
      "  ++position;",
      "  return character;",
      "}"
    ]

-- | Lower a program as a library module, compile it with the entry unit
-- and the runtime into a WASI P3 component, and run it under wasmtime.
programTest :: Maybe WasmTools -> String -> GrinProgram -> IO ()
programTest tools expected program = do
  let linkedProgram =
        program
          { grinGlobals =
              [ (if name == "main" then executableEntryName else name, node)
              | (name, node) <- grinGlobals program
              ]
          }
  assertEqual "direct GRIN lint" [] (lintProgram linkedProgram)
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin linkedProgram)
  moduleLir <- either (assertFailure . show) pure (lowerModule wasip3Target gc)
  entryLir <- either (assertFailure . show) pure (lowerEntry wasip3Target)
  assertEqual "module Lir lint" [] (map renderLintError (lintModule moduleLir))
  assertEqual "entry Lir lint" [] (map renderLintError (lintModule entryLir))
  moduleAssembly <- compileText moduleLir
  entryAssembly <- compileText entryLir
  assertBool "the entry exports the P3 start" ("aihc_lir_program_start" `T.isInfixOf` entryAssembly)
  case tools of
    Just available | toolsComponents available ->
      withTempDirectory "aihc-wasm-program" $ \directory -> do
        let storeRoot = directory </> "store"
            assemblyPath = directory </> "program.s"
            programObject = directory </> "program.o"
            stubPath = directory </> "putchar.c"
            stubObject = directory </> "putchar.o"
            coreModule = directory </> "program-core.wasm"
            component = directory </> "program.wasm"
        entry <- prepareEntryArchive storeRoot Wasm32Wasip3
        runtime <- prepareRuntimeArchive storeRoot Wasm32Wasip3 GcSemispace
        TIO.writeFile assemblyPath moduleAssembly
        writeFile stubPath (putcharStub expected)
        (_, backendArguments) <- backendCompiler Wasm32Wasip3
        runTool (toolsClang available) (backendArguments <> ["-c", assemblyPath, "-o", programObject])
        runTool (toolsClang available) (toolsClangArguments available <> ["-O1", "-std=c11", "-nostdlib", "-ffreestanding", "-Wall", "-Wextra", "-Werror", "-c", stubPath, "-o", stubObject])
        sysroot <- wasmSysroot
        runTool "wasm-ld" ["--no-entry", "--export-memory", "--allow-undefined", programObject, stubObject, "--whole-archive", entry, runtime, "--no-whole-archive", wasmSysrootLibc sysroot, "-o", coreModule]
        runTool "wasm-tools" ["component", "new", coreModule, "-o", component]
        (exit, out, err) <- readProcessWithExitCode "wasmtime" ["run", "-C", "cache=n", "-S", "cli", component] ""
        assertEqual ("program stderr: " <> err) ExitSuccess exit
        assertEqual "program stdout" "" out
    _ -> pure ()

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
