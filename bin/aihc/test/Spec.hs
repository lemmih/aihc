module Main (main) where

import Aihc.Testing.HostStdout (detachHostStdout, processStreams)
import Aihc.Testing.RuntimeArchive (releaseCachedRuntimeArchives)
import Control.Exception (finally)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath (takeDirectory, (</>))
import Test.Aihc.Spec qualified as Aihc
import Test.Amd64.Spec qualified as Amd64
import Test.Arm64.Spec qualified as Arm64
import Test.Fc.Spec qualified as Fc
import Test.Grin.Spec qualified as Grin
import Test.Lir.Spec qualified as Lir
import Test.Llvm.Spec qualified as Llvm
import Test.Native.Spec qualified as Native
import Test.Tasty (defaultMain, testGroup)
import Test.Wasm.Spec qualified as Wasm

main :: IO ()
main = do
  detachHostStdout
  streams <- processStreams
  configureTestRoot
  fc <- Fc.tests
  grin <- Grin.tests streams
  lir <- Lir.tests
  amd64 <- Amd64.tests
  arm64 <- Arm64.tests
  llvm <- Llvm.tests
  wasm <- Wasm.tests
  -- The runtime archives that links share outlive every individual test, so
  -- they are removed once the whole suite is done.
  defaultMain
    ( testGroup
        "aihc"
        [ testGroup "spec" [Aihc.tests],
          testGroup "fc-spec" [fc],
          testGroup "grin-spec" [grin],
          testGroup "lir-spec" [lir],
          testGroup "native-spec" [Native.tests],
          testGroup "amd64-spec" [amd64],
          testGroup "arm64-spec" [arm64],
          testGroup "llvm-spec" [llvm],
          testGroup "wasm-spec" [wasm]
        ]
    )
    `finally` releaseCachedRuntimeArchives

configureTestRoot :: IO ()
configureTestRoot = do
  configured <- lookupEnv "AIHC_TEST_ROOT"
  root <- maybe (getCurrentDirectory >>= findRoot) pure configured
  setDefault "AIHC_TEST_ROOT" root
  setDefault "AIHC_CORE_LIBS_ROOT" root
  setDefault "AIHC_BASE_SRC" (root </> "core-libs" </> "aihc-base")
  setDefault "AIHC_PRIM_SRC" (root </> "core-libs" </> "aihc-prim")
  setDefault "AIHC_EVAL_FIXTURES" (root </> "test" </> "Test" </> "Fixtures" </> "eval")
  where
    setDefault name value = do
      current <- lookupEnv name
      case current of
        Just _ -> pure ()
        Nothing -> setEnv name value
    findRoot directory = do
      exists <- doesFileExist (directory </> "bin" </> "aihc" </> "aihc.cabal")
      if exists
        then pure directory
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail "Cannot find the test source root."
            else findRoot parent
