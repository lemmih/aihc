{-# LANGUAGE OverloadedStrings #-}

module Test.Amd64.Spec (tests) where

import Aihc.Amd64.Lir (compileLirObject, compileLirStatements)
import Aihc.Amd64.Text (renderAmd64Statements)
import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Lir.Lower (posixTarget64)
import Aihc.Native (NativeTarget (LinuxAmd64))
import System.Info (arch, os)
import Test.Lir.AsmSuite (AsmBackend (..))
import Test.Lir.AsmSuite qualified as AsmSuite
import Test.Lir.NativeSuite (NativeBackend (..))
import Test.Lir.NativeSuite qualified as NativeSuite
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  native <- nativeTests
  assembly <- assemblyTests
  pure (testGroup "amd64" [native, assembly])

assemblyTests :: IO TestTree
assemblyTests =
  AsmSuite.tests
    AsmBackend
      { asmBackendName = "aihc-amd64",
        asmBackendExtension = ".amd64.s",
        asmBackendRender = either (Left . show) (Right . renderAmd64Statements) . compileLirStatements
      }

nativeTests :: IO TestTree
nativeTests =
  NativeSuite.tests
    NativeBackend
      { backendName = "aihc-amd64",
        backendTarget = LinuxAmd64,
        backendLowerTarget = posixTarget64,
        backendClangArguments = ["--target=x86_64-unknown-linux-gnu"],
        backendRuns = arch == "x86_64" && os == "linux",
        backendAllocationKey = "linux-amd64",
        backendSourceExtension = ".o",
        backendCompile = either (Left . show) (Right . BackendObject) . compileLirObject
      }
