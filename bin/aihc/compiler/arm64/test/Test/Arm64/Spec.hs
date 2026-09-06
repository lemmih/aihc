{-# LANGUAGE OverloadedStrings #-}

module Test.Arm64.Spec (tests) where

import Aihc.Arm64.Assemble
import Aihc.Arm64.Lir (compileLirObject, compileLirStatements, elideSlotReloads)
import Aihc.Arm64.Text (renderArm64Statements)
import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Lir.Lower (posixTarget64)
import Aihc.Native (NativeTarget (AppleArm64))
import System.Info (arch, os)
import Test.Lir.AsmSuite (AsmBackend (..))
import Test.Lir.AsmSuite qualified as AsmSuite
import Test.Lir.NativeSuite (NativeBackend (..))
import Test.Lir.NativeSuite qualified as NativeSuite
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: IO TestTree
tests = do
  suite <-
    NativeSuite.tests
      NativeBackend
        { backendName = "aihc-arm64",
          backendTarget = AppleArm64,
          backendLowerTarget = posixTarget64,
          backendClangArguments = ["--target=arm64-apple-darwin"],
          backendRuns = arch == "aarch64" && os == "darwin",
          backendAllocationKey = "macos-arm64",
          backendSourceExtension = ".o",
          backendCompile = either (Left . show) (Right . BackendObject) . compileLirObject
        }
  assembly <-
    AsmSuite.tests
      AsmBackend
        { asmBackendName = "aihc-arm64",
          asmBackendExtension = ".arm64.s",
          asmBackendRender = either (Left . show) (Right . renderArm64Statements) . compileLirStatements
        }
  pure (testGroup "arm64" [suite, assembly, slotReloadTests])

slotReloadTests :: TestTree
slotReloadTests =
  testGroup
    "slot reloads"
    [ testCase "a register that holds the slot skips the load" $
        elideSlotReloads [store X9 0, load X9 0] @?= [store X9 0],
      testCase "a load into another register stays" $
        elideSlotReloads [store X9 0, load X10 0] @?= [store X9 0, load X10 0],
      testCase "a write to the register ends the run" $
        elideSlotReloads [store X9 0, add X9, load X9 0] @?= [store X9 0, add X9, load X9 0],
      testCase "a store through a register base keeps the run" $
        elideSlotReloads [load X10 8, storeThrough X9 X10, load X10 8]
          @?= [load X10 8, storeThrough X9 X10],
      testCase "a new value in the slot ends the run" $
        elideSlotReloads [load X9 0, store X10 0, load X9 0] @?= [load X9 0, store X10 0, load X9 0],
      testCase "a call ends the run" $
        elideSlotReloads [store X9 0, call, load X9 0] @?= [store X9 0, call, load X9 0],
      testCase "a moved stack pointer ends the run" $
        elideSlotReloads [store X9 0, dropStack, load X9 0] @?= [store X9 0, dropStack, load X9 0],
      testCase "a label ends the run" $
        elideSlotReloads [store X9 0, arm64Label "block", load X9 0]
          @?= [store X9 0, arm64Label "block", load X9 0],
      testCase "the narrow name of the register ends the run" $
        elideSlotReloads [store X9 0, add W9, load X9 0] @?= [store X9 0, add W9, load X9 0],
      testCase "a repeated copy is dropped" $
        elideSlotReloads [move X9 X19, move X9 X19] @?= [move X9 X19],
      testCase "a copy back the way it came is dropped" $
        elideSlotReloads [move X19 X9, move X9 X19] @?= [move X19 X9],
      testCase "a write to the source ends the copy" $
        elideSlotReloads [move X19 X9, add X9, move X9 X19] @?= [move X19 X9, add X9, move X9 X19],
      testCase "a write to the destination ends the copy" $
        elideSlotReloads [move X9 X19, add X9, move X9 X19] @?= [move X9 X19, add X9, move X9 X19],
      testCase "a copy of a narrow name is only a write" $
        elideSlotReloads [move W9 W19, move W9 W19] @?= [move W9 W19, move W9 W19]
    ]
  where
    load register offset = arm64Instruction (ArmLdr register (Arm64Offset SP offset))
    store register offset = arm64Instruction (ArmStr register (Arm64Offset SP offset))
    storeThrough source base = arm64Instruction (ArmStr source (Arm64Offset base 0))
    move destination source = arm64Instruction (ArmMov destination (Arm64RegisterValue source))
    add register = arm64Instruction (ArmAdd register register (Arm64ImmediateValue 1))
    call = arm64Instruction (ArmBl "_target")
    dropStack = arm64Instruction (ArmSub SP SP (Arm64ImmediateValue 16))
