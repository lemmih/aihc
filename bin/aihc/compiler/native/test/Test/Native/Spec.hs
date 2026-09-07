module Test.Native.Spec (tests) where

import Test.Native.Compiler qualified as Compiler
import Test.Native.GcFuzz qualified as GcFuzz
import Test.Native.Object qualified as Object
import Test.Native.Primitive qualified as Primitive
import Test.Native.Runtime qualified as Runtime
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "aihc-native"
    [ Compiler.tests,
      GcFuzz.tests,
      Object.tests,
      Primitive.tests,
      Runtime.tests
    ]
