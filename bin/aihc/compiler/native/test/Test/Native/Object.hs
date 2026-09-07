{-# LANGUAGE OverloadedStrings #-}

-- | The names an object file publishes.
module Test.Native.Object (tests) where

import Aihc.Amd64.Assemble qualified as Amd64
import Aihc.Arm64.Assemble qualified as Arm64
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- | A name this object defines and does not export. Only the relocation
-- beside it names it, so the object writer is free to number it instead.
privateName :: Text
privateName = "aihc_f_aihc_entry___sfIntegralCSUSeconds__uthunk"

-- | A name another object matches against, which must survive intact.
exportedName :: Text
exportedName = "aihc_base_4_21_2_0_hash_Foreign_C_Types_CSUSeconds"

-- | A name this object leaves to the linker, which must survive intact.
undefinedName :: Text
undefinedName = "aihc_base_4_21_2_0_hash_GHC_Real_toInteger"

-- | An object that defines both names and points an absolute relocation at
-- each. 'Absolute64' is the kind the assembler cannot patch in place, so
-- every target here reaches the symbol table.
assembled :: (Show error) => Either error BL.ByteString -> IO ByteString
assembled result =
  case result of
    Left failure -> assertFailure ("the object failed to assemble: " <> show failure)
    Right bytes -> pure (BL.toStrict bytes)

machO :: IO ByteString
machO =
  assembled . Arm64.assembleMachO $
    [ Arm64.arm64Section Arm64.DataSection,
      Arm64.arm64Label privateName,
      Arm64.arm64Quad 0,
      Arm64.arm64Global exportedName,
      Arm64.arm64Label exportedName,
      Arm64.arm64QuadSymbol privateName,
      Arm64.arm64QuadSymbol undefinedName
    ]

elf :: IO ByteString
elf =
  assembled . Amd64.assembleElf $
    [ Amd64.amd64Section Amd64.DataSection,
      Amd64.amd64Label privateName,
      Amd64.amd64Quad 0,
      Amd64.amd64Global exportedName,
      Amd64.amd64Label exportedName,
      Amd64.amd64QuadSymbol privateName,
      Amd64.amd64QuadSymbol undefinedName
    ]

contains :: Text -> ByteString -> Bool
contains name object = Text.encodeUtf8 name `BS.isInfixOf` object

format :: String -> IO ByteString -> TestTree
format name object =
  testGroup
    name
    [ testCase "a private name is not written" $ do
        bytes <- object
        assertBool "the private name is still in the object" (not (contains privateName bytes)),
      testCase "an exported name is written" $ do
        bytes <- object
        assertBool "the exported name is missing" (contains exportedName bytes),
      testCase "an undefined name is written" $ do
        bytes <- object
        assertBool "the undefined name is missing" (contains undefinedName bytes)
    ]

tests :: TestTree
tests = testGroup "object symbols" [format "mach-o" machO, format "elf" elf]
