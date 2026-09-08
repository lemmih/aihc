{-# LANGUAGE OverloadedStrings #-}

-- | Golden assembly for the native backends.
--
-- Each fixture in @Test\/Fixtures\/lir\/asm@ is a Lir module with a companion
-- file per backend holding the assembly that backend produces. The fixtures
-- are small and aimed at the register allocator, so a change of allocation or
-- of instruction selection reads as a diff of real code rather than as a
-- change of some object bytes.
--
-- Set @AIHC_ACCEPT_ASM=1@ to rewrite the companion files from the current
-- output. Read the diff before committing it: an accepted golden is only as
-- good as the review it had.
module Test.Lir.AsmSuite
  ( AsmBackend (..),
    tests,
    withFixtureStatus,
  )
where

import Aihc.Lir (Module, expandIncludes, parseModule, renderLoadError, renderParseError)
import Control.Exception (try)
import Control.Monad (forM_)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HUnitFailure, assertBool, assertFailure, testCase)

headerValues :: Text -> Text -> [Text]
headerValues key = mapMaybe (T.stripPrefix ("; " <> key <> ": ")) . T.lines

-- | Only assertion failures count as expected failures. Tool failures stay failures.
withFixtureStatus :: String -> Text -> Assertion -> Assertion
withFixtureStatus backend source assertion
  | T.pack backend `elem` headerValues "xfail" source = do
      assertBool "XFAIL requires a reason" (not (all (T.null . T.strip) (headerValues "reason" source)))
      outcome <- try assertion
      case outcome :: Either HUnitFailure () of
        Left _ -> pure ()
        Right () -> assertFailure ("XPASS: " <> backend <> " fixture now succeeds")
  | otherwise = assertion

-- | One backend that renders assembly text.
data AsmBackend = AsmBackend
  { asmBackendName :: !String,
    -- | The extension of the companion file, for example @.arm64.s@.
    asmBackendExtension :: !String,
    asmBackendRender :: !(Module -> Either String Text)
  }

tests :: AsmBackend -> IO TestTree
tests backend = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir" </> "asm"
  names <- filter ((== ".lir") . takeExtension) <$> listDirectory directory
  pure (testGroup "assembly fixtures" (map (fixtureTest backend directory) (sort names)))

fixtureTest :: AsmBackend -> FilePath -> FilePath -> TestTree
fixtureTest backend directory name = testCase name $ do
  source <- TIO.readFile (directory </> name)
  parsed <- either (assertFailure . renderParseError) pure (parseModule source)
  lirModule <- either (assertFailure . renderLoadError) pure =<< expandIncludes TIO.readFile (directory </> name) parsed
  actual <- either (assertFailure . ("backend failed: " <>)) pure (asmBackendRender backend lirModule)
  let checks = headerValues ("expect-asm-" <> T.pack (asmBackendName backend)) source
      hasChecks = any (T.isPrefixOf "; expect-asm-") (T.lines source)
  if hasChecks
    then withFixtureStatus (asmBackendName backend) source $
      forM_ checks $ \expected ->
        assertBool ("missing assembly: " <> T.unpack expected) (expected `T.isInfixOf` actual)
    else compareGolden backend directory name actual

compareGolden :: AsmBackend -> FilePath -> FilePath -> Text -> Assertion
compareGolden backend directory name actual = do
  let goldenPath = directory </> dropExtension name <> asmBackendExtension backend
  accept <- lookupEnv "AIHC_ACCEPT_ASM"
  exists <- doesFileExist goldenPath
  case accept of
    Just value | value /= "" && value /= "0" -> TIO.writeFile goldenPath actual
    _
      | not exists ->
          assertFailure
            ("missing golden " <> goldenPath <> "\nrun the suite with AIHC_ACCEPT_ASM=1 to write it")
      | otherwise -> do
          expected <- TIO.readFile goldenPath
          if expected == actual
            then pure ()
            else assertFailure (T.unpack (difference goldenPath expected actual))

-- | The first line that differs, with a little context. The whole output is
-- long, and a golden failure is nearly always one instruction.
difference :: FilePath -> Text -> Text -> Text
difference path expected actual =
  T.unlines
    [ T.pack path <> " differs at line " <> T.pack (show (length common + 1)),
      "expected: " <> line (drop (length common) expectedLines),
      "actual:   " <> line (drop (length common) actualLines),
      "",
      "run the suite with AIHC_ACCEPT_ASM=1 to rewrite the golden"
    ]
  where
    expectedLines = T.lines expected
    actualLines = T.lines actual
    common = takeWhile id (zipWith (==) expectedLines actualLines)
    line remaining = case remaining of
      [] -> "<end of file>"
      first : _ -> first
