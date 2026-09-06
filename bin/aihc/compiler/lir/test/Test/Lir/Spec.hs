{-# LANGUAGE OverloadedStrings #-}

module Test.Lir.Spec (tests) where

import Aihc.Lir
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Test.Lir.Arbitrary (prop_lirPrettyRoundTrip)
import Test.Lir.RegAllocSpec qualified as RegAllocSpec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

tests :: IO TestTree
tests = do
  root <- fixtureRoot
  evalCases <- loadFixtures (root </> "eval")
  lintCases <- loadFixtures (root </> "lint")
  regAlloc <- RegAllocSpec.tests (root </> "eval")
  pure
    ( testGroup
        "aihc-lir"
        [ testProperty "generated Lir pretty-printer round-trip" prop_lirPrettyRoundTrip,
          testGroup "evaluation fixtures" (map evalTest evalCases),
          testGroup "lint error fixtures" (map lintTest lintCases),
          regAlloc
        ]
    )

fixtureRoot :: IO FilePath
fixtureRoot = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  pure (root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir")

data Fixture = Fixture
  { fixtureName :: !String,
    fixtureSource :: !Text
  }

loadFixtures :: FilePath -> IO [Fixture]
loadFixtures directory = do
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory directory
  mapM (\name -> Fixture name <$> TIO.readFile (directory </> name)) names

-- | The values of every header comment @; key: value@.
headerValues :: Text -> Text -> [Text]
headerValues key source =
  mapMaybe (T.stripPrefix ("; " <> key <> ": ")) (T.lines source)

parseFixture :: Fixture -> IO Module
parseFixture fixture =
  case parseModule (fixtureSource fixture) of
    Left err -> assertFailure (renderParseError err)
    Right lirModule -> do
      -- Every fixture also round-trips through the pretty-printer.
      case parseModule (renderModule lirModule) of
        Left err -> assertFailure ("pretty-printer output does not parse:\n" <> renderParseError err)
        Right reparsed -> assertEqual "pretty-printer round-trip" lirModule reparsed
      pure lirModule

evalTest :: Fixture -> TestTree
evalTest fixture = testCase (fixtureName fixture) $ do
  lirModule <- parseFixture fixture
  case lintModule lirModule of
    [] -> pure ()
    errors -> assertFailure ("lint errors:\n" <> T.unpack (T.unlines (map renderLintError errors)))
  let entry = Symbol "main"
      resultTypes = concat [functionResults function | ItemFunction function <- moduleItems lirModule, functionName function == entry]
      expected = headerValues "expect" (fixtureSource fixture)
      expectedTrap = headerValues "expect-trap" (fixtureSource fixture)
  case (runFunction lirModule entry [], expected, expectedTrap) of
    (Right values, [want], []) -> assertEqual "result" want (renderValues resultTypes values)
    (Left (InterpretTrap message), [], [want]) -> assertEqual "trap" want message
    (Right values, _, _) -> assertFailure ("unexpected result " <> T.unpack (renderValues resultTypes values))
    (Left err, _, _) -> assertFailure (T.unpack (renderInterpretError err))

lintTest :: Fixture -> TestTree
lintTest fixture = testCase (fixtureName fixture) $ do
  lirModule <- parseFixture fixture
  let expected = headerValues "error" (fixtureSource fixture)
  assertEqual "lint errors" expected (map renderLintError (lintModule lirModule))
