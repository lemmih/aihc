{-# LANGUAGE OverloadedStrings #-}

-- | Fixture tests: every directory under @test/Test/Fixtures/haddock@ is a
-- small package with mainline Haddock reference output next to it.
--
-- Each fixture checks four goldens under @expected/@: the model JSON, the
-- Hoogle database, and the two comparison reports against the reference
-- outputs. @expected/status.txt@ states the expected verdict of each
-- comparison as @json: PASS@ or @json: XFAIL <reason>@ (likewise @hoogle:@).
-- An unexpected pass fails the test so a stale XFAIL is noticed.
--
-- Set @AIHC_HADDOCK_ACCEPT=1@ to rewrite the goldens.
module Test.Haddock.Fixtures (tests) where

import Aihc.Haddock.Compare
import Aihc.Haddock.Hoogle (renderHoogle)
import Aihc.Haddock.Model (decodePackageDoc, encodePackageDoc)
import Aihc.Haddock.Package (loadPackageDoc)
import Aihc.Haddock.Reference.Hoogle (parseHoogleFile)
import Aihc.Haddock.Reference.Json (decodeReferenceInterface)
import Control.Monad (filterM, unless)
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/haddock"

tests :: IO TestTree
tests = do
  entries <- sort <$> listDirectory fixtureRoot
  fixtures <- filterM (doesDirectoryExist . (fixtureRoot </>)) entries
  accept <- isJust <$> lookupEnv "AIHC_HADDOCK_ACCEPT"
  pure (testGroup "fixtures" (map (fixtureTests accept) fixtures))

fixtureTests :: Bool -> FilePath -> TestTree
fixtureTests accept name =
  testGroup
    name
    [ testCase "model" $ do
        package <- loadPackageDoc dir []
        checkGolden accept (expected "model.json") (TE.decodeUtf8 (BL.toStrict (encodePackageDoc package)))
        -- The model must survive a round trip through its own JSON.
        case decodePackageDoc (encodePackageDoc package) of
          Left err -> assertFailure ("model does not decode: " <> err)
          Right decoded -> unless (decoded == package) (assertFailure "model changes across a JSON round trip"),
      testCase "hoogle" $ do
        package <- loadPackageDoc dir []
        checkGolden accept (expected "hoogle.txt") (renderHoogle package),
      testCase "compare-json" $ do
        package <- loadPackageDoc dir []
        referenceBytes <- BL.readFile (dir </> "reference" </> "haddock.json")
        reference <- either (assertFailure . ("reference JSON: " <>)) pure (decodeReferenceInterface referenceBytes)
        let report = compareInterface defaultNormalization package reference
        checkGolden accept (expected "compare-json.txt") (renderReport report)
        checkStatus "json" (reportVerdict report),
      testCase "compare-hoogle" $ do
        package <- loadPackageDoc dir []
        reference <- parseHoogleFile <$> TIO.readFile (dir </> "reference" </> "hoogle.txt")
        let report = compareHoogle reference (parseHoogleFile (renderHoogle package))
        checkGolden accept (expected "compare-hoogle.txt") (renderReport report)
        checkStatus "hoogle" (reportVerdict report)
    ]
  where
    dir = fixtureRoot </> name
    expected file = dir </> "expected" </> file

    checkStatus layer verdict = do
      statusText <- TIO.readFile (expected "status.txt")
      let status = lookupStatus layer statusText
      case (status, verdict) of
        (Just "PASS", Pass) -> pure ()
        (Just expectation, Fail)
          | T.isPrefixOf "XFAIL" expectation -> pure ()
        (Just "PASS", Fail) ->
          assertFailure (layer <> " comparison failed; see " <> expected ("compare-" <> layer <> ".txt"))
        (Just expectation, Pass)
          | T.isPrefixOf "XFAIL" expectation ->
              assertFailure (layer <> " comparison passed unexpectedly (XPASS); remove the XFAIL from status.txt")
        (Just other, _) -> assertFailure ("unknown status " <> T.unpack other <> " for " <> layer)
        (Nothing, _) -> assertFailure ("status.txt has no entry for " <> layer)

lookupStatus :: String -> Text -> Maybe Text
lookupStatus layer statusText =
  case [T.strip rest | line <- T.lines statusText, Just rest <- [T.stripPrefix (T.pack layer <> ":") line]] of
    value : _ -> Just value
    [] -> Nothing

checkGolden :: Bool -> FilePath -> Text -> IO ()
checkGolden accept path actual = do
  exists <- doesFileExist path
  if accept || not exists
    then do
      createDirectoryIfMissing True (takeDirectoryOf path)
      TIO.writeFile path actual
      unless (accept || exists) $
        assertFailure ("golden file " <> path <> " did not exist; it has been written, review and rerun")
    else do
      expectedText <- TIO.readFile path
      unless (expectedText == actual) $
        assertFailure
          ( "golden mismatch for "
              <> path
              <> "\n--- expected\n"
              <> T.unpack expectedText
              <> "\n--- actual\n"
              <> T.unpack actual
          )
  where
    takeDirectoryOf = reverse . drop 1 . dropWhile (/= '/') . reverse
