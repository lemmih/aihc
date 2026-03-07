{-# LANGUAGE OverloadedStrings #-}

module ExtensionSupport
  ( Expected (..),
    Outcome (..),
    ExtensionSpec (..),
    CaseMeta (..),
    fixtureDirFor,
    manifestPathFor,
    hasManifest,
    loadRegistry,
    loadManifest,
    classifyOutcome,
    finalizeOutcome,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data ExtensionSpec = ExtensionSpec
  { extName :: !String,
    extFixtureDir :: !FilePath,
    extNotes :: !String
  }
  deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExpected :: !Expected,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"

registryPath :: FilePath
registryPath = fixtureRoot </> "extensions.tsv"

fixtureDirFor :: ExtensionSpec -> FilePath
fixtureDirFor spec = fixtureRoot </> extFixtureDir spec

manifestPathFor :: ExtensionSpec -> FilePath
manifestPathFor spec = fixtureDirFor spec </> "manifest.tsv"

hasManifest :: ExtensionSpec -> IO Bool
hasManifest = doesFileExist . manifestPathFor

loadRegistry :: IO [ExtensionSpec]
loadRegistry = do
  raw <- TIO.readFile registryPath
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM parseRegistryRow rows

parseRegistryRow :: Text -> IO ExtensionSpec
parseRegistryRow row =
  case T.splitOn "\t" row of
    [nameTxt, dirTxt] ->
      pure
        ExtensionSpec
          { extName = T.unpack (T.strip nameTxt),
            extFixtureDir = T.unpack (T.strip dirTxt),
            extNotes = ""
          }
    [nameTxt, dirTxt, notesTxt] ->
      pure
        ExtensionSpec
          { extName = T.unpack (T.strip nameTxt),
            extFixtureDir = T.unpack (T.strip dirTxt),
            extNotes = T.unpack (T.strip notesTxt)
          }
    _ -> fail ("Invalid extension registry row (expected 2 or 3 tab-separated columns): " <> T.unpack row)

loadManifest :: ExtensionSpec -> IO [CaseMeta]
loadManifest spec = do
  raw <- TIO.readFile (manifestPathFor spec)
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM (parseManifestRow spec) rows

parseManifestRow :: ExtensionSpec -> Text -> IO CaseMeta
parseManifestRow spec row =
  case T.splitOn "\t" row of
    [cid, cat, pathTxt, expectedTxt] ->
      parseManifestRowWithReason spec cid cat pathTxt expectedTxt ""
    [cid, cat, pathTxt, expectedTxt, reasonTxt] ->
      parseManifestRowWithReason spec cid cat pathTxt expectedTxt reasonTxt
    _ -> fail ("Invalid manifest row (expected 4 or 5 tab-separated columns): " <> T.unpack row)

parseManifestRowWithReason :: ExtensionSpec -> Text -> Text -> Text -> Text -> Text -> IO CaseMeta
parseManifestRowWithReason spec cid cat pathTxt expectedTxt reasonTxt = do
  let path = T.unpack pathTxt
  exists <- doesFileExist (fixtureDirFor spec </> path)
  if not exists
    then fail ("Manifest references missing case file: " <> path)
    else do
      expected <-
        case expectedTxt of
          "pass" -> pure ExpectPass
          "xfail" -> pure ExpectXFail
          _ -> fail ("Unknown expected value in manifest: " <> T.unpack expectedTxt)
      let reason = trim (T.unpack reasonTxt)
      case expected of
        ExpectXFail | null reason -> fail ("xfail case requires a reason: " <> T.unpack cid)
        _ -> pure ()
      pure
        CaseMeta
          { caseId = T.unpack cid,
            caseCategory = T.unpack cat,
            casePath = path,
            caseExpected = expected,
            caseReason = reason
          }

classifyOutcome :: Expected -> Bool -> Bool -> (Outcome, String)
classifyOutcome expected oracleOk roundtripOk =
  case expected of
    ExpectPass
      | not oracleOk -> (OutcomeFail, "oracle rejected pass case")
      | not roundtripOk -> (OutcomeFail, "roundtrip mismatch against oracle AST")
      | otherwise -> (OutcomePass, "")
    ExpectXFail
      | not oracleOk ->
          ( OutcomeFail,
            "oracle rejected xfail case (fixture invalid or missing oracle extension mapping)"
          )
      | roundtripOk -> (OutcomeXPass, "case now passes oracle and roundtrip checks")
      | otherwise -> (OutcomeXFail, "")

finalizeOutcome :: CaseMeta -> Bool -> Bool -> (CaseMeta, Outcome, String)
finalizeOutcome meta oracleOk roundtripOk =
  let (outcome, details) = classifyOutcome (caseExpected meta) oracleOk roundtripOk
   in (meta, outcome, details)

stripComment :: Text -> Text
stripComment line =
  let core = fst (T.breakOn "#" line)
   in T.strip core

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
