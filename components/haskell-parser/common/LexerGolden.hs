{-# LANGUAGE OverloadedStrings #-}

module LexerGolden
  ( ExpectedStatus (..),
    Outcome (..),
    LexerCase (..),
    fixtureRoot,
    loadLexerCases,
    parseLexerCaseText,
    evaluateLexerCase,
    progressSummary,
  )
where

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Parser
  ( Extension,
    LexToken (..),
    LexTokenKind,
    lexTokensWithExtensions,
  )
import Parser.Ast (parseExtensionName)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.Read (readMaybe)

data ExpectedStatus
  = StatusPass
  | StatusFail
  | StatusXPass
  | StatusXFail
  deriving (Eq, Show)

data Outcome
  = OutcomePass
  | OutcomeXFail
  | OutcomeXPass
  | OutcomeFail
  deriving (Eq, Show)

data LexerCase = LexerCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseInput :: !Text,
    caseTokens :: ![LexTokenKind],
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/lexer"

loadLexerCases :: IO [LexerCase]
loadLexerCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadLexerCase paths

loadLexerCase :: FilePath -> IO LexerCase
loadLexerCase path = do
  source <- TIO.readFile path
  case parseLexerCaseText path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseLexerCaseText :: FilePath -> Text -> Either String LexerCase
parseLexerCaseText path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputText, tokenTexts, statusText, reasonText) <- parseYamlFixture path value
  exts <- mapM (parseFixtureExtensionName path) extNames
  toks <- mapM (parseTokenKind path) tokenTexts
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    LexerCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseInput = inputText,
        caseTokens = toks,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], Text, [Text], Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "lexer fixture" $ \obj -> do
        exts <- obj .: "extensions"
        inputText <- obj .: "input"
        tokenTexts <- obj .: "tokens"
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, inputText, tokenTexts, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid lexer fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

evaluateLexerCase :: LexerCase -> (Outcome, String)
evaluateLexerCase meta =
  let expectedKinds = caseTokens meta
      actual = lexTokensWithExtensions (caseExtensions meta) (caseInput meta)
      actualKinds = fmap (map lexTokenKind) actual
      lexOk = either (const False) (const True) actual
      tokenMatch = actualKinds == Right expectedKinds
      lexFail = either (const True) (const False) actual
   in case caseStatus meta of
        StatusPass
          | tokenMatch -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "expected successful lex with matching token kinds"
                  <> detailsSuffix actualKinds expectedKinds
              )
        StatusFail
          | lexFail -> (OutcomePass, "")
          | otherwise -> (OutcomeFail, "expected lex failure but lexing succeeded")
        StatusXFail
          | lexFail -> (OutcomeXFail, "")
          | otherwise -> (OutcomeFail, "expected xfail (known failing bug), but lexing succeeded")
        StatusXPass
          | lexOk && tokenMatch -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeFail, "expected xpass (known passing bug), but case no longer matches xpass expectation")

progressSummary :: [(LexerCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

detailsSuffix :: Either String [LexTokenKind] -> [LexTokenKind] -> String
detailsSuffix actual expected =
  case actual of
    Left err -> " (lexer error: " <> err <> ")"
    Right actualKinds ->
      if actualKinds == expected
        then ""
        else " (expected=" <> show expected <> ", actual=" <> show actualKinds <> ")"

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles dir = do
  entries <- sort <$> listDirectory dir
  concat
    <$> mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else
              if takeExtension path `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

parseFixtureExtensionName :: FilePath -> Text -> Either String Extension
parseFixtureExtensionName path name =
  case parseExtensionName name of
    Just ext -> Right ext
    Nothing -> Left ("Unknown lexer extension in " <> path <> ": " <> T.unpack name)

parseTokenKind :: FilePath -> Text -> Either String LexTokenKind
parseTokenKind path raw =
  case readMaybe (T.unpack (T.strip raw)) of
    Just parsed -> Right parsed
    Nothing -> Left ("Invalid token constructor in " <> path <> ": " <> T.unpack raw)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid [status] in " <> path <> ": " <> T.unpack raw)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
        _ -> Right trimmed

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "lexer"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
