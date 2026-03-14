module HackageTester.CLI
  ( Options (..),
    parseOptionsIO,
    parseOptionsPure,
  )
where

import qualified Options.Applicative as OA

data Options = Options
  { optPackage :: String,
    optVersion :: Maybe String,
    optJobs :: Maybe Int,
    optJson :: Bool
  }
  deriving (Eq, Show)

parseOptionsIO :: IO Options
parseOptionsIO = OA.execParser parserInfo

parseOptionsPure :: [String] -> Either String Options
parseOptionsPure args =
  case OA.execParserPure OA.defaultPrefs parserInfo args of
    OA.Success opts -> Right opts
    OA.Failure failure ->
      let (msg, _) = OA.renderFailure failure "hackage-tester"
       in Left msg
    OA.CompletionInvoked _ ->
      Left "shell completion requested"

parserInfo :: OA.ParserInfo Options
parserInfo =
  OA.info
    (optionsParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Test parser behavior on a Hackage package"
        <> OA.header "hackage-tester"
    )

optionsParser :: OA.Parser Options
optionsParser =
  Options
    <$> OA.strArgument
      ( OA.metavar "PACKAGE"
          <> OA.help "Hackage package name"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "version"
              <> OA.metavar "VERSION"
              <> OA.help "Exact package version (defaults to latest from Hackage)"
          )
      )
    <*> OA.optional
      ( OA.option
          positiveIntReader
          ( OA.long "jobs"
              <> OA.metavar "N"
              <> OA.help "Number of files to process concurrently (default: CPU cores)"
          )
      )
    <*> OA.switch
      ( OA.long "json"
          <> OA.help "Print final summary as JSON"
      )

positiveIntReader :: OA.ReadM Int
positiveIntReader = OA.eitherReader $ \raw ->
  case reads raw of
    [(n, "")] | n > 0 -> Right n
    _ -> Left "must be a positive integer"
