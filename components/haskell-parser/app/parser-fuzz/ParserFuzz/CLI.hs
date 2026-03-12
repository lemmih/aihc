module ParserFuzz.CLI
  ( Options (..),
    parseOptions,
  )
where

import Options.Applicative qualified as OA

data Options = Options
  { optSeed :: Maybe Int,
    optMaxTests :: Int,
    optSize :: Int,
    optMaxShrinkPasses :: Int,
    optOutput :: Maybe FilePath,
    optPrintGeneratedModules :: Bool
  }

parseOptions :: IO Options
parseOptions = OA.execParser parserInfo

parserInfo :: OA.ParserInfo Options
parserInfo =
  OA.info
    (optionsParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Generate HSE modules, find parser failures, and shrink with exactPrint"
        <> OA.header "parser-fuzz"
    )

optionsParser :: OA.Parser Options
optionsParser =
  Options
    <$> OA.optional
      ( OA.option
          OA.auto
          ( OA.long "seed"
              <> OA.metavar "N"
              <> OA.help "Deterministic random seed (default: randomIO)"
          )
      )
    <*> OA.option
      OA.auto
      ( OA.long "max-tests"
          <> OA.metavar "N"
          <> OA.value 10000
          <> OA.showDefault
          <> OA.help "Number of generated modules to try"
      )
    <*> OA.option
      OA.auto
      ( OA.long "size"
          <> OA.metavar "N"
          <> OA.value 10
          <> OA.showDefault
          <> OA.help "QuickCheck generation size"
      )
    <*> OA.option
      OA.auto
      ( OA.long "max-shrink-passes"
          <> OA.metavar "N"
          <> OA.value 1000
          <> OA.showDefault
          <> OA.help "Maximum accepted shrink steps"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "output"
              <> OA.metavar "PATH"
              <> OA.help "Write minimized source to PATH"
          )
      )
    <*> OA.switch
      ( OA.long "print-generated-modules"
          <> OA.help "Print each generated module before testing it"
      )
