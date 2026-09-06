module Main (main) where

import Aihc.Dev.ExtractHi (extractPackage)
import Aihc.Dev.ExtractHi.Compare (comparePackageSubset, renderCoreLibProgressReports, renderInterfaceMismatch, runCoreLibApiDivergences, runCoreLibProgressReports)
import Aihc.Dev.ExtractHi.ToResolveIface (toResolveIface)
import Aihc.Dev.Fuzz qualified as Fuzz
import Aihc.Dev.Fuzz.CLI qualified as FuzzCLI
import Control.Monad (unless, when)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.Yaml qualified as Yaml
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  cmd <- execParser opts
  runCommand cmd
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> header "aihc-dev - developer tools for the aihc compiler"
        )

-- | Top-level command type. New subcommands are added here.
data Command
  = ExtractHi ExtractHiOpts
  | CompareHiSubset CompareHiSubsetOpts
  | CoreLibsProgress CoreLibsProgressOpts
  | ExtractResolveIface ExtractResolveIfaceOpts
  | Fuzz FuzzCLI.Command

data ExtractHiOpts = ExtractHiOpts
  { ehPackage :: String,
    ehFormat :: OutputFormat
  }

data ExtractResolveIfaceOpts = ExtractResolveIfaceOpts
  { eriPackage :: String,
    eriOutput :: FilePath
  }

data CompareHiSubsetOpts = CompareHiSubsetOpts
  { chsCandidate :: String,
    chsOracle :: String
  }

newtype CoreLibsProgressOpts = CoreLibsProgressOpts
  { clpDivergences :: Bool
  }

data OutputFormat = YAML | JSON
  deriving (Show)

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "extract-hi"
        ( info
            (ExtractHi <$> extractHiParser <**> helper)
            (progDesc "Extract scoping and typing information from .hi interface files")
        )
        <> command
          "compare-hi-subset"
          ( info
              (CompareHiSubset <$> compareHiSubsetParser <**> helper)
              (progDesc "Check that a candidate .hi interface is a subset of an oracle package")
          )
        <> command
          "core-libs-progress"
          ( info
              (CoreLibsProgress <$> coreLibsProgressParser <**> helper)
              (progDesc "Report ghc-prim/base API coverage for aihc-prim/aihc-base")
          )
        <> command
          "extract-resolve-iface"
          ( info
              (ExtractResolveIface <$> extractResolveIfaceParser <**> helper)
              (progDesc "Extract minimal resolver interface (names only) from .hi files")
          )
        <> command
          "fuzz"
          ( info
              (Fuzz <$> FuzzCLI.commandParser <**> helper)
              (progDesc "Continuously run Hedgehog properties in parallel")
          )
    )

extractHiParser :: Parser ExtractHiOpts
extractHiParser =
  ExtractHiOpts
    <$> strArgument
      ( metavar "PACKAGE"
          <> help "Package name to extract (e.g. 'base', 'containers')"
      )
    <*> flag
      YAML
      JSON
      ( long "json"
          <> help "Output JSON instead of YAML"
      )

extractResolveIfaceParser :: Parser ExtractResolveIfaceOpts
extractResolveIfaceParser =
  ExtractResolveIfaceOpts
    <$> strOption
      ( long "package"
          <> metavar "PACKAGE"
          <> help "Package name to extract (e.g. 'base', 'ghc-prim')"
      )
    <*> strOption
      ( long "output"
          <> metavar "FILE"
          <> help "Output file path for the JSON interface"
      )

coreLibsProgressParser :: Parser CoreLibsProgressOpts
coreLibsProgressParser =
  CoreLibsProgressOpts
    <$> switch
      ( long "divergences"
          <> help "Also list every export of a module shared with ghc-prim or base that GHC does not provide, and fail if there are any"
      )

compareHiSubsetParser :: Parser CompareHiSubsetOpts
compareHiSubsetParser =
  CompareHiSubsetOpts
    <$> strOption
      ( long "candidate"
          <> metavar "PACKAGE"
          <> help "Candidate package that must be a subset"
      )
    <*> strOption
      ( long "oracle"
          <> metavar "PACKAGE"
          <> help "Oracle package that defines the compatible API"
      )

runCommand :: Command -> IO ()
runCommand (ExtractHi opts) = do
  pkg <- extractPackage (ehPackage opts)
  case ehFormat opts of
    YAML -> BL.putStr (BL.fromStrict (Yaml.encode pkg))
    JSON -> BL.putStr (encode pkg)
runCommand (CompareHiSubset opts) = do
  candidate <- extractPackage (chsCandidate opts)
  oracle <- extractPackage (chsOracle opts)
  let mismatches = comparePackageSubset candidate oracle
  if null mismatches
    then putStrLn "OK"
    else do
      mapM_ (putStrLn . renderInterfaceMismatch) mismatches
      exitFailure
runCommand (CoreLibsProgress opts) = do
  putStr . renderCoreLibProgressReports =<< runCoreLibProgressReports
  when (clpDivergences opts) $ do
    divergences <- runCoreLibApiDivergences
    unless (null divergences) $ do
      mapM_ (putStrLn . renderInterfaceMismatch) divergences
      exitFailure
runCommand (ExtractResolveIface opts) = do
  pkg <- extractPackage (eriPackage opts)
  let resolveIface = toResolveIface pkg
      outputPath = eriOutput opts
  createDirectoryIfMissing True (takeDirectory outputPath)
  BL.writeFile outputPath (encodePretty resolveIface)
runCommand (Fuzz fuzzCommand) =
  Fuzz.runCommand fuzzCommand
