module Aihc.Cli.Options
  ( Command (..),
    BuildExeOptions (..),
    GarbageCollector (..),
    InstallErrorFormat (..),
    InstallOptions (..),
    PrepareRuntimeOptions (..),
    parseCommandIO,
    parseCommandPure,
    parserInfo,
  )
where

import Aihc.Native (NativeTarget, parseNativeTarget)
import Options.Applicative qualified as OA

data Command
  = CmdBuildExe !BuildExeOptions
  | CmdInstall !InstallOptions
  | CmdPrepareRuntime !PrepareRuntimeOptions
  deriving (Eq, Show)

data GarbageCollector
  = GcSemispace
  deriving (Eq, Show)

data BuildExeOptions = BuildExeOptions
  { buildExeSourceFile :: !FilePath,
    buildExeSourceDirectories :: ![FilePath],
    buildExePackageConstraints :: ![String],
    buildExeTarget :: !NativeTarget,
    buildExeGarbageCollector :: !GarbageCollector,
    buildExeStoreRoot :: !(Maybe FilePath),
    buildExeBuildRoot :: !(Maybe FilePath),
    buildExeLint :: !Bool,
    buildExeOutputFile :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

data PrepareRuntimeOptions = PrepareRuntimeOptions
  { prepareRuntimeTarget :: !NativeTarget,
    prepareRuntimeGarbageCollector :: !GarbageCollector,
    prepareRuntimeStoreRoot :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

data InstallOptions = InstallOptions
  { installPackageTarget :: !String,
    installStoreRoot :: !(Maybe FilePath),
    installKeepCore :: !Bool,
    installKeepGrin :: !Bool,
    installKeepNative :: !Bool,
    installLint :: !Bool,
    installReinstall :: !Bool,
    installNoCode :: !Bool,
    installVerbose :: !Bool,
    installPrintTimings :: !Bool,
    installTarget :: !NativeTarget
  }
  deriving (Eq, Show)

data InstallErrorFormat
  = InstallErrorsJson
  | InstallErrorsHuman
  deriving (Eq, Show)

parseCommandIO :: IO Command
parseCommandIO = OA.execParser parserInfo

parseCommandPure :: [String] -> Either String Command
parseCommandPure args =
  case OA.execParserPure OA.defaultPrefs parserInfo args of
    OA.Success command -> Right command
    OA.Failure failure ->
      let (message, _) = OA.renderFailure failure "aihc"
       in Left message
    OA.CompletionInvoked _ -> Left "completion invoked"

parserInfo :: OA.ParserInfo Command
parserInfo =
  OA.info
    (commandParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.header "aihc - command-line interface for the aihc compiler"
    )

commandParser :: OA.Parser Command
commandParser =
  OA.subparser
    ( OA.command
        "build-exe"
        ( OA.info
            (CmdBuildExe <$> buildExeOptionsParser OA.<**> OA.helper)
            (OA.progDesc "Build one Haskell executable")
        )
        <> OA.command
          "install"
          ( OA.info
              (CmdInstall <$> installOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Build and install one Cabal library from a local directory or Hackage")
          )
        <> OA.command
          "prepare-runtime"
          ( OA.info
              (CmdPrepareRuntime <$> prepareRuntimeOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Compile and install target entry and runtime archives")
          )
    )

buildExeOptionsParser :: OA.Parser BuildExeOptions
buildExeOptionsParser =
  BuildExeOptions
    <$> OA.strArgument
      ( OA.metavar "MODULE"
          <> OA.help "Main Haskell module"
      )
    <*> sourceDirectoryOptions
    <*> OA.many
      ( OA.strOption
          ( OA.long "package"
              <> OA.short 'p'
              <> OA.metavar "CONSTRAINT"
              <> OA.help "Add an installed package constraint"
          )
      )
    <*> nativeTargetOption
    <*> garbageCollectorOption
    <*> storeRootOption "Override the aihc store root"
    <*> OA.optional
      ( OA.strOption
          ( OA.long "build-root"
              <> OA.metavar "DIR"
              <> OA.help "Write local compiler artifacts under DIR"
          )
      )
    <*> lintOption
    <*> OA.optional
      ( OA.strOption
          ( OA.long "output"
              <> OA.short 'o'
              <> OA.metavar "FILE"
              <> OA.help "Write the executable to FILE"
          )
      )

sourceDirectoryOptions :: OA.Parser [FilePath]
sourceDirectoryOptions =
  defaultDirectory
    <$> OA.many
      ( OA.strOption
          ( OA.long "source-dir"
              <> OA.short 'i'
              <> OA.metavar "DIR"
              <> OA.help "Add a source directory. The default directory is ."
          )
      )
  where
    defaultDirectory [] = ["."]
    defaultDirectory directories = directories

lintOption :: OA.Parser Bool
lintOption =
  OA.switch
    ( OA.long "lint"
        <> OA.help "Run compiler intermediate-language lint checks"
    )

parseGarbageCollector :: String -> Either String GarbageCollector
parseGarbageCollector value =
  case value of
    "semispace" -> Right GcSemispace
    _ -> Left "expected semispace"

prepareRuntimeOptionsParser :: OA.Parser PrepareRuntimeOptions
prepareRuntimeOptionsParser =
  PrepareRuntimeOptions
    <$> nativeTargetOption
    <*> garbageCollectorOption
    <*> storeRootOption "Install the prepared archives into DIR"

nativeTargetOption :: OA.Parser NativeTarget
nativeTargetOption =
  OA.option
    (OA.eitherReader parseNativeTarget)
    ( OA.long "target"
        <> OA.metavar "TARGET"
        <> OA.help "Target: apple-arm64, linux-amd64, llvm, or wasm32-wasip3"
    )

garbageCollectorOption :: OA.Parser GarbageCollector
garbageCollectorOption =
  OA.option
    (OA.eitherReader parseGarbageCollector)
    ( OA.long "gc"
        <> OA.metavar "semispace"
        <> OA.value GcSemispace
        <> OA.showDefaultWith (const "semispace")
        <> OA.help "Select the garbage collector"
    )

storeRootOption :: String -> OA.Parser (Maybe FilePath)
storeRootOption description =
  OA.optional
    ( OA.strOption
        ( OA.long "store"
            <> OA.metavar "DIR"
            <> OA.help description
        )
    )

installOptionsParser :: OA.Parser InstallOptions
installOptionsParser =
  InstallOptions
    <$> OA.strArgument
      ( OA.metavar "PACKAGE"
          <> OA.help "Local Cabal package directory, or a Hackage package name with an optional version (NAME[-VERSION])"
      )
    <*> storeRootOption "Override the aihc store root"
    <*> OA.switch
      ( OA.long "keep-core"
          <> OA.help "Retain Core (System FC) files"
      )
    <*> OA.switch
      ( OA.long "keep-grin"
          <> OA.help "Retain GRIN files"
      )
    <*> OA.switch
      ( OA.long "keep-native"
          <> OA.help "Retain native output files"
      )
    <*> lintOption
    <*> OA.switch
      ( OA.long "reinstall"
          <> OA.help "Build the package again when it exists in the store"
      )
    <*> OA.switch
      ( OA.long "no-code"
          <> OA.help "Do not generate compiler or native code"
      )
    <*> OA.switch
      ( OA.long "verbose"
          <> OA.short 'v'
          <> OA.help "Print each installation step"
      )
    <*> OA.switch
      ( OA.long "print-timings"
          <> OA.help "Print compiler stage timings"
      )
    <*> nativeTargetOption
