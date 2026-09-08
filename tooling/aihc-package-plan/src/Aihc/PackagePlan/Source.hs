-- |
-- Module      : Aihc.PackagePlan.Source
-- Description : Load and parse one module of a planned package
--
-- Reads a source file the way the compiler front end does: BOM stripping,
-- bird-track unliterating, CPP with the emulated GHC's @MIN_VERSION_*@
-- macros, and parsing with the extensions from the cabal file and the module
-- header.
module Aihc.PackagePlan.Source
  ( ParsedInterfaceFile (..),
    parseInterfaceFile,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (DependencyVersions, cppMacrosFromOptions, injectSyntheticCppMacros)
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.PackagePlan.Diagnostic (DiagnosticSourceMap, cppDiagnosticValue, diagnosticSourceMap, parseDiagnosticValue)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (..),
    ExtensionSetting (..),
    LanguageEdition (..),
    Module (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist)
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (</>))

data ParsedInterfaceFile
  = ParsedInterfaceFile !FilePath Module !DiagnosticSourceMap [Aeson.Value] [Aeson.Value] [Extension]

parseInterfaceFile :: FilePath -> DependencyVersions -> HackageCabal.FileInfo -> IO ParsedInterfaceFile
parseInterfaceFile packageRoot versions fileInfo = do
  rawSource <- HackageUtil.readTextFileLenient path
  let normalized = normalizeSource path rawSource
      cabalExtSettings = mapMaybe (parseExtensionSettingName . T.pack) (HackageCabal.fileInfoExtensions fileInfo)
      cppEnabledGlobally = any isCppExtension cabalExtSettings
      cppEnabledInFile = any isCppExtension (headerExtensionSettings (readModuleHeaderPragmas normalized))
  (source, cppDiagnostics) <-
    if cppEnabledGlobally || cppEnabledInFile
      then preprocessInterfaceSource packageRoot versions fileInfo normalized
      else pure (normalized, [])
  let headerPragmas = readModuleHeaderPragmas source
      allExtSettings = cabalExtSettings <> headerExtensionSettings headerPragmas
      language =
        headerLanguageEdition headerPragmas
          `orElse` (HackageCabal.fileInfoLanguage fileInfo >>= parseLanguageEdition . T.pack)
      extensions = effectiveExtensions (fromMaybe Haskell98Edition language) allExtSettings
      cfg = defaultConfig {parserSourceName = path, parserExtensions = extensions}
      (parseErrs, modu) = parseModule cfg source
      parseDiagnostics = map (parseDiagnosticValue path) parseErrs
      sourceLines = diagnosticSourceMap path source
  pure (ParsedInterfaceFile path modu sourceLines parseDiagnostics cppDiagnostics extensions)
  where
    path = HackageCabal.fileInfoPath fileInfo

    orElse (Just value) _ = Just value
    orElse Nothing fallback = fallback

    isCppExtension setting =
      case setting of
        EnableExtension CPP -> True
        _ -> False

preprocessInterfaceSource :: FilePath -> DependencyVersions -> HackageCabal.FileInfo -> Text -> IO (Text, [Aeson.Value])
preprocessInterfaceSource packageRoot versions fileInfo source = do
  drive (Cpp.preprocess cppConfig (TE.encodeUtf8 injectedSource))
  where
    path = HackageCabal.fileInfoPath fileInfo
    cppOptions = HackageCabal.fileInfoCppOptions fileInfo
    injectedSource = injectSyntheticCppMacros cppOptions versions (HackageCabal.fileInfoDependencies fileInfo) source
    cppConfig =
      Cpp.defaultConfig
        { Cpp.configInputFile = path,
          Cpp.configMacros = cppMacrosFromOptions cppOptions
        }

    drive step =
      case step of
        Cpp.Done result ->
          pure (Cpp.resultOutput result, map cppDiagnosticValue (Cpp.resultDiagnostics result))
        Cpp.NeedInclude req k -> do
          content <- resolveInclude packageRoot (HackageCabal.fileInfoIncludeDirs fileInfo) path req
          drive (k content)

resolveInclude :: FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> IO (Maybe BS.ByteString)
resolveInclude packageRoot includeDirs currentFile req =
  findFirst (includeCandidates packageRoot includeDirs currentFile req)
  where
    findFirst [] = pure Nothing
    findFirst (candidate : rest) = do
      exists <- doesFileExist candidate
      if exists
        then Just <$> BS.readFile candidate
        else findFirst rest

includeCandidates :: FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> [FilePath]
includeCandidates packageRoot includeDirs currentFile req =
  map normalise $
    nub
      [ dir </> Cpp.includePath req
      | dir <- searchDirs
      ]
  where
    includeDir = takeDirectory (Cpp.includeFrom req)
    sourceRelDir = takeDirectory (makeRelative packageRoot currentFile)
    packageAncestors = ancestorDirs sourceRelDir
    localRoots =
      [ takeDirectory currentFile,
        packageRoot </> sourceRelDir,
        packageRoot </> includeDir
      ]
    systemRoots =
      includeDirs
        <> [ packageRoot </> "include",
             packageRoot </> "includes",
             packageRoot </> "cbits",
             packageRoot
           ]
    searchDirs =
      case Cpp.includeKind req of
        Cpp.IncludeLocal -> localRoots <> map (packageRoot </>) packageAncestors <> systemRoots
        Cpp.IncludeSystem -> systemRoots <> localRoots <> map (packageRoot </>) packageAncestors

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  case filter (not . null) (splitDirectories path) of
    [] -> []
    parts ->
      [ foldl (</>) "." (take n parts)
      | n <- [length parts, length parts - 1 .. 1]
      ]

normalizeSource :: FilePath -> Text -> Text
normalizeSource path source =
  let withoutBom = T.dropWhile (== '\xfeff') source
   in if takeExtension path == ".lhs"
        then T.unlines (unlitBird (T.lines withoutBom))
        else withoutBom

unlitBird :: [Text] -> [Text]
unlitBird =
  map $ \line ->
    case T.uncons line of
      Just ('>', rest) -> rest
      _ -> ""
