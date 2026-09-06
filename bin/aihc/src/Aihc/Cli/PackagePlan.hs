module Aihc.Cli.PackagePlan
  ( DependencyResolver (..),
    PackagePlan (..),
    buildPackagePlanWithResolver,
    ParsedInterfaceFile (..),
    parseInterfaceFile,
    DependencyVersions,
    dependencyVersionsFromManifests,
    coreProviders,
    coreProviderSourcePath,
    CoreProvider (..),
    localDependencyResolverWithFallback,
    packageSpecFromSource,
    renderHumanDiagnostic,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (DependencyVersions, cppMacrosFromOptions, injectSyntheticCppMacros)
import Aihc.Hackage.Release (BootLibrary (..), emulatedGhc, lookupBootLibraryByStandin, showVersionBranch)
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (..),
    ExtensionSetting (..),
    LanguageEdition (..),
    Module (..),
    SourceSpan (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Control.Applicative ((<|>))
import Data.Aeson (object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (buildable, condLibrary, condSubLibraries, libBuildInfo, package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (</>))
import Text.Read (readMaybe)

data PackagePlan = PackagePlan
  { planSourcePath :: !FilePath,
    planDependencyPlans :: ![PackagePlan]
  }
  deriving (Eq, Show)

data DependencyResolver = DependencyResolver
  { resolverResolveVersion :: String -> IO String,
    resolverSourcePath :: PackageSpec -> IO FilePath
  }

data CoreProvider = CoreProvider
  { coreProviderName :: !String,
    coreProviderVersion :: !String,
    coreProviderSourceRel :: !FilePath
  }

localDependencyResolverWithFallback :: DependencyResolver -> FilePath -> DependencyResolver
localDependencyResolverWithFallback fallback rootSource =
  DependencyResolver
    { resolverResolveVersion = \name -> do
        local <- localPackage name
        maybe (resolverResolveVersion fallback name) (pure . pkgVersion . fst) local,
      resolverSourcePath = \spec -> do
        local <- localPackage (pkgName spec)
        case local of
          Just (localSpec, path)
            | pkgVersion localSpec == pkgVersion spec -> pure path
          _ -> resolverSourcePath fallback spec
    }
  where
    workspace = takeDirectory (normalise rootSource)
    localPackage name = do
      rootSpec <- packageSpecFromSource rootSource
      if pkgName rootSpec == name
        then pure (Just (rootSpec, rootSource))
        else do
          let candidate = workspace </> name
          exists <- doesDirectoryExist candidate
          if exists
            then do
              spec <- packageSpecFromSource candidate
              pure (Just (spec, candidate))
            else pure Nothing

packageSpecFromSource :: FilePath -> IO PackageSpec
packageSpecFromSource sourcePath = do
  cabalFiles <- HackageUtil.findCabalFiles sourcePath
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
      files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
  cabalBytes <- BS.readFile cabalFile
  gpd <-
    case runParseResult (parseGenericPackageDescription cabalBytes) of
      (_, Right parsed) -> pure parsed
      (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errs))
  let packageId = package (packageDescription gpd)
  pure
    PackageSpec
      { pkgName = CabalPackage.unPackageName (CabalPackage.packageName packageId),
        pkgVersion = prettyShow (CabalPackage.packageVersion packageId)
      }

buildPackagePlanWithResolver :: DependencyResolver -> PackageSpec -> IO PackagePlan
buildPackagePlanWithResolver resolver = buildPackagePlanRecursive resolver []

buildPackagePlanRecursive :: DependencyResolver -> [PackageSpec] -> PackageSpec -> IO PackagePlan
buildPackagePlanRecursive resolver stack rawSpec
  | packageSpecIdentity spec `elem` map packageSpecIdentity stack =
      ioError (userError ("Cyclic dependency while installing " <> formatPackage spec))
  | otherwise = do
      sourcePath <- sourcePathForSpec resolver spec
      dependencyNames <- packageDependencyNamesFromSource sourcePath
      dependencySpecs <- mapM resolveDependencySpec (withImplicitPrimDependency spec dependencyNames)
      dependencyPlans <- mapM (buildPackagePlanRecursive resolver (spec : stack)) dependencySpecs
      pure
        PackagePlan
          { planSourcePath = sourcePath,
            planDependencyPlans = dependencyPlans
          }
  where
    spec = canonicalPackageSpec rawSpec
    resolveDependencySpec dependencyName = do
      version <- resolveVersionForDependency dependencyName
      pure (canonicalPackageSpec (PackageSpec dependencyName version))

    resolveVersionForDependency dependencyName =
      case lookupCoreProvider dependencyName of
        Just provider -> pure (coreProviderVersion provider)
        Nothing -> resolverResolveVersion resolver dependencyName

withImplicitPrimDependency :: PackageSpec -> [String] -> [String]
withImplicitPrimDependency spec dependencies
  | pkgName spec == "aihc-prim" = dependencies
  | any isPrimDependency dependencies = dependencies
  | otherwise = "aihc-prim" : dependencies
  where
    isPrimDependency name = name == "aihc-prim" || name == "ghc-prim"

sourcePathForSpec :: DependencyResolver -> PackageSpec -> IO FilePath
sourcePathForSpec resolver spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> coreProviderSourcePath provider
    Nothing -> resolverSourcePath resolver spec

packageDependencyNamesFromSource :: FilePath -> IO [String]
packageDependencyNamesFromSource sourcePath = do
  cabalFiles <- HackageUtil.findCabalFiles sourcePath
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
      files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
  cabalBytes <- BS.readFile cabalFile
  case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right gpd) -> pure (packageDependencyNames gpd)
    (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errs))

lookupCoreProvider :: String -> Maybe CoreProvider
lookupCoreProvider name =
  case name of
    "base" -> Just aihcBaseProvider
    "aihc-base" -> Just aihcBaseProvider
    "ghc-prim" -> Just aihcPrimProvider
    "aihc-prim" -> Just aihcPrimProvider
    "ghc-internal" -> Just aihcInternalProvider
    "aihc-internal" -> Just aihcInternalProvider
    "template-haskell" -> Just aihcTemplateHaskellProvider
    "aihc-template-haskell" -> Just aihcTemplateHaskellProvider
    "system-cxx-std-lib" -> Just systemCxxStdLibProvider
    _ -> Nothing

canonicalPackageSpec :: PackageSpec -> PackageSpec
canonicalPackageSpec spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> PackageSpec (coreProviderName provider) (coreProviderVersion provider)
    Nothing -> spec

-- | Every standin under @core-libs@, with the version of the boot library it
-- replaces. The versions come from the emulated GHC release so that a
-- package sees the same @base@ version in its @MIN_VERSION_base@ macro, in
-- its resolved dependencies and in the standin's own @.cabal@ file.
coreProviders :: [CoreProvider]
coreProviders = map (uncurry coreProvider) coreProviderSources
  where
    coreProviderSources =
      [ ("aihc-base", "core-libs" </> "aihc-base"),
        ("aihc-prim", "core-libs" </> "aihc-prim"),
        ("aihc-internal", "core-libs" </> "aihc-internal"),
        ("aihc-template-haskell", "core-libs" </> "aihc-template-haskell"),
        ("system-cxx-std-lib", "core-libs" </> "system-cxx-std-lib")
      ]
    coreProvider name sourceRel =
      CoreProvider
        { coreProviderName = name,
          coreProviderVersion =
            maybe
              (error ("core-libs package " <> name <> " is not a boot library of the emulated GHC release"))
              (showVersionBranch . bootLibraryVersion)
              (lookupBootLibraryByStandin name emulatedGhc),
          coreProviderSourceRel = sourceRel
        }

namedCoreProvider :: String -> CoreProvider
namedCoreProvider name =
  case [provider | provider <- coreProviders, coreProviderName provider == name] of
    provider : _ -> provider
    [] -> error ("unknown core provider " <> name)

aihcBaseProvider :: CoreProvider
aihcBaseProvider = namedCoreProvider "aihc-base"

aihcPrimProvider :: CoreProvider
aihcPrimProvider = namedCoreProvider "aihc-prim"

aihcInternalProvider :: CoreProvider
aihcInternalProvider = namedCoreProvider "aihc-internal"

aihcTemplateHaskellProvider :: CoreProvider
aihcTemplateHaskellProvider = namedCoreProvider "aihc-template-haskell"

systemCxxStdLibProvider :: CoreProvider
systemCxxStdLibProvider = namedCoreProvider "system-cxx-std-lib"

-- | The versions a file's @MIN_VERSION_*@ macros report, from the manifests
-- of the packages it is compiled against. A standin is reachable under both
-- its own name and the name of the boot library it replaces, because a
-- Hackage package writes @MIN_VERSION_base@ while the installed package is
-- called @aihc-base@.
dependencyVersionsFromManifests :: [(Text, Text)] -> DependencyVersions
dependencyVersionsFromManifests manifests =
  Map.fromList (concatMap entries manifests)
  where
    entries (name, versionText) =
      case mapM readComponent (T.splitOn "." versionText) of
        Just version ->
          (name, version)
            : [ (T.pack (bootLibraryName library), version)
              | Just library <- [lookupBootLibraryByStandin (T.unpack name) emulatedGhc]
              ]
        Nothing -> []
    readComponent component =
      case reads (T.unpack component) of
        [(value, "")] -> Just value
        _ -> Nothing

coreProviderSourcePath :: CoreProvider -> IO FilePath
coreProviderSourcePath provider = do
  override <- lookupEnv "AIHC_CORE_LIBS_ROOT"
  case override of
    Just root -> pure (root </> coreProviderSourceRel provider)
    Nothing -> do
      cwd <- getCurrentDirectory
      findAncestorContaining providerMarker cwd
  where
    providerRel = coreProviderSourceRel provider
    providerMarker = providerRel </> coreProviderName provider <> ".cabal"

    findAncestorContaining marker dir = do
      exists <- doesFileExist (dir </> marker)
      if exists
        then pure (dir </> providerRel)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then ioError (userError ("Could not find local core library " <> providerRel <> " from current directory"))
            else findAncestorContaining marker parent

packageSpecIdentity :: PackageSpec -> (String, String)
packageSpecIdentity spec =
  (pkgName spec, pkgVersion spec)

renderHumanDiagnostic :: String -> Aeson.Value -> String
renderHumanDiagnostic phase diagnostic =
  unlines $
    T.unpack (locationPrefix <> severityText <> ": " <> modulePrefix <> messageText)
      : renderHumanDiagnosticExcerpt diagnostic
  where
    locationPrefix =
      case diagnosticLocation diagnostic of
        Just location -> location <> ": "
        Nothing -> ""
    modulePrefix =
      case diagnosticModule diagnostic of
        Just moduleName -> "[" <> moduleName <> "] "
        Nothing -> ""
    severityText = fromMaybe "error" (stringField "severity" diagnostic)
    messageText = renderHumanDiagnosticMessage phase diagnostic

renderHumanDiagnosticMessage :: String -> Aeson.Value -> Text
renderHumanDiagnosticMessage phase diagnostic
  | phase == "rename",
    Just message <- stringField "message" diagnostic,
    Just name <- stringField "name" diagnostic,
    Just namespace <- stringField "namespace" diagnostic =
      renderResolveMessage message name namespace
  | otherwise = fromMaybe (diagnosticSummary diagnostic) (stringField "message" diagnostic)

renderResolveMessage :: Text -> Text -> Text -> Text
renderResolveMessage message name namespace
  | message == "unbound" = "unbound " <> renderNamespace namespace <> " name ‘" <> name <> "’"
  | message == "not found" = renderNamespace namespace <> " ‘" <> name <> "’ not found"
  | otherwise = message <> ": " <> renderNamespace namespace <> " name ‘" <> name <> "’"

renderNamespace :: Text -> Text
renderNamespace namespace =
  case namespace of
    "ResolutionNamespaceTerm" -> "term"
    "ResolutionNamespaceType" -> "type"
    "ResolutionNamespaceModule" -> "module"
    _ -> namespace

renderHumanDiagnosticExcerpt :: Aeson.Value -> [String]
renderHumanDiagnosticExcerpt diagnostic =
  case (diagnosticSourceLines diagnostic, diagnosticSpanLines diagnostic) of
    (sourceLines@(_ : _), Just (startLine, startColumn, endLine, endColumn)) ->
      renderSourceExcerpt sourceLines startLine startColumn endLine endColumn
    _ -> []

renderSourceExcerpt :: [DiagnosticSourceLine] -> Int -> Int -> Int -> Int -> [String]
renderSourceExcerpt sourceLines startLine startColumn endLine endColumn
  | null selectedLines = []
  | otherwise = concatMap renderLine selectedLines
  where
    selectedLines =
      filter
        ( \sourceLine ->
            sourceLineNumber sourceLine >= startLine
              && sourceLineNumber sourceLine <= endLine
        )
        sourceLines
    width = length (show (maximum (map sourceLineNumber selectedLines)))
    renderLine sourceLine =
      [ "  " <> padLeft width ' ' (show lineNumber) <> " | " <> T.unpack lineText,
        "  " <> replicate width ' ' <> " | " <> T.unpack (caretIndicator lineNumber lineText)
      ]
      where
        lineNumber = sourceLineNumber sourceLine
        lineText = sourceLineText sourceLine
    caretIndicator lineNumber lineText =
      T.replicate (max 0 (lineStartColumn lineNumber - 1)) " "
        <> T.replicate (lineCaretWidth lineNumber lineText) "^"
    lineStartColumn lineNumber
      | lineNumber == startLine = max 1 startColumn
      | otherwise = 1
    lineCaretWidth lineNumber lineText
      | startLine == endLine =
          max 1 (endColumn - startColumn)
      | lineNumber == startLine =
          max 1 (T.length lineText - lineStartColumn lineNumber + 1)
      | lineNumber == endLine =
          max 1 (endColumn - 1)
      | otherwise =
          max 1 (T.length lineText)

diagnosticModule :: Aeson.Value -> Maybe Text
diagnosticModule =
  stringField "module"

diagnosticFile :: Aeson.Value -> Maybe Text
diagnosticFile diagnostic =
  stringField "file" diagnostic
    <|> (objectField "span" diagnostic >>= stringField "file")

diagnosticLocation :: Aeson.Value -> Maybe Text
diagnosticLocation diagnostic =
  case diagnosticFile diagnostic of
    Nothing -> Nothing
    Just file ->
      Just $
        file
          <> maybe "" (":" <>) lineText
          <> maybe "" (":" <>) columnText
  where
    spanValue = objectField "span" diagnostic
    lineText = scalarFieldText "line" diagnostic <|> (spanValue >>= scalarFieldText "startLine")
    columnText = spanValue >>= scalarFieldText "startColumn"

diagnosticSpanLines :: Aeson.Value -> Maybe (Int, Int, Int, Int)
diagnosticSpanLines diagnostic = do
  spanValue <- objectField "span" diagnostic
  startLine <- intField "startLine" spanValue
  startColumn <- intField "startColumn" spanValue
  endLine <- intField "endLine" spanValue
  endColumn <- intField "endColumn" spanValue
  pure (startLine, startColumn, endLine, endColumn)

data DiagnosticSourceLine = DiagnosticSourceLine
  { sourceLineNumber :: !Int,
    sourceLineText :: !Text
  }

instance Aeson.FromJSON DiagnosticSourceLine where
  parseJSON =
    Aeson.withObject "DiagnosticSourceLine" $ \obj ->
      DiagnosticSourceLine
        <$> obj .: "line"
        <*> obj .: "text"

diagnosticSourceLines :: Aeson.Value -> [DiagnosticSourceLine]
diagnosticSourceLines diagnostic =
  case objectField "sourceLines" diagnostic of
    Just value ->
      case Aeson.fromJSON value of
        Aeson.Success sourceLines -> sourceLines
        Aeson.Error {} -> []
    Nothing -> []

stringField :: String -> Aeson.Value -> Maybe Text
stringField name value =
  case objectField name value of
    Just (Aeson.String text) -> Just text
    _ -> Nothing

scalarFieldText :: String -> Aeson.Value -> Maybe Text
scalarFieldText name value =
  scalarValueText =<< objectField name value

intField :: String -> Aeson.Value -> Maybe Int
intField name value =
  case objectField name value of
    Just fieldValue ->
      case Aeson.fromJSON fieldValue of
        Aeson.Success int -> Just int
        Aeson.Error {} -> Nothing
    Nothing -> Nothing

scalarValueText :: Aeson.Value -> Maybe Text
scalarValueText value =
  case value of
    Aeson.String text -> Just text
    Aeson.Number {} ->
      let parsedInt :: Aeson.Result Int
          parsedInt = Aeson.fromJSON value
       in case parsedInt of
            Aeson.Success int -> Just (T.pack (show int))
            Aeson.Error {} -> Just (diagnosticSummary value)
    _ -> Nothing

objectField :: String -> Aeson.Value -> Maybe Aeson.Value
objectField name value =
  case value of
    Aeson.Object obj -> KeyMap.lookup (Key.fromString name) obj
    _ -> Nothing

diagnosticSummary :: Aeson.Value -> Text
diagnosticSummary =
  TE.decodeUtf8 . BL.toStrict . Aeson.encode

type DiagnosticSourceMap = Map.Map FilePath (Map.Map Int Text)

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

diagnosticSourceMap :: FilePath -> Text -> DiagnosticSourceMap
diagnosticSourceMap initialFile =
  third . foldl' step (initialFile, 1, Map.empty) . T.lines
  where
    third (_, _, value) = value
    step (currentFile, currentLine, sourceMap) line =
      case parseLineDirective line of
        Just (nextLine, nextFile) -> (fromMaybe currentFile nextFile, nextLine, sourceMap)
        Nothing ->
          ( currentFile,
            currentLine + 1,
            Map.insertWith Map.union currentFile (Map.singleton currentLine line) sourceMap
          )

parseLineDirective :: Text -> Maybe (Int, Maybe FilePath)
parseLineDirective line = do
  afterHash <- T.stripPrefix "#" line
  let directive = T.stripStart afterHash
      afterLine = fromMaybe directive (T.stripPrefix "line" directive)
      (lineNumberText, rest) = T.span (`elem` ['0' .. '9']) (T.stripStart afterLine)
  lineNumber <- readMaybe (T.unpack lineNumberText)
  pure (lineNumber, directiveFile rest)
  where
    directiveFile rest =
      case T.breakOn "\"" rest of
        (_, quoted)
          | Just afterQuote <- T.stripPrefix "\"" quoted,
            let (file, closingQuote) = T.breakOn "\"" afterQuote,
            not (T.null closingQuote) ->
              Just (T.unpack file)
        _ -> Nothing

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

parseDiagnosticValue :: FilePath -> (SourceSpan, Text) -> Aeson.Value
parseDiagnosticValue path (span', message) =
  object
    [ "file" .= path,
      "span" .= sourceSpanValue span',
      "message" .= message
    ]

cppDiagnosticValue :: Cpp.Diagnostic -> Aeson.Value
cppDiagnosticValue diag =
  object
    [ "file" .= Cpp.diagFile diag,
      "line" .= Cpp.diagLine diag,
      "severity" .= show (Cpp.diagSeverity diag),
      "message" .= Cpp.diagMessage diag
    ]

sourceSpanValue :: SourceSpan -> Aeson.Value
sourceSpanValue span' =
  case span' of
    NoSourceSpan -> Aeson.Null
    SourceSpan file startLine startCol endLine endCol startOffset endOffset ->
      object
        [ "file" .= file,
          "startLine" .= startLine,
          "startColumn" .= startCol,
          "endLine" .= endLine,
          "endColumn" .= endCol,
          "startOffset" .= startOffset,
          "endOffset" .= endOffset
        ]

padLeft :: Int -> Char -> String -> String
padLeft width char value =
  replicate (max 0 (width - length value)) char <> value

packageDependencyNames :: GenericPackageDescription -> [String]
packageDependencyNames gpd =
  (sort . nub . map T.unpack)
    ( concatMap
        (filter (/= currentPackageName) . libraryDependencies)
        libraryTrees
    )
  where
    evalCond = HackageCabal.conditionEvaluator gpd
    currentPackageName = T.pack . CabalPackage.unPackageName . CabalPackage.packageName . package $ packageDescription gpd
    libraryTrees =
      maybe [] pure (condLibrary gpd)
        <> map snd (condSubLibraries gpd)

    libraryDependencies tree =
      let build = HackageCabal.collectMergedBuildInfo evalCond libBuildInfo tree
       in if buildable build
            then HackageCabal.extractDependencies build
            else []
