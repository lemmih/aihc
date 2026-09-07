{-# LANGUAGE OverloadedStrings #-}

-- | Source-to-GRIN fixture support.
module GrinGolden
  ( GrinCase (..),
    Outcome (..),
    evaluateGrinCase,
    loadGrinCases,
  )
where

import Aihc.Fc (DesugarConfig, FcDesugarResult (..), desugarModuleFc, moduleDesugarConfig)
import Aihc.Fc qualified as Fc
import Aihc.Grin (lintProgram, lowerProgram, prettyProgram)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    LanguageEdition (Haskell98Edition),
    Module,
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    moduleName,
    parseExtensionName,
    parseLanguageEdition,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (ModuleExports, Package (..), PackageId (..), ResolveResult (..), Scope, collectModuleExportsWithDeps, emptyScope, extractInterface, lookupImportedModule, modulesInPackage, resolveWithDeps, unionScope)
import Aihc.Tc
  ( TcInterface,
    emptyTcInterface,
    tcConfig,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
    typecheckModulesWithInterface,
  )
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)

data ExpectedStatus = StatusPass | StatusFail | StatusXPass | StatusXFail
  deriving (Eq, Show)

data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail
  deriving (Eq, Show)

data GrinCase = GrinCase
  { caseId :: !String,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseExpected :: !String,
    caseStatus :: !ExpectedStatus
  }
  deriving (Eq, Show)

data PrimitiveSupport = PrimitiveSupport
  { supportScopes :: !ModuleExports,
    supportTcInterface :: !TcInterface
  }

loadGrinCases :: IO [GrinCase]
loadGrinCases = do
  root <- fixtureRoot
  paths <- listFixtureFiles root
  mapM (loadCase root) paths

evaluateGrinCase :: GrinCase -> (Outcome, String)
evaluateGrinCase fixture =
  case renderCase fixture of
    Left problem -> classifyFailure fixture problem
    Right actual -> classifySuccess fixture actual

renderCase :: GrinCase -> Either String String
renderCase fixture = do
  programs <- buildFcPrograms (caseExtensions fixture) (caseModules fixture)
  lowered <- traverse lowerProgram programs
  case concatMap lintProgram lowered of
    [] -> pure (trim (unlines (map (renderString . layoutPretty defaultLayoutOptions . prettyProgram) lowered)))
    errors -> Left ("GRIN lint error: " <> show errors)

buildFcPrograms :: [Extension] -> [Text] -> Either String [Fc.Program]
buildFcPrograms extensions sources = do
  modules <- traverse (parseFixtureModule extensions) sources
  let fixtureModules = modulesInPackage fixturePackage modules
  resolved <-
    case resolveWithDeps (fixtureBuiltinScope modules) (supportScopes primitiveSupport) fixtureModules of
      result@ResolveResult {resolveErrors = []} -> Right result
      ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)
  let fixtureAsts = map snd (resolvedModules resolved)
      (fixtureTcResults, tcInterface) =
        typecheckModulesWithInterface
          (tcConfig (PackageId "aihc-prim") (PackageId "aihc-base"))
          (supportTcInterface primitiveSupport)
          fixtureAsts
  if not (all tcModuleSuccess fixtureTcResults)
    then Left ("typecheck error: " <> unlines [show diagnostic | result <- fixtureTcResults, diagnostic <- tcModuleDiagnostics result])
    else do
      let availableInterface = supportTcInterface primitiveSupport <> tcInterface
          fixtureBindings = concatMap tcModuleBindings fixtureTcResults
          fixtureExports =
            collectModuleExportsWithDeps (supportScopes primitiveSupport) fixtureModules
          fixtureResults =
            map
              (\checked -> desugarModuleFc (desugarConfig fixturePackage fixtureExports checked) fixtureBindings availableInterface checked)
              fixtureTcResults
      if not (all dsSuccess fixtureResults)
        then Left (unlines (concatMap dsErrors fixtureResults))
        else do
          let fixturePrograms = map dsProgram fixtureResults
          case concatMap Fc.lintProgram fixturePrograms of
            [] -> Right fixturePrograms
            errors -> Left (unlines ["System FC lint error: " <> show errorValue | errorValue <- errors])

parseFixtureModule :: [Extension] -> Text -> Either String Module
parseFixtureModule extensions input =
  parseModuleText (T.unpack (T.takeWhile (/= '\n') input)) extensions input

primitiveSupport :: PrimitiveSupport
primitiveSupport = unsafePerformIO $ do
  modules <- loadPrimitiveModules
  case preparePrimitiveSupport modules of
    Left problem -> fail problem
    Right support -> pure support
{-# NOINLINE primitiveSupport #-}

loadPrimitiveModules :: IO [(FilePath, Text)]
loadPrimitiveModules = do
  sourceRoot <- findPrimitiveSourceRoot
  traverse (loadOne sourceRoot) primitiveModulePaths
  where
    loadOne sourceRoot relativePath = do
      let path = sourceRoot </> relativePath
      source <- TIO.readFile path
      pure (path, source)

-- | Prefer the configured source root over walking up from the working
-- directory, the way FcGolden does: tests that compile a program change the
-- process working directory while other tests are running, so the walk finds
-- nothing if it happens to run then.
findPrimitiveSourceRoot :: IO FilePath
findPrimitiveSourceRoot = do
  configuredRoot <- lookupEnv "AIHC_PRIM_SRC"
  case configuredRoot of
    Just root -> requireModules (root </> "src")
    Nothing -> getCurrentDirectory >>= findUp
  where
    requireModules candidate = do
      exists <- and <$> traverse (doesFileExist . (candidate </>)) primitiveModulePaths
      if exists
        then pure candidate
        else fail "Cannot find the aihc-prim source modules."
    findUp directory = do
      let candidate = directory </> "core-libs" </> "aihc-prim" </> "src"
      exists <- and <$> traverse (doesFileExist . (candidate </>)) primitiveModulePaths
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail "Cannot find the aihc-prim source modules."
            else findUp parent

primitiveModulePaths :: [FilePath]
primitiveModulePaths = ["GHC/Types.hs", "GHC/Prim.hs", "GHC/Tuple.hs", "GHC/CString.hs"]

preparePrimitiveSupport :: [(FilePath, Text)] -> Either String PrimitiveSupport
preparePrimitiveSupport sources = do
  modules <- traverse (uncurry parsePrimitiveModule) sources
  let packageModules = modulesInPackage primitivePackage modules
      exports = collectModuleExportsWithDeps mempty packageModules
      builtinScope = lookupImportedModule primitivePackage Nothing "GHC.Prim" exports
  resolved <-
    case resolveWithDeps builtinScope mempty packageModules of
      result@ResolveResult {resolveErrors = []} -> Right result
      ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)
  let primitiveAsts = map snd (resolvedModules resolved)
      (primitiveTcResults, tcInterface) =
        typecheckModuleSccWithInterface
          (tcConfig (PackageId "aihc-prim") (PackageId "aihc-base"))
          emptyTcInterface
          primitiveAsts
  if not (all tcModuleSuccess primitiveTcResults)
    then
      Left
        ( "typecheck error: "
            <> unlines
              [ show (moduleName ast) <> ": " <> show diagnostic
              | (ast, result) <- zip primitiveAsts primitiveTcResults,
                diagnostic <- tcModuleDiagnostics result
              ]
        )
    else do
      let primitiveBindings = concatMap tcModuleBindings primitiveTcResults
          primitiveResults =
            map
              (\checked -> desugarModuleFc (desugarConfig primitivePackage exports checked) primitiveBindings tcInterface checked)
              primitiveTcResults
      if all dsSuccess primitiveResults
        then
          Right
            PrimitiveSupport
              { supportScopes = extractInterface resolved,
                supportTcInterface = tcInterface
              }
        else Left (unlines (concatMap dsErrors primitiveResults))

primitivePackage :: Package
primitivePackage = Package "aihc-prim" (PackageId "aihc-prim")

fixturePackage :: Package
fixturePackage = Package "" (PackageId "")

fixtureBuiltinScope :: [Module] -> Scope
fixtureBuiltinScope modules =
  foldr (unionScope . lookupBuiltin) emptyScope builtinFunctionModules
  where
    dependencyExports = supportScopes primitiveSupport
    packageModules = modulesInPackage fixturePackage modules
    allExports = collectModuleExportsWithDeps dependencyExports packageModules <> dependencyExports
    lookupBuiltin name = lookupImportedModule fixturePackage Nothing name allExports
    builtinFunctionModules = ["GHC.Base", "GHC.Classes", "GHC.Num", "GHC.Prim", "GHC.Prim.Enum"]

desugarConfig :: Package -> ModuleExports -> Module -> DesugarConfig
desugarConfig package exports modu =
  moduleDesugarConfig (PackageId "aihc-prim") package (fromMaybe "Main" (moduleName modu)) exports

parsePrimitiveModule :: FilePath -> Text -> Either String Module
parsePrimitiveModule sourceName input =
  parseModuleText sourceName (primitiveExtensions input) input

parseModuleText :: FilePath -> [Extension] -> Text -> Either String Module
parseModuleText sourceName extensions input =
  let config =
        defaultConfig
          { parserSourceName = sourceName,
            parserExtensions = extensions
          }
      (errors, ast) = parseModule config input
   in if null errors then Right ast else Left (show errors)

primitiveExtensions :: Text -> [Extension]
primitiveExtensions source =
  filter (/= ImplicitPrelude) (effectiveExtensions language (headerExtensionSettings header))
  where
    header = readModuleHeaderPragmas source
    defaultLanguage = fromMaybe Haskell98Edition (parseLanguageEdition "GHC2021")
    language = fromMaybe defaultLanguage (headerLanguageEdition header)

loadCase :: FilePath -> FilePath -> IO GrinCase
loadCase root path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left problem -> fail ("Invalid GRIN fixture " <> path <> ": " <> Y.prettyPrintParseException problem)
    Right value -> either fail pure (parseFixture root path value)

parseFixture :: FilePath -> FilePath -> Y.Value -> Either String GrinCase
parseFixture root path value = do
  (extensionNames, source, modules, expected, statusText) <-
    parseEither
      ( withObject "GRIN fixture" $ \object ->
          (,,,,)
            <$> object .:? "extensions" .!= []
            <*> object .:? "source"
            <*> object .:? "modules"
            <*> object .:? "expected" .!= ""
            <*> object .: "status"
      )
      value
  extensions <- traverse parseExtension extensionNames
  status <- parseStatus statusText
  moduleTexts <-
    case (source, modules) of
      (Just one, Nothing) -> Right [one]
      (Nothing, Just many) -> Right many
      _ -> Left ("GRIN fixture must contain source or modules: " <> path)
  pure
    GrinCase
      { caseId = makeRelative root path,
        caseExtensions = extensions,
        caseModules = moduleTexts,
        caseExpected = trim (T.unpack expected),
        caseStatus = status
      }
  where
    parseExtension name = maybe (Left ("Unknown extension in " <> path <> ": " <> T.unpack name)) Right (parseExtensionName name)

parseStatus :: Text -> Either String ExpectedStatus
parseStatus value =
  case T.toLower value of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid GRIN fixture status: " <> T.unpack value)

classifySuccess :: GrinCase -> String -> (Outcome, String)
classifySuccess fixture actual =
  case caseStatus fixture of
    StatusPass
      | actual == caseExpected fixture -> (OutcomePass, "")
      | otherwise -> (OutcomeFail, "output mismatch\nexpected:\n" <> caseExpected fixture <> "\nactual:\n" <> actual)
    StatusFail -> (OutcomeFail, "expected failure")
    StatusXFail
      | actual == caseExpected fixture -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | actual == caseExpected fixture -> (OutcomeXPass, "")
      | otherwise -> (OutcomeFail, "expected xpass output")

classifyFailure :: GrinCase -> String -> (Outcome, String)
classifyFailure fixture problem =
  case caseStatus fixture of
    StatusPass -> (OutcomeFail, problem)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, problem)

fixtureRoot :: IO FilePath
fixtureRoot = getCurrentDirectory >>= findUp
  where
    findUp directory = do
      let candidate = directory </> "compiler" </> "grin" </> "test" </> "Test" </> "Fixtures" </> "grin"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else
          let parent = takeDirectory directory
           in if parent == directory then fail "GRIN fixture root does not exist" else findUp parent

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles directory = do
  entries <- sort <$> listDirectory directory
  concat <$> traverse select entries
  where
    select entry = do
      let path = directory </> entry
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then listFixtureFiles path
        else pure [path | map toLower (takeExtension path) `elem` [".yaml", ".yml"]]

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
