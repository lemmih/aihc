{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for System FC desugaring.
module FcGolden
  ( ExpectedStatus (..),
    Outcome (..),
    FcCase (..),
    fixtureRoot,
    loadFcCases,
    evaluateFcCase,
  )
where

import Aihc.Fc (DesugarConfig, FcDesugarResult (..), desugarModuleFc, lintProgram, moduleDesugarConfig, parseProgram, renderParseError, renderProgram)
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
import Aihc.Prim.Wiring (primTcConfig, primTcWiring)
import Aihc.Resolve (ModuleExports, Package (..), PackageId (..), ResolveResult (..), Scope, collectModuleExportsWithDeps, emptyScope, extractInterface, lookupImportedModule, modulesInPackage, resolveWithDeps, unionScope)
import Aihc.Tc
  ( TcInterface,
    TcKinds,
    emptyTcInterface,
    mkTcKinds,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
    typecheckModulesWithInterface,
  )
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)

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

data FcCase = FcCase
  { caseId :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseExpected :: !(Maybe String),
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

data PrimitiveSupport = PrimitiveSupport
  { supportScopes :: !ModuleExports,
    supportTcInterface :: !TcInterface
  }

fixtureRoot :: FilePath
fixtureRoot = "compiler/fc/test/Test/Fixtures/golden"

loadFcCases :: IO [FcCase]
loadFcCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadFcCase paths

primitiveSupport :: PrimitiveSupport
primitiveSupport = unsafePerformIO $ do
  primitiveModules <- loadPrimitiveModules
  case preparePrimitiveSupport primitiveModules of
    Left errMsg -> fail errMsg
    Right support -> pure support
{-# NOINLINE primitiveSupport #-}

loadPrimitiveModules :: IO [(FilePath, Text)]
loadPrimitiveModules = do
  sourceRoot <- findPrimitiveSourceRoot
  mapM (loadOne sourceRoot) primitiveModulePaths
  where
    loadOne sourceRoot relativePath = do
      let path = sourceRoot </> relativePath
      source <- TIO.readFile path
      pure (path, source)

findPrimitiveSourceRoot :: IO FilePath
findPrimitiveSourceRoot = do
  configuredRoot <- lookupEnv "AIHC_PRIM_SRC"
  case configuredRoot of
    Just root -> requireModules (root </> "src")
    Nothing -> getCurrentDirectory >>= findUp
  where
    requireModules candidate = do
      exists <- and <$> mapM (doesFileExist . (candidate </>)) primitiveModulePaths
      if exists
        then pure candidate
        else fail "Cannot find the aihc-prim source modules."
    findUp directory = do
      let candidate = directory </> "core-libs/aihc-prim/src"
      exists <- and <$> mapM (doesFileExist . (candidate </>)) primitiveModulePaths
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail "Cannot find the aihc-prim source modules."
            else findUp parent

loadFcCase :: FilePath -> IO FcCase
loadFcCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseFcFixture path value of
      Left e -> fail e
      Right c -> pure c

parseFcFixture :: FilePath -> Y.Value -> Either String FcCase
parseFcFixture path value = do
  (extNames, modules, expectedText, statusText, reasonText) <-
    parseEither
      ( withObject "fc fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          expected <- obj .:? "expected" >>= traverse parseExpectedValue
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, expected, status, reason)
      )
      value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  let relPath = dropRootPrefix path
      expected = trim . T.unpack <$> expectedText
      reason = trim (T.unpack reasonText)
  pure
    FcCase
      { caseId = relPath,
        casePath = relPath,
        caseExtensions = exts,
        caseModules = modules,
        caseExpected = expected,
        caseStatus = status,
        caseReason = reason
      }

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

parseExpectedValue :: Y.Value -> Y.Parser Text
parseExpectedValue (Y.String txt) = pure txt
parseExpectedValue (Y.Array arr) = T.intercalate "\n" <$> mapM parseLine (foldr (:) [] arr)
  where
    parseLine (Y.String t) = pure t
    parseLine _ = fail "each expected line must be a string"
parseExpectedValue _ = fail "expected must be a string or list"

evaluateFcCase :: FcCase -> (Outcome, String)
evaluateFcCase tc =
  case renderFcCase tc of
    Left details -> classifyFailure tc details
    Right actual -> classifySuccess tc actual

renderFcCase :: FcCase -> Either String String
renderFcCase tc =
  let parsedModules = map parseFixtureModule (caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> Left ("parse error: " <> errMsg)
        Right modules ->
          case resolveWithDeps (fixtureBuiltinScope modules) (supportScopes primitiveSupport) (fixtureModules modules) of
            ResolveResult {resolvedModules, resolveErrors = []} ->
              let fixtureAsts = map snd resolvedModules
                  primitiveInterface = supportTcInterface primitiveSupport
                  (fixtureTcResults, tcInterface) = typecheckModulesWithInterface (primTcConfig (PackageId "aihc-prim")) primitiveInterface fixtureAsts
               in if all tcModuleSuccess fixtureTcResults
                    then do
                      let availableInterface = primitiveInterface <> tcInterface
                          fixtureExports =
                            collectModuleExportsWithDeps (supportScopes primitiveSupport) (fixtureModules modules)
                          fixtureResults =
                            map
                              (\checked -> desugarModuleFc (desugarConfig fixturePackage fixtureExports checked) (tcModuleBindings fixtureKinds checked) availableInterface checked)
                              fixtureTcResults
                      if all dsSuccess fixtureResults
                        then lintAndRenderResults fixtureResults
                        else Left (unlines (concatMap dsErrors fixtureResults))
                    else Left ("typecheck error: " <> unlines [show d | r <- fixtureTcResults, d <- tcModuleDiagnostics r])
            ResolveResult {resolveErrors} ->
              Left ("resolve error: " <> show resolveErrors)
  where
    fixtureModules = modulesInPackage fixturePackage
    parseFixtureModule input =
      parseModuleText (T.unpack (T.takeWhile (/= '\n') input)) (caseExtensions tc) input
    lintAndRenderResults fixtureResults =
      case renderResults fixtureResults of
        Left renderError -> Left renderError
        Right rendered ->
          case concatMap (lintProgram . dsProgram) fixtureResults of
            [] -> Right rendered
            lintErrors ->
              Left
                ( unlines ["System FC lint error: " <> show lintError | lintError <- lintErrors]
                    <> "\nSystem FC output:\n"
                    <> rendered
                )
    renderResults results =
      unlines <$> traverse renderResult results
    renderResult result =
      let rendered = renderProgram (dsProgram result)
       in case parseProgram rendered of
            Left parseError -> Left ("System FC round-trip parse error:\n" <> renderParseError parseError <> "\n" <> T.unpack rendered)
            Right parsed ->
              let canonical = renderProgram parsed
               in if canonical == rendered
                    then Right (T.unpack rendered)
                    else Left ("System FC round trip changed canonical syntax:\n" <> T.unpack canonical <> "\noriginal:\n" <> T.unpack rendered)

preparePrimitiveSupport :: [(FilePath, Text)] -> Either String PrimitiveSupport
preparePrimitiveSupport primitiveModules =
  case mapM (uncurry parsePrimitiveModule) primitiveModules of
    Left errMsg -> Left ("parse error: " <> errMsg)
    Right modules ->
      let packageModules = modulesInPackage primitivePackage modules
          exports = collectModuleExportsWithDeps mempty packageModules
          builtinScope = lookupImportedModule primitivePackage Nothing "GHC.Prim" exports
       in case resolveWithDeps builtinScope mempty packageModules of
            resolved@ResolveResult {resolvedModules, resolveErrors = []} ->
              let primitiveAsts = map snd resolvedModules
                  (primitiveTcResults, tcInterface) = typecheckModuleSccWithInterface (primTcConfig (PackageId "aihc-prim")) emptyTcInterface primitiveAsts
               in if all tcModuleSuccess primitiveTcResults
                    then
                      let primitiveBindings = concatMap (tcModuleBindings fixtureKinds) primitiveTcResults
                          primitiveResults =
                            map
                              (\checked -> desugarModuleFc (desugarConfig primitivePackage exports checked) primitiveBindings tcInterface checked)
                              primitiveTcResults
                       in if all dsSuccess primitiveResults
                            then
                              Right
                                PrimitiveSupport
                                  { supportScopes = extractInterface resolved,
                                    supportTcInterface = tcInterface
                                  }
                            else Left (unlines (concatMap dsErrors primitiveResults))
                    else Left ("typecheck error: " <> unlines [show (moduleName ast) <> ": " <> show diagnostic | (ast, result) <- zip primitiveAsts primitiveTcResults, diagnostic <- tcModuleDiagnostics result])
            ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)

primitivePackage :: Package
primitivePackage = Package "aihc-prim" (PackageId "aihc-prim")

fixturePackage :: Package
fixturePackage = Package "" (PackageId "")

primitiveModulePaths :: [FilePath]
primitiveModulePaths =
  [ "GHC/CString.hs",
    "GHC/Classes.hs",
    "GHC/Prim.hs",
    "GHC/Prim/Base.hs",
    "GHC/Prim/Enum.hs",
    "GHC/Prim/IO.hs",
    "GHC/Prim/Integer.hs",
    "GHC/Prim/Num.hs",
    "GHC/Prim/Real.hs",
    "GHC/Prim/String.hs",
    "GHC/Tuple.hs",
    "GHC/Types.hs"
  ]

fixtureBuiltinScope :: [Module] -> Scope
fixtureBuiltinScope modules =
  foldr (unionScope . lookupBuiltin) emptyScope builtinFunctionModules
  where
    dependencyExports = supportScopes primitiveSupport
    packageModules = modulesInPackage fixturePackage modules
    allExports = collectModuleExportsWithDeps dependencyExports packageModules <> dependencyExports
    lookupBuiltin name = lookupImportedModule fixturePackage Nothing name allExports
    builtinFunctionModules = ["GHC.Prim", "GHC.Prim.Base", "GHC.Classes", "GHC.Prim.Enum", "GHC.Prim.Num", "GHC.Prim.Real", "GHC.Prim.String"]

-- | The kind vocabulary of the fixture compiler.
fixtureKinds :: TcKinds
fixtureKinds = mkTcKinds (primTcWiring (PackageId "aihc-prim"))

desugarConfig :: Package -> ModuleExports -> Module -> DesugarConfig
desugarConfig package exports modu =
  moduleDesugarConfig fixtureKinds (PackageId "aihc-prim") package (fromMaybe "Main" (moduleName modu)) exports

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
      (errs, ast) = parseModule config input
   in if null errs
        then Right ast
        else Left (show errs)

primitiveExtensions :: Text -> [Extension]
primitiveExtensions source =
  filter (/= ImplicitPrelude) (effectiveExtensions language (headerExtensionSettings header))
  where
    header = readModuleHeaderPragmas source
    defaultLanguage = fromMaybe Haskell98Edition (parseLanguageEdition "GHC2021")
    language = fromMaybe defaultLanguage (headerLanguageEdition header)

classifySuccess :: FcCase -> String -> (Outcome, String)
classifySuccess tc actual =
  case caseStatus tc of
    StatusPass
      | outputMatches -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "output mismatch\nexpected:\n" <> fromMaybe "" (caseExpected tc) <> "\nactual:\n" <> trim actual
          )
    StatusFail -> (OutcomeFail, "expected failure but desugaring succeeded")
    StatusXFail
      | outputMatches -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | outputMatches -> (OutcomeXPass, "known bug still passes")
      | otherwise -> (OutcomeFail, "expected xpass output match but got: " <> trim actual)
  where
    outputMatches = maybe True (== trim actual) (caseExpected tc)

classifyFailure :: FcCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case caseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

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

validateExtensions :: FilePath -> [Text] -> Either String [Extension]
validateExtensions path = traverse parseOne
  where
    parseOne raw =
      case parseExtensionName raw of
        Just ext -> Right ext
        Nothing -> Left ("Unknown extension " <> show raw <> " in " <> path)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
