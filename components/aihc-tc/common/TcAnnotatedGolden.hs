{-# LANGUAGE OverloadedStrings #-}

-- | Inline annotated golden test infrastructure for the type checker.
--
-- This is intentionally separate from the existing TC golden fixtures. The
-- existing fixtures assert the compact top-level signature summary; these
-- fixtures assert a human-readable source overlay for type-checker output.
module TcAnnotatedGolden
  ( ExpectedStatus (..),
    Outcome (..),
    TcAnnotatedCase (..),
    fixtureRoot,
    loadTcAnnotatedCases,
    checkTcAnnotatedCase,
    evaluateTcAnnotatedCase,
    renderAnnotatedTcResults,
  )
where

import Aihc.Parser
  ( ParserConfig (..),
    defaultConfig,
    parseModule,
  )
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    LanguageEdition (Haskell98Edition),
    Module,
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    importDeclModule,
    moduleImports,
    moduleName,
    parseExtensionName,
    parseLanguageEdition,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (ModuleExports, Package (..), PackageId (..), ResolveResult (..), Scope, collectModuleExportsWithDeps, emptyScope, extractInterface, lookupImportedModule, modulesInPackage, resolveWithDeps, unionScope)
import Aihc.Tc
  ( TcConfig,
    TcInterface,
    emptyTcInterface,
    tcConfig,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
  )
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (when)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (dropWhileEnd, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)
import TcAnnotatedRender (renderAnnotatedTcResults)

data ExpectedStatus
  = StatusPass
  | StatusXPass
  | StatusXFail
  deriving (Eq, Show)

data Outcome
  = OutcomePass
  | OutcomeXFail
  | OutcomeXPass
  | OutcomeFail
  deriving (Eq, Show)

data TcAnnotatedCase = TcAnnotatedCase
  { caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseAnnotated :: !(Maybe [String]),
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

data PrimitiveSupport = PrimitiveSupport
  { supportScopes :: !ModuleExports,
    supportTcInterface :: !TcInterface
  }

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/annotated"

testTcConfig :: TcConfig
testTcConfig = tcConfig (PackageId "aihc-prim")

-- | Load every annotated fixture. The fixture root is resolved relative to the
-- working directory, so running the suite from the wrong directory would
-- otherwise silently produce an empty, always-green test tree. Fail loudly
-- instead.
loadTcAnnotatedCases :: IO [TcAnnotatedCase]
loadTcAnnotatedCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then do
      cwd <- getCurrentDirectory
      fail
        ( "TC annotated fixture directory not found: "
            <> fixtureRoot
            <> " (working directory: "
            <> cwd
            <> "). Run the suite from the aihc-tc package root."
        )
    else do
      primitiveSupport `seq` pure ()
      paths <- listFixtureFiles fixtureRoot
      when (null paths) $
        fail ("TC annotated fixture directory is empty: " <> fixtureRoot)
      mapM loadTcAnnotatedCase paths

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
  mapM (loadOne sourceRoot) ["GHC/Classes.hs", "GHC/Types.hs", "GHC/Prim.hs", "GHC/Prim/IO.hs", "GHC/Prim/Base.hs", "GHC/Tuple.hs"]
  where
    loadOne sourceRoot relativePath = do
      let path = sourceRoot </> relativePath
      source <- TIO.readFile path
      pure (path, source)

findPrimitiveSourceRoot :: IO FilePath
findPrimitiveSourceRoot = getCurrentDirectory >>= findUp
  where
    findUp directory = do
      let candidate = directory </> "core-libs/aihc-prim/src"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail "Cannot find the aihc-prim source directory."
            else findUp parent

loadTcAnnotatedCase :: FilePath -> IO TcAnnotatedCase
loadTcAnnotatedCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseTcAnnotatedFixture path value of
      Left e -> fail e
      Right c -> pure c

parseTcAnnotatedFixture :: FilePath -> Y.Value -> Either String TcAnnotatedCase
parseTcAnnotatedFixture path value = do
  (extNames, modules, annotatedTexts, statusText, reasonText) <-
    parseEither
      ( withObject "tc annotated fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          annotated <- obj .:? "annotated" >>= traverse parseAnnotatedList
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, annotated, status, reason)
      )
      value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
      reason = trim (T.unpack reasonText)
      annotated = map (trim . T.unpack) <$> annotatedTexts
  pure
    TcAnnotatedCase
      { caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseModules = modules,
        caseAnnotated = annotated,
        caseStatus = status,
        caseReason = reason
      }

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

parseAnnotatedList :: Y.Value -> Y.Parser [Text]
parseAnnotatedList = withArray "annotated" $ \arr ->
  mapM parseAnnotatedEntry (foldr (:) [] arr)
  where
    parseAnnotatedEntry (Y.String t) = pure t
    parseAnnotatedEntry _ = fail "each annotated entry must be a string"

evaluateTcAnnotatedCase :: TcAnnotatedCase -> IO (Outcome, String)
evaluateTcAnnotatedCase tc = do
  result <- try (evaluate (forceEvaluation (evaluateTcAnnotatedCasePure tc))) :: IO (Either ErrorCall (Outcome, String))
  pure $
    case result of
      Left exception -> classifyFailure tc ("exception: " <> displayException exception)
      Right outcome -> outcome

forceEvaluation :: (Outcome, String) -> (Outcome, String)
forceEvaluation result@(outcome, details) = outcome `seq` length details `seq` result

evaluateTcAnnotatedCasePure :: TcAnnotatedCase -> (Outcome, String)
evaluateTcAnnotatedCasePure tc =
  case renderTcAnnotatedCase tc of
    Left errMsg -> classifyFailure tc errMsg
    Right actual -> classifySuccess tc actual

renderTcAnnotatedCase :: TcAnnotatedCase -> Either String [String]
renderTcAnnotatedCase tc = do
  checked <- checkTcAnnotatedCase tc
  case caseAnnotated tc of
    Just _ -> pure (renderAnnotatedTcResults (caseModules tc) checked)
    Nothing
      | all tcModuleSuccess checked -> pure []
      | otherwise ->
          Left
            ( "Expected successful type checks.\n"
                <> unlines [show diagnostic | modu <- checked, diagnostic <- tcModuleDiagnostics modu]
            )

-- | Parse, resolve, and type-check the modules of one case.
checkTcAnnotatedCase :: TcAnnotatedCase -> Either String [Module]
checkTcAnnotatedCase tc =
  let parsedModules = map parseOne (caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> Left ("parse error: " <> errMsg)
        Right modules ->
          case resolveWithDeps (fixtureBuiltinScope modules) (supportScopes primitiveSupport) (modulesInPackage fixturePackage modules) of
            ResolveResult {resolvedModules, resolveErrors = []} ->
              typecheckModuleGraph (supportTcInterface primitiveSupport) (map snd resolvedModules)
            ResolveResult {resolveErrors} ->
              Left ("resolve error: " <> show resolveErrors)
  where
    parseOne input =
      parseModuleText (T.unpack (T.takeWhile (/= '\n') input)) (caseExtensions tc) input

data ModuleNode = ModuleNode
  { nodeIndex :: !Int,
    nodeModule :: !Module,
    nodeDependencies :: ![Int]
  }

typecheckModuleGraph :: TcInterface -> [Module] -> Either String [Module]
typecheckModuleGraph baseInterface modules = do
  (checkedModules, _) <- foldl' checkComponent (Right (Map.empty, Map.empty)) components
  traverse (lookupCheckedModule checkedModules) [0 .. length modules - 1]
  where
    moduleIndices =
      Map.fromList
        [ (name, index)
        | (index, modu) <- zip [0 ..] modules,
          Just name <- [moduleName modu]
        ]
    nodes =
      [ let dependencies = mapMaybe ((`Map.lookup` moduleIndices) . importDeclModule) (moduleImports modu)
         in (ModuleNode index modu dependencies, index, dependencies)
      | (index, modu) <- zip [0 ..] modules
      ]
    components = stronglyConnComp nodes
    checkComponent stateResult component = do
      (checkedByIndex, interfacesByIndex) <- stateResult
      let componentNodes = flattenComponent component
          componentIndices = Set.fromList (map nodeIndex componentNodes)
          dependencyIndices =
            Set.toList
              ( Set.fromList (concatMap nodeDependencies componentNodes)
                  `Set.difference` componentIndices
              )
      dependencyInterfaces <- traverse (lookupDependencyInterface interfacesByIndex) dependencyIndices
      let importedInterface = mconcat (baseInterface : dependencyInterfaces)
          (checked, checkedInterface) =
            typecheckModuleSccWithInterface testTcConfig importedInterface (map nodeModule componentNodes)
          checkedByIndex' = foldl' (\acc (node, modu) -> Map.insert (nodeIndex node) modu acc) checkedByIndex (zip componentNodes checked)
          interfacesByIndex' = foldl' (\acc node -> Map.insert (nodeIndex node) checkedInterface acc) interfacesByIndex componentNodes
      pure (checkedByIndex', interfacesByIndex')

flattenComponent :: SCC ModuleNode -> [ModuleNode]
flattenComponent component =
  case component of
    AcyclicSCC node -> [node]
    CyclicSCC nodes -> nodes

lookupDependencyInterface :: Map Int TcInterface -> Int -> Either String TcInterface
lookupDependencyInterface interfaces index =
  maybe (Left ("module graph dependency was not checked: " <> show index)) Right (Map.lookup index interfaces)

lookupCheckedModule :: Map Int Module -> Int -> Either String Module
lookupCheckedModule checked index =
  maybe (Left ("module graph result is missing: " <> show index)) Right (Map.lookup index checked)

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
                  (primitiveTcResults, tcInterface) = typecheckModuleSccWithInterface testTcConfig emptyTcInterface primitiveAsts
               in if all tcModuleSuccess primitiveTcResults
                    then
                      Right
                        PrimitiveSupport
                          { supportScopes = extractInterface resolved,
                            supportTcInterface = tcInterface
                          }
                    else Left ("typecheck error: " <> unlines [show d | r <- primitiveTcResults, d <- tcModuleDiagnostics r])
            ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)

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
    builtinFunctionModules = ["GHC.Base", "GHC.Classes", "GHC.Num", "GHC.Prim", "GHC.Prim.String", "GHC.Real"]

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

classifySuccess :: TcAnnotatedCase -> [String] -> (Outcome, String)
classifySuccess tc actual =
  let expected = caseAnnotated tc
      outputMatches = maybe True ((== map trim actual) . map trim) expected
   in case caseStatus tc of
        StatusPass
          | outputMatches -> (OutcomePass, "")
          | otherwise ->
              ( OutcomeFail,
                "annotated output mismatch\nexpected:\n"
                  <> unlines (fromMaybe [] expected)
                  <> "\nactual:\n"
                  <> unlines actual
              )
        StatusXFail
          | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeXFail, "")
        StatusXPass
          | outputMatches -> (OutcomeXPass, "known bug still passes unexpectedly")
          | otherwise -> (OutcomeFail, "expected xpass output match")

classifyFailure :: TcAnnotatedCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case caseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
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
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "annotated"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
