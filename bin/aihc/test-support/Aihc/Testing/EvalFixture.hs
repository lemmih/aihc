{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared source-to-runtime evaluation fixtures.
module Aihc.Testing.EvalFixture
  ( Outcome (..),
    EvalCase (..),
    EvalEnvironment,
    EvaluationFailure (..),
    ProgramEvaluator,
    evalFixtureRoot,
    evalBindingName,
    evalBindingNameInProgram,
    loadEvalCases,
    loadEvalEnvironment,
    evalEnvironmentProgram,
    compileEvalCase,
    evaluateEvalCase,
  )
where

import Aihc.Fc qualified as Fc
import Aihc.Parser
  ( ParseResult (..),
    ParserConfig (..),
    defaultConfig,
    parseExpr,
    parseModule,
  )
import Aihc.Parser.Syntax
  ( Decl (..),
    Expr,
    Extension,
    ImportDecl (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NameType (..),
    Rhs (..),
    ValueDecl (..),
    mkUnqualifiedName,
    parseExtensionName,
  )
import Aihc.Parser.Syntax qualified as Surface
import Aihc.Resolve
  ( ModuleExports,
    Package (..),
    PackageId (..),
    ResolveResult (..),
    Scope,
    collectModuleExportsWithDeps,
    emptyScope,
    lookupImportedModule,
    resolveWithDeps,
    unionScope,
    unnamedPackage,
  )
import Aihc.Tc (TcBindingResult, TcConfig, TcErrorKind (..), TcInterface (..), diagKind, emptyTcInterface, renderPred, renderTcType, tcConfig, tcInterfaceTerms, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModuleSccWithInterface, typecheckModulesWithInterface)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, evaluate, mask, onException)
import Control.Monad (forM, unless)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, nub, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import Foreign.LibFFI (argPtr, callFFI, retCInt)
import Foreign.Ptr (nullPtr)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush, openTempFile, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.DynamicLinker (DL (Default), dlsym)
import System.Posix.IO (closeFd, dup, dupTo, handleToFd, stdOutput)

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

data EvalCase = EvalCase
  { evalCaseId :: !String,
    evalCaseCategory :: !String,
    evalCasePath :: !FilePath,
    evalCaseExtensions :: ![Extension],
    evalCaseModules :: ![Text],
    evalCaseExpression :: !Text,
    evalCaseOutput :: !String,
    evalCaseError :: !(Maybe String),
    evalCaseException :: !(Maybe String),
    evalCaseStdout :: !(Maybe String),
    evalCaseEvaluators :: ![Text],
    evalCaseStatus :: !ExpectedStatus,
    evalCaseReason :: !String
  }
  deriving (Eq, Show)

data EvaluationFailure
  = EvaluationError !String
  | EvaluationRaised !Text
  deriving (Eq, Show)

-- | A phase evaluator receives the synthetic binding name and the fully
-- desugared FC program, then renders the resulting value.
type ProgramEvaluator = Text -> Fc.Program -> IO (Either EvaluationFailure Text)

evalFixtureRoot :: IO FilePath
evalFixtureRoot = do
  configured <- lookupEnv "AIHC_EVAL_FIXTURES"
  maybe defaultEvalFixtureRoot pure configured

defaultEvalFixtureRoot :: IO FilePath
defaultEvalFixtureRoot = do
  cwd <- getCurrentDirectory
  findUp cwd
  where
    findUp dir = do
      let candidate = dir </> "test" </> "Test" </> "Fixtures" </> "eval"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure candidate
            else findUp parent

evalBindingName :: Text
evalBindingName = "__aihc_eval__"

evalBindingNameInProgram :: Fc.Program -> Text
evalBindingNameInProgram program =
  fromMaybe
    evalBindingName
    ( listToMaybe
        [ Fc.nameText (Fc.valName declaration)
        | Fc.DeclVal declaration <- Fc.programDecls program,
          Fc.nameText (Fc.valName declaration) == evalBindingName
        ]
    )

loadEvalCases :: IO [EvalCase]
loadEvalCases = do
  root <- evalFixtureRoot
  exists <- doesDirectoryExist root
  if not exists
    then fail ("Shared eval fixture root does not exist: " <> root)
    else do
      paths <- listFixtureFiles root
      mapM (loadEvalCase root) paths

loadEvalCase :: FilePath -> FilePath -> IO EvalCase
loadEvalCase root path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML eval fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseEvalFixture root path value of
      Left e -> fail e
      Right c -> pure c

parseEvalFixture :: FilePath -> FilePath -> Y.Value -> Either String EvalCase
parseEvalFixture root path value = do
  (extNames, modules, expression, output, expectedError, expectedException, expectedStdout, evaluators, statusText, reasonText) <-
    parseEither
      ( withObject "eval fixture" $ \obj -> do
          exts <- obj .:? "extensions" .!= []
          mods <- obj .: "modules" >>= parseModules
          expr <- obj .: "expression"
          expected <- obj .: "output"
          failureError <- obj .:? "error"
          exception <- obj .:? "exception"
          stdoutOutput <- obj .:? "stdout"
          fixtureEvaluators <- obj .:? "evaluators" .!= ["fc", "grin"]
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, expr, expected, failureError, exception, stdoutOutput, fixtureEvaluators, status, reason)
      )
      value
  if null modules
    then Left ("Eval fixture must define at least one module in " <> path)
    else do
      exts <- validateExtensions path extNames
      status <- parseStatus path statusText
      validateEvaluators path evaluators
      let relPath = makeRelative root path
          category = categoryFromPath relPath
          failureError = trim . T.unpack <$> expectedError
          exception = trim . T.unpack <$> expectedException
      case (status, failureError) of
        (StatusFail, Nothing) -> Left ("Fail eval fixture must define error in " <> path)
        (StatusFail, Just "") -> Left ("Fail eval fixture error must not be empty in " <> path)
        _ -> pure ()
      case exception of
        Just "" -> Left ("Eval fixture exception must not be empty in " <> path)
        Just _
          | status == StatusFail ->
              Left ("Eval fixture exception assertions must use status pass, xfail, or xpass in " <> path)
        _ -> pure ()
      pure
        EvalCase
          { evalCaseId = relPath,
            evalCaseCategory = category,
            evalCasePath = relPath,
            evalCaseExtensions = exts,
            evalCaseModules = modules,
            evalCaseExpression = expression,
            evalCaseOutput = trim (T.unpack output),
            evalCaseError = failureError,
            evalCaseException = exception,
            evalCaseStdout = T.unpack <$> expectedStdout,
            evalCaseEvaluators = evaluators,
            evalCaseStatus = status,
            evalCaseReason = trim (T.unpack reasonText)
          }

validateEvaluators :: FilePath -> [Text] -> Either String ()
validateEvaluators path evaluators
  | null evaluators = Left ("Eval fixture evaluators must not be empty in " <> path)
  | otherwise =
      case filter (`notElem` ["fc", "grin"]) evaluators of
        [] -> Right ()
        invalid -> Left ("Unknown eval fixture evaluators " <> show invalid <> " in " <> path)

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

-- | The shared compilation environment: every module of aihc-prim and
-- aihc-base, resolved, typechecked and desugared once per test run.
-- Fixtures compile only their own modules against it.
-- Evaluators lower that core program one time.
data EvalEnvironment = EvalEnvironment
  { envExports :: !ModuleExports,
    envBuiltinScope :: !Scope,
    envInterface :: !TcInterface,
    envBindings :: ![TcBindingResult],
    envProgram :: !Fc.Program
  }

-- | The desugared System FC program of aihc-prim and aihc-base.
evalEnvironmentProgram :: EvalEnvironment -> Fc.Program
evalEnvironmentProgram = envProgram

evaluateEvalCase :: EvalEnvironment -> ProgramEvaluator -> EvalCase -> IO (Outcome, String)
evaluateEvalCase env evaluator tc =
  case compileEvalCase env tc of
    Left errMsg -> pure (classifyCompileFailure tc errMsg)
    Right program -> do
      (actualStdout, renderResult) <-
        evaluateWithExpectedStdout tc (evaluator evalBindingName program)
      pure $
        case renderResult of
          Right actual -> classifySuccess tc (T.unpack actual) actualStdout
          Left failure -> classifyEvaluationFailure tc failure actualStdout

compileEvalCase :: EvalEnvironment -> EvalCase -> Either String Fc.Program
compileEvalCase env tc = do
  (modules, expr) <- parseInputs tc
  let packageModules = [(unnamedPackage, modu) | modu <- combineModules modules expr]
  case resolveWithDeps (envBuiltinScope env) (envExports env) packageModules of
    ResolveResult {resolvedModules, resolveErrors = []} -> do
      let (tcResults, localInterface) = typecheckModulesWithInterface evalTcConfig (envInterface env) (map snd resolvedModules)
      unless (all tcModuleSuccess tcResults) $
        Left ("typecheck error: " <> renderTcErrors tcResults)
      let interface = envInterface env <> localInterface
          bindings = envBindings env <> moduleGroupBindings tcResults
          results = map (Fc.desugarModuleFc evalDesugarConfig bindings interface) tcResults
      unless (all Fc.dsSuccess results) $
        Left ("desugar error: " <> unlines (concatMap Fc.dsErrors results))
      -- This program contains only the fixture modules. The shared
      -- environment already holds aihc-prim and aihc-base.
      pure (concatPrograms (map Fc.dsProgram results))
    ResolveResult {resolveErrors} ->
      Left ("resolve error: " <> show resolveErrors)

evalTcConfig :: TcConfig
evalTcConfig = tcConfig primPackageId

evalDesugarConfig :: Fc.DesugarConfig
evalDesugarConfig = Fc.DesugarConfig {Fc.primPackageId = primPackageId}

primPackageId :: PackageId
primPackageId = PackageId "aihc-prim"

primPackage :: Package
primPackage = Package "aihc-prim" primPackageId

evalBuiltinScope :: ModuleExports -> Scope
evalBuiltinScope allExports =
  foldr (unionScope . lookupBuiltin) emptyScope ["GHC.Base", "GHC.Classes", "GHC.Num", "GHC.Prim", "GHC.Real"]
  where
    lookupBuiltin name = lookupImportedModule unnamedPackage Nothing name allExports

parseInputs :: EvalCase -> Either String ([Module], Expr)
parseInputs tc = do
  modules <- mapM (parseOneModuleWithExtensions (evalCaseExtensions tc)) (evalCaseModules tc)
  expr <- parseOneExpr (evalCaseExpression tc)
  pure (modules, expr)
  where
    parseOneExpr input =
      case parseExpr (config (evalCasePath tc <> ":expression")) input of
        ParseOk expr -> Right expr
        ParseErr err -> Left ("parse expression error: " <> show err)
    config source =
      defaultConfig
        { parserSourceName = source,
          parserExtensions = evalCaseExtensions tc
        }

parseOneModuleWithExtensions :: [Extension] -> Text -> Either String Module
parseOneModuleWithExtensions extensions input =
  parseOneModule (T.unpack (T.takeWhile (/= '\n') input)) extensions input

parseOneModule :: FilePath -> [Extension] -> Text -> Either String Module
parseOneModule sourceName extensions input =
  let cfg =
        defaultConfig
          { parserSourceName = sourceName,
            parserExtensions = extensions
          }
      (errs, ast) = parseModule cfg input
   in if null errs
        then Right ast
        else Left ("parse module error: " <> show errs)

combineModules :: [Module] -> Expr -> [Module]
combineModules modules expr =
  case modules of
    [] -> [emptyEvalModule expr]
    _ ->
      let depModules = init modules
          evalModule = last modules
       in depModules
            <> [ evalModule
                   { moduleImports = primitiveImports <> moduleImports evalModule,
                     moduleDecls = moduleDecls evalModule <> [evalDecl expr]
                   }
               ]
  where
    primitiveImports =
      case parseOneModule "eval-primitive-import" [] "module AihcEvalImports where\nimport GHC.Prim\n" of
        Right modu -> moduleImports modu
        Left problem -> error problem

emptyEvalModule :: Expr -> Module
emptyEvalModule expr =
  Module
    { moduleAnns = [],
      moduleHead = Nothing,
      moduleLanguagePragmas = [],
      moduleImports = [],
      moduleDecls = [evalDecl expr]
    }

evalDecl :: Expr -> Decl
evalDecl expr =
  DeclValue $
    FunctionBind
      (mkUnqualifiedName NameVarId evalBindingName)
      [ Match
          { matchAnns = [],
            matchHeadForm = MatchHeadPrefix,
            matchPats = [],
            matchRhs = UnguardedRhs [] expr Nothing
          }
      ]

renderTcErrors :: [Module] -> String
renderTcErrors results =
  let rendered =
        unlines
          [ T.unpack (fromMaybe "<unknown>" (Surface.moduleName result))
              <> ": "
              <> renderTcErrorKind (diagKind diagnostic)
          | result <- results,
            diagnostic <- tcModuleDiagnostics result
          ]
   in if null (trim rendered)
        then "type checker failed without diagnostics"
        else rendered

renderTcErrorKind :: TcErrorKind -> String
renderTcErrorKind errorKind =
  case errorKind of
    UnificationError left right _ _ ->
      "could not match " <> renderTcType left <> " with " <> renderTcType right
    OccursCheckError variable ty ->
      "occurs check failed: " <> renderTcType variable <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderTcType expected <> ", got " <> renderTcType actual
    UnsolvedWanted predicate _ ->
      "unsolved constraint " <> renderPred predicate
    TopLevelUnliftedBinding name ty ->
      "top-level binding " <> T.unpack name <> " has unlifted type " <> renderTcType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      "function argument " <> T.unpack name <> " has type " <> renderTcType ty <> " without a fixed runtime representation"
    OtherError message -> message

moduleGroupBindings :: [Module] -> [TcBindingResult]
moduleGroupBindings =
  concatMap tcModuleBindings

-- | Typecheck the core library modules, which must arrive in dependency
-- order. The wired-in modules are checked first as one group.
typecheckCoreModules :: [Module] -> ([Module], TcInterface)
typecheckCoreModules modules =
  let (checkedPrim, primInterface) =
        typecheckModuleSccWithInterface evalTcConfig emptyTcInterface (sortOn moduleOrder primModules)
      (checkedOther, localInterface) =
        typecheckModulesWithInterface evalTcConfig primInterface orderedOtherModules
   in (checkedPrim <> checkedOther, primInterface <> localInterface)
  where
    primModules = filter ((`elem` wiredTypeModules) . moduleKey) modules
    orderedOtherModules = filter ((`notElem` wiredTypeModules) . moduleKey) modules
    moduleOrder modu =
      case moduleKey modu of
        "GHC.Types" -> (0 :: Int, moduleKey modu)
        "GHC.Prim" -> (1, moduleKey modu)
        "GHC.Tuple" -> (2, moduleKey modu)
        _ -> (3, moduleKey modu)

moduleKey :: Module -> Text
moduleKey = fromMaybe "Main" . Surface.moduleName

wiredTypeModules :: [Text]
wiredTypeModules = ["GHC.Prim", "GHC.Tuple", "GHC.Types"]

concatPrograms :: [Fc.Program] -> Fc.Program
concatPrograms programs =
  Fc.Program
    { Fc.programScopes = Fc.insertScope minBound primPackageId "GHC.Types" mergedScopes,
      Fc.programImports =
        Fc.Imports
          { Fc.importHeaders = Map.unions (map (Fc.importHeaders . Fc.programImports) programs),
            Fc.importSynonyms = Map.unions (map (Fc.importSynonyms . Fc.programImports) programs),
            Fc.importAxioms = Map.unions (map (Fc.importAxioms . Fc.programImports) programs),
            Fc.importBinders = Map.unions (map (Fc.importBinders . Fc.programImports) programs)
          },
      Fc.programDecls = concatMap Fc.programDecls programs
    }
  where
    mergedScopes = foldl addScopes Fc.emptyScopeTable programs
    addScopes scopes program =
      foldl
        (\current (scopeId, package, moduleName') -> Fc.insertScope scopeId package moduleName' current)
        scopes
        (Fc.scopeEntries (Fc.programScopes program))

-- | Compile the complete aihc-prim and aihc-base packages. Any failure is
-- fatal: every fixture depends on this environment.
loadEvalEnvironment :: IO EvalEnvironment
loadEvalEnvironment = do
  primRoot <- packageSourceRoot "AIHC_PRIM_SRC" "aihc-prim"
  baseRoot <- packageSourceRoot "AIHC_BASE_SRC" "aihc-base"
  primModules <- loadPackageModules primPackage primRoot
  baseModules <- loadPackageModules unnamedPackage baseRoot
  let packageModules = orderPackageModules (primModules <> baseModules)
  let exports = collectModuleExportsWithDeps mempty packageModules
      builtinScope = evalBuiltinScope exports
  case resolveWithDeps builtinScope mempty packageModules of
    ResolveResult {resolvedModules, resolveErrors = []} -> do
      let (tcResults, interface) = typecheckCoreModules (map snd resolvedModules)
      unless (all tcModuleSuccess tcResults) $
        fail ("core library typecheck error: " <> renderTcErrors tcResults)
      let bindings = moduleGroupBindings tcResults
          results = map (Fc.desugarModuleFc evalDesugarConfig bindings interface) tcResults
      unless (all Fc.dsSuccess results) $
        fail ("core library desugar error: " <> unlines (concatMap Fc.dsErrors results))
      let program = concatPrograms (map Fc.dsProgram results)
      -- Force the shared structures once so no fixture pays for them again.
      _ <- evaluate (length (Fc.programDecls program))
      _ <- evaluate (length (tcInterfaceTerms interface))
      _ <- evaluate (length bindings)
      pure
        EvalEnvironment
          { envExports = exports,
            envBuiltinScope = builtinScope,
            envInterface = interface,
            envBindings = bindings,
            envProgram = program
          }
    ResolveResult {resolveErrors} ->
      fail ("core library resolve error: " <> show resolveErrors)

packageSourceRoot :: String -> FilePath -> IO FilePath
packageSourceRoot variable packageName = do
  configured <- lookupEnv variable
  maybe (findUp =<< getCurrentDirectory) pure configured
  where
    findUp dir = do
      let candidate = dir </> "core-libs" </> packageName
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory dir
          if parent == dir
            then fail ("Cannot find the " <> packageName <> " sources; set " <> variable)
            else findUp parent

-- | Parse every Haskell source file below the package's @src@ directory.
loadPackageModules :: Package -> FilePath -> IO [(Package, Module)]
loadPackageModules package root = do
  paths <- listSourceFiles (root </> "src")
  forM (sort paths) $ \path -> do
    source <- TIO.readFile path
    case parseOneModule path [] source of
      Left errMsg -> fail ("core library module " <> path <> ": " <> errMsg)
      Right modu -> pure (package, modu)

listSourceFiles :: FilePath -> IO [FilePath]
listSourceFiles dir = do
  entries <- listDirectory dir
  concat
    <$> forM
      entries
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listSourceFiles path
            else pure [path | takeExtension path == ".hs"]
      )

-- | Order the modules depth first along their imports, so that a module
-- follows everything it imports unless the import is part of a cycle. The
-- core libraries contain import cycles through Prelude, and the sequential
-- typechecker accepts this order for them.
orderPackageModules :: [(Package, Module)] -> [(Package, Module)]
orderPackageModules packageModules =
  reverse (snd (foldl visit (Set.empty, []) ("Prelude" : Map.keys byName)))
  where
    byName = Map.fromList [(name, entry) | entry@(_, modu) <- packageModules, Just name <- [Surface.moduleName modu]]
    visit (seen, ordered) name
      | name `Set.member` seen = (seen, ordered)
      | Just entry@(_, modu) <- Map.lookup name byName =
          let imports = sort (nub (map importDeclModule (moduleImports modu)))
              (seen', ordered') = foldl visit (Set.insert name seen, ordered) imports
           in (seen', entry : ordered')
      | otherwise = (seen, ordered)

classifySuccess :: EvalCase -> String -> Maybe String -> (Outcome, String)
classifySuccess tc actual actualStdout =
  case evalCaseStatus tc of
    StatusPass
      | Just details <- mismatchDetails -> (OutcomeFail, details)
      | otherwise -> (OutcomePass, "")
    StatusFail ->
      (OutcomeFail, "expected failure but evaluation succeeded")
    StatusXFail
      | isNothing mismatchDetails -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | isNothing mismatchDetails -> (OutcomeXPass, "known bug still passes")
      | otherwise ->
          (OutcomeFail, "expected xpass output match but got: " <> trim actual)
  where
    mismatchDetails
      | Just expectedException <- evalCaseException tc =
          Just
            ( "expected raised exception "
                <> show expectedException
                <> " but evaluation succeeded with output:\n"
                <> trim actual
            )
      | trim actual /= trim (evalCaseOutput tc) =
          Just ("output mismatch\nexpected:\n" <> evalCaseOutput tc <> "\nactual:\n" <> trim actual)
      | otherwise = stdoutMismatch tc actualStdout

stdoutMismatch :: EvalCase -> Maybe String -> Maybe String
stdoutMismatch tc actual =
  case (evalCaseStdout tc, actual) of
    (Nothing, _) -> Nothing
    (Just expected, Just captured)
      | expected == captured -> Nothing
      | otherwise ->
          Just
            ( "stdout mismatch\nexpected: "
                <> show expected
                <> "\nactual: "
                <> show captured
            )
    (Just expected, Nothing) ->
      Just ("stdout was not captured\nexpected: " <> show expected)

evaluateWithExpectedStdout :: EvalCase -> IO a -> IO (Maybe String, a)
evaluateWithExpectedStdout tc action =
  case evalCaseStdout tc of
    Nothing -> do
      result <- action
      pure (Nothing, result)
    Just _ -> do
      (result, captured) <- captureStdout action
      pure (Just (T.unpack captured), result)

-- | Run the program with file descriptor 1 redirected to a file and return
-- what the program wrote.
--
-- The redirect is on the descriptor, not on the 'stdout' handle. A foreign
-- call writes through the C library, and the GRIN interpreter writes through
-- its own handle on descriptor 1. The test runner writes its progress to the
-- 'stdout' handle, which "Aihc.Testing.HostStdout" moves to another
-- descriptor before the tests start. That progress does not land in the file.
captureStdout :: IO a -> IO (a, Text)
captureStdout action =
  withMVar stdoutCaptureLock $ \() ->
    bracket acquire release $ \(path, captureFd) ->
      bracket (dup stdOutput) closeFd $ \originalStdout ->
        mask $ \restore -> do
          hFlush stdout
          flushCStdout
          _ <- dupTo captureFd stdOutput
          result <- restore action `onException` restoreStdout originalStdout
          restoreStdout originalStdout
          captured <- TIO.readFile path
          pure (result, captured)
  where
    acquire = do
      tempDir <- getTemporaryDirectory
      (path, handle) <- openTempFile tempDir "aihc-fc-stdout"
      captureFd <- handleToFd handle `onException` (hClose handle >> removeFile path)
      pure (path, captureFd)
    release (path, captureFd) = do
      closeFd captureFd
      removeFile path
    restoreStdout originalStdout = do
      flushCStdout
      hFlush stdout
      _ <- dupTo originalStdout stdOutput
      pure ()

stdoutCaptureLock :: MVar ()
stdoutCaptureLock = unsafePerformIO (newMVar ())
{-# NOINLINE stdoutCaptureLock #-}

flushCStdout :: IO ()
flushCStdout = do
  fflush <- dlsym Default "fflush"
  _ <- callFFI fflush retCInt [argPtr nullPtr]
  pure ()

classifyCompileFailure :: EvalCase -> String -> (Outcome, String)
classifyCompileFailure tc errDetails =
  case evalCaseStatus tc of
    StatusPass ->
      case evalCaseException tc of
        Just expected ->
          ( OutcomeFail,
            "expected raised exception " <> show expected <> ", but compilation failed: " <> errDetails
          )
        Nothing -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> classifyExpectedFailure tc errDetails
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

classifyEvaluationFailure :: EvalCase -> EvaluationFailure -> Maybe String -> (Outcome, String)
classifyEvaluationFailure tc failure actualStdout =
  case evalCaseStatus tc of
    StatusPass ->
      case evaluationFailureMismatch tc failure actualStdout of
        Nothing -> (OutcomePass, "")
        Just details -> (OutcomeFail, details)
    StatusFail -> classifyExpectedFailure tc (evaluationFailureDetails failure)
    StatusXFail
      | isNothing (evaluationFailureMismatch tc failure actualStdout) -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | isNothing (evaluationFailureMismatch tc failure actualStdout) -> (OutcomeXPass, "known bug still passes")
      | otherwise -> (OutcomeFail, evaluationFailureDetails failure)

evaluationFailureMismatch :: EvalCase -> EvaluationFailure -> Maybe String -> Maybe String
evaluationFailureMismatch tc failure actualStdout =
  case (evalCaseException tc, failure) of
    (Just expected, EvaluationRaised actual)
      | trim expected /= trim (T.unpack actual) ->
          Just
            ( "raised exception mismatch\nexpected:\n"
                <> trim expected
                <> "\nactual:\n"
                <> trim (T.unpack actual)
            )
      | otherwise -> stdoutMismatch tc actualStdout
    (Just expected, EvaluationError details) ->
      Just ("expected raised exception " <> show expected <> ", got evaluation error: " <> details)
    (Nothing, _) -> Just ("expected successful evaluation, got error: " <> evaluationFailureDetails failure)

evaluationFailureDetails :: EvaluationFailure -> String
evaluationFailureDetails failure =
  case failure of
    EvaluationError details -> details
    EvaluationRaised exception -> "uncaught exception: " <> T.unpack exception

classifyExpectedFailure :: EvalCase -> String -> (Outcome, String)
classifyExpectedFailure tc actual =
  case evalCaseError tc of
    Just expected
      | trim expected == trim actual -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "failure error mismatch\nexpected:\n"
              <> trim expected
              <> "\nactual:\n"
              <> trim actual
          )
    Nothing -> (OutcomeFail, "fail fixture does not define an error")

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

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "eval"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
