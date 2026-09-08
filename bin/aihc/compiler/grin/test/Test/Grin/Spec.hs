{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Spec (tests) where

import Aihc.Fc qualified as Fc
import Aihc.Fc.TypeOf qualified as FcType
import Aihc.Grin (GrinLintError (..), GrinProgram (..), InterpretError (..), ProgramStreams (..), interpretProgramBinding, interpretProgramIoBinding, lintProgram, lowerProgram)
import Aihc.Grin.Parser qualified as GrinParser
import Aihc.Resolve (PackageId (..))
import Aihc.Testing.EvalFixture qualified as EvalFixture
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Aeson ((.:))
import Data.Aeson.Types (parseEither, withObject)
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import GrinGolden qualified
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath (takeExtension, (</>))
import System.IO (stderr, stdin)
import Test.Grin.Anf qualified as Anf
import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
import Test.Grin.Lint qualified as Lint
import Test.Grin.Srt qualified as Srt
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

data GrinEvalEnvironment = GrinEvalEnvironment
  { grinEvalFrontend :: !EvalFixture.EvalEnvironment,
    grinEvalCore :: !GrinProgram
  }

tests :: IO TestTree
tests = do
  lintFixtures <- loadLintFixtures
  fixtures <- GrinGolden.loadGrinCases
  evalFixtures <- filter (("grin" `elem`) . EvalFixture.evalCaseEvaluators) <$> EvalFixture.loadEvalCases
  pure
    ( testGroup
        "aihc-grin"
        [ testProperty "generated GRIN pretty-printer round-trip" prop_grinPrettyRoundTrip,
          Anf.tests,
          Lint.tests,
          testGroup "GRIN lint fixtures" lintFixtures,
          Srt.tests,
          testGroup "GRIN golden tests" (map fixtureTest fixtures),
          withResource loadGrinEvalEnvironment (const (pure ())) $ \getEnvironment ->
            testGroup "shared evaluation fixtures via GRIN" (map (evalFixtureTest getEnvironment) evalFixtures)
        ]
    )

-- | Parse each textual GRIN fixture before the lint status check.
loadLintFixtures :: IO [TestTree]
loadLintFixtures = do
  root <- getEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin/aihc/compiler/grin/test/Test/Fixtures/grin-lint"
  paths <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory directory
  pure [testCase path (checkLintFixture (directory </> path)) | path <- paths]

checkLintFixture :: FilePath -> IO ()
checkLintFixture path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left problem -> assertFailure (Y.prettyPrintParseException problem)
    Right value ->
      case parseEither parseFixture value of
        Left problem -> assertFailure problem
        Right (source, status) ->
          case GrinParser.parseProgram source of
            Left problem -> assertFailure (GrinParser.renderParseError problem)
            Right program ->
              case lintProgram program of
                []
                  | status == "xfail" -> pure ()
                  | otherwise -> assertFailure "FAIL: lint accepted an invalid result layout"
                problems
                  | all isResultLayout problems ->
                      when (status == "xfail") $
                        assertFailure ("XPASS: lint rejected the result layout: " <> show problems)
                  | otherwise -> assertFailure ("FAIL: unrelated lint errors: " <> show problems)
  where
    parseFixture = withObject "GRIN lint fixture" $ \object -> do
      source <- object .: "program"
      status <- object .: "status"
      expected <- object .: "error"
      reason <- object .: "reason"
      if status `elem` (["pass", "xfail"] :: [Text]) && expected == ("result-layout" :: Text) && (status /= "xfail" || not (T.null reason))
        then pure (source, status)
        else fail "invalid GRIN lint fixture status or error"
    isResultLayout GrinLintResultLayout {} = True
    isResultLayout _ = False

-- | Lower aihc-prim and aihc-base to GRIN one time.
loadGrinEvalEnvironment :: IO GrinEvalEnvironment
loadGrinEvalEnvironment = do
  frontend <- EvalFixture.loadEvalEnvironment
  case lowerProgram (EvalFixture.evalEnvironmentProgram frontend) of
    Left problem -> fail ("core library GRIN lower error: " <> problem)
    Right grinProgram ->
      case lintProgram grinProgram of
        [] -> do
          -- Force the core GRIN program one time.
          _ <- evaluate (length (grinFunctions grinProgram))
          _ <- evaluate (length (grinGlobals grinProgram))
          pure
            GrinEvalEnvironment
              { grinEvalFrontend = frontend,
                grinEvalCore = grinProgram
              }
        problems -> fail ("core library GRIN lint error: " <> show problems)

fixtureTest :: GrinGolden.GrinCase -> TestTree
fixtureTest fixture = testCase (GrinGolden.caseId fixture) $
  case GrinGolden.evaluateGrinCase fixture of
    (GrinGolden.OutcomePass, _) -> pure ()
    (GrinGolden.OutcomeXFail, _) -> pure ()
    (GrinGolden.OutcomeXPass, details) -> assertFailure ("unexpected pass: " <> details)
    (GrinGolden.OutcomeFail, details) -> assertFailure details

evalFixtureTest :: IO GrinEvalEnvironment -> EvalFixture.EvalCase -> TestTree
evalFixtureTest getEnvironment fixture = testCase (EvalFixture.evalCaseId fixture) $ do
  environment <- getEnvironment
  (outcome, details) <-
    EvalFixture.evaluateEvalCase
      (grinEvalFrontend environment)
      (evaluateGrin (grinEvalCore environment))
      fixture
  case outcome of
    EvalFixture.OutcomePass -> pure ()
    EvalFixture.OutcomeXFail -> pure ()
    EvalFixture.OutcomeXPass -> assertFailure ("unexpected pass: " <> details)
    EvalFixture.OutcomeFail -> assertFailure details

-- | The program writes its standard output to the given handle.
evaluateGrin :: GrinProgram -> EvalFixture.ProgramEvaluator
evaluateGrin coreProgram output name program =
  case prepareEvalProgram name program of
    Left problem -> pure (Left (EvalFixture.EvaluationError problem))
    Right (prepared, unwrapResult) -> evaluatePrepared prepared unwrapResult
  where
    evaluatePrepared prepared unwrapResult =
      case lowerProgram prepared of
        Left problem -> pure (Left (EvalFixture.EvaluationError problem))
        Right fixtureProgram ->
          case lintProgram fixtureProgram of
            [] ->
              fmap unwrapResult . classifyResult
                <$> interpreter streams (bindingName name fixtureProgram) (appendGrinProgram coreProgram fixtureProgram)
            problems -> pure (Left (EvalFixture.EvaluationError ("GRIN lint error: " <> show problems)))
    streams = ProgramStreams {programStdin = stdin, programStdout = output, programStderr = stderr}
    interpreter
      | evalBindingIsIo name program = interpretProgramIoBinding
      | otherwise = interpretProgramBinding

-- | Put the fixture GRIN after the core GRIN.
-- The fixture has a different package name and a different module name.
appendGrinProgram :: GrinProgram -> GrinProgram -> GrinProgram
appendGrinProgram core fixture =
  GrinProgram
    { grinConstructors = grinConstructors core <> grinConstructors fixture,
      grinPrimitives = grinPrimitives core <> grinPrimitives fixture,
      grinForeignCalls = grinForeignCalls core <> grinForeignCalls fixture,
      grinGlobals = grinGlobals core <> grinGlobals fixture,
      grinFunctions = grinFunctions core <> grinFunctions fixture
    }

prepareEvalProgram :: Text -> Fc.Program -> Either String (Fc.Program, Text -> Text)
prepareEvalProgram sourceName program =
  case break isEvalDeclaration (Fc.programDecls program) of
    (_, []) -> Left ("missing evaluation binding " <> T.unpack sourceName)
    (before, Fc.DeclVal declaration : after) ->
      case FcType.repOf typeEnvironment (Fc.valType declaration) of
        Nothing -> Left ("missing evaluation binding representation for " <> T.unpack sourceName)
        Just representation
          | isLiftedRepresentation representation -> Right (program, id)
          | otherwise -> Right (wrapDeclaration before declaration after representation)
    _ -> Left ("invalid evaluation binding " <> T.unpack sourceName)
  where
    typeEnvironment = FcType.typeEnvFromProgram (PackageId "aihc-prim") program
    isEvalDeclaration (Fc.DeclVal declaration) = Fc.nameText (Fc.valName declaration) == sourceName
    isEvalDeclaration _ = False
    isLiftedRepresentation (Fc.TyCon name) = Fc.nameText name == "LiftedRep"
    isLiftedRepresentation _ = False
    wrapDeclaration before declaration after representation =
      let valueName = Fc.valName declaration
          origin = Fc.nameOrigin valueName
          resultTypeName = Fc.Name "__AihcEvalResultType" Fc.SortTypeConstructor origin
          constructorName = Fc.Name "__AihcEvalResult" Fc.SortDataConstructor origin
          primOrigin = Fc.OriginTop (PackageId "aihc-prim") "GHC.Types"
          typeName = Fc.Name "TYPE" Fc.SortTypeConstructor primOrigin
          liftedName = Fc.Name "LiftedRep" Fc.SortDataConstructor primOrigin
          liftedRepresentation = Fc.TyCon liftedName
          resultType = Fc.TyCon resultTypeName
          resultDeclaration =
            Fc.DeclType
              Fc.TypeDecl
                { Fc.typeVis = Fc.Private,
                  Fc.typeName = resultTypeName,
                  Fc.typeBinders = [],
                  Fc.typeResult = Fc.TyApp (Fc.TyCon typeName) liftedRepresentation,
                  Fc.typeRoles = [],
                  Fc.typeCons =
                    [ Fc.ConDecl
                        { Fc.conVis = Fc.Private,
                          Fc.conName = constructorName,
                          Fc.conType = Fc.TyFun representation liftedRepresentation (Fc.valType declaration) resultType
                        }
                    ]
                }
          wrappedDeclaration =
            Fc.DeclVal
              declaration
                { Fc.valType = resultType,
                  Fc.valBody = Fc.ExApp (Fc.ExVar constructorName) (Fc.valBody declaration)
                }
          prefix = "__AihcEvalResult "
          unwrap rendered = fromMaybe rendered (T.stripPrefix prefix rendered)
       in (program {Fc.programDecls = [resultDeclaration] <> before <> [wrappedDeclaration] <> after}, unwrap)

bindingName :: Text -> GrinProgram -> Text
bindingName name program =
  fromMaybe name (listToMaybe [globalName | (globalName, _) <- grinGlobals program, ("\0" <> name) `T.isSuffixOf` globalName])

evalBindingIsIo :: Text -> Fc.Program -> Bool
evalBindingIsIo sourceName program =
  any isIoDeclaration (Fc.programDecls program)
  where
    isIoDeclaration (Fc.DeclVal declaration) =
      Fc.nameText (Fc.valName declaration) == sourceName && isIoType (Fc.valType declaration)
    isIoDeclaration _ = False
    isIoType (Fc.TyApp function _) = isIoType function
    isIoType (Fc.TyForAll _ body) = isIoType body
    isIoType (Fc.TyCon name) = Fc.nameText name == "IO"
    isIoType _ = False

classifyResult :: Either InterpretError Text -> Either EvalFixture.EvaluationFailure Text
classifyResult result =
  case result of
    Left (InterpretRaisedException exception) -> Left (EvalFixture.EvaluationRaised exception)
    Left problem -> Left (EvalFixture.EvaluationError (show problem))
    Right value -> Right value
