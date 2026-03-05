{-# LANGUAGE OverloadedStrings #-}

module Test.Resolver
  ( resolverTests
  ) where

import Control.Monad (forM)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Parser
import Parser.Types (ParseResult (..))
import Resolver
import Resolver.Ast
import Resolver.Types
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Oracle.Resolve
import Test.ResolveFacts
import Test.Tasty
import Test.Tasty.HUnit

resolverTests :: IO TestTree
resolverTests = do
  diffModuleOk <- resolverFixtureGroup "golden/module/ok"
  diffRegressions <- resolverFixtureGroup "corpus/regressions"
  pure $
    testGroup
      "resolver"
      [ testGroup
          "unit"
          [ testCase "resolves top-level references" case_resolvesTopLevel
          , testCase "reports unbound variables" case_unboundVariable
          , testCase "reports duplicate top-level bindings" case_duplicateBinding
          , testCase "respects implicit prelude config" case_preludeToggle
          , testCase "deterministic output for same module" case_deterministic
          ]
      , testGroup
          "progress-matrix"
          [ testGroup "node-var" [testCase "top-level lookup" case_resolvesTopLevel]
          , testGroup "node-int" [testCase "integer literal" case_intLiteral]
          , testGroup "node-app" [testCase "application traversal" case_applicationTraversal]
          , testGroup "extension-haskell2010-core" [testCase "minimal module" case_haskell2010Core]
          ]
      , testGroup "differential-fixtures" [diffModuleOk, diffRegressions]
      ]

case_resolvesTopLevel :: Assertion
case_resolvesTopLevel = do
  res <- resolveFactsFromSource defaultResolveConfig "module M where\nid = x\nx = 1\n"
  rfVars res @?= [VarFact {vfName = "x", vfBinding = Just "x", vfClass = TopLevelBinder}]
  rfDiagnosticCodes res @?= []

case_unboundVariable :: Assertion
case_unboundVariable = do
  res <- resolveFactsFromSource defaultResolveConfig "module M where\nid = missing\n"
  rfVars res @?= [VarFact {vfName = "missing", vfBinding = Nothing, vfClass = Unresolved}]
  rfDiagnosticCodes res @?= [EUnboundVariable]

case_duplicateBinding :: Assertion
case_duplicateBinding = do
  res <- resolveFactsFromSource defaultResolveConfig "module M where\nx = 1\nx = 2\n"
  rfDiagnosticCodes res @?= [EDuplicateBinding]

case_preludeToggle :: Assertion
case_preludeToggle = do
  onFacts <- resolveFactsFromSource defaultResolveConfig "module M where\nx = return\n"
  let offCfg = defaultResolveConfig {preludeMode = ImplicitPreludeOff}
  offFacts <- resolveFactsFromSource offCfg "module M where\nx = return\n"
  rfVars onFacts @?= [VarFact {vfName = "return", vfBinding = Just "return", vfClass = PreludeBinder}]
  rfVars offFacts @?= [VarFact {vfName = "return", vfBinding = Nothing, vfClass = Unresolved}]
  rfDiagnosticCodes offFacts @?= [EUnboundVariable]

case_deterministic :: Assertion
case_deterministic = do
  let source = "module M where\nf = g 1\ng = 42\n"
  firstRun <- resolveFactsFromSource defaultResolveConfig source
  secondRun <- resolveFactsFromSource defaultResolveConfig source
  firstRun @?= secondRun

case_intLiteral :: Assertion
case_intLiteral = do
  res <- resolveFactsFromSource defaultResolveConfig "module M where\nx = 42\n"
  rfVars res @?= []
  rfDiagnosticCodes res @?= []

case_applicationTraversal :: Assertion
case_applicationTraversal = do
  res <- resolveFactsFromSource defaultResolveConfig "module M where\nx = f a\nf = a\n"
  rfVars res @?=
    [ VarFact {vfName = "f", vfBinding = Just "f", vfClass = TopLevelBinder}
    , VarFact {vfName = "a", vfBinding = Nothing, vfClass = Unresolved}
    , VarFact {vfName = "a", vfBinding = Nothing, vfClass = Unresolved}
    ]
  rfDiagnosticCodes res @?= [EUnboundVariable, EUnboundVariable]

case_haskell2010Core :: Assertion
case_haskell2010Core = do
  res <- resolveFactsFromSource defaultResolveConfig "module M where\nx = y\ny = 1\n"
  rfDiagnosticCodes res @?= []

resolverFixtureGroup :: FilePath -> IO TestTree
resolverFixtureGroup relDir = do
  let dir = fixtureRoot </> relDir
  files <- fmap sort (listDirectory dir)
  tests <- forM files $ \name -> do
    let path = dir </> name
    pure $ testCase name (fixtureOracleAgreement path)
  pure (testGroup relDir tests)

fixtureOracleAgreement :: FilePath -> Assertion
fixtureOracleAgreement path = do
  input <- TIO.readFile path
  ours <- resolveFactsFromSource defaultResolveConfig input
  case oracleResolveFacts defaultResolveConfig input of
    Left err -> assertFailure ("oracle failed for fixture: " <> show err)
    Right ghcFacts -> ours @?= ghcFacts

resolveFactsFromSource :: ResolveConfig -> Text -> IO ResolveFacts
resolveFactsFromSource cfg input =
  case parseModule defaultConfig input of
    ParseErr err -> assertFailure ("parse failed unexpectedly: " <> show err) >> pure impossible
    ParseOk modu -> pure (toFacts (resolveModule cfg modu))
  where
    impossible =
      ResolveFacts {rfModuleName = Nothing, rfDeclNames = [], rfVars = [], rfDiagnosticCodes = []}

toFacts :: ResolveResult ResolvedModule -> ResolveFacts
toFacts rr =
  let modu = resolved rr
      decls = resolvedDecls modu
      declById = M.fromList [(resolvedDeclId d, resolvedDeclName d) | d <- decls]
      vars = concatMap (collectVars declById . resolvedDeclExpr) decls
      codes = map diagCode (diagnostics rr)
   in ResolveFacts
        { rfModuleName = resolvedModuleName modu
        , rfDeclNames = map resolvedDeclName decls
        , rfVars = vars
        , rfDiagnosticCodes = codes
        }

collectVars :: M.Map NameId Text -> ResolvedExpr -> [VarFact]
collectVars declById expr =
  case expr of
    RInt _ -> []
    RApp f x -> collectVars declById f <> collectVars declById x
    RVar name ->
      [ VarFact
          { vfName = rnText name
          , vfBinding =
              case rnClass name of
                TopLevelBinder -> rnId name >>= (`M.lookup` declById)
                PreludeBinder -> Just (rnText name)
                LocalBinder -> Just (rnText name)
                ImportedBinder -> Just (rnText name)
                Unresolved -> Nothing
          , vfClass = rnClass name
          }
      ]

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"
