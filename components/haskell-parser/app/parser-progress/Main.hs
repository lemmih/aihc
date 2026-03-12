{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM)
import Cpp (resultOutput)
import CppSupport (preprocessForParserWithoutIncludes)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import ExtensionSupport
import GHC.LanguageExtensions.Type (Extension)
import GhcOracle
  ( oracleModuleAstFingerprintWithExtensionsAt,
    oracleParsesModuleWithExtensionsAt,
  )
import OracleExtensions (resolveOracleExtensions)
import qualified Parser
import Parser.Ast (Module)
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

data SuiteResult = SuiteResult
  { srSpec :: !ExtensionSpec,
    srOutcomes :: ![(CaseMeta, Outcome, String)]
  }

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args

  specs <- loadProgressSpecs
  results <- mapM evaluateSuite specs

  let outcomes = concatMap srOutcomes results
      passN = countOutcome OutcomePass outcomes
      xfailN = countOutcome OutcomeXFail outcomes
      xpassN = countOutcome OutcomeXPass outcomes
      failN = countOutcome OutcomeFail outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct (passN + xpassN) totalN

  putStrLn "Parser progress"
  putStrLn "==============="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  let regressions =
        [ (srSpec result, meta, details)
        | result <- results,
          (meta, OutcomeFail, details) <- srOutcomes result
        ]
      xpasses =
        [ (srSpec result, meta, details)
        | result <- results,
          (meta, OutcomeXPass, details) <- srOutcomes result
        ]

  mapM_ printRegression regressions
  mapM_ printXPass xpasses

  if null regressions && (not strict || null xpasses)
    then exitSuccess
    else exitFailure

loadProgressSpecs :: IO [ExtensionSpec]
loadProgressSpecs = do
  allExtensionSpecs <- loadRegistry
  extensionSpecs <- filterM hasManifest allExtensionSpecs
  pure (h2010Spec : extensionSpecs)

h2010Spec :: ExtensionSpec
h2010Spec =
  ExtensionSpec
    { extName = "Haskell2010",
      extFixtureDir = "haskell2010",
      extNotes = ""
    }

evaluateSuite :: ExtensionSpec -> IO SuiteResult
evaluateSuite spec = do
  exts <- oracleExtensionsFor spec
  cases <- loadManifest spec
  outcomes <- mapM (evaluateCase spec exts) cases
  pure
    SuiteResult
      { srSpec = spec,
        srOutcomes = outcomes
      }

oracleExtensionsFor :: ExtensionSpec -> IO [Extension]
oracleExtensionsFor spec
  | extFixtureDir spec == "haskell2010" = pure []
  | otherwise = resolveOracleExtensions spec

evaluateCase :: ExtensionSpec -> [Extension] -> CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase spec exts meta = do
  source <- TIO.readFile (fixtureDirFor spec </> casePath meta)
  let source' = resultOutput (preprocessForParserWithoutIncludes (casePath meta) source)
      parsed = Parser.parseModule Parser.defaultConfig source'
      oracleOk = oracleParsesModuleWithExtensionsAt "parser-progress" exts source'
      roundtripOk = moduleRoundtripsViaGhc exts source' parsed
      (outcome, details) = classifyForSuite spec (caseExpected meta) oracleOk roundtripOk
  pure (meta, outcome, details)

classifyForSuite :: ExtensionSpec -> Expected -> Bool -> Bool -> (Outcome, String)
classifyForSuite spec expected oracleOk roundtripOk
  | extFixtureDir spec == "haskell2010" =
      case expected of
        ExpectPass
          | not oracleOk -> (OutcomeFail, "oracle rejected pass case")
          | not roundtripOk -> (OutcomeFail, "roundtrip mismatch against oracle AST")
          | otherwise -> (OutcomePass, "")
        ExpectXFail
          | oracleOk && roundtripOk -> (OutcomeXPass, "case now passes oracle and roundtrip checks")
          | otherwise -> (OutcomeXFail, "")
  | otherwise = classifyOutcome expected oracleOk roundtripOk

moduleRoundtripsViaGhc :: [Extension] -> Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc exts source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = prettyModule parsed
       in case ( oracleModuleAstFingerprintWithExtensionsAt "parser-progress" exts source,
                 oracleModuleAstFingerprintWithExtensionsAt "parser-progress" exts rendered
               ) of
            (Right sourceAst, Right renderedAst) -> sourceAst == renderedAst
            _ -> False

countOutcome :: Outcome -> [(CaseMeta, Outcome, String)] -> Int
countOutcome target = length . filter (\(_, outcome, _) -> outcome == target)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

printRegression :: (ExtensionSpec, CaseMeta, String) -> IO ()
printRegression (spec, meta, details) =
  putStrLn
    ( "FAIL "
        <> extName spec
        <> "/"
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

printXPass :: (ExtensionSpec, CaseMeta, String) -> IO ()
printXPass (spec, meta, details) =
  putStrLn
    ( "XPASS "
        <> extName spec
        <> "/"
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )
