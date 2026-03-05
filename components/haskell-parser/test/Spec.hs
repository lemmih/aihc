{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Parser.Canonical
import Parser.Types (ParseResult (..))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.H2010.Suite (h2010Tests)
import Test.Oracle
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  exprOk <- goldenGroup "golden/expr/ok" expectExprOk
  exprErr <- goldenGroup "golden/expr/err" expectExprErr
  moduleOk <- goldenGroup "golden/module/ok" expectModuleOk
  moduleErr <- goldenGroup "golden/module/err" expectModuleErr
  diffModule <- goldenGroup "golden/module/ok" oracleEquivalent
  regressions <- goldenGroup "corpus/regressions" oracleEquivalent
  h2010 <- h2010Tests
  pure $
    testGroup
      "aihc-parser"
      [ testGroup "golden" [exprOk, exprErr, moduleOk, moduleErr],
        testGroup "differential-fixtures" [diffModule, regressions],
        testGroup "properties" [QC.testProperty "generated modules agree with ghc oracle" prop_moduleAgreement],
        h2010
      ]

goldenGroup :: FilePath -> (Text -> Assertion) -> IO TestTree
goldenGroup relDir assertion = do
  let dir = fixtureRoot </> relDir
  files <- fmap sort (listDirectory dir)
  tests <- forM files $ \name -> do
    let path = dir </> name
    pure $ testCase name (TIO.readFile path >>= assertion)
  pure (testGroup relDir tests)

expectExprOk :: Text -> Assertion
expectExprOk input =
  case parseExpr defaultConfig input of
    ParseOk _ -> pure ()
    ParseErr err -> assertFailure ("expected expr success, got " <> show err)

expectExprErr :: Text -> Assertion
expectExprErr input =
  case parseExpr defaultConfig input of
    ParseOk ast -> assertFailure ("expected expr failure, got " <> show ast)
    ParseErr _ -> pure ()

expectModuleOk :: Text -> Assertion
expectModuleOk input =
  case parseModule defaultConfig input of
    ParseOk _ -> pure ()
    ParseErr err -> assertFailure ("expected module success, got " <> show err)

expectModuleErr :: Text -> Assertion
expectModuleErr input =
  case parseModule defaultConfig input of
    ParseOk ast -> assertFailure ("expected module failure, got " <> show ast)
    ParseErr _ -> pure ()

oracleEquivalent :: Text -> Assertion
oracleEquivalent input =
  case (parseModule defaultConfig input, oracleCanonicalModule input) of
    (ParseOk mine, Right ghcCanon) -> normalizeModule mine @?= ghcCanon
    (ParseErr mineErr, Left ghcErr) ->
      assertBool
        ("both failed as expected; ours=" <> show mineErr <> " oracle=" <> T.unpack ghcErr)
        True
    (ParseOk mine, Left ghcErr) ->
      assertFailure ("ours succeeded but oracle failed: " <> show mine <> " oracle=" <> T.unpack ghcErr)
    (ParseErr mineErr, Right ghcCanon) ->
      assertFailure ("oracle succeeded but ours failed: " <> show mineErr <> " oracle=" <> show ghcCanon)

prop_moduleAgreement :: GenModule -> Property
prop_moduleAgreement generated =
  let source = renderModule generated
   in counterexample (T.unpack source) $
        case (parseModule defaultConfig source, oracleCanonicalModule source) of
          (ParseOk mine, Right ghcCanon) -> normalizeModule mine === ghcCanon
          (ParseErr err, Right _) -> counterexample (show err) False
          (ParseOk mine, Left oracleErr) -> counterexample (show mine <> " | oracle: " <> T.unpack oracleErr) False
          (ParseErr err, Left oracleErr) -> counterexample (show err <> " | oracle: " <> T.unpack oracleErr) False

newtype GenModule = GenModule {unGenModule :: [(Text, GenExpr)]}
  deriving (Show)

instance Arbitrary GenModule where
  arbitrary = do
    n <- chooseInt (1, 6)
    names <- vectorOf n genIdent
    exprs <- vectorOf n (genExpr 4)
    pure (GenModule (zip names exprs))

newtype Ident = Ident {unIdent :: Text}
  deriving (Show)

genIdent :: Gen Text
genIdent = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 8)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if candidate `elem` reservedWords
    then genIdent
    else pure candidate

reservedWords :: [Text]
reservedWords =
  [ "_",
    "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where"
  ]

data GenExpr
  = GVar Text
  | GInt Integer
  | GApp GenExpr GenExpr
  deriving (Eq, Show)

genExpr :: Int -> Gen GenExpr
genExpr depth
  | depth <= 0 = oneof [GVar <$> genIdent, GInt <$> chooseInteger (0, 999)]
  | otherwise =
      frequency
        [ (3, GVar <$> genIdent),
          (3, GInt <$> chooseInteger (0, 999)),
          (4, GApp <$> genExpr (depth - 1) <*> genExpr (depth - 1))
        ]

renderModule :: GenModule -> Text
renderModule (GenModule decls) =
  T.unlines $
    ["module Generated where"]
      <> fmap (\(name, expr) -> name <> " = " <> renderExpr expr) decls

renderExpr :: GenExpr -> Text
renderExpr expr =
  case expr of
    GVar name -> name
    GInt value -> T.pack (show value)
    GApp f x -> renderAtom f <> " " <> renderAtom x

renderAtom :: GenExpr -> Text
renderAtom expr =
  case expr of
    GApp _ _ -> "(" <> renderExpr expr <> ")"
    _ -> renderExpr expr

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"
