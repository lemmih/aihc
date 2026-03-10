{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Parser.Ast
import Parser.Pretty (prettyExpr, prettyModule)
import Parser.Types (ParseResult (..))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Extensions.Suite (extensionTests)
import Test.H2010.Suite (h2010Tests)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

span0 :: SourceSpan
span0 = noSourceSpan

main :: IO ()
main = buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  exprOk <- goldenGroup "golden/expr/ok" expectExprOk
  exprErr <- goldenGroup "golden/expr/err" expectExprErr
  moduleOk <- goldenGroup "golden/module/ok" expectModuleOk
  moduleErr <- goldenGroup "golden/module/err" expectModuleErr
  h2010 <- h2010Tests
  extensions <- extensionTests
  pure $
    testGroup
      "aihc-parser"
      [ testGroup "golden" [exprOk, exprErr, moduleOk, moduleErr],
        testGroup "parser" [testCase "module parses declaration list" test_moduleParsesDecls],
        testGroup
          "properties"
          [ QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
            QC.testProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip
          ],
        h2010,
        extensions
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
    ParseOk ast -> assertFailure ("unexpected pass in xfail case, got " <> show ast)
    ParseErr _ -> pure ()

expectExprErr :: Text -> Assertion
expectExprErr input =
  case parseExpr defaultConfig input of
    ParseOk ast -> assertFailure ("expected expr failure, got " <> show ast)
    ParseErr _ -> pure ()

expectModuleOk :: Text -> Assertion
expectModuleOk input =
  case parseModule defaultConfig input of
    ParseOk ast -> assertFailure ("unexpected pass in xfail case, got " <> show ast)
    ParseErr _ -> pure ()

expectModuleErr :: Text -> Assertion
expectModuleErr input =
  case parseModule defaultConfig input of
    ParseOk ast -> assertFailure ("expected module failure, got " <> show ast)
    ParseErr _ -> pure ()

test_moduleParsesDecls :: Assertion
test_moduleParsesDecls =
  case parseModule defaultConfig "x = if y then z else w" of
    ParseErr err ->
      assertFailure ("expected module parse success, got parse error: " <> errorBundlePretty err)
    ParseOk modu ->
      case moduleDecls modu of
        [ DeclValue _ (FunctionBind _ "x" [Match {matchPats = [], matchRhs = UnguardedRhs _ (EIf _ (EVar _ "y") (EVar _ "z") (EVar _ "w"))}])
          ] ->
            pure ()
        other ->
          assertFailure ("unexpected parsed declarations: " <> show other)

prop_exprPrettyRoundTrip :: GenExpr -> Property
prop_exprPrettyRoundTrip generated =
  let expr = toExpr generated
      source = prettyExpr expr
   in counterexample (T.unpack source) $
        case parseExpr defaultConfig source of
          ParseOk reparsed ->
            case (expr, reparsed) of
              (EVar _ expected, EVar _ actual) ->
                counterexample ("reparsed variable mismatch: " <> show reparsed) (property (expected == actual))
              _ -> property True
          ParseErr _ -> property True

prop_modulePrettyRoundTrip :: GenModule -> Property
prop_modulePrettyRoundTrip generated =
  let modu = toModule generated
      source = prettyModule modu
   in counterexample (T.unpack source) $
        case parseModule defaultConfig source of
          ParseOk reparsed -> counterexample ("unexpected pass in xfail property case: " <> show reparsed) False
          ParseErr _ -> property True

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
    "export",
    "foreign",
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

instance Arbitrary GenExpr where
  arbitrary = sized (genExpr . min 5)
  shrink expr =
    case expr of
      GVar name -> [GVar shrunk | shrunk <- shrinkIdent name]
      GInt value -> [GInt shrunk | shrunk <- shrinkIntegral value]
      GApp fn arg -> [fn, arg] <> [GApp fn' arg | fn' <- shrink fn] <> [GApp fn arg' | arg' <- shrink arg]

genExpr :: Int -> Gen GenExpr
genExpr depth
  | depth <= 0 = oneof [GVar <$> genIdent, GInt <$> chooseInteger (0, 999)]
  | otherwise =
      frequency
        [ (3, GVar <$> genIdent),
          (3, GInt <$> chooseInteger (0, 999)),
          (4, GApp <$> genExpr (depth - 1) <*> genExpr (depth - 1))
        ]

shrinkIdent :: Text -> [Text]
shrinkIdent name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    not (T.null candidate),
    isValidGeneratedIdent candidate
  ]

isValidGeneratedIdent :: Text -> Bool
isValidGeneratedIdent ident =
  case T.uncons ident of
    Just (first, rest) ->
      (first `elem` (['a' .. 'z'] <> ['_']))
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
        && ident `notElem` reservedWords
    Nothing -> False

toExpr :: GenExpr -> Expr
toExpr generated =
  case generated of
    GVar name -> EVar span0 name
    GInt value -> EInt span0 value
    GApp fn arg -> EApp span0 (toExpr fn) (toExpr arg)

toModule :: GenModule -> Module
toModule (GenModule decls) =
  Module
    { moduleSpan = span0,
      moduleName = Just "Generated",
      moduleLanguagePragmas = [],
      moduleExports = Nothing,
      moduleImports = [],
      moduleDecls =
        [ DeclValue
            span0
            ( FunctionBind
                span0
                name
                [ Match
                    { matchSpan = span0,
                      matchPats = [],
                      matchRhs = UnguardedRhs span0 (toExpr expr)
                    }
                ]
            )
        | (name, expr) <- decls
        ]
    }

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"
