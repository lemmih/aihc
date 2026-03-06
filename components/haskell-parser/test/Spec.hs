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
import Test.H2010.Suite (h2010Tests)
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
  h2010 <- h2010Tests
  pure $
    testGroup
      "aihc-parser"
      [ testGroup "golden" [exprOk, exprErr, moduleOk, moduleErr],
        testGroup
          "properties"
          [ QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
            QC.testProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip
          ],
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

prop_exprPrettyRoundTrip :: GenExpr -> Property
prop_exprPrettyRoundTrip generated =
  let expr = toExpr generated
      source = prettyExpr expr
   in counterexample (T.unpack source) $
        case parseExpr defaultConfig source of
          ParseOk reparsed ->
            counterexample ("reparsed: " <> show reparsed) $
              stripExprParens reparsed === stripExprParens expr .&&. prettyExpr reparsed === source
          ParseErr err -> counterexample (show err) False

prop_modulePrettyRoundTrip :: GenModule -> Property
prop_modulePrettyRoundTrip generated =
  let modu = toModule generated
      source = prettyModule modu
   in counterexample (T.unpack source) $
        case parseModule defaultConfig source of
          ParseOk reparsed ->
            counterexample ("reparsed: " <> show reparsed) $
              stripModuleParens reparsed === stripModuleParens modu .&&. prettyModule reparsed === source
          ParseErr err -> counterexample (show err) False

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
    GVar name -> EVar name
    GInt value -> EInt value
    GApp fn arg -> EApp (toExpr fn) (toExpr arg)

toModule :: GenModule -> Module
toModule (GenModule decls) =
  Module
    { moduleName = Just "Generated",
      moduleDecls =
        [ DeclValue
            ( FunctionBind
                name
                [ Match
                    { matchPats = [],
                      matchRhs = UnguardedRhs (toExpr expr)
                    }
                ]
            )
        | (name, expr) <- decls
        ]
    }

stripModuleParens :: Module -> Module
stripModuleParens modu =
  modu {moduleDecls = map stripDeclParens (moduleDecls modu)}

stripDeclParens :: Decl -> Decl
stripDeclParens decl =
  case decl of
    DeclValue valueDecl -> DeclValue (stripValueDeclParens valueDecl)
    _ -> decl

stripValueDeclParens :: ValueDecl -> ValueDecl
stripValueDeclParens valueDecl =
  case valueDecl of
    FunctionBind name matches -> FunctionBind name [m {matchRhs = stripRhsParens (matchRhs m)} | m <- matches]
    PatternBind pat rhs -> PatternBind pat (stripRhsParens rhs)

stripRhsParens :: Rhs -> Rhs
stripRhsParens rhs =
  case rhs of
    UnguardedRhs expr -> UnguardedRhs (stripExprParens expr)
    GuardedRhss guards ->
      GuardedRhss
        [ grhs {guardedRhsGuards = map stripExprParens (guardedRhsGuards grhs), guardedRhsBody = stripExprParens (guardedRhsBody grhs)}
        | grhs <- guards
        ]

stripExprParens :: Expr -> Expr
stripExprParens expr =
  case expr of
    EParen inner -> stripExprParens inner
    EApp f x -> EApp (stripExprParens f) (stripExprParens x)
    EInfix l op r -> EInfix (stripExprParens l) op (stripExprParens r)
    ENegate x -> ENegate (stripExprParens x)
    ESectionL l op -> ESectionL (stripExprParens l) op
    ESectionR op r -> ESectionR op (stripExprParens r)
    EIf c t f -> EIf (stripExprParens c) (stripExprParens t) (stripExprParens f)
    ELambda ps b -> ELambda ps (stripExprParens b)
    ELet binds body -> ELet [(n, stripExprParens v) | (n, v) <- binds] (stripExprParens body)
    ECase scrut alts ->
      ECase
        (stripExprParens scrut)
        [ alt {caseAltRhs = stripRhsParens (caseAltRhs alt)}
        | alt <- alts
        ]
    EDo stmts ->
      EDo
        [ case stmt of
            DoBind p e -> DoBind p (stripExprParens e)
            DoLet binds -> DoLet [(n, stripExprParens v) | (n, v) <- binds]
            DoExpr e -> DoExpr (stripExprParens e)
        | stmt <- stmts
        ]
    EListComp body quals ->
      EListComp
        (stripExprParens body)
        [ case q of
            CompGen p e -> CompGen p (stripExprParens e)
            CompGuard e -> CompGuard (stripExprParens e)
            CompLet binds -> CompLet [(n, stripExprParens v) | (n, v) <- binds]
        | q <- quals
        ]
    EArithSeq seqInfo ->
      EArithSeq
        ( case seqInfo of
            ArithSeqFrom a -> ArithSeqFrom (stripExprParens a)
            ArithSeqFromThen a b -> ArithSeqFromThen (stripExprParens a) (stripExprParens b)
            ArithSeqFromTo a b -> ArithSeqFromTo (stripExprParens a) (stripExprParens b)
            ArithSeqFromThenTo a b c -> ArithSeqFromThenTo (stripExprParens a) (stripExprParens b) (stripExprParens c)
        )
    ERecordCon n fields -> ERecordCon n [(f, stripExprParens v) | (f, v) <- fields]
    ERecordUpd base fields -> ERecordUpd (stripExprParens base) [(f, stripExprParens v) | (f, v) <- fields]
    ETypeSig inner ty -> ETypeSig (stripExprParens inner) ty
    EWhere body binds -> EWhere (stripExprParens body) [(n, stripExprParens v) | (n, v) <- binds]
    EList xs -> EList (map stripExprParens xs)
    ETuple xs -> ETuple (map stripExprParens xs)
    _ -> expr

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures"
