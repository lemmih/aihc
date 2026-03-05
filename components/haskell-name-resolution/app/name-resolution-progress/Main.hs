{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isSpace)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension)
import qualified GHC.Parser as GHCParser
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..))
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts)
import Parser.Canonical
import Parser (defaultConfig, parseModule)
import Parser.Types (ParseResult (..))
import Resolver (defaultResolveConfig, resolveModule)
import Resolver.Ast
import Resolver.Types (DiagnosticCode (..), NameClass (..), diagCode, diagnostics, preludeMode, resolved, ResolveConfig (..), PreludeMode (..))
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

data Expected = ExpectPass | ExpectXFail deriving (Eq, Show)
data Outcome = OutcomePass | OutcomeXFail | OutcomeXPass | OutcomeFail deriving (Eq, Show)

data CaseMeta = CaseMeta
  { caseId :: !String
  , caseCategory :: !String
  , casePath :: !FilePath
  , caseExpected :: !Expected
  , _caseReason :: !String
  }
  deriving (Eq, Show)

data VarFact = VarFact
  { vfName :: T.Text
  , vfBinding :: Maybe T.Text
  , vfClass :: NameClass
  }
  deriving (Eq, Show)

data ResolveFacts = ResolveFacts
  { rfModuleName :: Maybe T.Text
  , rfDeclNames :: [T.Text]
  , rfVars :: [VarFact]
  , rfDiagnosticCodes :: [DiagnosticCode]
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/progress"

manifestPath :: FilePath
manifestPath = fixtureRoot </> "manifest.tsv"

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
  cases <- loadManifest
  outcomes <- mapM evaluateCase cases
  let passN = count OutcomePass outcomes
      xfailN = count OutcomeXFail outcomes
      xpassN = count OutcomeXPass outcomes
      failN = count OutcomeFail outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct (passN + xpassN) totalN
  putStrLn "Haskell name-resolution progress"
  putStrLn "==============================="
  putStrLn ("PASS      " <> show passN)
  putStrLn ("XFAIL     " <> show xfailN)
  putStrLn ("XPASS     " <> show xpassN)
  putStrLn ("FAIL      " <> show failN)
  putStrLn ("TOTAL     " <> show totalN)
  putStrLn ("COMPLETE  " <> show completion <> "%")

  mapM_ printFail [(m, d) | (m, OutcomeFail, d) <- outcomes]
  mapM_ printXPass [(m, d) | (m, OutcomeXPass, d) <- outcomes]

  if failN == 0 && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

count :: Outcome -> [(CaseMeta, Outcome, String)] -> Int
count wanted = length . filter (\(_, o, _) -> o == wanted)

printFail :: (CaseMeta, String) -> IO ()
printFail (meta, details) =
  putStrLn ("FAIL " <> caseId meta <> " [" <> caseCategory meta <> "] " <> details)

printXPass :: (CaseMeta, String) -> IO ()
printXPass (meta, details) =
  putStrLn ("XPASS " <> caseId meta <> " [" <> caseCategory meta <> "] " <> details)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

evaluateCase :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase meta = do
  source <- TIO.readFile (fixtureRoot </> casePath meta)
  case oracleResolveFacts defaultResolveConfig source of
    Left oracleErr -> pure (meta, OutcomeFail, "oracle failed: " <> T.unpack oracleErr)
    Right oracleFacts ->
      let ours = resolveFacts source
          (outcome, details) = classify (caseExpected meta) ours oracleFacts
       in pure (meta, outcome, details)

classify :: Expected -> Either String ResolveFacts -> ResolveFacts -> (Outcome, String)
classify expected ours oracleFacts =
  case expected of
    ExpectPass ->
      case ours of
        Left err -> (OutcomeFail, "expected pass but parser/resolver failed: " <> err)
        Right oursFacts
          | oursFacts == oracleFacts -> (OutcomePass, "")
          | otherwise -> (OutcomeFail, "facts differ from oracle")
    ExpectXFail ->
      case ours of
        Left _ -> (OutcomeXFail, "")
        Right oursFacts
          | oursFacts == oracleFacts -> (OutcomeXPass, "expected xfail but now matches oracle")
          | otherwise -> (OutcomeXFail, "")

resolveFacts :: T.Text -> Either String ResolveFacts
resolveFacts input =
  case parseModule defaultConfig input of
    ParseErr err -> Left (show err)
    ParseOk modu ->
      let rr = resolveModule defaultResolveConfig modu
          resolvedMod = resolved rr
          decls = resolvedDecls resolvedMod
          vars = concatMap (collectExpr . resolvedDeclExpr) decls
       in Right
            ResolveFacts
              { rfModuleName = resolvedModuleName resolvedMod
              , rfDeclNames = map resolvedDeclName decls
              , rfVars = vars
              , rfDiagnosticCodes = map diagCode (diagnostics rr)
              }
  where
    collectExpr expr =
      case expr of
        RInt _ -> []
        RApp f x -> collectExpr f <> collectExpr x
        RVar name ->
          [ VarFact
              { vfName = rnText name
              , vfBinding = fmap (const (rnText name)) (rnId name)
              , vfClass = rnClass name
              }
          ]

loadManifest :: IO [CaseMeta]
loadManifest = do
  raw <- TIO.readFile manifestPath
  let rows = filter (not . T.null) (map stripComment (T.lines raw))
  mapM parseRow rows

stripComment :: T.Text -> T.Text
stripComment line = T.strip (fst (T.breakOn "#" line))

parseRow :: T.Text -> IO CaseMeta
parseRow row =
  case T.splitOn "\t" row of
    [cid, cat, pathTxt, expectedTxt] -> parseRowWithReason cid cat pathTxt expectedTxt ""
    [cid, cat, pathTxt, expectedTxt, reasonTxt] -> parseRowWithReason cid cat pathTxt expectedTxt reasonTxt
    _ -> fail ("Invalid manifest row (expected 4 or 5 columns): " <> T.unpack row)

parseRowWithReason :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> IO CaseMeta
parseRowWithReason cid cat pathTxt expectedTxt reasonTxt = do
  let path = T.unpack pathTxt
  exists <- doesFileExist (fixtureRoot </> path)
  if not exists
    then fail ("Manifest references missing case file: " <> path)
    else do
      expected <-
        case expectedTxt of
          "pass" -> pure ExpectPass
          "xfail" -> pure ExpectXFail
          _ -> fail ("Unknown expected value: " <> T.unpack expectedTxt)
      let reason = trim (T.unpack reasonTxt)
      case expected of
        ExpectXFail | null reason -> fail ("xfail case requires reason: " <> T.unpack cid)
        _ -> pure ()
      pure
        CaseMeta
          { caseId = T.unpack cid
          , caseCategory = T.unpack cat
          , casePath = path
          , caseExpected = expected
          , _caseReason = reason
          }

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

oracleResolveFacts :: ResolveConfig -> T.Text -> Either T.Text ResolveFacts
oracleResolveFacts cfg source = do
  canon <- oracleCanonicalModule source
  pure (resolveCanonical cfg canon)

resolveCanonical :: ResolveConfig -> CanonicalModule -> ResolveFacts
resolveCanonical cfg modu =
  let (declMap, dupDiags) = gatherDecls (canonicalDecls modu)
      (varFacts, unboundDiags) = foldMap (resolveDeclExpr declMap) (canonicalDecls modu)
   in ResolveFacts
        { rfModuleName = canonicalModuleName modu
        , rfDeclNames = map canonicalDeclName (canonicalDecls modu)
        , rfVars = varFacts
        , rfDiagnosticCodes = dupDiags <> unboundDiags
        }
  where
    resolveDeclExpr declMap decl = resolveExpr declMap (canonicalDeclExpr decl)

    resolveExpr declMap expr =
      case expr of
        CInt _ -> ([], [])
        CApp f x ->
          let (fFacts, fDiags) = resolveExpr declMap f
              (xFacts, xDiags) = resolveExpr declMap x
           in (fFacts <> xFacts, fDiags <> xDiags)
        CVar name ->
          case M.lookup name declMap of
            Just _ ->
              ([VarFact {vfName = name, vfBinding = Just name, vfClass = TopLevelBinder}], [])
            Nothing ->
              if isPreludeName cfg name
                then ([VarFact {vfName = name, vfBinding = Just name, vfClass = PreludeBinder}], [])
                else ([VarFact {vfName = name, vfBinding = Nothing, vfClass = Unresolved}], [EUnboundVariable])

gatherDecls :: [CanonicalDecl] -> (M.Map T.Text (), [DiagnosticCode])
gatherDecls = foldl step (M.empty, [])
  where
    step (seen, diags) decl =
      let name = canonicalDeclName decl
       in case M.lookup name seen of
            Just _ -> (seen, diags <> [EDuplicateBinding])
            Nothing -> (M.insert name () seen, diags)

isPreludeName :: ResolveConfig -> T.Text -> Bool
isPreludeName cfg name =
  case preludeMode cfg of
    ImplicitPreludeOff -> False
    ImplicitPreludeOn -> name `elem` preludeNames

preludeNames :: [T.Text]
preludeNames = ["return", ">>=", ">>", "fail", "fromInteger", "ifThenElse"]

oracleCanonicalModule :: T.Text -> Either T.Text CanonicalModule
oracleCanonicalModule input = do
  parsed <- parseWithGhc input
  first T.pack (toCanonicalModule parsed)

parseWithGhc :: T.Text -> Either T.Text (HsModule GhcPs)
parseWithGhc input =
  let opts = Lexer.mkParserOpts (EnumSet.empty :: EnumSet.EnumSet Extension) emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<name-resolution-progress-oracle>") 1 1
   in case Lexer.unP GHCParser.parseModule (Lexer.initParserState opts buffer start) of
        Lexer.POk _ modu -> Right (unLoc modu)
        Lexer.PFailed _ -> Left "oracle parse failed"

toCanonicalModule :: HsModule GhcPs -> Either String CanonicalModule
toCanonicalModule modu = do
  decls <- traverse toCanonicalDecl (hsmodDecls modu)
  pure
    CanonicalModule
      { canonicalModuleName = fmap (T.pack . moduleNameString . unLoc) (hsmodName modu)
      , canonicalDecls = decls
      }

toCanonicalDecl :: LHsDecl GhcPs -> Either String CanonicalDecl
toCanonicalDecl locatedDecl =
  let decl = unLoc locatedDecl
   in case decl of
        ValD _ bind ->
          case bind of
            FunBind {fun_id = locatedName, fun_matches = MG {mg_alts = locatedMatches}} -> do
              let name = unLoc locatedName
                  matches = unLoc locatedMatches
              match <- case matches of
                [singleMatch] -> Right (unLoc singleMatch)
                _ -> Left "unsupported multiple matches"
              expr <- case m_grhss match of
                GRHSs _ (grhs :| []) _ ->
                  case unLoc grhs of
                    GRHS _ [] body -> toCanonicalExpr (unLoc body)
                    _ -> Left "unsupported guarded rhs"
                _ -> Left "unsupported function rhs"
              pure
                CanonicalDecl
                  { canonicalDeclName = T.pack (occNameString (rdrNameOcc name))
                  , canonicalDeclExpr = expr
                  }
            _ -> Left "unsupported value binding"
        _ -> Left "unsupported declaration kind"

toCanonicalExpr :: HsExpr GhcPs -> Either String CanonicalExpr
toCanonicalExpr expr =
  case expr of
    HsVar _ locatedName ->
      let name = unLoc locatedName
       in Right (CVar (T.pack (occNameString (rdrNameOcc name))))
    HsPar _ inner -> toCanonicalExpr (unLoc inner)
    HsApp _ f x -> CApp <$> toCanonicalExpr (unLoc f) <*> toCanonicalExpr (unLoc x)
    HsOverLit _ lit ->
      case ol_val lit of
        HsIntegral (IL _ _ value) -> Right (CInt value)
        _ -> Left "unsupported literal"
    _ -> Left "unsupported expression"
