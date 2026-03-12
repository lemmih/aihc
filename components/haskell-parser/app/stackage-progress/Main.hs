{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, when)
import Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import CppSupport (preprocessForParser)
import Data.Char (isAlphaNum, isSpace)
import Data.Either (lefts)
import Data.List (isPrefixOf, nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Conc (getNumProcessors)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import GHC.LanguageExtensions.Type (Extension (ForeignFunctionInterface))
import qualified GHC.Parser as GHCParser
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import HackageSupport (diagToText, downloadPackageQuietWithNetwork, findTargetFilesFromCabal, prefixCppErrors, resolveIncludeBestEffort)
import qualified Parser
import Parser.Ast
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (readProcess)

data Check
  = CheckParse
  | CheckRoundtripGhc
  | CheckSourceSpan
  deriving (Eq, Show)

data Options = Options
  { optSnapshot :: String,
    optChecks :: [Check],
    optJobs :: Maybe Int,
    optOffline :: Bool,
    optPrintSucceeded :: Bool
  }

data PackageSpec = PackageSpec
  { pkgName :: String,
    pkgVersion :: String
  }
  deriving (Eq, Show)

data PackageResult = PackageResult
  { package :: PackageSpec,
    packageOk :: Bool,
    packageReason :: String
  }

main :: IO ()
main = do
  args <- getArgs
  opts <-
    case parseOptions args of
      Left err -> do
        hPutStrLn stderr err
        hPutStrLn stderr usage
        exitFailure
      Right parsed -> pure parsed

  snapshotResult <- loadStackageSnapshotWithMode (optSnapshot opts) (optOffline opts)
  packages <-
    case snapshotResult of
      Left err -> do
        hPutStrLn stderr ("Failed to load snapshot: " ++ err)
        exitFailure
      Right specs -> pure specs

  let total = length packages
  jobs <- maybe getNumProcessors pure (optJobs opts)
  putProgressLine (ProgressState 0 0 total)
  results <- mapConcurrentlyChunksWithProgress jobs (runPackage opts) packages total
  let successN = length [() | result <- results, packageOk result]
  putStrLn ""

  when (optPrintSucceeded opts) $ do
    mapM_ putStrLn [formatPackage (package result) | result <- results, packageOk result]
    putStrLn ""

  if successN == total then exitSuccess else exitFailure

usage :: String
usage =
  unlines
    [ "Usage: cabal run stackage-progress -- [--snapshot lts-24.33] [--checks parse,roundtrip-ghc,source-span] [--jobs N] [--offline] [--print-succeeded]",
      "",
      "Defaults:",
      "  --snapshot lts-24.33",
      "  --checks parse",
      "  --jobs <num processors>",
      "  --offline false",
      "  --print-succeeded false"
    ]

parseOptions :: [String] -> Either String Options
parseOptions = go (Options "lts-24.33" [CheckParse] Nothing False False)
  where
    go opts [] = Right opts
    go opts ("--snapshot" : value : rest)
      | null value = Left "--snapshot requires a value"
      | otherwise = go opts {optSnapshot = value} rest
    go opts ("--checks" : value : rest) = do
      checks <- parseChecks value
      go opts {optChecks = checks} rest
    go opts ("--jobs" : value : rest) =
      case reads value of
        [(n, "")] | n > 0 -> go opts {optJobs = Just n} rest
        _ -> Left "--jobs must be a positive integer"
    go opts ("--offline" : rest) =
      go opts {optOffline = True} rest
    go opts ("--print-succeeded" : rest) =
      go opts {optPrintSucceeded = True} rest
    go _ ("--help" : _) = Left ""
    go _ (arg : _) = Left ("Unknown argument: " ++ arg)

formatPackage :: PackageSpec -> String
formatPackage spec = pkgName spec ++ "-" ++ pkgVersion spec

parseChecks :: String -> Either String [Check]
parseChecks raw = do
  checks <- mapM parseCheck (splitComma raw)
  let uniq = nub checks
  if null uniq
    then Left "--checks cannot be empty"
    else Right uniq

parseCheck :: String -> Either String Check
parseCheck raw =
  case trim raw of
    "parse" -> Right CheckParse
    "roundtrip-ghc" -> Right CheckRoundtripGhc
    "source-span" -> Right CheckSourceSpan
    other -> Left ("Unknown check: " ++ other)

splitComma :: String -> [String]
splitComma s =
  case break (== ',') s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitComma rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

loadStackageSnapshotWithMode :: String -> Bool -> IO (Either String [PackageSpec])
loadStackageSnapshotWithMode snapshot offline = do
  cacheFile <- snapshotCacheFile snapshot
  hasCache <- doesFileExist cacheFile
  if hasCache
    then do
      cachedBody <- readFile cacheFile
      pure (parseSnapshotConstraints cachedBody)
    else
      if offline
        then pure (Left ("Snapshot missing from cache in offline mode: " ++ snapshot))
        else do
          let url = "https://www.stackage.org/" ++ snapshot ++ "/cabal.config"
          fetched <- try (readProcess "curl" ["-s", "-f", url] "")
          case fetched of
            Left err -> pure (Left (displayException (err :: SomeException)))
            Right body ->
              case parseSnapshotConstraints body of
                Left parseErr -> pure (Left parseErr)
                Right specs -> do
                  writeFile cacheFile body
                  pure (Right specs)

snapshotCacheFile :: String -> IO FilePath
snapshotCacheFile snapshot = do
  base <- getXdgDirectory XdgCache "aihc"
  let dir = base </> "stackage"
      file = sanitizeSnapshotName snapshot ++ "-cabal.config"
  createDirectoryIfMissing True dir
  pure (dir </> file)

sanitizeSnapshotName :: String -> String
sanitizeSnapshotName = map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c || c == '-' || c == '_' = c
      | otherwise = '_'

parseSnapshotConstraints :: String -> Either String [PackageSpec]
parseSnapshotConstraints content = do
  let section = constraintLines (lines content)
      entries = map trim (splitComma (concat section))
      specs = mapMaybe parseConstraint entries
  if null specs
    then Left "No package constraints found"
    else Right specs

constraintLines :: [String] -> [String]
constraintLines ls =
  case break (isPrefixOf "constraints:" . trimLeft) ls of
    (_, []) -> []
    (_, firstRaw : restRaw) ->
      let firstLine = trimLeft firstRaw
          start = [drop 12 firstLine]
          cont = [trimLeft line | line <- takeWhile isConstraintContinuation restRaw]
       in start <> cont

isConstraintContinuation :: String -> Bool
isConstraintContinuation line =
  case line of
    c : _ -> isSpace c
    [] -> False

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseConstraint :: String -> Maybe PackageSpec
parseConstraint entry
  | null entry = Nothing
  | "--" `isPrefixOf` trim entry = Nothing
  | otherwise =
      case breakOn "==" entry of
        Just (name, ver) -> Just (PackageSpec (trim name) (trim ver))
        Nothing ->
          let ws = words entry
           in case ws of
                [name, "installed"] -> Just (PackageSpec name "installed")
                _ -> Nothing

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack =
  case findNeedle needle haystack of
    Nothing -> Nothing
    Just i ->
      let (left, right) = splitAt i haystack
       in Just (left, drop (length needle) right)

findNeedle :: String -> String -> Maybe Int
findNeedle needle = go 0
  where
    go _ [] = Nothing
    go i xs
      | needle `isPrefixOf` xs = Just i
      | otherwise = go (i + 1) (drop 1 xs)

runPackage :: Options -> PackageSpec -> IO PackageResult
runPackage opts spec = do
  result <- try (runPackageOrThrow opts spec)
  pure $ case result of
    Left err ->
      PackageResult
        { package = spec,
          packageOk = False,
          packageReason = displayException (err :: SomeException)
        }
    Right pkgResult -> pkgResult

runPackageOrThrow :: Options -> PackageSpec -> IO PackageResult
runPackageOrThrow opts spec = do
  if pkgVersion spec == "installed"
    then
      pure
        PackageResult
          { package = spec,
            packageOk = False,
            packageReason = "installed package has no downloadable snapshot version"
          }
    else do
      srcDir <- downloadPackageQuietWithNetwork (not (optOffline opts)) (pkgName spec) (pkgVersion spec)
      files <- findTargetFilesFromCabal srcDir
      if null files
        then
          pure
            PackageResult
              { package = spec,
                packageOk = False,
                packageReason = "no target source files"
              }
        else do
          fileResults <- forM files (checkFile opts srcDir)
          let failures = lefts fileResults
          if null failures
            then
              pure
                PackageResult
                  { package = spec,
                    packageOk = True,
                    packageReason = ""
                  }
            else
              let firstFailure =
                    case failures of
                      f : _ -> f
                      [] -> "unknown failure"
               in pure
                    PackageResult
                      { package = spec,
                        packageOk = False,
                        packageReason = firstFailure
                      }

checkFile :: Options -> FilePath -> FilePath -> IO (Either String ())
checkFile opts packageRoot file = do
  source <- TIO.readFile file
  preprocessed <- preprocessForParser file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrors = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      cppErrorMsg =
        if null cppErrors
          then Nothing
          else Just (T.intercalate "\n" cppErrors)
      oursResult = Parser.parseModule Parser.defaultConfig source'

  case oursResult of
    ParseErr err ->
      if CheckParse `elem` optChecks opts || needsParsedModule (optChecks opts)
        then pure (Left (T.unpack (prefixCppErrors cppErrorMsg ("parse failed in " <> T.pack file <> ": " <> T.pack (show err)))))
        else pure (Right ())
    ParseOk parsed -> do
      roundtripRes <-
        if CheckRoundtripGhc `elem` optChecks opts
          then pure (checkRoundtrip file cppErrorMsg source' parsed)
          else pure (Right ())
      case roundtripRes of
        Left err -> pure (Left err)
        Right () ->
          if CheckSourceSpan `elem` optChecks opts
            then pure (checkSourceSpans file source' parsed)
            else pure (Right ())

needsParsedModule :: [Check] -> Bool
needsParsedModule checks =
  CheckRoundtripGhc `elem` checks || CheckSourceSpan `elem` checks

checkRoundtrip :: FilePath -> Maybe Text -> Text -> Module -> Either String ()
checkRoundtrip file cppErrorMsg source' parsed =
  let rendered = prettyModule parsed
      sourceAst = oracleModuleAstFingerprint source'
      renderedAst = oracleModuleAstFingerprint rendered
   in case (sourceAst, renderedAst) of
        (Right sa, Right ra)
          | sa == ra -> Right ()
        _ -> Left (T.unpack (prefixCppErrors cppErrorMsg ("roundtrip mismatch in " <> T.pack file)))

oracleModuleAstFingerprint :: Text -> Either Text Text
oracleModuleAstFingerprint input = do
  parsed <- parseWithGhc input
  pure (T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhc :: Text -> Either Text (HsModule GhcPs)
#if __GLASGOW_HASKELL__ >= 910
parseWithGhc input =
  let exts = EnumSet.fromList [ForeignFunctionInterface] :: EnumSet.EnumSet Extension
      opts = mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<stackage-progress>") 1 1
   in case unP GHCParser.parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)
#else
parseWithGhc input =
  let exts = EnumSet.fromList [ForeignFunctionInterface] :: EnumSet.EnumSet Extension
      opts = mkParserOpts exts emptyDiagOpts [] False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<stackage-progress>") 1 1
   in case unP GHCParser.parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)
#endif

checkSourceSpans :: FilePath -> Text -> Module -> Either String ()
checkSourceSpans file source modu =
  let exprs = [expr | expr <- collectModuleExprs modu, hasRealSourceSpan (exprSpan expr)]
   in case firstLeft (map (validateExprSpan source) exprs) of
        Nothing -> Right ()
        Just err -> Left (file ++ ": " ++ err)

validateExprSpan :: Text -> Expr -> Either String ()
validateExprSpan source expr = do
  snippet <- extractSpanText source (exprSpan expr)
  case Parser.parseExpr Parser.defaultConfig snippet of
    ParseErr err -> Left ("source-span parse failed for span " ++ show (exprSpan expr) ++ ": " ++ show err)
    ParseOk reparsed ->
      if stripExpr reparsed == stripExpr expr
        then Right ()
        else Left ("source-span mismatch at " ++ show (exprSpan expr))

firstLeft :: [Either a b] -> Maybe a
firstLeft [] = Nothing
firstLeft (x : xs) =
  case x of
    Left err -> Just err
    Right _ -> firstLeft xs

hasRealSourceSpan :: SourceSpan -> Bool
hasRealSourceSpan span' =
  case span' of
    SourceSpan {} -> True
    NoSourceSpan -> False

extractSpanText :: Text -> SourceSpan -> Either String Text
extractSpanText input span' =
  case span' of
    NoSourceSpan -> Left "missing source span"
    SourceSpan sLine sCol eLine eCol
      | sLine <= 0 || sCol <= 0 || eLine <= 0 || eCol <= 0 -> Left "invalid non-positive span coordinates"
      | (eLine, eCol) < (sLine, sCol) -> Left "invalid reversed span"
      | otherwise ->
          let ls = T.splitOn "\n" input
           in if sLine > length ls || eLine > length ls
                then Left "span exceeds input line count"
                else
                  let lineAt n = ls !! (n - 1)
                      startLine = lineAt sLine
                      endLine = lineAt eLine
                   in if sLine == eLine
                        then
                          let startIx = sCol - 1
                              len = eCol - sCol
                           in if startIx < 0 || len < 0 || startIx + len > T.length startLine
                                then Left "single-line span exceeds line bounds"
                                else Right (T.take len (T.drop startIx startLine))
                        else
                          let startIx = sCol - 1
                              endIx = eCol - 1
                              firstPart = T.drop startIx startLine
                              middleParts = [lineAt n | n <- [sLine + 1 .. eLine - 1]]
                              lastPart = T.take endIx endLine
                           in if startIx < 0 || endIx < 0 || startIx > T.length startLine || endIx > T.length endLine
                                then Left "multi-line span exceeds line bounds"
                                else Right (T.intercalate "\n" (firstPart : middleParts <> [lastPart]))

exprSpan :: Expr -> SourceSpan
exprSpan expr =
  case expr of
    EVar span' _ -> span'
    EInt span' _ _ -> span'
    EIntBase span' _ _ -> span'
    EFloat span' _ _ -> span'
    EChar span' _ -> span'
    EString span' _ -> span'
    EQuasiQuote span' _ _ -> span'
    EIf span' _ _ _ -> span'
    ELambdaPats span' _ _ -> span'
    ELambdaCase span' _ -> span'
    EInfix span' _ _ _ -> span'
    ENegate span' _ -> span'
    ESectionL span' _ _ -> span'
    ESectionR span' _ _ -> span'
    ELetDecls span' _ _ -> span'
    ECase span' _ _ -> span'
    EDo span' _ -> span'
    EListComp span' _ _ -> span'
    EListCompParallel span' _ _ -> span'
    EArithSeq span' _ -> span'
    ERecordCon span' _ _ -> span'
    ERecordUpd span' _ _ -> span'
    ETypeSig span' _ _ -> span'
    EParen span' _ -> span'
    EWhereDecls span' _ _ -> span'
    EList span' _ -> span'
    ETuple span' _ -> span'
    ETupleCon span' _ -> span'
    ETypeApp span' _ _ -> span'
    EApp span' _ _ -> span'

collectModuleExprs :: Module -> [Expr]
collectModuleExprs modu = concatMap collectDeclExprs (moduleDecls modu)

collectDeclExprs :: Decl -> [Expr]
collectDeclExprs decl =
  case decl of
    DeclValue _ valueDecl -> collectValueDeclExprs valueDecl
    DeclClass _ classDecl -> concatMap collectClassDeclItemExprs (classDeclItems classDecl)
    DeclInstance _ instDecl -> concatMap collectInstanceDeclItemExprs (instanceDeclItems instDecl)
    _ -> []

collectClassDeclItemExprs :: ClassDeclItem -> [Expr]
collectClassDeclItemExprs item =
  case item of
    ClassItemDefault _ valueDecl -> collectValueDeclExprs valueDecl
    _ -> []

collectInstanceDeclItemExprs :: InstanceDeclItem -> [Expr]
collectInstanceDeclItemExprs item =
  case item of
    InstanceItemBind _ valueDecl -> collectValueDeclExprs valueDecl
    _ -> []

collectValueDeclExprs :: ValueDecl -> [Expr]
collectValueDeclExprs valueDecl =
  case valueDecl of
    FunctionBind _ _ matches -> concatMap collectMatchExprs matches
    PatternBind _ pat rhs -> collectPatternExprs pat <> collectRhsExprs rhs

collectMatchExprs :: Match -> [Expr]
collectMatchExprs match =
  concatMap collectPatternExprs (matchPats match)
    <> collectRhsExprs (matchRhs match)

collectRhsExprs :: Rhs -> [Expr]
collectRhsExprs rhs =
  case rhs of
    UnguardedRhs _ expr -> collectExprTree expr
    GuardedRhss _ guarded -> concatMap collectGuardedRhsExprs guarded

collectGuardedRhsExprs :: GuardedRhs -> [Expr]
collectGuardedRhsExprs guarded =
  concatMap collectExprTree (guardedRhsGuards guarded)
    <> collectExprTree (guardedRhsBody guarded)

collectPatternExprs :: Pattern -> [Expr]
collectPatternExprs pat =
  case pat of
    PView _ viewExpr inner -> collectExprTree viewExpr <> collectPatternExprs inner
    PCon _ _ pats -> concatMap collectPatternExprs pats
    PInfix _ left _ right -> collectPatternExprs left <> collectPatternExprs right
    PAs _ _ inner -> collectPatternExprs inner
    PStrict _ inner -> collectPatternExprs inner
    PIrrefutable _ inner -> collectPatternExprs inner
    PParen _ inner -> collectPatternExprs inner
    PRecord _ _ fields -> concatMap (collectPatternExprs . snd) fields
    PTuple _ pats -> concatMap collectPatternExprs pats
    PList _ pats -> concatMap collectPatternExprs pats
    _ -> []

collectExprTree :: Expr -> [Expr]
collectExprTree expr =
  expr
    : case expr of
      EIf _ c t e -> collectExprTree c <> collectExprTree t <> collectExprTree e
      ELambdaPats _ pats body -> concatMap collectPatternExprs pats <> collectExprTree body
      ELambdaCase _ alts -> concatMap collectCaseAltExprs alts
      EInfix _ l _ r -> collectExprTree l <> collectExprTree r
      ENegate _ e -> collectExprTree e
      ESectionL _ e _ -> collectExprTree e
      ESectionR _ _ e -> collectExprTree e
      ELetDecls _ decls body -> concatMap collectDeclExprs decls <> collectExprTree body
      ECase _ scrutinee alts -> collectExprTree scrutinee <> concatMap collectCaseAltExprs alts
      EDo _ stmts -> concatMap collectDoStmtExprs stmts
      EListComp _ body stmts -> collectExprTree body <> concatMap collectCompStmtExprs stmts
      EListCompParallel _ body stmtGroups ->
        collectExprTree body <> concatMap (concatMap collectCompStmtExprs) stmtGroups
      EArithSeq _ seqExpr -> collectArithSeqExprs seqExpr
      ERecordCon _ _ fields -> concatMap (collectExprTree . snd) fields
      ERecordUpd _ base fields -> collectExprTree base <> concatMap (collectExprTree . snd) fields
      ETypeSig _ e _ -> collectExprTree e
      EParen _ e -> collectExprTree e
      EWhereDecls _ e decls -> collectExprTree e <> concatMap collectDeclExprs decls
      EList _ es -> concatMap collectExprTree es
      ETuple _ es -> concatMap collectExprTree es
      ETypeApp _ e _ -> collectExprTree e
      EApp _ f x -> collectExprTree f <> collectExprTree x
      _ -> []

collectCaseAltExprs :: CaseAlt -> [Expr]
collectCaseAltExprs alt =
  collectPatternExprs (caseAltPattern alt)
    <> collectRhsExprs (caseAltRhs alt)

collectDoStmtExprs :: DoStmt -> [Expr]
collectDoStmtExprs stmt =
  case stmt of
    DoBind _ pat expr -> collectPatternExprs pat <> collectExprTree expr
    DoLet _ binds -> concatMap (collectExprTree . snd) binds
    DoLetDecls _ decls -> concatMap collectDeclExprs decls
    DoExpr _ expr -> collectExprTree expr

collectCompStmtExprs :: CompStmt -> [Expr]
collectCompStmtExprs stmt =
  case stmt of
    CompGen _ pat expr -> collectPatternExprs pat <> collectExprTree expr
    CompGuard _ expr -> collectExprTree expr
    CompLet _ binds -> concatMap (collectExprTree . snd) binds
    CompLetDecls _ decls -> concatMap collectDeclExprs decls

collectArithSeqExprs :: ArithSeq -> [Expr]
collectArithSeqExprs seqExpr =
  case seqExpr of
    ArithSeqFrom _ a -> collectExprTree a
    ArithSeqFromThen _ a b -> collectExprTree a <> collectExprTree b
    ArithSeqFromTo _ a b -> collectExprTree a <> collectExprTree b
    ArithSeqFromThenTo _ a b c -> collectExprTree a <> collectExprTree b <> collectExprTree c

stripExpr :: Expr -> Expr
stripExpr expr =
  case expr of
    EVar _ t -> EVar noSourceSpan t
    EInt _ n repr -> EInt noSourceSpan n repr
    EIntBase _ n txt -> EIntBase noSourceSpan n txt
    EFloat _ d repr -> EFloat noSourceSpan d repr
    EChar _ c -> EChar noSourceSpan c
    EString _ s -> EString noSourceSpan s
    EQuasiQuote _ q body -> EQuasiQuote noSourceSpan q body
    EIf _ a b c -> EIf noSourceSpan (stripExpr a) (stripExpr b) (stripExpr c)
    ELambdaPats _ pats e -> ELambdaPats noSourceSpan (map stripPattern pats) (stripExpr e)
    ELambdaCase _ alts -> ELambdaCase noSourceSpan (map stripCaseAlt alts)
    EInfix _ a op b -> EInfix noSourceSpan (stripExpr a) op (stripExpr b)
    ENegate _ e -> ENegate noSourceSpan (stripExpr e)
    ESectionL _ e op -> ESectionL noSourceSpan (stripExpr e) op
    ESectionR _ op e -> ESectionR noSourceSpan op (stripExpr e)
    ELetDecls _ decls e -> ELetDecls noSourceSpan (map stripDecl decls) (stripExpr e)
    ECase _ e alts -> ECase noSourceSpan (stripExpr e) (map stripCaseAlt alts)
    EDo _ stmts -> EDo noSourceSpan (map stripDoStmt stmts)
    EListComp _ e stmts -> EListComp noSourceSpan (stripExpr e) (map stripCompStmt stmts)
    EListCompParallel _ e groups -> EListCompParallel noSourceSpan (stripExpr e) (map (map stripCompStmt) groups)
    EArithSeq _ a -> EArithSeq noSourceSpan (stripArithSeq a)
    ERecordCon _ name fields -> ERecordCon noSourceSpan name [(field, stripExpr val) | (field, val) <- fields]
    ERecordUpd _ base fields -> ERecordUpd noSourceSpan (stripExpr base) [(field, stripExpr val) | (field, val) <- fields]
    ETypeSig _ e t -> ETypeSig noSourceSpan (stripExpr e) (stripType t)
    EParen _ e -> EParen noSourceSpan (stripExpr e)
    EWhereDecls _ e decls -> EWhereDecls noSourceSpan (stripExpr e) (map stripDecl decls)
    EList _ es -> EList noSourceSpan (map stripExpr es)
    ETuple _ es -> ETuple noSourceSpan (map stripExpr es)
    ETupleCon _ n -> ETupleCon noSourceSpan n
    ETypeApp _ e t -> ETypeApp noSourceSpan (stripExpr e) (stripType t)
    EApp _ f x -> EApp noSourceSpan (stripExpr f) (stripExpr x)

stripDecl :: Decl -> Decl
stripDecl decl =
  case decl of
    DeclValue _ value -> DeclValue noSourceSpan (stripValueDecl value)
    DeclTypeSig _ names t -> DeclTypeSig noSourceSpan names (stripType t)
    DeclFixity _ assoc prec ops -> DeclFixity noSourceSpan assoc prec ops
    DeclTypeSyn _ syn -> DeclTypeSyn noSourceSpan (stripTypeSynDecl syn)
    DeclData _ dat -> DeclData noSourceSpan (stripDataDecl dat)
    DeclNewtype _ nt -> DeclNewtype noSourceSpan (stripNewtypeDecl nt)
    DeclClass _ cls -> DeclClass noSourceSpan (stripClassDecl cls)
    DeclInstance _ inst -> DeclInstance noSourceSpan (stripInstanceDecl inst)
    DeclDefault _ tys -> DeclDefault noSourceSpan (map stripType tys)
    DeclForeign _ foreignDecl -> DeclForeign noSourceSpan (stripForeignDecl foreignDecl)

stripValueDecl :: ValueDecl -> ValueDecl
stripValueDecl valueDecl =
  case valueDecl of
    FunctionBind _ name matches -> FunctionBind noSourceSpan name (map stripMatch matches)
    PatternBind _ pat rhs -> PatternBind noSourceSpan (stripPattern pat) (stripRhs rhs)

stripMatch :: Match -> Match
stripMatch match =
  Match
    { matchSpan = noSourceSpan,
      matchPats = map stripPattern (matchPats match),
      matchRhs = stripRhs (matchRhs match)
    }

stripRhs :: Rhs -> Rhs
stripRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> UnguardedRhs noSourceSpan (stripExpr expr)
    GuardedRhss _ guarded -> GuardedRhss noSourceSpan (map stripGuardedRhs guarded)

stripGuardedRhs :: GuardedRhs -> GuardedRhs
stripGuardedRhs guarded =
  GuardedRhs
    { guardedRhsSpan = noSourceSpan,
      guardedRhsGuards = map stripExpr (guardedRhsGuards guarded),
      guardedRhsBody = stripExpr (guardedRhsBody guarded)
    }

stripPattern :: Pattern -> Pattern
stripPattern pat =
  case pat of
    PVar _ n -> PVar noSourceSpan n
    PWildcard _ -> PWildcard noSourceSpan
    PLit _ lit -> PLit noSourceSpan (stripLiteral lit)
    PQuasiQuote _ q body -> PQuasiQuote noSourceSpan q body
    PTuple _ pats -> PTuple noSourceSpan (map stripPattern pats)
    PList _ pats -> PList noSourceSpan (map stripPattern pats)
    PCon _ name pats -> PCon noSourceSpan name (map stripPattern pats)
    PInfix _ a op b -> PInfix noSourceSpan (stripPattern a) op (stripPattern b)
    PView _ expr inner -> PView noSourceSpan (stripExpr expr) (stripPattern inner)
    PAs _ n inner -> PAs noSourceSpan n (stripPattern inner)
    PStrict _ inner -> PStrict noSourceSpan (stripPattern inner)
    PIrrefutable _ inner -> PIrrefutable noSourceSpan (stripPattern inner)
    PNegLit _ lit -> PNegLit noSourceSpan (stripLiteral lit)
    PParen _ inner -> PParen noSourceSpan (stripPattern inner)
    PRecord _ name fields -> PRecord noSourceSpan name [(field, stripPattern value) | (field, value) <- fields]

stripLiteral :: Literal -> Literal
stripLiteral lit =
  case lit of
    LitInt _ i repr -> LitInt noSourceSpan i repr
    LitIntBase _ i txt -> LitIntBase noSourceSpan i txt
    LitFloat _ d repr -> LitFloat noSourceSpan d repr
    LitChar _ c -> LitChar noSourceSpan c
    LitString _ s -> LitString noSourceSpan s

stripType :: Type -> Type
stripType ty =
  case ty of
    TVar _ n -> TVar noSourceSpan n
    TCon _ n -> TCon noSourceSpan n
    TQuasiQuote _ q body -> TQuasiQuote noSourceSpan q body
    TApp _ a b -> TApp noSourceSpan (stripType a) (stripType b)
    TFun _ a b -> TFun noSourceSpan (stripType a) (stripType b)
    TTuple _ tys -> TTuple noSourceSpan (map stripType tys)
    TList _ t -> TList noSourceSpan (stripType t)
    TParen _ t -> TParen noSourceSpan (stripType t)
    TContext _ constraints t -> TContext noSourceSpan (map stripConstraint constraints) (stripType t)

stripConstraint :: Constraint -> Constraint
stripConstraint c =
  Constraint
    { constraintSpan = noSourceSpan,
      constraintClass = constraintClass c,
      constraintArgs = map stripType (constraintArgs c),
      constraintParen = constraintParen c
    }

stripTypeSynDecl :: TypeSynDecl -> TypeSynDecl
stripTypeSynDecl d =
  TypeSynDecl
    { typeSynSpan = noSourceSpan,
      typeSynName = typeSynName d,
      typeSynParams = typeSynParams d,
      typeSynBody = stripType (typeSynBody d)
    }

stripDataDecl :: DataDecl -> DataDecl
stripDataDecl d =
  DataDecl
    { dataDeclSpan = noSourceSpan,
      dataDeclContext = map stripConstraint (dataDeclContext d),
      dataDeclName = dataDeclName d,
      dataDeclParams = dataDeclParams d,
      dataDeclConstructors = map stripDataConDecl (dataDeclConstructors d),
      dataDeclDeriving = dataDeclDeriving d
    }

stripNewtypeDecl :: NewtypeDecl -> NewtypeDecl
stripNewtypeDecl d =
  NewtypeDecl
    { newtypeDeclSpan = noSourceSpan,
      newtypeDeclContext = map stripConstraint (newtypeDeclContext d),
      newtypeDeclName = newtypeDeclName d,
      newtypeDeclParams = newtypeDeclParams d,
      newtypeDeclConstructor = fmap stripDataConDecl (newtypeDeclConstructor d),
      newtypeDeclDeriving = newtypeDeclDeriving d
    }

stripDataConDecl :: DataConDecl -> DataConDecl
stripDataConDecl con =
  case con of
    PrefixCon _ name args -> PrefixCon noSourceSpan name (map stripBangType args)
    InfixCon _ left op right -> InfixCon noSourceSpan (stripBangType left) op (stripBangType right)
    RecordCon _ name fields -> RecordCon noSourceSpan name (map stripFieldDecl fields)

stripBangType :: BangType -> BangType
stripBangType b =
  BangType
    { bangSpan = noSourceSpan,
      bangStrict = bangStrict b,
      bangType = stripType (bangType b)
    }

stripFieldDecl :: FieldDecl -> FieldDecl
stripFieldDecl f =
  FieldDecl
    { fieldSpan = noSourceSpan,
      fieldNames = fieldNames f,
      fieldType = stripBangType (fieldType f)
    }

stripClassDecl :: ClassDecl -> ClassDecl
stripClassDecl d =
  ClassDecl
    { classDeclSpan = noSourceSpan,
      classDeclContext = map stripConstraint (classDeclContext d),
      classDeclName = classDeclName d,
      classDeclParam = classDeclParam d,
      classDeclItems = map stripClassDeclItem (classDeclItems d)
    }

stripClassDeclItem :: ClassDeclItem -> ClassDeclItem
stripClassDeclItem item =
  case item of
    ClassItemTypeSig _ names t -> ClassItemTypeSig noSourceSpan names (stripType t)
    ClassItemFixity _ assoc prec ops -> ClassItemFixity noSourceSpan assoc prec ops
    ClassItemDefault _ value -> ClassItemDefault noSourceSpan (stripValueDecl value)

stripInstanceDecl :: InstanceDecl -> InstanceDecl
stripInstanceDecl d =
  InstanceDecl
    { instanceDeclSpan = noSourceSpan,
      instanceDeclContext = map stripConstraint (instanceDeclContext d),
      instanceDeclClassName = instanceDeclClassName d,
      instanceDeclTypes = map stripType (instanceDeclTypes d),
      instanceDeclItems = map stripInstanceDeclItem (instanceDeclItems d)
    }

stripInstanceDeclItem :: InstanceDeclItem -> InstanceDeclItem
stripInstanceDeclItem item =
  case item of
    InstanceItemBind _ value -> InstanceItemBind noSourceSpan (stripValueDecl value)
    InstanceItemTypeSig _ names t -> InstanceItemTypeSig noSourceSpan names (stripType t)
    InstanceItemFixity _ assoc prec ops -> InstanceItemFixity noSourceSpan assoc prec ops

stripForeignDecl :: ForeignDecl -> ForeignDecl
stripForeignDecl d =
  ForeignDecl
    { foreignDeclSpan = noSourceSpan,
      foreignDirection = foreignDirection d,
      foreignCallConv = foreignCallConv d,
      foreignSafety = foreignSafety d,
      foreignEntity = foreignEntity d,
      foreignName = foreignName d,
      foreignType = stripType (foreignType d)
    }

stripCaseAlt :: CaseAlt -> CaseAlt
stripCaseAlt alt =
  CaseAlt
    { caseAltSpan = noSourceSpan,
      caseAltPattern = stripPattern (caseAltPattern alt),
      caseAltRhs = stripRhs (caseAltRhs alt)
    }

stripDoStmt :: DoStmt -> DoStmt
stripDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> DoBind noSourceSpan (stripPattern pat) (stripExpr expr)
    DoLet _ binds -> DoLet noSourceSpan [(name, stripExpr e) | (name, e) <- binds]
    DoLetDecls _ decls -> DoLetDecls noSourceSpan (map stripDecl decls)
    DoExpr _ expr -> DoExpr noSourceSpan (stripExpr expr)

stripCompStmt :: CompStmt -> CompStmt
stripCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> CompGen noSourceSpan (stripPattern pat) (stripExpr expr)
    CompGuard _ expr -> CompGuard noSourceSpan (stripExpr expr)
    CompLet _ binds -> CompLet noSourceSpan [(name, stripExpr e) | (name, e) <- binds]
    CompLetDecls _ decls -> CompLetDecls noSourceSpan (map stripDecl decls)

stripArithSeq :: ArithSeq -> ArithSeq
stripArithSeq seqExpr =
  case seqExpr of
    ArithSeqFrom _ a -> ArithSeqFrom noSourceSpan (stripExpr a)
    ArithSeqFromThen _ a b -> ArithSeqFromThen noSourceSpan (stripExpr a) (stripExpr b)
    ArithSeqFromTo _ a b -> ArithSeqFromTo noSourceSpan (stripExpr a) (stripExpr b)
    ArithSeqFromThenTo _ a b c -> ArithSeqFromThenTo noSourceSpan (stripExpr a) (stripExpr b) (stripExpr c)

mapConcurrentlyChunksWithProgress :: Int -> (a -> IO PackageResult) -> [a] -> Int -> IO [PackageResult]
mapConcurrentlyChunksWithProgress n action items total =
  go 0 0 [] (chunksOf chunkSize items)
  where
    chunkSize = if n <= 0 then 1 else n
    go _ _ acc [] = pure (concat (reverse acc))
    go done success acc (chunk : rest) = do
      batch <- mapConcurrently action chunk
      let done' = done + length batch
          success' = success + length [() | result <- batch, packageOk result]
      putProgressLine (ProgressState done' success' total)
      go done' success' (batch : acc) rest

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunksOf n rest

data ProgressState = ProgressState
  { progressDone :: Int,
    progressSuccess :: Int,
    progressTotal :: Int
  }

putProgressLine :: ProgressState -> IO ()
putProgressLine p =
  do
    putStr
      ( "\r"
          ++ show (progressSuccess p)
          ++ "/"
          ++ show (progressTotal p)
          ++ " ("
          ++ show (progressDone p)
          ++ "/"
          ++ show (progressTotal p)
          ++ " processed)"
      )
    hFlush stdout
