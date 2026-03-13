{-# LANGUAGE OverloadedStrings #-}

module ParserGolden
  ( CaseKind (..),
    ExpectedStatus (..),
    Outcome (..),
    ParserCase (..),
    fixtureRoot,
    exprFixtureRoot,
    moduleFixtureRoot,
    loadExprCases,
    loadModuleCases,
    parseParserCaseText,
    evaluateExprCase,
    evaluateModuleCase,
    renderExprAst,
    renderModuleAst,
    progressSummary,
  )
where

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Parser
  ( defaultConfig,
    errorBundlePretty,
    parseExpr,
    parseModule,
  )
import Parser.Ast
import Parser.Types (ParseResult (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

data CaseKind = CaseExpr | CaseModule deriving (Eq, Show)

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

data ParserCase = ParserCase
  { caseKind :: !CaseKind,
    caseId :: !String,
    caseCategory :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Text],
    caseInput :: !Text,
    caseAst :: !String,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/golden"

exprFixtureRoot :: FilePath
exprFixtureRoot = fixtureRoot </> "expr"

moduleFixtureRoot :: FilePath
moduleFixtureRoot = fixtureRoot </> "module"

loadExprCases :: IO [ParserCase]
loadExprCases = loadCases CaseExpr exprFixtureRoot

loadModuleCases :: IO [ParserCase]
loadModuleCases = loadCases CaseModule moduleFixtureRoot

loadCases :: CaseKind -> FilePath -> IO [ParserCase]
loadCases kind root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles root
      mapM (loadParserCase kind) paths

loadParserCase :: CaseKind -> FilePath -> IO ParserCase
loadParserCase kind path = do
  source <- TIO.readFile path
  case parseParserCaseText kind path source of
    Left err -> fail err
    Right parsed -> pure parsed

parseParserCaseText :: CaseKind -> FilePath -> Text -> Either String ParserCase
parseParserCaseText kind path source = do
  value <-
    case Y.decodeEither' (encodeUtf8 source) of
      Left err -> Left ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputText, astText, statusText, reasonText) <- parseYamlFixture path value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  reason <- validateReason path status (T.unpack reasonText)
  ast <- validateAst path status (T.unpack astText)
  let relPath = dropRootPrefix path
      category = categoryFromPath relPath
  pure
    ParserCase
      { caseKind = kind,
        caseId = relPath,
        caseCategory = category,
        casePath = relPath,
        caseExtensions = exts,
        caseInput = inputText,
        caseAst = ast,
        caseStatus = status,
        caseReason = reason
      }

parseYamlFixture :: FilePath -> Y.Value -> Either String ([Text], Text, Text, Text, Text)
parseYamlFixture path value =
  case parseEither
    ( withObject "parser fixture" $ \obj -> do
        exts <- obj .: "extensions"
        inputText <- obj .: "input"
        astText <- obj .:? "ast" .!= ""
        statusText <- obj .: "status"
        reasonText <- obj .:? "reason" .!= ""
        pure (exts, inputText, astText, statusText, reasonText)
    )
    value of
    Left err -> Left ("Invalid parser fixture schema in " <> path <> ": " <> err)
    Right parsed -> Right parsed

evaluateExprCase :: ParserCase -> (Outcome, String)
evaluateExprCase meta =
  case parseExpr defaultConfig (caseInput meta) of
    ParseOk ast -> classifySuccess meta (renderExprAst ast)
    ParseErr err -> classifyFailure meta (errorBundlePretty err)

evaluateModuleCase :: ParserCase -> (Outcome, String)
evaluateModuleCase meta =
  case parseModule defaultConfig (caseInput meta) of
    ParseOk ast -> classifySuccess meta (renderModuleAst ast)
    ParseErr err -> classifyFailure meta (errorBundlePretty err)

classifySuccess :: ParserCase -> String -> (Outcome, String)
classifySuccess meta actualAst =
  case caseStatus meta of
    StatusPass
      | actualAst == caseAst meta -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "AST mismatch (expected=" <> show (caseAst meta) <> ", actual=" <> show actualAst <> ")"
          )
    StatusFail ->
      ( OutcomeFail,
        "expected parse failure but parser succeeded with AST=" <> actualAst
      )
    StatusXFail ->
      (OutcomeFail, "expected xfail (known failing bug), but parser succeeded")
    StatusXPass
      | actualAst == caseAst meta -> (OutcomeXPass, "known bug still passes unexpectedly")
      | otherwise ->
          ( OutcomeFail,
            "expected xpass AST match but got AST=" <> actualAst
          )

classifyFailure :: ParserCase -> String -> (Outcome, String)
classifyFailure meta errDetails =
  case caseStatus meta of
    StatusPass ->
      ( OutcomeFail,
        "expected parse success, got parse error: " <> errDetails
      )
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass ->
      ( OutcomeFail,
        "expected xpass (known passing bug), got parse error: " <> errDetails
      )

progressSummary :: [(ParserCase, Outcome, String)] -> (Int, Int, Int, Int)
progressSummary outcomes =
  ( count OutcomePass,
    count OutcomeXFail,
    count OutcomeXPass,
    count OutcomeFail
  )
  where
    count wanted = length [() | (_, out, _) <- outcomes, out == wanted]

renderExprAst :: Expr -> String
renderExprAst expr =
  case expr of
    EVar _ name -> "EVar " <> show name
    EInt _ value _raw -> "EInt " <> show value
    EIntBase _ value raw -> "EIntBase " <> show value <> " " <> show raw
    EFloat _ value raw -> "EFloat " <> show value <> " " <> show raw
    EChar _ value -> "EChar " <> show value
    EString _ value -> "EString " <> show value
    EQuasiQuote _ quoter body -> "EQuasiQuote " <> show quoter <> " " <> show body
    EIf _ cond yes no -> "EIf " <> par (renderExprAst cond) <> " " <> par (renderExprAst yes) <> " " <> par (renderExprAst no)
    ELambdaPats _ pats body -> "ELambdaPats " <> showListWith renderPattern pats <> " " <> par (renderExprAst body)
    ELambdaCase _ alts -> "ELambdaCase " <> showListWith renderCaseAlt alts
    EInfix _ lhs op rhs -> "EInfix " <> par (renderExprAst lhs) <> " " <> show op <> " " <> par (renderExprAst rhs)
    ENegate _ sub -> "ENegate " <> par (renderExprAst sub)
    ESectionL _ lhs op -> "ESectionL " <> par (renderExprAst lhs) <> " " <> show op
    ESectionR _ op rhs -> "ESectionR " <> show op <> " " <> par (renderExprAst rhs)
    ELetDecls _ decls body -> "ELetDecls " <> showListWith renderDecl decls <> " " <> par (renderExprAst body)
    ECase _ scrut alts -> "ECase " <> par (renderExprAst scrut) <> " " <> showListWith renderCaseAlt alts
    EDo _ stmts -> "EDo " <> showListWith renderDoStmt stmts
    EListComp _ body stmts -> "EListComp " <> par (renderExprAst body) <> " " <> showListWith renderCompStmt stmts
    EListCompParallel _ body stmtGroups -> "EListCompParallel " <> par (renderExprAst body) <> " " <> showListWith (showListWith renderCompStmt) stmtGroups
    EArithSeq _ seqAst -> "EArithSeq " <> par (renderArithSeq seqAst)
    ERecordCon _ name fields -> "ERecordCon " <> show name <> " " <> showListWith (renderField renderExprAst) fields
    ERecordUpd _ rec fields -> "ERecordUpd " <> par (renderExprAst rec) <> " " <> showListWith (renderField renderExprAst) fields
    ETypeSig _ sub ty -> "ETypeSig " <> par (renderExprAst sub) <> " " <> par (renderType ty)
    EParen _ sub -> "EParen " <> par (renderExprAst sub)
    EWhereDecls _ body decls -> "EWhereDecls " <> par (renderExprAst body) <> " " <> showListWith renderDecl decls
    EList _ items -> "EList " <> showListWith renderExprAst items
    ETuple _ items -> "ETuple " <> showListWith renderExprAst items
    ETupleCon _ n -> "ETupleCon " <> show n
    ETypeApp _ fn ty -> "ETypeApp " <> par (renderExprAst fn) <> " " <> par (renderType ty)
    EApp _ fn arg -> "EApp " <> par (renderExprAst fn) <> " " <> par (renderExprAst arg)

renderModuleAst :: Module -> String
renderModuleAst modu =
  "Module {name = "
    <> show (moduleName modu)
    <> ", languagePragmas = "
    <> show (moduleLanguagePragmas modu)
    <> ", exports = "
    <> renderMaybe (showListWith renderExportSpec) (moduleExports modu)
    <> ", imports = "
    <> showListWith renderImportDecl (moduleImports modu)
    <> ", decls = "
    <> showListWith renderDecl (moduleDecls modu)
    <> "}"

renderExportSpec :: ExportSpec -> String
renderExportSpec spec =
  case spec of
    ExportModule _ name -> "ExportModule " <> show name
    ExportVar _ name -> "ExportVar " <> show name
    ExportAbs _ name -> "ExportAbs " <> show name
    ExportAll _ name -> "ExportAll " <> show name
    ExportWith _ name names -> "ExportWith " <> show name <> " " <> show names

renderImportDecl :: ImportDecl -> String
renderImportDecl decl =
  "ImportDecl {package = "
    <> show (importDeclPackage decl)
    <> ", qualified = "
    <> show (importDeclQualified decl)
    <> ", qualifiedPost = "
    <> show (importDeclQualifiedPost decl)
    <> ", module = "
    <> show (importDeclModule decl)
    <> ", as = "
    <> show (importDeclAs decl)
    <> ", spec = "
    <> renderMaybe renderImportSpec (importDeclSpec decl)
    <> "}"

renderImportSpec :: ImportSpec -> String
renderImportSpec spec =
  "ImportSpec {hiding = "
    <> show (importSpecHiding spec)
    <> ", items = "
    <> showListWith renderImportItem (importSpecItems spec)
    <> "}"

renderImportItem :: ImportItem -> String
renderImportItem item =
  case item of
    ImportItemVar _ name -> "ImportItemVar " <> show name
    ImportItemAbs _ name -> "ImportItemAbs " <> show name
    ImportItemAll _ name -> "ImportItemAll " <> show name
    ImportItemWith _ name names -> "ImportItemWith " <> show name <> " " <> show names

renderDecl :: Decl -> String
renderDecl decl =
  case decl of
    DeclValue _ valueDecl -> "DeclValue " <> par (renderValueDecl valueDecl)
    DeclTypeSig _ names ty -> "DeclTypeSig " <> show names <> " " <> par (renderType ty)
    DeclFixity _ assoc prec ops -> "DeclFixity " <> show assoc <> " " <> show prec <> " " <> show ops
    DeclTypeSyn _ syn -> "DeclTypeSyn " <> par (renderTypeSynDecl syn)
    DeclData _ dat -> "DeclData " <> par (renderDataDecl dat)
    DeclNewtype _ nt -> "DeclNewtype " <> par (renderNewtypeDecl nt)
    DeclClass _ cls -> "DeclClass " <> par (renderClassDecl cls)
    DeclInstance _ inst -> "DeclInstance " <> par (renderInstanceDecl inst)
    DeclDefault _ tys -> "DeclDefault " <> showListWith renderType tys
    DeclForeign _ foreignDecl -> "DeclForeign " <> par (renderForeignDecl foreignDecl)

renderValueDecl :: ValueDecl -> String
renderValueDecl vdecl =
  case vdecl of
    FunctionBind _ name matches -> "FunctionBind " <> show name <> " " <> showListWith renderMatch matches
    PatternBind _ pat rhs -> "PatternBind " <> par (renderPattern pat) <> " " <> par (renderRhs rhs)

renderMatch :: Match -> String
renderMatch m =
  "Match {pats = "
    <> showListWith renderPattern (matchPats m)
    <> ", rhs = "
    <> renderRhs (matchRhs m)
    <> "}"

renderRhs :: Rhs -> String
renderRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> "UnguardedRhs " <> par (renderExprAst expr)
    GuardedRhss _ rhss -> "GuardedRhss " <> showListWith renderGuardedRhs rhss

renderGuardedRhs :: GuardedRhs -> String
renderGuardedRhs grhs =
  "GuardedRhs {guards = "
    <> showListWith renderExprAst (guardedRhsGuards grhs)
    <> ", body = "
    <> renderExprAst (guardedRhsBody grhs)
    <> "}"

renderLiteral :: Literal -> String
renderLiteral lit =
  case lit of
    LitInt _ value _raw -> "LitInt " <> show value
    LitIntBase _ value raw -> "LitIntBase " <> show value <> " " <> show raw
    LitFloat _ value raw -> "LitFloat " <> show value <> " " <> show raw
    LitChar _ value -> "LitChar " <> show value
    LitString _ value -> "LitString " <> show value

renderPattern :: Pattern -> String
renderPattern pat =
  case pat of
    PVar _ name -> "PVar " <> show name
    PWildcard _ -> "PWildcard"
    PLit _ lit -> "PLit " <> par (renderLiteral lit)
    PQuasiQuote _ quoter body -> "PQuasiQuote " <> show quoter <> " " <> show body
    PTuple _ items -> "PTuple " <> showListWith renderPattern items
    PList _ items -> "PList " <> showListWith renderPattern items
    PCon _ name pats -> "PCon " <> show name <> " " <> showListWith renderPattern pats
    PInfix _ lhs op rhs -> "PInfix " <> par (renderPattern lhs) <> " " <> show op <> " " <> par (renderPattern rhs)
    PView _ expr sub -> "PView " <> par (renderExprAst expr) <> " " <> par (renderPattern sub)
    PAs _ name sub -> "PAs " <> show name <> " " <> par (renderPattern sub)
    PStrict _ sub -> "PStrict " <> par (renderPattern sub)
    PIrrefutable _ sub -> "PIrrefutable " <> par (renderPattern sub)
    PNegLit _ lit -> "PNegLit " <> par (renderLiteral lit)
    PParen _ sub -> "PParen " <> par (renderPattern sub)
    PRecord _ name fields -> "PRecord " <> show name <> " " <> showListWith (renderField renderPattern) fields

renderType :: Type -> String
renderType ty =
  case ty of
    TVar _ name -> "TVar " <> show name
    TCon _ name -> "TCon " <> show name
    TQuasiQuote _ quoter body -> "TQuasiQuote " <> show quoter <> " " <> show body
    TForall _ names body -> "TForall " <> show names <> " " <> par (renderType body)
    TApp _ fn arg -> "TApp " <> par (renderType fn) <> " " <> par (renderType arg)
    TFun _ from to -> "TFun " <> par (renderType from) <> " " <> par (renderType to)
    TTuple _ items -> "TTuple " <> showListWith renderType items
    TList _ sub -> "TList " <> par (renderType sub)
    TParen _ sub -> "TParen " <> par (renderType sub)
    TContext _ constraints body -> "TContext " <> showListWith renderConstraint constraints <> " " <> par (renderType body)

renderConstraint :: Constraint -> String
renderConstraint c =
  "Constraint {class = "
    <> show (constraintClass c)
    <> ", args = "
    <> showListWith renderType (constraintArgs c)
    <> ", paren = "
    <> show (constraintParen c)
    <> "}"

renderTypeSynDecl :: TypeSynDecl -> String
renderTypeSynDecl syn =
  "TypeSynDecl {name = "
    <> show (typeSynName syn)
    <> ", params = "
    <> show (typeSynParams syn)
    <> ", body = "
    <> renderType (typeSynBody syn)
    <> "}"

renderDataDecl :: DataDecl -> String
renderDataDecl dat =
  "DataDecl {context = "
    <> showListWith renderConstraint (dataDeclContext dat)
    <> ", name = "
    <> show (dataDeclName dat)
    <> ", params = "
    <> show (dataDeclParams dat)
    <> ", constructors = "
    <> showListWith renderDataConDecl (dataDeclConstructors dat)
    <> ", deriving = "
    <> renderMaybe renderDerivingClause (dataDeclDeriving dat)
    <> "}"

renderNewtypeDecl :: NewtypeDecl -> String
renderNewtypeDecl dat =
  "NewtypeDecl {context = "
    <> showListWith renderConstraint (newtypeDeclContext dat)
    <> ", name = "
    <> show (newtypeDeclName dat)
    <> ", params = "
    <> show (newtypeDeclParams dat)
    <> ", constructor = "
    <> renderMaybe renderDataConDecl (newtypeDeclConstructor dat)
    <> ", deriving = "
    <> renderMaybe renderDerivingClause (newtypeDeclDeriving dat)
    <> "}"

renderDataConDecl :: DataConDecl -> String
renderDataConDecl con =
  case con of
    PrefixCon _ name tys -> "PrefixCon " <> show name <> " " <> showListWith renderBangType tys
    InfixCon _ lhs name rhs -> "InfixCon " <> renderBangType lhs <> " " <> show name <> " " <> renderBangType rhs
    RecordCon _ name fields -> "RecordCon " <> show name <> " " <> showListWith renderFieldDecl fields

renderBangType :: BangType -> String
renderBangType bt =
  "BangType {strict = " <> show (bangStrict bt) <> ", type = " <> renderType (bangType bt) <> "}"

renderFieldDecl :: FieldDecl -> String
renderFieldDecl fd =
  "FieldDecl {names = " <> show (fieldNames fd) <> ", type = " <> renderBangType (fieldType fd) <> "}"

renderDerivingClause :: DerivingClause -> String
renderDerivingClause (DerivingClause classes) =
  "DerivingClause " <> show classes

renderClassDecl :: ClassDecl -> String
renderClassDecl decl =
  "ClassDecl {context = "
    <> showListWith renderConstraint (classDeclContext decl)
    <> ", name = "
    <> show (classDeclName decl)
    <> ", param = "
    <> show (classDeclParam decl)
    <> ", items = "
    <> showListWith renderClassDeclItem (classDeclItems decl)
    <> "}"

renderClassDeclItem :: ClassDeclItem -> String
renderClassDeclItem item =
  case item of
    ClassItemTypeSig _ names ty -> "ClassItemTypeSig " <> show names <> " " <> par (renderType ty)
    ClassItemFixity _ assoc prec ops -> "ClassItemFixity " <> show assoc <> " " <> show prec <> " " <> show ops
    ClassItemDefault _ decl -> "ClassItemDefault " <> renderValueDecl decl

renderInstanceDecl :: InstanceDecl -> String
renderInstanceDecl decl =
  "InstanceDecl {context = "
    <> showListWith renderConstraint (instanceDeclContext decl)
    <> ", class = "
    <> show (instanceDeclClassName decl)
    <> ", types = "
    <> showListWith renderType (instanceDeclTypes decl)
    <> ", items = "
    <> showListWith renderInstanceDeclItem (instanceDeclItems decl)
    <> "}"

renderInstanceDeclItem :: InstanceDeclItem -> String
renderInstanceDeclItem item =
  case item of
    InstanceItemBind _ decl -> "InstanceItemBind " <> renderValueDecl decl
    InstanceItemTypeSig _ names ty -> "InstanceItemTypeSig " <> show names <> " " <> par (renderType ty)
    InstanceItemFixity _ assoc prec ops -> "InstanceItemFixity " <> show assoc <> " " <> show prec <> " " <> show ops

renderForeignDecl :: ForeignDecl -> String
renderForeignDecl decl =
  "ForeignDecl {direction = "
    <> show (foreignDirection decl)
    <> ", callConv = "
    <> show (foreignCallConv decl)
    <> ", safety = "
    <> show (foreignSafety decl)
    <> ", entity = "
    <> renderForeignEntitySpec (foreignEntity decl)
    <> ", name = "
    <> show (foreignName decl)
    <> ", type = "
    <> renderType (foreignType decl)
    <> "}"

renderForeignEntitySpec :: ForeignEntitySpec -> String
renderForeignEntitySpec spec =
  case spec of
    ForeignEntityDynamic -> "ForeignEntityDynamic"
    ForeignEntityWrapper -> "ForeignEntityWrapper"
    ForeignEntityStatic header -> "ForeignEntityStatic " <> show header
    ForeignEntityAddress header -> "ForeignEntityAddress " <> show header
    ForeignEntityNamed name -> "ForeignEntityNamed " <> show name
    ForeignEntityOmitted -> "ForeignEntityOmitted"

renderCaseAlt :: CaseAlt -> String
renderCaseAlt alt =
  "CaseAlt {pattern = "
    <> renderPattern (caseAltPattern alt)
    <> ", rhs = "
    <> renderRhs (caseAltRhs alt)
    <> "}"

renderDoStmt :: DoStmt -> String
renderDoStmt stmt =
  case stmt of
    DoBind _ pat expr -> "DoBind " <> par (renderPattern pat) <> " " <> par (renderExprAst expr)
    DoLet _ binds -> "DoLet " <> showListWith (renderField renderExprAst) binds
    DoLetDecls _ decls -> "DoLetDecls " <> showListWith renderDecl decls
    DoExpr _ expr -> "DoExpr " <> par (renderExprAst expr)

renderCompStmt :: CompStmt -> String
renderCompStmt stmt =
  case stmt of
    CompGen _ pat expr -> "CompGen " <> par (renderPattern pat) <> " " <> par (renderExprAst expr)
    CompGuard _ expr -> "CompGuard " <> par (renderExprAst expr)
    CompLet _ binds -> "CompLet " <> showListWith (renderField renderExprAst) binds
    CompLetDecls _ decls -> "CompLetDecls " <> showListWith renderDecl decls

renderArithSeq :: ArithSeq -> String
renderArithSeq seqAst =
  case seqAst of
    ArithSeqFrom _ from -> "ArithSeqFrom " <> par (renderExprAst from)
    ArithSeqFromThen _ from thenExpr -> "ArithSeqFromThen " <> par (renderExprAst from) <> " " <> par (renderExprAst thenExpr)
    ArithSeqFromTo _ from toExpr -> "ArithSeqFromTo " <> par (renderExprAst from) <> " " <> par (renderExprAst toExpr)
    ArithSeqFromThenTo _ from thenExpr toExpr -> "ArithSeqFromThenTo " <> par (renderExprAst from) <> " " <> par (renderExprAst thenExpr) <> " " <> par (renderExprAst toExpr)

renderField :: (a -> String) -> (Text, a) -> String
renderField renderValue (name, value) = "(" <> show name <> ", " <> renderValue value <> ")"

renderMaybe :: (a -> String) -> Maybe a -> String
renderMaybe renderValue value =
  case value of
    Nothing -> "Nothing"
    Just a -> "Just " <> par (renderValue a)

showListWith :: (a -> String) -> [a] -> String
showListWith renderOne xs = "[" <> T.unpack (T.intercalate ", " (map (T.pack . renderOne) xs)) <> "]"

par :: String -> String
par x = "(" <> x <> ")"

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

validateExtensions :: FilePath -> [Text] -> Either String [Text]
validateExtensions path names =
  if null names
    then Right []
    else Left ("Parser golden fixtures currently require [extensions] to be empty in " <> path)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid [status] in " <> path <> ": " <> T.unpack raw)

validateReason :: FilePath -> ExpectedStatus -> String -> Either String String
validateReason path status reason =
  let trimmed = trim reason
   in case status of
        StatusXFail | null trimmed -> Left ("[reason] is required for xfail status in " <> path)
        StatusXPass | null trimmed -> Left ("[reason] is required for xpass status in " <> path)
        _ -> Right trimmed

validateAst :: FilePath -> ExpectedStatus -> String -> Either String String
validateAst path status ast =
  let trimmed = trim ast
   in case status of
        StatusPass | null trimmed -> Left ("[ast] is required for pass status in " <> path)
        StatusXPass | null trimmed -> Left ("[ast] is required for xpass status in " <> path)
        _ -> Right trimmed

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

categoryFromPath :: FilePath -> String
categoryFromPath path =
  case takeDirectory path of
    "." -> "golden"
    dir -> dir

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
