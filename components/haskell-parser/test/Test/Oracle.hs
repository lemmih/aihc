{-# LANGUAGE OverloadedStrings #-}

module Test.Oracle
  ( oracleCanonicalModule,
    oracleParsesModule,
  )
where

import Data.Bifunctor (first)
import Data.Char (isDigit, isUpper)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import qualified GHC.Data.FastString as FastString
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (ForeignFunctionInterface))
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.ForeignCall
  ( CCallConv (CCallConv, StdCallConv),
    CCallTarget (DynamicTarget, StaticTarget),
    CExportSpec (CExportStatic),
    Header,
    Safety (PlayInterruptible, PlayRisky, PlaySafe),
  )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..))
import GHC.Types.SrcLoc (GenLocated, mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Parser.Canonical
import Text.Read (readMaybe)

oracleParsesModule :: Text -> Bool
oracleParsesModule input =
  case parseWithGhc input of
    Left _ -> False
    Right _ -> True

oracleCanonicalModule :: Text -> Either Text CanonicalModule
oracleCanonicalModule input = do
  parsed <- parseWithGhc input
  first T.pack (toCanonicalModule parsed)

parseWithGhc :: Text -> Either Text (HsModule GhcPs)
parseWithGhc input =
  let exts = EnumSet.fromList [ForeignFunctionInterface] :: EnumSet.EnumSet Extension
      opts = mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<oracle>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

toCanonicalModule :: HsModule GhcPs -> Either String CanonicalModule
toCanonicalModule modu = do
  declGroups <- traverse toCanonicalDecls (hsmodDecls modu)
  pure
    CanonicalModule
      { canonicalModuleName = fmap (T.pack . moduleNameString . unLoc) (hsmodName modu),
        canonicalDecls = concat declGroups
      }

toCanonicalDecls :: LHsDecl GhcPs -> Either String [CanonicalDecl]
toCanonicalDecls locatedDecl =
  case unLoc locatedDecl of
    ValD _ bind -> (: []) <$> toCanonicalValueDecl bind
    SigD _ sig -> toCanonicalSigDecls sig
    TyClD _ tyClDecl -> (: []) <$> toCanonicalTyClDecl tyClDecl
    InstD _ instDecl -> (: []) <$> toCanonicalInstanceDecl instDecl
    DefD _ defDecl -> (: []) <$> toCanonicalDefaultDecl defDecl
    ForD _ foreignDecl -> (: []) <$> toCanonicalForeignDecl foreignDecl
    _ -> Left "unsupported declaration kind"

toCanonicalValueDecl :: HsBind GhcPs -> Either String CanonicalDecl
toCanonicalValueDecl bind =
  case bind of
    FunBind {fun_id = locatedName, fun_matches = MG {mg_alts = locatedMatches}} ->
      let name = occNameText (unLoc locatedName)
          matches = unLoc locatedMatches
          maybeExpr = do
            singleMatch <- case matches of
              [m] -> Just (unLoc m)
              _ -> Nothing
            toSimpleBody singleMatch
       in pure $
            case maybeExpr of
              Just expr ->
                CanonicalValueDecl
                  { canonicalDeclName = name,
                    canonicalDeclExpr = expr
                  }
              Nothing ->
                CanonicalFunctionDecl
                  { canonicalFunctionName = name
                  }
    PatBind {pat_lhs = locatedPat} ->
      Right
        CanonicalPatternDecl
          { canonicalPatternLhs = normalizeSpaceText (renderLocated locatedPat)
          }
    _ -> Left "unsupported value binding"
  where
    toSimpleBody match =
      case m_grhss match of
        GRHSs _ grhss _ ->
          case toList grhss of
            [grhs] ->
              case unLoc grhs of
                GRHS _ [] body ->
                  case toCanonicalExpr (unLoc body) of
                    Right expr -> Just expr
                    Left _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        XGRHSs _ -> Nothing

toCanonicalSigDecls :: Sig GhcPs -> Either String [CanonicalDecl]
toCanonicalSigDecls sig =
  case sig of
    TypeSig _ locatedNames _ ->
      pure
        [ CanonicalTypeSigDecl
            { canonicalTypeSigName = occNameText (unLoc locatedName)
            }
        | locatedName <- locatedNames
        ]
    FixSig _ (FixitySig _ locatedNames fixity) ->
      let assoc = fixityAssocText fixity
          precedence = fixityPrecedenceValue fixity
       in pure
            [ CanonicalFixityDecl
                { canonicalFixityAssoc = assoc,
                  canonicalFixityPrecedence = precedence,
                  canonicalFixityOperator = occNameText (unLoc locatedName)
                }
            | locatedName <- locatedNames
            ]
    _ -> Left "unsupported signature declaration"

toCanonicalTyClDecl :: TyClDecl GhcPs -> Either String CanonicalDecl
toCanonicalTyClDecl tyClDecl =
  case tyClDecl of
    SynDecl {tcdLName = locatedName} ->
      Right
        CanonicalTypeDecl
          { canonicalTypeDeclName = occNameText (unLoc locatedName)
          }
    DataDecl {tcdLName = locatedName, tcdDataDefn = dataDefn} ->
      let typeName = occNameText (unLoc locatedName)
          renderedDecl = renderPrinted dataDefn
          constructors = constructorsFromRendered renderedDecl
       in if "newtype " `T.isPrefixOf` T.strip renderedDecl
            then
              Right
                CanonicalNewtypeDecl
                  { canonicalNewtypeName = typeName,
                    canonicalNewtypeConstructor =
                      case constructors of
                        [] -> Nothing
                        (ctor : _) -> Just ctor
                  }
            else
              Right
                CanonicalDataDecl
                  { canonicalTypeName = typeName,
                    canonicalConstructors = constructors
                  }
    ClassDecl {tcdLName = locatedName} ->
      Right
        CanonicalClassDecl
          { canonicalClassName = occNameText (unLoc locatedName)
          }
    _ -> Left "unsupported type/class declaration"

toCanonicalInstanceDecl :: InstDecl GhcPs -> Either String CanonicalDecl
toCanonicalInstanceDecl instDecl =
  case instDecl of
    ClsInstD _ clsInst ->
      let rendered = renderPrinted clsInst
       in Right
            CanonicalInstanceDecl
              { canonicalInstanceClassName = classNameFromRendered rendered
              }
    _ -> Left "unsupported instance declaration"

toCanonicalDefaultDecl :: DefaultDecl GhcPs -> Either String CanonicalDecl
toCanonicalDefaultDecl defDecl =
  case defDecl of
    DefaultDecl _ _ tys ->
      Right
        CanonicalDefaultDecl
          { canonicalDefaultTypes = map (normalizeTypeToken . renderLocated) (toList tys)
          }

toCanonicalForeignDecl :: ForeignDecl GhcPs -> Either String CanonicalDecl
toCanonicalForeignDecl foreignDecl =
  case foreignDecl of
    ForeignImport {fd_name = locatedName, fd_fi = fi} -> do
      (callConv, safety, entity) <- toCanonicalForeignImport fi
      pure
        CanonicalForeignDecl
          { canonicalForeignDirection = CanonicalForeignImport,
            canonicalForeignCallConv = callConv,
            canonicalForeignSafety = safety,
            canonicalForeignEntity = entity,
            canonicalForeignName = occNameText (unLoc locatedName)
          }
    ForeignExport {fd_name = locatedName, fd_fe = fe} -> do
      (callConv, entity) <- toCanonicalForeignExport (occNameText (unLoc locatedName)) fe
      pure
        CanonicalForeignDecl
          { canonicalForeignDirection = CanonicalForeignExport,
            canonicalForeignCallConv = callConv,
            canonicalForeignSafety = Nothing,
            canonicalForeignEntity = entity,
            canonicalForeignName = occNameText (unLoc locatedName)
          }

toCanonicalForeignImport :: ForeignImport GhcPs -> Either String (CanonicalCallConv, Maybe CanonicalForeignSafety, Maybe Text)
toCanonicalForeignImport fi =
  case fi of
    CImport _ locatedConv locatedSafety header importSpec -> do
      callConv <- toCanonicalCallConv (unLoc locatedConv)
      safety <- toCanonicalSafety (unLoc locatedSafety)
      pure (callConv, safety, classifyImportEntity header importSpec)

toCanonicalForeignExport :: Text -> ForeignExport GhcPs -> Either String (CanonicalCallConv, Maybe Text)
toCanonicalForeignExport haskellName fe =
  case fe of
    CExport _ locatedSpec ->
      case unLoc locatedSpec of
        CExportStatic _ exportedName callConv -> do
          canonConv <- toCanonicalCallConv callConv
          let entity
                | T.pack (show exportedName) == haskellName = Nothing
                | otherwise = Just "named"
          pure (canonConv, entity)

toCanonicalCallConv :: CCallConv -> Either String CanonicalCallConv
toCanonicalCallConv callConv =
  case callConv of
    CCallConv -> Right CanonicalCCall
    StdCallConv -> Right CanonicalStdCall
    _ -> Left "unsupported calling convention"

toCanonicalSafety :: Safety -> Either String (Maybe CanonicalForeignSafety)
toCanonicalSafety safety =
  case safety of
    PlaySafe -> Right (Just CanonicalSafe)
    PlayRisky -> Right (Just CanonicalUnsafe)
    PlayInterruptible -> Right (Just CanonicalSafe)

classifyImportEntity :: Maybe Header -> CImportSpec -> Maybe Text
classifyImportEntity mHeader importSpec =
  case importSpec of
    CLabel _ -> Just "address"
    CWrapper -> Just "wrapper"
    CFunction DynamicTarget -> Just "dynamic"
    CFunction (StaticTarget _ _ _ isFunction)
      | not isFunction -> Just "address"
      | hasHeader mHeader -> Just "static"
      | otherwise -> Just "named"
  where
    hasHeader = isJust

occNameText :: RdrName -> Text
occNameText = T.pack . occNameString . rdrNameOcc

constructorsFromRendered :: Text -> [Text]
constructorsFromRendered rendered =
  let (_, afterEq) = T.breakOn "=" rendered
      rhs = if T.null afterEq then "" else T.drop 1 afterEq
      rhsCore = T.strip (fst (T.breakOn " deriving" rhs))
      parts = filter (not . T.null) (map (T.strip . fst . T.breakOn "{") (T.splitOn "|" rhsCore))
   in mapMaybe constructorName parts

constructorName :: Text -> Maybe Text
constructorName part =
  let tokens = map normalizeTypeToken (T.words (T.replace "!" "" part))
      symbolic = filter isSymbolicToken tokens
   in case tokens of
        [] -> Nothing
        (firstToken : _)
          | isClassToken firstToken -> Just firstToken
          | otherwise ->
              case symbolic of
                (op : _) -> Just op
                [] -> Nothing

fixityAssocText :: Fixity -> Text
fixityAssocText fixity =
  case T.words (renderPrinted fixity) of
    (assoc : _)
      | assoc `elem` ["infix", "infixl", "infixr"] -> assoc
    _ -> "infix"

fixityPrecedenceValue :: Fixity -> Maybe Int
fixityPrecedenceValue fixity =
  case T.words (renderPrinted fixity) of
    (_ : prec : _) | T.all isDigit prec -> Just (read (T.unpack prec))
    _ -> Nothing

classNameFromRendered :: Text -> Text
classNameFromRendered rendered =
  let withoutInstance = T.strip (fromMaybe rendered (T.stripPrefix "instance" (T.strip rendered)))
      noWhere = T.strip (fst (T.breakOn " where" withoutInstance))
      noContext =
        case T.breakOn "=>" noWhere of
          (lhs, rhs)
            | T.null rhs -> lhs
            | otherwise -> T.strip (T.drop 2 rhs)
      tokens = map normalizeTypeToken (T.words noContext)
   in case filter isClassToken tokens of
        (name : _) -> name
        [] -> "Instance"

isClassToken :: Text -> Bool
isClassToken token =
  case T.uncons token of
    Just (c, _) -> isUpper c
    Nothing -> False

normalizeTypeToken :: Text -> Text
normalizeTypeToken =
  T.dropAround (\c -> c == '(' || c == ')' || c == ',' || c == ' ')

isSymbolicToken :: Text -> Bool
isSymbolicToken tok =
  not (T.null tok) && T.all (`elem` (":!#$%&*+./<=>?@\\^|-~" :: String)) tok

normalizeSpaceText :: Text -> Text
normalizeSpaceText = T.unwords . T.words

renderLocated :: (Outputable a) => GenLocated l a -> Text
renderLocated = renderPrinted . unLoc

renderPrinted :: (Outputable a) => a -> Text
renderPrinted = T.pack . showSDocUnsafe . ppr

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f =
  foldr
    ( \x acc ->
        case f x of
          Just y -> y : acc
          Nothing -> acc
    )
    []

toCanonicalExpr :: HsExpr GhcPs -> Either String CanonicalExpr
toCanonicalExpr expr =
  case expr of
    HsVar _ locatedName ->
      let name = unLoc locatedName
          rendered = T.pack (occNameString (rdrNameOcc name))
       in Right (canonicalizeVar rendered)
    HsIf _ cond yes no ->
      CIf <$> toCanonicalExpr (unLoc cond) <*> toCanonicalExpr (unLoc yes) <*> toCanonicalExpr (unLoc no)
    HsLam _ _ mg -> toCanonicalLambda mg
    HsPar _ inner -> toCanonicalExpr (unLoc inner)
    HsApp _ f x -> CApp <$> toCanonicalExpr (unLoc f) <*> toCanonicalExpr (unLoc x)
    OpApp _ lhs op rhs ->
      CInfix
        <$> toCanonicalExpr (unLoc lhs)
        <*> toCanonicalOperator (unLoc op)
        <*> toCanonicalExpr (unLoc rhs)
    NegApp _ inner _ -> CNegate <$> toCanonicalExpr (unLoc inner)
    SectionL _ lhs op ->
      CSectionL <$> toCanonicalExpr (unLoc lhs) <*> toCanonicalOperator (unLoc op)
    SectionR _ op rhs ->
      CSectionR <$> toCanonicalOperator (unLoc op) <*> toCanonicalExpr (unLoc rhs)
    HsLet _ localBinds body ->
      CLet <$> toCanonicalLocalBinds localBinds <*> toCanonicalExpr (unLoc body)
    HsCase _ scrutinee mg ->
      CCase <$> toCanonicalExpr (unLoc scrutinee) <*> toCanonicalCaseAlts mg
    HsDo _ ctxt locatedStmts ->
      case ctxt of
        ListComp -> toCanonicalListComp (toList (unLoc locatedStmts))
        _ -> CDo <$> traverse toCanonicalDoStmt (toList (unLoc locatedStmts))
    ArithSeq _ _ seqInfo -> CArithSeq <$> toCanonicalArithSeq seqInfo
    RecordCon _ conName fields ->
      CRecordCon (renderText conName) <$> toCanonicalRecordFields fields
    RecordUpd {rupd_expr = baseExpr, rupd_flds = fields} ->
      CRecordUpd <$> toCanonicalExpr (unLoc baseExpr) <*> toCanonicalRecordUpdFields fields
    ExprWithTySig _ inner ty ->
      CTypeSig <$> toCanonicalExpr (unLoc inner) <*> pure (renderText ty)
    ExplicitList _ values -> CList <$> traverse (toCanonicalExpr . unLoc) (toList values)
    ExplicitTuple _ args _ ->
      if all isTupleMissing args
        then Right (CTupleCon (length args))
        else CTuple <$> traverse toTupleArgExpr args
    HsOverLit _ lit ->
      case ol_val lit of
        HsIntegral (IL _ _ value) -> Right (CInt value)
        HsFractional frac ->
          case readMaybe (showSDocUnsafe (ppr frac)) of
            Just value -> Right (CFloat value)
            Nothing -> Left "unsupported fractional literal"
        _ -> Left "unsupported literal"
    HsLit _ lit ->
      case lit of
        HsChar _ c -> Right (CChar c)
        HsString _ s -> Right (CString (T.pack (FastString.unpackFS s)))
        _ -> Left "unsupported literal"
    _ -> Left "unsupported expression"

canonicalizeVar :: Text -> CanonicalExpr
canonicalizeVar name
  | name == "[]" = CList []
  | name == "()" = CTuple []
  | T.length name >= 3 && T.head name == '(' && T.last name == ')' && T.all (== ',') (T.init (T.tail name)) =
      CTupleCon (T.length (T.init (T.tail name)) + 1)
  | otherwise = CVar name

isTupleMissing :: HsTupArg GhcPs -> Bool
isTupleMissing arg =
  case arg of
    Missing _ -> True
    _ -> False

toTupleArgExpr :: HsTupArg GhcPs -> Either String CanonicalExpr
toTupleArgExpr arg =
  case arg of
    Present _ expr -> toCanonicalExpr (unLoc expr)
    Missing _ -> Left "mixed tuple sections unsupported"

toCanonicalLambda :: MatchGroup GhcPs (LHsExpr GhcPs) -> Either String CanonicalExpr
toCanonicalLambda mg =
  case toList (unLoc (mg_alts mg)) of
    [locatedMatch] ->
      let match = unLoc locatedMatch
       in case toSimpleMatchBody match of
            Just body ->
              Right
                ( CLambda
                    [renderText pat | pat <- unLoc (m_pats match)]
                    body
                )
            Nothing -> Left "unsupported lambda body"
    _ -> Left "unsupported lambda alternatives"

toSimpleMatchBody :: Match GhcPs (LHsExpr GhcPs) -> Maybe CanonicalExpr
toSimpleMatchBody match =
  case m_grhss match of
    GRHSs _ grhss _ ->
      case toList grhss of
        [grhs] ->
          case unLoc grhs of
            GRHS _ [] body ->
              case toCanonicalExpr (unLoc body) of
                Right canonExpr -> Just canonExpr
                Left _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    XGRHSs _ -> Nothing

toCanonicalOperator :: HsExpr GhcPs -> Either String Text
toCanonicalOperator opExpr =
  case opExpr of
    HsVar _ locatedName -> Right (renderRdrName (unLoc locatedName))
    _ -> Right (renderText opExpr)

toCanonicalLocalBinds :: HsLocalBinds GhcPs -> Either String [(Text, CanonicalExpr)]
toCanonicalLocalBinds localBinds =
  case localBinds of
    HsValBinds _ valBinds ->
      case valBinds of
        ValBinds _ bag _ -> do
          let binds = toList bag
          fmap concat (traverse toCanonicalLocalBind binds)
        XValBindsLR (NValBinds binds _) ->
          concat
            <$> traverse
              ( \(_, bag) ->
                  fmap concat (traverse toCanonicalLocalBind (toList bag))
              )
              binds
    EmptyLocalBinds _ -> Right []
    _ -> Left "unsupported let bindings"

toCanonicalLocalBind :: LHsBindLR GhcPs GhcPs -> Either String [(Text, CanonicalExpr)]
toCanonicalLocalBind locatedBind =
  case unLoc locatedBind of
    FunBind {fun_id = locatedName, fun_matches = MG {mg_alts = locatedMatches}} ->
      case toList (unLoc locatedMatches) of
        [singleMatch] ->
          case toSimpleMatchBody (unLoc singleMatch) of
            Just body -> Right [(occNameText (unLoc locatedName), body)]
            Nothing -> Left "unsupported let binding rhs"
        _ -> Left "unsupported let binding matches"
    _ -> Left "unsupported let binding"

toCanonicalCaseAlts :: MatchGroup GhcPs (LHsExpr GhcPs) -> Either String [CanonicalCaseAlt]
toCanonicalCaseAlts mg = traverse toCanonicalCaseAlt (toList (unLoc (mg_alts mg)))

toCanonicalCaseAlt :: LMatch GhcPs (LHsExpr GhcPs) -> Either String CanonicalCaseAlt
toCanonicalCaseAlt locatedMatch =
  let match = unLoc locatedMatch
   in case unLoc (m_pats match) of
        [pat] ->
          case toSimpleMatchBody match of
            Just body ->
              Right
                CanonicalCaseAlt
                  { canonicalCaseAltPattern = renderText pat,
                    canonicalCaseAltExpr = body
                  }
            Nothing -> Left "unsupported case alternative"
        _ -> Left "unsupported case pattern"

toCanonicalDoStmt :: ExprLStmt GhcPs -> Either String CanonicalDoStmt
toCanonicalDoStmt locatedStmt =
  case unLoc locatedStmt of
    LastStmt _ body _ _ -> CDoExpr <$> toCanonicalExpr (unLoc body)
    BodyStmt _ body _ _ -> CDoExpr <$> toCanonicalExpr (unLoc body)
    BindStmt _ pat body ->
      CDoBind (renderText pat) <$> toCanonicalExpr (unLoc body)
    LetStmt _ localBinds -> CDoLet <$> toCanonicalLocalBinds localBinds
    _ -> Left "unsupported do statement"

toCanonicalListComp :: [ExprLStmt GhcPs] -> Either String CanonicalExpr
toCanonicalListComp stmts =
  case reverse stmts of
    [] -> Left "unsupported list comprehension"
    (lastStmt : reversedQuals) ->
      case unLoc lastStmt of
        LastStmt _ body _ _ ->
          CListComp <$> toCanonicalExpr (unLoc body) <*> traverse toCanonicalCompStmt (reverse reversedQuals)
        _ -> Left "unsupported list comprehension"

toCanonicalCompStmt :: ExprLStmt GhcPs -> Either String CanonicalCompStmt
toCanonicalCompStmt locatedStmt =
  case unLoc locatedStmt of
    BindStmt _ pat body ->
      CCompGen (renderText pat) <$> toCanonicalExpr (unLoc body)
    LetStmt _ localBinds -> CCompLet <$> toCanonicalLocalBinds localBinds
    BodyStmt _ guardExpr _ _ -> CCompGuard <$> toCanonicalExpr (unLoc guardExpr)
    _ -> Left "unsupported list-comprehension qualifier"

toCanonicalArithSeq :: ArithSeqInfo GhcPs -> Either String CanonicalArithSeq
toCanonicalArithSeq seqInfo =
  case seqInfo of
    From start -> CArithSeqFrom <$> toCanonicalExpr (unLoc start)
    FromThen start next -> CArithSeqFromThen <$> toCanonicalExpr (unLoc start) <*> toCanonicalExpr (unLoc next)
    FromTo start end -> CArithSeqFromTo <$> toCanonicalExpr (unLoc start) <*> toCanonicalExpr (unLoc end)
    FromThenTo start next end ->
      CArithSeqFromThenTo
        <$> toCanonicalExpr (unLoc start)
        <*> toCanonicalExpr (unLoc next)
        <*> toCanonicalExpr (unLoc end)

toCanonicalRecordFields :: HsRecordBinds GhcPs -> Either String [(Text, CanonicalExpr)]
toCanonicalRecordFields fields =
  traverse toCanonicalRecordField (toList (rec_flds fields))

toCanonicalRecordField :: LHsRecField GhcPs (LHsExpr GhcPs) -> Either String (Text, CanonicalExpr)
toCanonicalRecordField locatedField =
  let field = unLoc locatedField
   in do
        expr <- toCanonicalExpr (unLoc (hfbRHS field))
        pure (renderText (hfbLHS field), expr)

toCanonicalRecordUpdFields :: LHsRecUpdFields GhcPs -> Either String [(Text, CanonicalExpr)]
toCanonicalRecordUpdFields fields =
  case fields of
    RegularRecUpdFields {recUpdFields = recordFields} ->
      traverse toCanonicalRecordUpdField recordFields
    OverloadedRecUpdFields {} ->
      Left "unsupported overloaded record update fields"

toCanonicalRecordUpdField :: LHsRecUpdField GhcPs GhcPs -> Either String (Text, CanonicalExpr)
toCanonicalRecordUpdField locatedField =
  let field = unLoc locatedField
   in do
        expr <- toCanonicalExpr (unLoc (hfbRHS field))
        pure (renderText (hfbLHS field), expr)

renderRdrName :: RdrName -> Text
renderRdrName = T.pack . occNameString . rdrNameOcc

renderText :: (Outputable a) => a -> Text
renderText = normalizeRenderedText . T.pack . showSDocUnsafe . ppr

normalizeRenderedText :: Text -> Text
normalizeRenderedText =
  T.strip . T.map replaceLayout
  where
    replaceLayout '\n' = ' '
    replaceLayout '\r' = ' '
    replaceLayout '\t' = ' '
    replaceLayout c = c
