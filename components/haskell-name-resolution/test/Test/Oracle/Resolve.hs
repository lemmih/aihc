{-# LANGUAGE OverloadedStrings #-}

module Test.Oracle.Resolve
  ( oracleResolveFacts,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( ParseResult (..),
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SourceText (IntegralLit (..))
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (showSDocUnsafe)
import Resolver.Types
import Test.ResolveFacts

data OracleDecl = OracleDecl
  { oracleDeclName :: Text,
    oracleDeclExpr :: HsExpr GhcPs
  }

oracleResolveFacts :: ResolveConfig -> Text -> Either Text ResolveFacts
oracleResolveFacts cfg source = do
  modu <- parseWithGhc source
  decls <- extractDecls modu
  let moduleName = fmap (T.pack . moduleNameString . unLoc) (hsmodName modu)
  resolveFactsFromDecls cfg moduleName decls

resolveFactsFromDecls :: ResolveConfig -> Maybe Text -> [OracleDecl] -> Either Text ResolveFacts
resolveFactsFromDecls cfg moduleName decls = do
  let declNames = map oracleDeclName decls
      (declMap, dupDiags) = gatherDecls declNames
  (varFacts, unboundDiags) <- foldM (resolveDeclExpr declMap) ([], []) decls
  pure
    ResolveFacts
      { rfModuleName = moduleName,
        rfDeclNames = declNames,
        rfVars = varFacts,
        rfDiagnosticCodes = dupDiags <> unboundDiags
      }
  where
    resolveDeclExpr declMap (accFacts, accDiags) decl = do
      (facts, diags) <- resolveExpr cfg declMap (oracleDeclExpr decl)
      pure (accFacts <> facts, accDiags <> diags)

resolveExpr :: ResolveConfig -> M.Map Text () -> HsExpr GhcPs -> Either Text ([VarFact], [DiagnosticCode])
resolveExpr cfg declMap expr =
  case expr of
    HsPar _ inner -> resolveExpr cfg declMap (unLoc inner)
    HsApp _ f x -> do
      (fFacts, fDiags) <- resolveExpr cfg declMap (unLoc f)
      (xFacts, xDiags) <- resolveExpr cfg declMap (unLoc x)
      pure (fFacts <> xFacts, fDiags <> xDiags)
    HsVar _ locatedName ->
      let name = T.pack (occNameString (rdrNameOcc (unLoc locatedName)))
       in case M.lookup name declMap of
            Just _ ->
              pure ([VarFact {vfName = name, vfBinding = Just name, vfClass = TopLevelBinder}], [])
            Nothing ->
              if isPreludeName name
                then pure ([VarFact {vfName = name, vfBinding = Just name, vfClass = PreludeBinder}], [])
                else pure ([VarFact {vfName = name, vfBinding = Nothing, vfClass = Unresolved}], [EUnboundVariable])
    HsOverLit _ lit ->
      case ol_val lit of
        HsIntegral IL {} -> pure ([], [])
        _ -> Left "unsupported literal"
    _ -> Left "unsupported expression"
  where
    isPreludeName name =
      case preludeMode cfg of
        ImplicitPreludeOff -> False
        ImplicitPreludeOn -> name `elem` preludeNames

extractDecls :: HsModule GhcPs -> Either Text [OracleDecl]
extractDecls modu = traverse toOracleDecl (hsmodDecls modu)

toOracleDecl :: LHsDecl GhcPs -> Either Text OracleDecl
toOracleDecl locatedDecl =
  case unLoc locatedDecl of
    ValD _ bind ->
      case bind of
        FunBind {fun_id = locatedName, fun_matches = MG {mg_alts = locatedMatches}} -> do
          let name = T.pack (occNameString (rdrNameOcc (unLoc locatedName)))
          match <-
            case unLoc locatedMatches of
              [singleMatch] -> Right (unLoc singleMatch)
              _ -> Left "unsupported multiple matches"
          expr <-
            case m_grhss match of
              GRHSs _ grhss _ ->
                case toList grhss of
                  [grhs] ->
                    case unLoc grhs of
                      GRHS _ [] body -> Right (unLoc body)
                      _ -> Left "unsupported guarded rhs"
                  _ -> Left "unsupported function rhs"
          pure (OracleDecl {oracleDeclName = name, oracleDeclExpr = expr})
        _ -> Left "unsupported value binding"
    _ -> Left "unsupported declaration kind"

parseWithGhc :: Text -> Either Text (HsModule GhcPs)
parseWithGhc input =
  let opts = mkParserOpts (EnumSet.empty :: EnumSet.EnumSet Extension) emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<oracle>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

gatherDecls :: [Text] -> (M.Map Text (), [DiagnosticCode])
gatherDecls = foldl step (M.empty, [])
  where
    step (seen, diags) name =
      case M.lookup name seen of
        Just _ -> (seen, diags <> [EDuplicateBinding])
        Nothing -> (M.insert name () seen, diags)

preludeNames :: [Text]
preludeNames = ["return", ">>=", ">>", "fail", "fromInteger", "ifThenElse"]
