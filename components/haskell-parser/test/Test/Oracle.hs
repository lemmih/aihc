{-# LANGUAGE OverloadedStrings #-}

module Test.Oracle
  ( oracleCanonicalModule
  , oracleParsesModule
  ) where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import qualified GHC.Data.FastString as FastString
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( ParseResult (..)
  , getPsErrorMessages
  , initParserState
  , mkParserOpts
  , unP
  )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SourceText (IntegralLit (..))
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (pprMessages)
import GHC.Utils.Error (emptyDiagOpts)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
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
  let opts = mkParserOpts (EnumSet.empty :: EnumSet.EnumSet Extension) emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString "<oracle>") 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

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
   in
  case decl of
    ValD _ bind ->
      case bind of
        FunBind {fun_id = locatedName, fun_matches = MG {mg_alts = locatedMatches}} -> do
          let name = unLoc locatedName
              matches = unLoc locatedMatches
          match <- case matches of
            [singleMatch] -> Right (unLoc singleMatch)
            _ -> Left "unsupported multiple matches"
          expr <- case m_grhss match of
            GRHSs _ grhss _ ->
              case toList grhss of
                [grhs] ->
                  case unLoc grhs of
                    GRHS _ [] body -> toCanonicalExpr (unLoc body)
                    _ -> Left "unsupported guarded rhs"
                _ -> Left "unsupported function rhs"
          pure
            CanonicalValueDecl
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
          rendered = T.pack (occNameString (rdrNameOcc name))
       in Right (canonicalizeVar rendered)
    HsPar _ inner -> toCanonicalExpr (unLoc inner)
    HsApp _ f x -> CApp <$> toCanonicalExpr (unLoc f) <*> toCanonicalExpr (unLoc x)
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
