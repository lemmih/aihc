{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GhcOracle
  ( oracleParsesModuleWithExtensions,
    oracleParsesModuleWithExtensionsAt,
    oracleModuleAstFingerprintWithExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
  )
where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
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
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

oracleParsesModuleWithExtensions :: [Extension] -> Text -> Bool
oracleParsesModuleWithExtensions = oracleParsesModuleWithExtensionsAt "oracle"

oracleParsesModuleWithExtensionsAt :: String -> [Extension] -> Text -> Bool
oracleParsesModuleWithExtensionsAt sourceTag exts input =
  case parseWithGhcWithExtensions sourceTag exts input of
    Left _ -> False
    Right _ -> True

oracleModuleAstFingerprintWithExtensions :: [Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensions = oracleModuleAstFingerprintWithExtensionsAt "oracle"

oracleModuleAstFingerprintWithExtensionsAt :: String -> [Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensionsAt sourceTag exts input = do
  (pragmas, parsed) <- parseWithGhcWithExtensions sourceTag exts input
  let pragmaFingerprint =
        if null pragmas
          then ""
          else "LANGUAGE " <> T.intercalate "," pragmas <> "\n"
  pure (pragmaFingerprint <> T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhcWithExtensions :: String -> [Extension] -> Text -> Either Text ([Text], HsModule GhcPs)
#if __GLASGOW_HASKELL__ >= 910
parseWithGhcWithExtensions sourceTag extraExts input =
  let parseExts = EnumSet.fromList (nub (ForeignFunctionInterface : extraExts)) :: EnumSet.EnumSet Extension
      opts = mkParserOpts parseExts emptyDiagOpts False False False False
      languagePragmas = extractLanguagePragmas input
      sanitizedInput = stripLanguagePragmaLines input
      buffer = stringToStringBuffer (T.unpack sanitizedInput)
      start = mkRealSrcLoc (mkFastString sourceTag) 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (languagePragmas, unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)
#else
parseWithGhcWithExtensions sourceTag extraExts input =
  let parseExts = EnumSet.fromList (nub (ForeignFunctionInterface : extraExts)) :: EnumSet.EnumSet Extension
      opts = mkParserOpts parseExts emptyDiagOpts [] False False False False
      languagePragmas = extractLanguagePragmas input
      sanitizedInput = stripLanguagePragmaLines input
      buffer = stringToStringBuffer (T.unpack sanitizedInput)
      start = mkRealSrcLoc (mkFastString sourceTag) 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (languagePragmas, unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)
#endif

extractLanguagePragmas :: Text -> [Text]
extractLanguagePragmas =
  concat . mapMaybe (parseLanguagePragmaLine . T.strip) . T.lines

parseLanguagePragmaLine :: Text -> Maybe [Text]
parseLanguagePragmaLine txt
  | "{-#" `T.isPrefixOf` txt && "#-}" `T.isSuffixOf` txt =
      case T.stripPrefix "LANGUAGE" (T.strip (T.dropEnd 3 (T.drop 3 txt))) of
        Just rawNames ->
          let names = filter (not . T.null) (map T.strip (T.splitOn "," rawNames))
           in if null names then Nothing else Just names
        Nothing -> Nothing
  | otherwise = Nothing

stripLanguagePragmaLines :: Text -> Text
stripLanguagePragmaLines =
  T.unlines
    . filter (not . isLanguagePragmaLine . T.strip)
    . T.lines
  where
    isLanguagePragmaLine t =
      case parseLanguagePragmaLine t of
        Just _ -> True
        Nothing -> False
