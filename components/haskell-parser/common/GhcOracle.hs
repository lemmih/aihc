{-# LANGUAGE OverloadedStrings #-}

module GhcOracle
  ( oracleParsesModuleWithExtensions,
    oracleParsesModuleWithExtensionsAt,
    oracleModuleAstFingerprintWithExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
  )
where

import Data.List (nub)
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
  parsed <- parseWithGhcWithExtensions sourceTag exts input
  pure (T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhcWithExtensions :: String -> [Extension] -> Text -> Either Text (HsModule GhcPs)
parseWithGhcWithExtensions sourceTag extraExts input =
  let exts = EnumSet.fromList (nub (ForeignFunctionInterface : extraExts)) :: EnumSet.EnumSet Extension
      opts = mkParserOpts exts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString sourceTag) 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)
