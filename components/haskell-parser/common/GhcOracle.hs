{-# LANGUAGE OverloadedStrings #-}

module GhcOracle
  ( oracleParsesModuleWithExtensions,
    oracleParsesModuleWithExtensionsAt,
    oracleModuleAstFingerprintWithExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
    oracleParsesModuleWithNames,
    oracleParsesModuleWithNamesAt,
    oracleDetailedParsesModuleWithNames,
    oracleDetailedParsesModuleWithNamesAt,
    toGhcExtension,
    fromGhcExtension,
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
import qualified GHC.LanguageExtensions.Type as GHC
import GHC.Parser (parseModule)
import GHC.Parser.Header (getOptions)
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
import qualified Parser.Ast as Ast

oracleParsesModuleWithExtensions :: [GHC.Extension] -> Text -> Bool
oracleParsesModuleWithExtensions = oracleParsesModuleWithExtensionsAt "oracle"

oracleParsesModuleWithExtensionsAt :: String -> [GHC.Extension] -> Text -> Bool
oracleParsesModuleWithExtensionsAt sourceTag exts input =
  case parseWithGhcWithExtensions sourceTag exts input of
    Left _ -> False
    Right _ -> True

oracleModuleAstFingerprintWithExtensions :: [GHC.Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensions = oracleModuleAstFingerprintWithExtensionsAt "oracle"

oracleModuleAstFingerprintWithExtensionsAt :: String -> [GHC.Extension] -> Text -> Either Text Text
oracleModuleAstFingerprintWithExtensionsAt sourceTag exts input = do
  (pragmas, parsed) <- parseWithGhcWithExtensions sourceTag exts input
  let pragmaFingerprint =
        if null pragmas
          then ""
          else "LANGUAGE " <> T.intercalate "," (map Ast.extensionSettingName pragmas) <> "\n"
  pure (pragmaFingerprint <> T.pack (showSDocUnsafe (ppr parsed)))

parseWithGhcWithExtensions :: String -> [GHC.Extension] -> Text -> Either Text ([Ast.ExtensionSetting], HsModule GhcPs)
parseWithGhcWithExtensions sourceTag extraExts input =
  let baseExts = nub extraExts
      languagePragmas = extractLanguagePragmas sourceTag baseExts input
      parseExts = foldl' applyExtensionSetting (EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension) languagePragmas
      opts = mkParserOpts parseExts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString sourceTag) 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (languagePragmas, unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

applyExtensionSetting :: EnumSet.EnumSet GHC.Extension -> Ast.ExtensionSetting -> EnumSet.EnumSet GHC.Extension
applyExtensionSetting exts setting =
  case setting of
    Ast.EnableExtension ext ->
      maybe exts (`EnumSet.insert` exts) (toGhcExtension ext)
    Ast.DisableExtension ext ->
      maybe exts (`EnumSet.delete` exts) (toGhcExtension ext)

extractLanguagePragmas :: String -> [GHC.Extension] -> Text -> [Ast.ExtensionSetting]
extractLanguagePragmas sourceTag baseExts input =
  let buffer = stringToStringBuffer (T.unpack input)
      baseOpts =
        mkParserOpts
          (EnumSet.fromList baseExts :: EnumSet.EnumSet GHC.Extension)
          emptyDiagOpts
          False
          False
          False
          False
      (_warns, rawOptions) = getOptions baseOpts supportedLanguagePragmas buffer sourceTag
   in mapMaybe optionToLanguagePragma rawOptions
  where
    supportedLanguagePragmas =
      "CPP" : concatMap (includeNegative . show) ([minBound .. maxBound] :: [GHC.Extension])

    includeNegative extName =
      case extName of
        "Cpp" -> [extName, "NoCPP", "NoCpp"]
        _ -> [extName, "No" <> extName]

    optionToLanguagePragma locatedOpt =
      let opt = T.pack (unLoc locatedOpt)
       in case T.stripPrefix "-X" opt of
            Just pragmaName | not (T.null pragmaName) -> Ast.parseExtensionSettingName pragmaName
            _ -> Nothing

oracleParsesModuleWithNames :: [String] -> Maybe String -> Text -> Bool
oracleParsesModuleWithNames = oracleParsesModuleWithNamesAt "oracle"

oracleParsesModuleWithNamesAt :: String -> [String] -> Maybe String -> Text -> Bool
oracleParsesModuleWithNamesAt sourceTag extNames langName input =
  case oracleDetailedParsesModuleWithNamesAt sourceTag extNames langName input of
    Left _ -> False
    Right _ -> True

oracleDetailedParsesModuleWithNames :: [String] -> Maybe String -> Text -> Either Text ()
oracleDetailedParsesModuleWithNames = oracleDetailedParsesModuleWithNamesAt "oracle"

oracleDetailedParsesModuleWithNamesAt :: String -> [String] -> Maybe String -> Text -> Either Text ()
oracleDetailedParsesModuleWithNamesAt sourceTag extNames langName input =
  let extSettings = mapMaybe (Ast.parseExtensionSettingName . T.pack) extNames
      langExts = maybe [] languageExtensions langName
      allExts = EnumSet.toList (foldl' applyExtensionSetting (EnumSet.fromList langExts) extSettings)
   in case parseWithGhcWithExtensions sourceTag allExts input of
        Left err ->
          let extList = T.pack (show extNames)
              langInfo = maybe "" (\l -> " Language: " <> T.pack l) langName
           in Left (err <> "\n(Extensions: " <> extList <> langInfo <> ")")
        Right _ -> Right ()

toGhcExtension :: Ast.Extension -> Maybe GHC.Extension
toGhcExtension ext =
  lookup (toGhcExtensionName ext) [(show ghcExt, ghcExt) | ghcExt <- [minBound .. maxBound]]
  where
    toGhcExtensionName Ast.CPP = "Cpp"
    toGhcExtensionName Ast.GeneralizedNewtypeDeriving = "GeneralisedNewtypeDeriving"
    toGhcExtensionName other = T.unpack (Ast.extensionName other)

fromGhcExtension :: GHC.Extension -> Maybe Ast.Extension
fromGhcExtension ghcExt = Ast.parseExtensionName (T.pack (show ghcExt))

languageExtensions :: String -> [GHC.Extension]
languageExtensions lang =
  case lang of
    "Haskell98" -> []
    "Haskell2010" -> [] -- GHC uses some by default anyway
    _ -> []
