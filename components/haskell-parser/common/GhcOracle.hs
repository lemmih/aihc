{-# LANGUAGE OverloadedStrings #-}

module GhcOracle
  ( oracleParsesModuleWithExtensions,
    oracleParsesModuleWithExtensionsAt,
    oracleModuleAstFingerprintWithExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
  )
where

import Control.Exception (SomeException, evaluate, try)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import GHC.LanguageExtensions.Type (Extension (ForeignFunctionInterface))
import GHC.Parser (parseModule)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer
  ( ParseResult (..),
    ParserOpts,
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error (NoDiagnosticOpts (NoDiagnosticOpts))
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import GHC.Utils.Error (emptyDiagOpts, pprMessages)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import System.IO.Unsafe (unsafePerformIO)

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
parseWithGhcWithExtensions sourceTag extraExts input =
  let exts = EnumSet.fromList (nub (ForeignFunctionInterface : extraExts)) :: EnumSet.EnumSet Extension
      opts = mkParserOpts exts emptyDiagOpts False False False False
      languagePragmas = extractLanguagePragmas opts sourceTag input
      sanitizedInput = stripLanguagePragmaLines input
      buffer = stringToStringBuffer (T.unpack sanitizedInput)
      start = mkRealSrcLoc (mkFastString sourceTag) 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (languagePragmas, unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

extractLanguagePragmas :: ParserOpts -> FilePath -> Text -> [Text]
extractLanguagePragmas opts sourcePath input =
  let buffer = stringToStringBuffer (T.unpack input)
      fallbackPragmas = extractLeadingLanguagePragmas input
      pragmasFromOptions =
        let (_, rawOpts) = getOptions opts [] buffer sourcePath
         in foldr collectLanguagePragma [] rawOpts
      optsResult :: Either SomeException [Text]
      optsResult =
        unsafePerformIO (try (evaluate (forceList pragmasFromOptions)))
   in case optsResult of
        Right pragmas ->
          case (pragmas, fallbackPragmas) of
            ([], _) -> fallbackPragmas
            (_, []) -> pragmas
            _ ->
              if pragmas == fallbackPragmas
                then pragmas
                else fallbackPragmas
        Left _ -> fallbackPragmas
  where
    forceList :: [Text] -> [Text]
    forceList xs = go xs `seq` xs

    go :: [Text] -> ()
    go xs =
      case xs of
        [] -> ()
        (y : ys) -> T.length y `seq` go ys

    collectLanguagePragma located acc =
      let raw = T.strip (T.pack (unLoc located))
       in case T.stripPrefix "-X" raw of
            Just ext
              | T.null (T.strip ext) -> acc
              | otherwise -> T.strip ext : acc
            Nothing -> acc

extractLeadingLanguagePragmas :: Text -> [Text]
extractLeadingLanguagePragmas = go False []
  where
    go inBlock acc txt =
      case T.uncons txt of
        Nothing -> reverse acc
        Just _ ->
          let (line, rest0) = T.break (== '\n') txt
              rest = if T.null rest0 then rest0 else T.drop 1 rest0
              stripped = T.strip line
           in if inBlock
                then
                  if "-}" `T.isInfixOf` stripped
                    then go False acc rest
                    else go True acc rest
                else
                  if T.null stripped || "--" `T.isPrefixOf` stripped
                    then go False acc rest
                    else
                      if "{-" `T.isPrefixOf` stripped && not ("{-#" `T.isPrefixOf` stripped)
                        then go (not ("-}" `T.isInfixOf` stripped)) acc rest
                        else case parseLanguagePragmaLine stripped of
                          Just names -> go False (reverse names <> acc) rest
                          Nothing -> reverse acc

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
