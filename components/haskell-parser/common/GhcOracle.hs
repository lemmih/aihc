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
import GHC.LanguageExtensions.Type (Extension (..))
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
  let baseExts = nub extraExts
      languagePragmas = extractLanguagePragmas sourceTag baseExts input
      pragmaExts = mapMaybe (parseExtension . T.unpack) languagePragmas
      parseExts = EnumSet.fromList (nub (baseExts <> pragmaExts)) :: EnumSet.EnumSet Extension
      opts = mkParserOpts parseExts emptyDiagOpts False False False False
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString sourceTag) 1 1
   in case unP parseModule (initParserState opts buffer start) of
        POk _ modu -> Right (languagePragmas, unLoc modu)
        PFailed st ->
          let rendered = showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))
           in Left (T.pack rendered)

extractLanguagePragmas :: String -> [Extension] -> Text -> [Text]
extractLanguagePragmas sourceTag baseExts input =
  let buffer = stringToStringBuffer (T.unpack input)
      baseOpts =
        mkParserOpts
          (EnumSet.fromList baseExts :: EnumSet.EnumSet Extension)
          emptyDiagOpts
          False
          False
          False
          False
      (_warns, rawOptions) = getOptions baseOpts supportedLanguagePragmas buffer sourceTag
   in mapMaybe optionToLanguagePragma rawOptions
  where
    supportedLanguagePragmas = "CPP" : map show ([minBound .. maxBound] :: [Extension])

    optionToLanguagePragma locatedOpt =
      let opt = T.pack (unLoc locatedOpt)
       in case T.stripPrefix "-X" opt of
            Just pragmaName | not (T.null pragmaName) -> Just pragmaName
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
  let exts = mapMaybe parseExtension extNames
      langExts = maybe [] languageExtensions langName
      allExts = nub (exts <> langExts)
   in case parseWithGhcWithExtensions sourceTag allExts input of
        Left err ->
          let extList = T.pack (show extNames)
              langInfo = maybe "" (\l -> " Language: " <> T.pack l) langName
           in Left (err <> "\n(Extensions: " <> extList <> langInfo <> ")")
        Right _ -> Right ()

parseExtension :: String -> Maybe Extension
parseExtension name =
  case name of
    "CPP" -> Just Cpp
    "BangPatterns" -> Just BangPatterns
    "BinaryLiterals" -> Just BinaryLiterals
    "DerivingStrategies" -> Just DerivingStrategies
    "DoAndIfThenElse" -> Just DoAndIfThenElse
    "EmptyCase" -> Just EmptyCase
    "EmptyDataDecls" -> Just EmptyDataDecls
    "ExistentialQuantification" -> Just ExistentialQuantification
    "ExplicitForAll" -> Just ExplicitForAll
    "ExplicitNamespaces" -> Just ExplicitNamespaces
    "FunctionalDependencies" -> Just FunctionalDependencies
    "GADTs" -> Just GADTs
    "HexFloatLiterals" -> Just HexFloatLiterals
    "ImportQualifiedPost" -> Just ImportQualifiedPost
    "InstanceSigs" -> Just InstanceSigs
    "KindSignatures" -> Just KindSignatures
    "LambdaCase" -> Just LambdaCase
    "MultiParamTypeClasses" -> Just MultiParamTypeClasses
    "NamedFieldPuns" -> Just NamedFieldPuns
    "NamedWildCards" -> Just NamedWildCards
    "NumericUnderscores" -> Just NumericUnderscores
    "PackageImports" -> Just PackageImports
    "ParallelListComp" -> Just ParallelListComp
    "PatternGuards" -> Just PatternGuards
    "QuasiQuotes" -> Just QuasiQuotes
    "RoleAnnotations" -> Just RoleAnnotations
    "StandaloneDeriving" -> Just StandaloneDeriving
    "StandaloneKindSignatures" -> Just StandaloneKindSignatures
    "TupleSections" -> Just TupleSections
    "TypeApplications" -> Just TypeApplications
    "TypeOperators" -> Just TypeOperators
    "ViewPatterns" -> Just ViewPatterns
    "OverloadedStrings" -> Just OverloadedStrings
    "ScopedTypeVariables" -> Just ScopedTypeVariables
    "FlexibleContexts" -> Just FlexibleContexts
    "FlexibleInstances" -> Just FlexibleInstances
    "GeneralizedNewtypeDeriving" -> Just GeneralizedNewtypeDeriving
    "DeriveGeneric" -> Just DeriveGeneric
    "DeriveFunctor" -> Just DeriveFunctor
    "DeriveDataTypeable" -> Just DeriveDataTypeable
    "DeriveFoldable" -> Just DeriveFoldable
    "DeriveTraversable" -> Just DeriveTraversable
    "TypeFamilies" -> Just TypeFamilies
    "ConstraintKinds" -> Just ConstraintKinds
    "PolyKinds" -> Just PolyKinds
    "DataKinds" -> Just DataKinds
    "RankNTypes" -> Just RankNTypes
    "GADTSyntax" -> Just GADTSyntax
    "EmptyDataDeriving" -> Just EmptyDataDeriving
    "MultiWayIf" -> Just MultiWayIf
    "PatternSynonyms" -> Just PatternSynonyms
    "RecordWildCards" -> Just RecordWildCards
    "RecursiveDo" -> Just RecursiveDo
    "Strict" -> Just Strict
    "StrictData" -> Just StrictData
    "TemplateHaskell" -> Just TemplateHaskell
    "TemplateHaskellQuotes" -> Just TemplateHaskellQuotes
    "UnboxedTuples" -> Just UnboxedTuples
    "UnboxedSums" -> Just UnboxedSums
    "UndecidableInstances" -> Just UndecidableInstances
    "UnicodeSyntax" -> Just UnicodeSyntax
    _ -> lookup name [(show ext, ext) | ext <- [minBound .. maxBound]]

languageExtensions :: String -> [Extension]
languageExtensions lang =
  case lang of
    "Haskell98" -> []
    "Haskell2010" -> [] -- GHC uses some by default anyway
    _ -> []
