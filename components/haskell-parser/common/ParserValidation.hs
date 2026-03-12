{-# LANGUAGE OverloadedStrings #-}

module ParserValidation
  ( ValidationErrorKind (..),
    ValidationError (..),
    validateParser,
    validateParserWithExtensions,
    validateParserDetailed,
    validateParserDetailedWithExtensions,
  )
where

import Data.Char (isAlphaNum, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.LanguageExtensions.Type (Extension)
import qualified GhcOracle
import qualified Language.Haskell.Exts as HSE
import Parser (defaultConfig, parseModule)
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))

data ValidationErrorKind
  = ValidationParseError
  | ValidationRoundtripError
  deriving (Eq, Show)

data ValidationError = ValidationError
  { validationErrorKind :: ValidationErrorKind,
    validationErrorMessage :: String
  }
  deriving (Eq, Show)

validateParser :: Text -> Maybe String
validateParser = fmap validationErrorMessage . validateParserDetailed

validateParserWithExtensions :: [Extension] -> Text -> Maybe String
validateParserWithExtensions exts = fmap validationErrorMessage . validateParserDetailedWithExtensions exts

validateParserDetailed :: Text -> Maybe ValidationError
validateParserDetailed = validateParserDetailedWithExtensions []

validateParserDetailedWithExtensions :: [Extension] -> Text -> Maybe ValidationError
validateParserDetailedWithExtensions exts source =
  case validateParserDetailedCore exts source of
    Nothing -> Nothing
    Just err ->
      Just
        err
          { validationErrorMessage =
              validationErrorMessage err <> optionalShrunkDiagnostic exts source
          }

validateParserDetailedCore :: [Extension] -> Text -> Maybe ValidationError
validateParserDetailedCore exts source =
  case parseModule defaultConfig source of
    ParseErr err ->
      Just
        ValidationError
          { validationErrorKind = ValidationParseError,
            validationErrorMessage = "Parse failed: " <> show err
          }
    ParseOk parsed ->
      let rendered = prettyModule parsed
          sourceAst = GhcOracle.oracleModuleAstFingerprintWithExtensionsAt "parser-validation" exts source
          renderedAst = GhcOracle.oracleModuleAstFingerprintWithExtensionsAt "parser-validation" exts rendered
       in case (sourceAst, renderedAst) of
            (Right sourceFp, Right renderedFp)
              | sourceFp == renderedFp -> Nothing
              | otherwise ->
                  Just
                    ValidationError
                      { validationErrorKind = ValidationRoundtripError,
                        validationErrorMessage = formatFingerprintMismatch sourceFp renderedFp
                      }
            (Left sourceErr, Left renderedErr) ->
              Just
                ValidationError
                  { validationErrorKind = ValidationRoundtripError,
                    validationErrorMessage =
                      unlines
                        [ "Roundtrip check failed: GHC rejected both module versions.",
                          "Original error:",
                          T.unpack sourceErr,
                          "Roundtripped error:",
                          T.unpack renderedErr
                        ]
                  }
            (Left sourceErr, Right _) ->
              Just
                ValidationError
                  { validationErrorKind = ValidationRoundtripError,
                    validationErrorMessage =
                      unlines
                        [ "Roundtrip check failed: GHC rejected the original module.",
                          T.unpack sourceErr
                        ]
                  }
            (Right _, Left renderedErr) ->
              Just
                ValidationError
                  { validationErrorKind = ValidationRoundtripError,
                    validationErrorMessage =
                      unlines
                        [ "Roundtrip check failed: GHC rejected the roundtripped module.",
                          T.unpack renderedErr
                        ]
                  }

formatFingerprintMismatch :: Text -> Text -> String
formatFingerprintMismatch sourceFp renderedFp =
  let header = "Roundtrip mismatch: GHC fingerprint changed after pretty-print(parse(module))."
      diffText = formatDiff sourceFp renderedFp
   in case diffText of
        Nothing -> header
        Just diffChunk ->
          unlines
            [ header,
              "Changed section in GHC pretty-printed output:",
              T.unpack diffChunk
            ]

formatDiff :: Text -> Text -> Maybe Text
formatDiff before after =
  let beforeLines = T.lines before
      afterLines = T.lines after
      prefixLen = commonPrefixLen beforeLines afterLines
      beforeRest = drop prefixLen beforeLines
      afterRest = drop prefixLen afterLines
      suffixLen = commonSuffixLen beforeRest afterRest
      changedBefore = take (length beforeRest - suffixLen) beforeRest
      changedAfter = take (length afterRest - suffixLen) afterRest
      removed = map ("- " <>) (take 30 changedBefore)
      added = map ("+ " <>) (take 30 changedAfter)
   in if null changedBefore && null changedAfter
        then Nothing
        else
          Just
            ( T.unlines
                ( ["@@ line " <> T.pack (show (prefixLen + 1)) <> " @@"]
                    <> removed
                    <> added
                    <> [truncationNote (length changedBefore) (length changedAfter)]
                )
            )

truncationNote :: Int -> Int -> Text
truncationNote removedN addedN
  | removedN <= 30 && addedN <= 30 = ""
  | otherwise = "...diff truncated..."

commonPrefixLen :: (Eq a) => [a] -> [a] -> Int
commonPrefixLen = go 0
  where
    go n (a : as) (b : bs)
      | a == b = go (n + 1) as bs
      | otherwise = n
    go n _ _ = n

commonSuffixLen :: (Eq a) => [a] -> [a] -> Int
commonSuffixLen xs ys = commonPrefixLen (reverse xs) (reverse ys)

optionalShrunkDiagnostic :: [Extension] -> Text -> String
optionalShrunkDiagnostic exts source =
  case shrinkFailingModule (stillFails exts) source of
    Nothing -> ""
    Just shrunk ->
      if T.strip shrunk == T.strip source
        then ""
        else
          unlines
            [ "",
              "HSE minimized reproducer:",
              "---8<---",
              T.unpack shrunk,
              "--->8---"
            ]

stillFails :: [Extension] -> Text -> Bool
stillFails exts source =
  case validateParserDetailedCore exts source of
    Nothing -> False
    Just _ -> True

shrinkFailingModule :: (Text -> Bool) -> Text -> Maybe Text
shrinkFailingModule fails source
  | not (fails source) = Nothing
  | otherwise =
      case parseCandidate source of
        Nothing -> Nothing
        Just candidate0 -> Just (runShrinks candidate0)
  where
    runShrinks candidate0 =
      let steps = iterateShrink 0 candidate0
       in candSource steps

    iterateShrink :: Int -> ShrinkCandidate -> ShrinkCandidate
    iterateShrink accepted candidate
      | accepted >= 200 = candidate
      | otherwise =
          case firstSuccessfulShrink candidate of
            Nothing -> candidate
            Just candidate' -> iterateShrink (accepted + 1) candidate'

    firstSuccessfulShrink candidate = tryCandidates (candidateTransforms candidate)
      where
        tryCandidates [] = Nothing
        tryCandidates (ast' : rest) =
          case normalizeCandidateAst ast' of
            Nothing -> tryCandidates rest
            Just candidate'
              | fails (candSource candidate') -> Just candidate'
              | otherwise -> tryCandidates rest

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<validate-parser>",
      HSE.extensions = HSE.glasgowExts
    }

data ShrinkCandidate = ShrinkCandidate
  { candAst :: HSE.Module HSE.SrcSpanInfo,
    candSource :: Text
  }

parseCandidate :: Text -> Maybe ShrinkCandidate
parseCandidate source0 =
  case HSE.parseFileContentsWithMode hseParseMode (T.unpack source0) of
    HSE.ParseFailed _ _ -> Nothing
    HSE.ParseOk modu0 -> normalizeCandidateAst modu0

normalizeCandidateAst :: HSE.Module HSE.SrcSpanInfo -> Maybe ShrinkCandidate
normalizeCandidateAst modu0 =
  let source0 = HSE.prettyPrint modu0
   in case HSE.parseFileContentsWithMode hseParseMode source0 of
        HSE.ParseFailed _ _ -> Nothing
        HSE.ParseOk modu1 ->
          let source1 = HSE.exactPrint modu1 []
           in case HSE.parseFileContentsWithMode hseParseMode source1 of
                HSE.ParseFailed _ _ -> Nothing
                HSE.ParseOk modu2 ->
                  Just
                    ShrinkCandidate
                      { candAst = modu2,
                        candSource = T.pack (HSE.exactPrint modu2 [])
                      }

candidateTransforms :: ShrinkCandidate -> [HSE.Module HSE.SrcSpanInfo]
candidateTransforms candidate =
  removeModuleHead (candAst candidate)
    <> shrinkModuleHeadName (candAst candidate)
    <> removeModuleHeadExports (candAst candidate)
    <> shrinkModuleHeadExports (candAst candidate)

removeModuleHead :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleHead modu =
  case modu of
    HSE.Module loc (Just _) pragmas imports decls ->
      [HSE.Module loc Nothing pragmas imports decls]
    _ -> []

shrinkModuleHeadName :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleHeadName modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc (HSE.ModuleName nLoc name) warning exports)) pragmas imports decls ->
      [ HSE.Module
          loc
          (Just (HSE.ModuleHead hLoc (HSE.ModuleName nLoc name') warning exports))
          pragmas
          imports
          decls
      | name' <- shrinkModuleName name
      ]
    _ -> []

removeModuleHeadExports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleHeadExports modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning (Just _))) pragmas imports decls ->
      [HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning Nothing)) pragmas imports decls]
    _ -> []

shrinkModuleHeadExports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleHeadExports modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning (Just (HSE.ExportSpecList eLoc specs)))) pragmas imports decls ->
      let removeAt idx = take idx specs <> drop (idx + 1) specs
          shrunkLists = [removeAt idx | idx <- [0 .. length specs - 1]] <> [[]]
       in [ HSE.Module
              loc
              (Just (HSE.ModuleHead hLoc modName warning (Just (HSE.ExportSpecList eLoc specs'))))
              pragmas
              imports
              decls
          | specs' <- unique shrunkLists,
            specs' /= specs
          ]
    _ -> []

shrinkModuleName :: String -> [String]
shrinkModuleName name =
  let parts = splitModuleName name
      dropped = [joinModuleName parts' | parts' <- dropSegmentShrinks parts, isValidModuleName parts']
      trimmed = [joinModuleName parts' | parts' <- trimSegmentShrinks parts, isValidModuleName parts']
   in unique (dropped <> trimmed)

splitModuleName :: String -> [String]
splitModuleName raw =
  case raw of
    "" -> []
    _ -> splitOnDot raw

splitOnDot :: String -> [String]
splitOnDot s =
  case break (== '.') s of
    (prefix, []) -> [prefix]
    (prefix, _ : rest) -> prefix : splitOnDot rest

joinModuleName :: [String] -> String
joinModuleName = foldr1 join
  where
    join a b = a <> "." <> b

dropSegmentShrinks :: [String] -> [[String]]
dropSegmentShrinks parts =
  [before <> after | idx <- [0 .. length parts - 1], let (before, rest) = splitAt idx parts, (_ : after) <- [rest], not (null (before <> after))]

trimSegmentShrinks :: [String] -> [[String]]
trimSegmentShrinks parts =
  [ replaceAt idx segment' parts
  | (idx, segment) <- zip [0 ..] parts,
    segment' <- trimSegment segment,
    isValidModuleSegment segment'
  ]

trimSegment :: String -> [String]
trimSegment segment =
  unique
    [ candidate
    | n <- [1 .. length segment - 1],
      let candidate = take n segment,
      not (null candidate)
    ]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx value xs =
  let (before, rest) = splitAt idx xs
   in case rest of
        [] -> xs
        (_ : after) -> before <> (value : after)

unique :: (Eq a) => [a] -> [a]
unique = foldr keep []
  where
    keep x acc
      | x `elem` acc = acc
      | otherwise = x : acc

isValidModuleName :: [String] -> Bool
isValidModuleName segments = not (null segments) && all isValidModuleSegment segments

isValidModuleSegment :: String -> Bool
isValidModuleSegment segment =
  case segment of
    first : rest -> isUpper first && all isSegmentRestChar rest
    [] -> False

isSegmentRestChar :: Char -> Bool
isSegmentRestChar ch = isAlphaNum ch || ch == '\''
