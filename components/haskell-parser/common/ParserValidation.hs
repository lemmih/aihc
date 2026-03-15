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

import Control.Monad ((<=<))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.LanguageExtensions.Type (Extension)
import qualified GhcOracle
import HseExtensions (toHseExtension)
import qualified Language.Haskell.Exts as HSE
import Parser (defaultConfig, parseModule)
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import ShrinkUtils (candidateTransformsWith, unique)

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
commonSuffixLen xs ys =
  let lenXs = length xs
      lenYs = length ys
      minLen = min lenXs lenYs
      alignedXs = drop (lenXs - minLen) xs
      alignedYs = drop (lenYs - minLen) ys
      suffixEqs = zipWith (==) (reverse alignedXs) (reverse alignedYs)
   in length (takeWhile id suffixEqs)

optionalShrunkDiagnostic :: [Extension] -> Text -> String
optionalShrunkDiagnostic exts source =
  case shrinkFailingModule exts (stillFails exts) source of
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

shrinkFailingModule :: [Extension] -> (Text -> Bool) -> Text -> Maybe Text
shrinkFailingModule exts fails source
  | not (fails source) = Nothing
  | otherwise =
      case parseCandidate exts source of
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
          case normalizeCandidateAst exts ast' of
            Nothing -> tryCandidates rest
            Just candidate'
              | fails (candSource candidate') -> Just candidate'
              | otherwise -> tryCandidates rest

candidateTransforms :: ShrinkCandidate -> [HSE.Module HSE.SrcSpanInfo]
candidateTransforms candidate = candidateTransformsWith trimSegment (candAst candidate)

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<validate-parser>",
      HSE.extensions = []
    }

data ShrinkCandidate = ShrinkCandidate
  { candAst :: HSE.Module HSE.SrcSpanInfo,
    candSource :: Text
  }

parseCandidate :: [Extension] -> Text -> Maybe ShrinkCandidate
parseCandidate exts source0 =
  case HSE.parseFileContentsWithMode (hseParseModeFor exts) (T.unpack source0) of
    HSE.ParseFailed _ _ -> Nothing
    HSE.ParseOk modu0 -> normalizeCandidateAst exts modu0

normalizeCandidateAst :: [Extension] -> HSE.Module HSE.SrcSpanInfo -> Maybe ShrinkCandidate
normalizeCandidateAst exts modu0 =
  let source0 = HSE.prettyPrint modu0
   in case HSE.parseFileContentsWithMode (hseParseModeFor exts) source0 of
        HSE.ParseFailed _ _ -> Nothing
        HSE.ParseOk modu1 ->
          let source1 = HSE.exactPrint modu1 []
           in case HSE.parseFileContentsWithMode (hseParseModeFor exts) source1 of
                HSE.ParseFailed _ _ -> Nothing
                HSE.ParseOk modu2 ->
                  Just
                    ShrinkCandidate
                      { candAst = modu2,
                        candSource = T.pack (HSE.exactPrint modu2 [])
                      }

trimSegment :: String -> [String]
trimSegment segment =
  unique
    [ candidate
    | n <- [1 .. length segment - 1],
      let candidate = take n segment,
      not (null candidate)
    ]

hseParseModeFor :: [Extension] -> HSE.ParseMode
hseParseModeFor exts =
  hseParseMode
    { HSE.extensions =
        mapMaybe
          ( toHseExtension
              <=< GhcOracle.fromGhcExtension
          )
          exts
    }
