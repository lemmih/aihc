-- |
-- Module      : Aihc.PackagePlan.Diagnostic
-- Description : Human-readable rendering of JSON diagnostics
--
-- Renders the JSON diagnostics produced while parsing, preprocessing,
-- resolving and type checking a package as GHC-style messages with a source
-- excerpt.
module Aihc.PackagePlan.Diagnostic
  ( renderHumanDiagnostic,
    DiagnosticSourceMap,
    diagnosticSourceMap,
    parseDiagnosticValue,
    cppDiagnosticValue,
    sourceSpanValue,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Parser.Syntax (SourceSpan (..))
import Control.Applicative ((<|>))
import Data.Aeson (object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Read (readMaybe)

renderHumanDiagnostic :: String -> Aeson.Value -> String
renderHumanDiagnostic phase diagnostic =
  unlines $
    T.unpack (locationPrefix <> severityText <> ": " <> modulePrefix <> messageText)
      : renderHumanDiagnosticExcerpt diagnostic
  where
    locationPrefix =
      case diagnosticLocation diagnostic of
        Just location -> location <> ": "
        Nothing -> ""
    modulePrefix =
      case diagnosticModule diagnostic of
        Just moduleName -> "[" <> moduleName <> "] "
        Nothing -> ""
    severityText = fromMaybe "error" (stringField "severity" diagnostic)
    messageText = renderHumanDiagnosticMessage phase diagnostic

renderHumanDiagnosticMessage :: String -> Aeson.Value -> Text
renderHumanDiagnosticMessage phase diagnostic
  | phase == "rename",
    Just message <- stringField "message" diagnostic,
    Just name <- stringField "name" diagnostic,
    Just namespace <- stringField "namespace" diagnostic =
      renderResolveMessage message name namespace
  | otherwise = fromMaybe (diagnosticSummary diagnostic) (stringField "message" diagnostic)

renderResolveMessage :: Text -> Text -> Text -> Text
renderResolveMessage message name namespace
  | message == "unbound" = "unbound " <> renderNamespace namespace <> " name ‘" <> name <> "’"
  | message == "not found" = renderNamespace namespace <> " ‘" <> name <> "’ not found"
  | otherwise = message <> ": " <> renderNamespace namespace <> " name ‘" <> name <> "’"

renderNamespace :: Text -> Text
renderNamespace namespace =
  case namespace of
    "ResolutionNamespaceTerm" -> "term"
    "ResolutionNamespaceType" -> "type"
    "ResolutionNamespaceModule" -> "module"
    _ -> namespace

renderHumanDiagnosticExcerpt :: Aeson.Value -> [String]
renderHumanDiagnosticExcerpt diagnostic =
  case (diagnosticSourceLines diagnostic, diagnosticSpanLines diagnostic) of
    (sourceLines@(_ : _), Just (startLine, startColumn, endLine, endColumn)) ->
      renderSourceExcerpt sourceLines startLine startColumn endLine endColumn
    _ -> []

renderSourceExcerpt :: [DiagnosticSourceLine] -> Int -> Int -> Int -> Int -> [String]
renderSourceExcerpt sourceLines startLine startColumn endLine endColumn
  | null selectedLines = []
  | otherwise = concatMap renderLine selectedLines
  where
    selectedLines =
      filter
        ( \sourceLine ->
            sourceLineNumber sourceLine >= startLine
              && sourceLineNumber sourceLine <= endLine
        )
        sourceLines
    width = length (show (maximum (map sourceLineNumber selectedLines)))
    renderLine sourceLine =
      [ "  " <> padLeft width ' ' (show lineNumber) <> " | " <> T.unpack lineText,
        "  " <> replicate width ' ' <> " | " <> T.unpack (caretIndicator lineNumber lineText)
      ]
      where
        lineNumber = sourceLineNumber sourceLine
        lineText = sourceLineText sourceLine
    caretIndicator lineNumber lineText =
      T.replicate (max 0 (lineStartColumn lineNumber - 1)) " "
        <> T.replicate (lineCaretWidth lineNumber lineText) "^"
    lineStartColumn lineNumber
      | lineNumber == startLine = max 1 startColumn
      | otherwise = 1
    lineCaretWidth lineNumber lineText
      | startLine == endLine =
          max 1 (endColumn - startColumn)
      | lineNumber == startLine =
          max 1 (T.length lineText - lineStartColumn lineNumber + 1)
      | lineNumber == endLine =
          max 1 (endColumn - 1)
      | otherwise =
          max 1 (T.length lineText)

diagnosticModule :: Aeson.Value -> Maybe Text
diagnosticModule =
  stringField "module"

diagnosticFile :: Aeson.Value -> Maybe Text
diagnosticFile diagnostic =
  stringField "file" diagnostic
    <|> (objectField "span" diagnostic >>= stringField "file")

diagnosticLocation :: Aeson.Value -> Maybe Text
diagnosticLocation diagnostic =
  case diagnosticFile diagnostic of
    Nothing -> Nothing
    Just file ->
      Just $
        file
          <> maybe "" (":" <>) lineText
          <> maybe "" (":" <>) columnText
  where
    spanValue = objectField "span" diagnostic
    lineText = scalarFieldText "line" diagnostic <|> (spanValue >>= scalarFieldText "startLine")
    columnText = spanValue >>= scalarFieldText "startColumn"

diagnosticSpanLines :: Aeson.Value -> Maybe (Int, Int, Int, Int)
diagnosticSpanLines diagnostic = do
  spanValue <- objectField "span" diagnostic
  startLine <- intField "startLine" spanValue
  startColumn <- intField "startColumn" spanValue
  endLine <- intField "endLine" spanValue
  endColumn <- intField "endColumn" spanValue
  pure (startLine, startColumn, endLine, endColumn)

data DiagnosticSourceLine = DiagnosticSourceLine
  { sourceLineNumber :: !Int,
    sourceLineText :: !Text
  }

instance Aeson.FromJSON DiagnosticSourceLine where
  parseJSON =
    Aeson.withObject "DiagnosticSourceLine" $ \obj ->
      DiagnosticSourceLine
        <$> obj .: "line"
        <*> obj .: "text"

diagnosticSourceLines :: Aeson.Value -> [DiagnosticSourceLine]
diagnosticSourceLines diagnostic =
  case objectField "sourceLines" diagnostic of
    Just value ->
      case Aeson.fromJSON value of
        Aeson.Success sourceLines -> sourceLines
        Aeson.Error {} -> []
    Nothing -> []

stringField :: String -> Aeson.Value -> Maybe Text
stringField name value =
  case objectField name value of
    Just (Aeson.String text) -> Just text
    _ -> Nothing

scalarFieldText :: String -> Aeson.Value -> Maybe Text
scalarFieldText name value =
  scalarValueText =<< objectField name value

intField :: String -> Aeson.Value -> Maybe Int
intField name value =
  case objectField name value of
    Just fieldValue ->
      case Aeson.fromJSON fieldValue of
        Aeson.Success int -> Just int
        Aeson.Error {} -> Nothing
    Nothing -> Nothing

scalarValueText :: Aeson.Value -> Maybe Text
scalarValueText value =
  case value of
    Aeson.String text -> Just text
    Aeson.Number {} ->
      let parsedInt :: Aeson.Result Int
          parsedInt = Aeson.fromJSON value
       in case parsedInt of
            Aeson.Success int -> Just (T.pack (show int))
            Aeson.Error {} -> Just (diagnosticSummary value)
    _ -> Nothing

objectField :: String -> Aeson.Value -> Maybe Aeson.Value
objectField name value =
  case value of
    Aeson.Object obj -> KeyMap.lookup (Key.fromString name) obj
    _ -> Nothing

diagnosticSummary :: Aeson.Value -> Text
diagnosticSummary =
  TE.decodeUtf8 . BL.toStrict . Aeson.encode

type DiagnosticSourceMap = Map.Map FilePath (Map.Map Int Text)

diagnosticSourceMap :: FilePath -> Text -> DiagnosticSourceMap
diagnosticSourceMap initialFile =
  third . foldl' step (initialFile, 1, Map.empty) . T.lines
  where
    third (_, _, value) = value
    step (currentFile, currentLine, sourceMap) line =
      case parseLineDirective line of
        Just (nextLine, nextFile) -> (fromMaybe currentFile nextFile, nextLine, sourceMap)
        Nothing ->
          ( currentFile,
            currentLine + 1,
            Map.insertWith Map.union currentFile (Map.singleton currentLine line) sourceMap
          )

parseLineDirective :: Text -> Maybe (Int, Maybe FilePath)
parseLineDirective line = do
  afterHash <- T.stripPrefix "#" line
  let directive = T.stripStart afterHash
      afterLine = fromMaybe directive (T.stripPrefix "line" directive)
      (lineNumberText, rest) = T.span (`elem` ['0' .. '9']) (T.stripStart afterLine)
  lineNumber <- readMaybe (T.unpack lineNumberText)
  pure (lineNumber, directiveFile rest)
  where
    directiveFile rest =
      case T.breakOn "\"" rest of
        (_, quoted)
          | Just afterQuote <- T.stripPrefix "\"" quoted,
            let (file, closingQuote) = T.breakOn "\"" afterQuote,
            not (T.null closingQuote) ->
              Just (T.unpack file)
        _ -> Nothing

parseDiagnosticValue :: FilePath -> (SourceSpan, Text) -> Aeson.Value
parseDiagnosticValue path (span', message) =
  object
    [ "file" .= path,
      "span" .= sourceSpanValue span',
      "message" .= message
    ]

cppDiagnosticValue :: Cpp.Diagnostic -> Aeson.Value
cppDiagnosticValue diag =
  object
    [ "file" .= Cpp.diagFile diag,
      "line" .= Cpp.diagLine diag,
      "severity" .= show (Cpp.diagSeverity diag),
      "message" .= Cpp.diagMessage diag
    ]

sourceSpanValue :: SourceSpan -> Aeson.Value
sourceSpanValue span' =
  case span' of
    NoSourceSpan -> Aeson.Null
    SourceSpan file startLine startCol endLine endCol startOffset endOffset ->
      object
        [ "file" .= file,
          "startLine" .= startLine,
          "startColumn" .= startCol,
          "endLine" .= endLine,
          "endColumn" .= endCol,
          "startOffset" .= startOffset,
          "endOffset" .= endOffset
        ]

padLeft :: Int -> Char -> String -> String
padLeft width char value =
  replicate (max 0 (width - length value)) char <> value
