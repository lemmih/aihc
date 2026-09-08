{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Markup
-- Description : Parse Haddock markup into the documentation model
--
-- A pragmatic parser for the Haddock markup language. It covers paragraphs,
-- headers, code blocks (@\@@ fences and bird tracks), examples, properties,
-- bullet, numbered and definition lists, and the inline forms: identifiers,
-- module links, emphasis, bold, monospace, hyperlinks, pictures, anchors and
-- math. Tables are recognised but their cells are not parsed yet.
module Aihc.Haddock.Markup
  ( parseDocText,
    parseMetaDocText,
    parseInline,
  )
where

import Aihc.Haddock.Model
import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation, isSpace, isSymbol, isUpper)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Parse a documentation comment body, splitting off its @\@since@ marker.
parseMetaDocText :: Text -> MetaDoc
parseMetaDocText text =
  MetaDoc since (joinBlocks blocks)
  where
    (since, blocks) = extractSince (parseBlocks (T.lines text))

parseDocText :: Text -> Doc
parseDocText = joinBlocks . snd . extractSince . parseBlocks . T.lines

extractSince :: [Block] -> (Maybe [Int], [Doc])
extractSince blocks =
  (firstSince, [doc | BlockDoc doc <- blocks])
  where
    firstSince = case [version | BlockSince version <- blocks] of
      version : _ -> Just version
      [] -> Nothing

joinBlocks :: [Doc] -> Doc
joinBlocks = joinDocs

joinDocs :: [Doc] -> Doc
joinDocs docs =
  case mergeStrings (filter (/= DocEmpty) docs) of
    [] -> DocEmpty
    merged -> foldr1 DocAppend merged
  where
    mergeStrings (DocString a : DocString b : rest) = mergeStrings (DocString (a <> b) : rest)
    mergeStrings (d : rest) = d : mergeStrings rest
    mergeStrings [] = []

data Block
  = BlockDoc Doc
  | BlockSince [Int]

-- Block structure --------------------------------------------------------------

parseBlocks :: [Text] -> [Block]
parseBlocks lines' =
  case lines' of
    [] -> []
    line : rest
      | isBlank line -> parseBlocks rest
      | Just (level, title) <- headerLine line ->
          BlockDoc (DocHeader level (parseInline title)) : parseBlocks rest
      | isBirdTrack line ->
          let (code, rest') = span isBirdTrack lines'
           in BlockDoc (DocCodeBlock (DocString (T.intercalate "\n" (map birdTrackText code)))) : parseBlocks rest'
      | isExampleLine line ->
          let (examples, rest') = parseExamples lines'
           in BlockDoc (DocExamples examples) : parseBlocks rest'
      | Just property <- propertyLine line ->
          BlockDoc (DocProperty property) : parseBlocks rest
      | T.strip line == "@" ->
          let indent = T.length (T.takeWhile (== ' ') line)
              (code, rest') = break ((== "@") . T.strip) rest
              body = T.unlines (map (dropIndent indent) code)
           in BlockDoc (DocCodeBlock (DocString body)) : parseBlocks (drop 1 rest')
      | Just _ <- bulletItem line ->
          let (items, rest') = parseListItems bulletItem lines'
           in BlockDoc (DocUnorderedList (map (DocParagraph . parseInline) items)) : parseBlocks rest'
      | Just _ <- orderedItem line ->
          let (items, rest') = parseListItems (fmap snd . orderedItem) lines'
              numbers = [n | Just (n, _) <- map orderedItem (filter (isJustItem orderedItem) (take (length items) lines'))]
              numbered = zip (numbers <> [length numbers + 1 ..]) items
           in BlockDoc (DocOrderedList [(n, DocParagraph (parseInline item)) | (n, item) <- numbered]) : parseBlocks rest'
      | Just _ <- definitionItem line ->
          let (items, rest') = span (isJustItem definitionItem) lines'
              defs = [(parseInline term, parseInline def) | Just (term, def) <- map definitionItem items]
           in BlockDoc (DocDefList defs) : parseBlocks rest'
      | isTableLine line ->
          let (_, rest') = span (\l -> isTableLine l || T.isPrefixOf "|" (T.stripStart l)) lines'
           in BlockDoc (DocTable (Table [] [])) : parseBlocks rest'
      | otherwise ->
          let (paragraph, rest') = break isBlank lines'
              text = T.intercalate "\n" (T.stripStart line : drop 1 paragraph)
           in case sinceParagraph text of
                Just version -> BlockSince version : parseBlocks rest'
                Nothing -> BlockDoc (DocParagraph (parseInline text)) : parseBlocks rest'
  where
    isJustItem f l = case f l of
      Just _ -> True
      Nothing -> False

isBlank :: Text -> Bool
isBlank = T.all isSpace

headerLine :: Text -> Maybe (Int, Text)
headerLine line =
  let stripped = T.stripStart line
      (equals, rest) = T.span (== '=') stripped
      level = T.length equals
   in if level >= 1 && level <= 6 && T.isPrefixOf " " rest
        then Just (level, T.strip rest)
        else Nothing

isBirdTrack :: Text -> Bool
isBirdTrack line =
  case T.uncons (T.stripStart line) of
    Just ('>', rest) -> not (T.isPrefixOf ">>" rest)
    _ -> False

birdTrackText :: Text -> Text
birdTrackText line =
  case T.uncons (T.stripStart line) of
    Just ('>', rest) -> fromMaybe rest (T.stripPrefix " " rest)
    _ -> line

isExampleLine :: Text -> Bool
isExampleLine line = T.isPrefixOf ">>>" (T.stripStart line)

parseExamples :: [Text] -> ([Example], [Text])
parseExamples lines' =
  case lines' of
    line : rest
      | isExampleLine line ->
          let expression = T.strip (T.drop 3 (T.stripStart line))
              (results, rest') = break (\l -> isBlank l || isExampleLine l) rest
              resultLines = map exampleResultLine results
              (more, rest'') = parseExamples rest'
           in (Example expression resultLines : more, rest'')
    _ -> ([], lines')
  where
    exampleResultLine l =
      let stripped = T.stripStart l
       in if stripped == "<BLANKLINE>" then "" else stripped

propertyLine :: Text -> Maybe Text
propertyLine line = T.strip <$> T.stripPrefix "prop>" (T.stripStart line)

dropIndent :: Int -> Text -> Text
dropIndent n line =
  let (spaces, rest) = T.span (== ' ') line
   in T.drop (min n (T.length spaces)) spaces <> rest

bulletItem :: Text -> Maybe Text
bulletItem line =
  let stripped = T.stripStart line
   in case T.uncons stripped of
        Just (c, rest)
          | c == '*' || c == '-',
            Just body <- T.stripPrefix " " rest ->
              Just (T.strip body)
        _ -> Nothing

orderedItem :: Text -> Maybe (Int, Text)
orderedItem line =
  let stripped = T.stripStart line
      (digits, rest) = T.span isDigit stripped
   in case () of
        _
          | not (T.null digits),
            Just body <- T.stripPrefix ". " rest,
            Just n <- readMaybe (T.unpack digits) ->
              Just (n, T.strip body)
          | Just inner <- T.stripPrefix "(" stripped,
            (digits', rest') <- T.span isDigit inner,
            not (T.null digits'),
            Just body <- T.stripPrefix ") " rest',
            Just n <- readMaybe (T.unpack digits') ->
              Just (n, T.strip body)
          | otherwise -> Nothing

definitionItem :: Text -> Maybe (Text, Text)
definitionItem line = do
  inner <- T.stripPrefix "[" (T.stripStart line)
  let (term, rest) = T.breakOn "]" inner
  afterBracket <- T.stripPrefix "]" rest
  let def = T.strip (fromMaybe afterBracket (T.stripPrefix ":" afterBracket))
  if T.null term then Nothing else Just (T.strip term, def)

isTableLine :: Text -> Bool
isTableLine line = T.isPrefixOf "+-" (T.stripStart line) || T.isPrefixOf "+=" (T.stripStart line)

-- | Collect list items: an item line followed by indented continuation lines.
parseListItems :: (Text -> Maybe Text) -> [Text] -> ([Text], [Text])
parseListItems itemStart = go
  where
    go lines' =
      case lines' of
        line : rest
          | Just body <- itemStart line ->
              let (continuation, rest') = break (\l -> isBlank l || isJust' (itemStart l)) rest
                  item = T.intercalate "\n" (body : map T.strip continuation)
                  (items, rest'') = go rest'
               in (item : items, rest'')
        _ -> ([], lines')
    isJust' = isJust

sinceParagraph :: Text -> Maybe [Int]
sinceParagraph text = do
  rest <- T.stripPrefix "@since" text
  let versionText = T.strip rest
  if T.null versionText || T.any (== '\n') versionText
    then Nothing
    else mapM (readMaybe . T.unpack) (T.splitOn "." versionText)

-- Inline markup --------------------------------------------------------------

-- | Parse the inline markup of a paragraph.
parseInline :: Text -> Doc
parseInline = joinDocs . go
  where
    go text =
      case T.uncons text of
        Nothing -> []
        Just (c, rest) ->
          case c of
            '\\'
              | Just body <- T.stripPrefix "[" rest,
                Just (math, after) <- breakOnClose "\\]" body ->
                  DocMathDisplay math : go after
              | Just body <- T.stripPrefix "(" rest,
                Just (math, after) <- breakOnClose "\\)" body ->
                  DocMathInline math : go after
              | Just (escaped, after) <- T.uncons rest ->
                  DocString (T.singleton escaped) : go after
            '\''
              | Just (content, after) <- breakOnSameLine "'" rest,
                Just identifier <- identifierDoc content ->
                  identifier : go after
            '"'
              | Just (content, after) <- breakOnSameLine "\"" rest,
                isModuleName content ->
                  DocModule content : go after
            '/'
              | Just (content, after) <- breakOnSameLine "/" rest,
                not (T.null content) ->
                  DocEmphasis (parseInline content) : go after
            '_'
              | Just body <- T.stripPrefix "_" rest,
                Just (content, after) <- breakOnSameLine "__" body,
                not (T.null content) ->
                  DocBold (parseInline content) : go after
            '@'
              | Just (content, after) <- breakOnSameLine "@" rest ->
                  DocMonospaced (parseInline content) : go after
            '<'
              | Just (content, after) <- breakOnSameLine ">" rest,
                Just link <- hyperlinkDoc content ->
                  link : go after
            '#'
              | Just (content, after) <- breakOnSameLine "#" rest,
                not (T.null content),
                T.all (\x -> isAlphaNum x || x == '_' || x == '-') content ->
                  DocAName content : go after
            '!'
              | Just body <- T.stripPrefix "[" rest,
                Just (label, afterLabel) <- breakOnSameLine "](" body,
                Just (url, after) <- breakOnSameLine ")" afterLabel ->
                  DocPic url (if T.null label then Nothing else Just label) : go after
            _ ->
              let (plain, after) = T.span (not . isMarkupChar) rest
               in DocString (T.cons c plain) : go after

    isMarkupChar x = x `elem` ("\\'\"/_@<#!" :: String)

    breakOnClose close body =
      let (before, after) = T.breakOn close body
       in if T.null after then Nothing else Just (before, T.drop (T.length close) after)

    breakOnSameLine close body = do
      (before, after) <- breakOnClose close body
      if T.any (== '\n') before then Nothing else Just (before, after)

identifierDoc :: Text -> Maybe Doc
identifierDoc content =
  case splitQualifier (stripParens content) of
    Just (Nothing, name)
      | isIdentifier name -> Just (DocIdentifier name)
    Just (Just modName, name)
      | isIdentifier name -> Just (DocIdentifierUnchecked modName name)
    _ -> Nothing
  where
    stripParens text =
      fromMaybe text (T.stripPrefix "(" text >>= T.stripSuffix ")")

-- | Split @Data.List.map@ into its module qualifier and the name. Operators
-- containing dots, such as @.@ itself, stay unqualified.
splitQualifier :: Text -> Maybe (Maybe Text, Text)
splitQualifier text
  | T.null text = Nothing
  | otherwise =
      let segments = T.splitOn "." text
          (modules, rest) = span isConId (init segments)
       in if null modules || not (null rest)
            then Just (Nothing, text)
            else Just (Just (T.intercalate "." modules), T.intercalate "." (drop (length modules) segments))

isConId :: Text -> Bool
isConId segment =
  case T.uncons segment of
    Just (c, rest) -> isUpper c && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') rest
    Nothing -> False

isIdentifier :: Text -> Bool
isIdentifier name =
  case T.uncons name of
    Nothing -> False
    Just (c, rest)
      | isAlpha c || c == '_' -> T.all (\x -> isAlphaNum x || x == '_' || x == '\'' || x == '#') rest
      | otherwise -> T.all isOperatorChar name
  where
    isOperatorChar x = (isSymbol x || isPunctuation x) && x `notElem` ("()[]{},;`'\"_" :: String)

isModuleName :: Text -> Bool
isModuleName text = not (T.null text) && all isConId (T.splitOn "." text)

hyperlinkDoc :: Text -> Maybe Doc
hyperlinkDoc content =
  case T.words content of
    url : labelWords
      | not (T.null url),
        not (T.isPrefixOf " " content),
        T.any (== ':') url ->
          Just (DocHyperlink url (if null labelWords then Nothing else Just (DocString (T.unwords labelWords))))
    _ -> Nothing
