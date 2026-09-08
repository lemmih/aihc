{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Comment
-- Description : Documentation comments and their attachment points
--
-- The parser drops comments, so documentation comments are recovered from the
-- token stream and attached to syntax by position. Every doc comment records
-- the code token before it and the code token after it. A @-- |@ comment
-- documents the node that starts at the following token; a @-- ^@ comment
-- documents the innermost eligible node that contains the preceding token.
--
-- GHC lets doc comments take part in layout: a @-- ^@ at or left of the
-- enclosing block's indentation closes that block. 'claimPrevWithin' emulates
-- this with a column check, so a comment at column one after a class body
-- documents the class rather than the last method.
module Aihc.Haddock.Comment
  ( DocCommentKind (..),
    DocComment (..),
    collectDocComments,
    CommentIndex,
    buildCommentIndex,
    claimNextAt,
    claimPrevWithin,
    claimBetween,
    namedChunkComments,
    unclaimedComments,
    spanOffsets,
    spanStartColumn,
  )
where

import Aihc.Parser.Syntax (Extension, SourceSpan (..))
import Aihc.Parser.Token
  ( LexToken (..),
    LexTokenKind (..),
    TokenOrigin (..),
    lexModuleTokensWithSourceNameAndExtensions,
  )
import Data.Char (isAlphaNum)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data DocCommentKind
  = -- | @-- |@ or @{- | -}@: documents the following node.
    DocCommentNext
  | -- | @-- ^@ or @{- ^ -}@: documents the preceding node.
    DocCommentPrev
  | -- | @-- $name@: a named chunk referenced from the export list.
    DocCommentNamed Text
  | -- | @-- *@, @-- **@ ...: a section heading in the export list.
    DocCommentSection Int
  deriving (Eq, Show)

data DocComment = DocComment
  { docCommentKind :: DocCommentKind,
    docCommentSpan :: SourceSpan,
    -- | The comment text with the comment syntax and the marker removed,
    -- lines joined with newlines. Leading whitespace on continuation lines
    -- is kept because the markup parser needs it.
    docCommentText :: Text,
    -- | The first code token after the comment.
    docCommentNextToken :: Maybe SourceSpan,
    -- | The last code token before the comment.
    docCommentPrevToken :: Maybe SourceSpan
  }
  deriving (Eq, Show)

-- | Lex the module and return its documentation comments in source order.
collectDocComments :: FilePath -> [Extension] -> Text -> [DocComment]
collectDocComments sourceName extensions input =
  go Nothing (filter fromSource (lexModuleTokensWithSourceNameAndExtensions sourceName extensions input))
  where
    fromSource tok = lexTokenOrigin tok == FromSource

    go _ [] = []
    go prevCode (tok : rest)
      -- A @-- ^@ after the comma that ends a record field documents that
      -- field, so separators do not count as the preceding token.
      | isCodeToken tok && lexTokenText tok `elem` [",", ";"] = go prevCode rest
      | isCodeToken tok = go (Just (lexTokenSpan tok)) rest
      | otherwise =
          case docCommentStart tok of
            Nothing -> go prevCode rest
            Just (kind, firstLine) ->
              let (continuation, rest') = takeContinuation tok rest
                  lastTok = if null continuation then tok else last continuation
                  text = T.intercalate "\n" (firstLine : map continuationText continuation)
                  comment =
                    DocComment
                      { docCommentKind = kind,
                        docCommentText = text,
                        docCommentSpan = mergeSpans (lexTokenSpan tok) (lexTokenSpan lastTok),
                        docCommentNextToken = lexTokenSpan <$> firstCodeToken rest',
                        docCommentPrevToken = prevCode
                      }
               in comment : go prevCode rest'

    takeContinuation start toks =
      let step (prev, acc) remaining =
            case remaining of
              tok : more
                | lexTokenKind tok == TkLineComment,
                  startLineOf tok == endLineOf prev + 1,
                  not (startsNewDocComment tok) ->
                    step (tok, tok : acc) more
              _ -> (reverse acc, remaining)
       in step (start, []) toks

    firstCodeToken toks =
      case filter isCodeToken toks of
        tok : _ -> Just tok
        [] -> Nothing

-- | GHC's lexer continues a documentation comment over every following
-- @--@ line except a @-- $name@ chunk header. A @-- |@ or @-- *@ line directly
-- below a doc line is therefore markup inside it, such as a table row or a
-- bullet.
startsNewDocComment :: LexToken -> Bool
startsNewDocComment tok =
  case docCommentStart tok of
    Just (DocCommentNamed _, _) -> True
    _ -> False

isCodeToken :: LexToken -> Bool
isCodeToken tok =
  case lexTokenKind tok of
    TkLineComment -> False
    TkBlockComment -> False
    TkPragma _ -> False
    TkEOF -> False
    TkError _ -> False
    _ -> True

-- | Classify a comment token. Returns the kind and the text of its first line
-- with the marker removed.
docCommentStart :: LexToken -> Maybe (DocCommentKind, Text)
docCommentStart tok =
  case lexTokenKind tok of
    TkLineComment ->
      let body = T.dropWhile (== '-') (lexTokenText tok)
       in case T.uncons body of
            Just (' ', rest) -> classifyMarker rest
            _ -> Nothing
    TkBlockComment ->
      let inner = T.dropEnd 2 (T.drop 2 (lexTokenText tok))
          body = case T.uncons inner of
            Just (' ', rest) -> rest
            _ -> inner
       in classifyMarker body
    _ -> Nothing
  where
    classifyMarker body =
      case T.uncons body of
        Just ('|', rest) -> Just (DocCommentNext, dropOneSpace rest)
        Just ('^', rest) -> Just (DocCommentPrev, dropOneSpace rest)
        Just ('$', rest)
          | (name, remaining) <- T.span isChunkNameChar rest,
            not (T.null name) ->
              Just (DocCommentNamed name, dropOneSpace remaining)
        Just ('*', rest) ->
          let (stars, remaining) = T.span (== '*') rest
           in Just (DocCommentSection (1 + T.length stars), T.stripStart remaining)
        _ -> Nothing

    isChunkNameChar c = isAlphaNum c || c == '_' || c == '\''

    dropOneSpace text =
      case T.uncons text of
        Just (' ', rest) -> rest
        _ -> text

continuationText :: LexToken -> Text
continuationText tok = T.dropWhile (== '-') (lexTokenText tok)

startLineOf :: LexToken -> Int
startLineOf tok =
  case lexTokenSpan tok of
    SourceSpan {sourceSpanStartLine = line} -> line
    NoSourceSpan -> 0

endLineOf :: LexToken -> Int
endLineOf tok =
  case lexTokenSpan tok of
    SourceSpan {sourceSpanEndLine = line} -> line
    NoSourceSpan -> 0

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans first second =
  case (first, second) of
    (SourceSpan name l1 c1 _ _ startOffset _, SourceSpan _ _ _ l2 c2 _ endOffset) ->
      SourceSpan name l1 c1 l2 c2 startOffset endOffset
    (NoSourceSpan, other) -> other
    (other, _) -> other

spanOffsets :: SourceSpan -> Maybe (Int, Int)
spanOffsets sp =
  case sp of
    SourceSpan {sourceSpanStartOffset = start, sourceSpanEndOffset = end} -> Just (start, end)
    NoSourceSpan -> Nothing

spanStartColumn :: SourceSpan -> Int
spanStartColumn sp =
  case sp of
    SourceSpan {sourceSpanStartCol = col} -> col
    NoSourceSpan -> 0

-- Attachment index -------------------------------------------------------------

-- | Doc comments waiting to be attached. Claiming a comment removes it, so a
-- parent that claims a @-- |@ comment hides it from its children, and a child
-- that claims a @-- ^@ comment hides it from its parent.
data CommentIndex = CommentIndex
  { indexNext :: Map Int [DocComment],
    indexPrev :: [DocComment],
    indexNamed :: [DocComment],
    indexSections :: [DocComment]
  }

buildCommentIndex :: [DocComment] -> CommentIndex
buildCommentIndex comments =
  CommentIndex
    { indexNext = Map.fromListWith (flip (<>)) [(start, [c]) | c <- nexts, Just (start, _) <- [docCommentNextToken c >>= spanOffsets]],
      indexPrev = prevs,
      indexNamed = named,
      indexSections = sections
    }
  where
    nexts = filter ((== DocCommentNext) . docCommentKind) comments
    prevs = filter ((== DocCommentPrev) . docCommentKind) comments
    named = [c | c <- comments, DocCommentNamed _ <- [docCommentKind c]]
    sections = [c | c <- comments, DocCommentSection _ <- [docCommentKind c]]

-- | Claim the @-- |@ comments whose following token starts at the given
-- offset.
claimNextAt :: Int -> CommentIndex -> ([DocComment], CommentIndex)
claimNextAt offset index =
  case Map.lookup offset (indexNext index) of
    Nothing -> ([], index)
    Just comments -> (comments, index {indexNext = Map.delete offset (indexNext index)})

-- | Claim the @-- ^@ comments whose preceding token lies inside the node span
-- and that are indented past the enclosing item's column. Call this for
-- children before their parent so the innermost node wins.
claimPrevWithin :: Int -> SourceSpan -> CommentIndex -> ([DocComment], CommentIndex)
claimPrevWithin parentColumn nodeSpan index =
  case spanOffsets nodeSpan of
    Nothing -> ([], index)
    Just (nodeStart, nodeEnd) ->
      let eligible comment =
            spanStartColumn (docCommentSpan comment) > parentColumn
              && case docCommentPrevToken comment >>= spanOffsets of
                Just (tokStart, tokEnd) -> nodeStart <= tokStart && tokEnd <= nodeEnd
                Nothing -> False
          (claimed, remaining) = partition eligible (indexPrev index)
       in (claimed, index {indexPrev = remaining})

-- | Claim every section heading, named-chunk reference and @-- |@ comment
-- located inside the span, in source order. Used for the export list, where
-- comments sit between export items.
claimBetween :: SourceSpan -> CommentIndex -> ([DocComment], CommentIndex)
claimBetween sp index =
  case spanOffsets sp of
    Nothing -> ([], index)
    Just (start, end) ->
      let inside comment =
            case spanOffsets (docCommentSpan comment) of
              Just (cStart, cEnd) -> start <= cStart && cEnd <= end
              Nothing -> False
          (sections, sections') = partition inside (indexSections index)
          (named, named') = partition inside (indexNamed index)
          nextsInside = [c | cs <- Map.elems (indexNext index), c <- cs, inside c]
          nexts' = Map.filter (not . null) (Map.map (filter (not . inside)) (indexNext index))
          claimed = sortBySpan (sections <> named <> nextsInside)
       in (claimed, index {indexSections = sections', indexNamed = named', indexNext = nexts'})
  where
    sortBySpan = map snd . Map.toAscList . Map.fromList . map (\c -> (fromMaybe (0, 0) (spanOffsets (docCommentSpan c)), c))

-- | Named chunks that were not consumed by an export list. They define the
-- module's @$name@ chunks.
namedChunkComments :: CommentIndex -> [(Text, DocComment)]
namedChunkComments index =
  [(name, c) | c <- indexNamed index, DocCommentNamed name <- [docCommentKind c]]

-- | Everything that never found a node.
unclaimedComments :: CommentIndex -> [DocComment]
unclaimedComments index =
  concat (Map.elems (indexNext index)) <> indexPrev index <> indexSections index
