{-# LANGUAGE TypeFamilies #-}

module Parser.Types
  ( TokStream (..),
    CoverageSlice (..),
    ParseErrorBundle,
    ParseResult (..),
    ParserConfig (..),
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Void (Void)
import Parser.Ast (SourceSpan (..))
import Parser.Lexer (LexToken (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MPE
import Text.Megaparsec.Pos (SourcePos (..), mkPos)
import Text.Megaparsec.Stream (Stream (..), TraversableStream (..), VisualStream (..))

-- | Parse error from token parser. Use 'errorBundlePretty' from "Parser" to render.
type ParseErrorBundle = MPE.ParseErrorBundle TokStream Void

newtype TokStream = TokStream
  { unTokStream :: [LexToken]
  }
  deriving (Eq, Ord, Show)

instance Stream TokStream where
  type Token TokStream = LexToken
  type Tokens TokStream = [LexToken]

  tokenToChunk _ tok = [tok]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null

  take1_ (TokStream toks) =
    case toks of
      [] -> Nothing
      tok : rest -> Just (tok, TokStream rest)

  takeN_ n (TokStream toks)
    | n <= 0 = Just ([], TokStream toks)
    | null toks = Nothing
    | otherwise =
        let (chunk, rest) = splitAt n toks
         in Just (chunk, TokStream rest)

  takeWhile_ f (TokStream toks) =
    let (chunk, rest) = span f toks
     in (chunk, TokStream rest)

instance VisualStream TokStream where
  showTokens _ toks =
    T.unpack (T.intercalate (T.pack " ") [lexTokenText tok | tok <- NE.toList toks])

instance TraversableStream TokStream where
  reachOffset o pst =
    let currOff = MP.pstateOffset pst
        currInput = unTokStream (MP.pstateInput pst)
        advance = max 0 (o - currOff)
        (consumed, rest) = splitAt advance currInput
        currPos = MP.pstateSourcePos pst
        newPos =
          case rest of
            tok : _ -> sourcePosFromStartSpan (sourceName currPos) (lexTokenSpan tok)
            [] ->
              case reverse consumed of
                tok : _ -> sourcePosFromEndSpan (sourceName currPos) (lexTokenSpan tok)
                [] -> currPos
        pst' =
          pst
            { MP.pstateInput = TokStream rest,
              MP.pstateOffset = currOff + advance,
              MP.pstateSourcePos = newPos
            }
     in (Nothing, pst')

sourcePosFromStartSpan :: FilePath -> SourceSpan -> SourcePos
sourcePosFromStartSpan file span' =
  case span' of
    SourceSpan line col _ _ -> SourcePos file (mkPos (max 1 line)) (mkPos (max 1 col))
    NoSourceSpan -> SourcePos file (mkPos 1) (mkPos 1)

sourcePosFromEndSpan :: FilePath -> SourceSpan -> SourcePos
sourcePosFromEndSpan file span' =
  case span' of
    SourceSpan _ _ line col -> SourcePos file (mkPos (max 1 line)) (mkPos (max 1 col))
    NoSourceSpan -> SourcePos file (mkPos 1) (mkPos 1)

newtype ParserConfig = ParserConfig
  { allowLineComments :: Bool
  }
  deriving (Eq, Show)

data ParseResult a
  = ParseOk a
  | ParseErr ParseErrorBundle
  deriving (Eq, Show)

data CoverageSlice
  = Lexing
  | ExprAtoms
  | ExprApp
  | Decls
  | Modules
  deriving (Eq, Ord, Show, Enum, Bounded)
