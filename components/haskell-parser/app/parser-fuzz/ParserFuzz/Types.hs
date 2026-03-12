module ParserFuzz.Types
  ( SearchResult (..),
    Candidate (..),
  )
where

import Language.Haskell.Exts qualified as HSE

data SearchResult = SearchResult
  { srTestsTried :: Int,
    srCandidate :: Candidate
  }

data Candidate = Candidate
  { candAst :: HSE.Module HSE.SrcSpanInfo,
    candSource :: String
  }
