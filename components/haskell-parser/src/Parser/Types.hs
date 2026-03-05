module Parser.Types
  ( CoverageSlice (..),
    Expectation,
    ParseError (..),
    ParseResult (..),
    ParserConfig (..),
  )
where

import Data.Text (Text)

type Expectation = Text

newtype ParserConfig = ParserConfig
  { allowLineComments :: Bool
  }
  deriving (Eq, Show)

data ParseError = ParseError
  { offset :: !Int,
    line :: !Int,
    col :: !Int,
    expected :: ![Expectation],
    found :: !(Maybe Text)
  }
  deriving (Eq, Show)

data ParseResult a
  = ParseOk a
  | ParseErr ParseError
  deriving (Eq, Show)

data CoverageSlice
  = Lexing
  | ExprAtoms
  | ExprApp
  | Decls
  | Modules
  deriving (Eq, Ord, Show, Enum, Bounded)
