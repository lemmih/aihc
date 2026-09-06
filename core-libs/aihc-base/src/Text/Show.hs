module Text.Show
  ( Show (..),
    ShowS,
    showChar,
    showListWith,
    showParen,
    shows,
    showString,
  )
where

import GHC.Show (showList__)
import Prelude (Show (..), ShowS, showChar, showParen, showString, shows)

-- | Show a list with a rendering for each element.
showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith = showList__
