{-# LANGUAGE MagicHash #-}

-- The suggested replacement names a value of the base library, which this
-- module must not use.
{- HLINT ignore "Use foldr" -}

-- | The 'Show' class. GHC defines it in @GHC.Internal.Show@; aihc keeps it
-- in the prim package because the type checker recognizes a stock class by
-- its package, module and name, and it may name the prim package only.
--
-- The instances and the helper functions stay in @GHC.Show@.
module GHC.Prim.Show
  ( Show (..),
    ShowS,
  )
where

import GHC.Prim.Base (String)
import GHC.Types (Char, Int (..), List (..))

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

  showsPrec _ value = showAppend (show value)
  show value = showsPrec (I# 0#) value []
  showList = showPrimList

-- | The default rendering of a list. @GHC.Show@ exports the same function
-- as @showList__ shows@; this copy keeps the class default inside the prim
-- package.
showPrimList :: (Show a) => [a] -> ShowS
showPrimList [] suffix = '[' : ']' : suffix
showPrimList (value : values) suffix =
  '[' : showsPrec (I# 0#) value (showPrimListTail values suffix)

showPrimListTail :: (Show a) => [a] -> ShowS
showPrimListTail [] suffix = ']' : suffix
showPrimListTail (value : values) suffix =
  ',' : showsPrec (I# 0#) value (showPrimListTail values suffix)

-- | List append. @(++)@ belongs to the base library.
showAppend :: [a] -> [a] -> [a]
showAppend [] suffix = suffix
showAppend (value : values) suffix = value : showAppend values suffix
