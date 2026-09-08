{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module      : Data.Sections
-- Description : A module with export sections
-- Copyright   : (c) Example
-- License     : Unlicense
-- Maintainer  : someone@example.com
-- Stability   : experimental
--
-- The module documentation follows the header fields. It links to
-- 'Counter' and to "Data.Sections".
module Data.Sections
  ( -- * Counters
    -- $intro
    Counter (..),
    newCounter,
    -- ** Operations
    increment,
    (|+|),
    -- * Pairs
    Pair,
    Tagged (Tagged, tagOf),
    -- | An inline documentation chunk between two items.
    Countable (..),
    legacy,
  )
where

-- $intro
-- Counters wrap an 'Int'. They /never/ go below zero.

-- | A counter.
--
-- @since 1.1.0
newtype Counter = Counter
  { getCounter :: Int
    -- ^ the current count
  }
  deriving (Eq, Ord, Show)

-- | A fresh counter at zero.
newCounter :: Counter
newCounter = Counter 0

-- | Add one.
increment ::
  Counter ->
  -- | the incremented counter
  Counter
increment (Counter n) = Counter (n + 1)

infixl 6 |+|

-- | Add two counters.
(|+|) :: Counter -> Counter -> Counter
Counter a |+| Counter b = Counter (a + b)

-- | A pair of the same thing.
type Pair a = (a, a)

-- | A value with a tag.
data Tagged a
  = Tagged
      { tagOf :: String, -- ^ the tag
        valueOf :: a -- ^ the value
      }
  | Untagged a
  deriving (Show)

deriving instance Eq a => Eq (Tagged a)

-- | Things that can be counted.
class Countable a where
  -- | Count the elements.
  count :: a -> Int
  count _ = 0

  -- | Whether there is nothing to count.
  isEmpty :: a -> Bool
  isEmpty x = count x == 0

  {-# MINIMAL count #-}

instance Countable Counter where
  count = getCounter

-- | Kept for compatibility.
legacy :: Int -> Int
legacy = id
{-# DEPRECATED legacy "use 'increment' instead" #-}
