{-# LANGUAGE TypeFamilies, PatternSynonyms #-}
-- | Module doc.
module Markup (Thing (..), markup, identity, T, C (..), (%%), F, pattern P) where

-- | A thing.
data Thing
  = -- | The A thing.
    A Int
    -- ^ trailing A doc
  | B {
      -- | field doc before
      fieldB :: Int
    }

-- | A type synonym.
type T = Thing

-- | A class.
class C a where
  -- | method
  meth :: a -> Int

-- | Function with markup.
--
-- Uses 'identity' and "Data.Maybe" and 'Data.List.map', also @mono@, /emph/, __bold__.
--
-- = Header
--
-- == Sub header
--
-- * bullet one
-- * bullet two
--
-- 1. first
-- 2. second
--
-- [term]: definition
--
-- > birdtrack code
-- > more
--
-- <http://example.com>
--
-- <http://example.com link label>
--
-- \[ math display \]
--
-- prop> markup x == x
--
-- #anchor#
--
-- ![pic](http://example.com/p.png)
--
-- +-----+-----+
-- | a   | b   |
-- +=====+=====+
-- | c   | d   |
-- +-----+-----+
--
-- Third paragraph with a
-- line break. Escaped \/slash\/ and unicode: λ.
markup :: Int -> Int
markup x = x

-- | @since 1.2.3
identity :: a -> a
identity a = a

-- | An operator.
(%%) :: Int -> Int -> Int
(%%) = (+)
infixr 5 %%

-- | A type family.
type family F a where
  F Int = Bool

-- | A pattern synonym.
pattern P :: Int -> Thing
pattern P x = A x
