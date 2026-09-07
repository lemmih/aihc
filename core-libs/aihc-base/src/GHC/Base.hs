{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Base
  ( module GHC.Prim.Base,
    module GHC.Prim,
    ord,
    unsafeChr,
    build,
    augment,
    unpackCString#,
    unpackCStringUtf8#,
    unpackFoldrCString#,
    ($),
    id,
    const,
    flip,
    (.),
    (++),
    foldr,
  )
where

import GHC.CString (unpackCString#, unpackCStringUtf8#, unpackFoldrCString#)
import GHC.Int (Int (..))
import GHC.Prim
import GHC.Prim.Base
import GHC.Types (Char (..), RuntimeRep, TYPE, Type)

-- | Convert a code point to a character without a range check.
unsafeChr :: Int -> Char
unsafeChr (I# value) = C# (chr# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build generate = generate (:) []

augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
augment generate = generate (:)

id :: a -> a
id x = x

const :: a -> b -> a
const value _ = value

flip :: (a -> b -> c) -> b -> a -> c
flip function right left = function left right

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = compose
  where
    compose value = f (g value)

infixr 9 .

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

infixr 5 ++

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ initial [] = initial
foldr combine initial (value : values) = combine value (foldr combine initial values)

($) :: forall (r :: RuntimeRep) (a :: Type) (b :: TYPE r). (a -> b) -> a -> b
($) function = function

infixr 0 $
