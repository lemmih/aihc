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

import GHC.Int (Int (..))
import GHC.Prim
import GHC.Prim.Base
import GHC.Types (Bool (..), Char (..), RuntimeRep, TYPE, Type, isTrue#)

-- | Convert a code point to a character without a range check.
unsafeChr :: Int -> Char
unsafeChr (I# value) = C# (chr# value)

ord :: Char -> Int
ord (C# value) = I# (ord# value)

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build generate = generate (:) []

augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
augment generate = generate (:)

-- | Read one byte of a C string as an integer.
byteAt :: Addr# -> Int# -> Int#
byteAt address index = word2Int# (word8ToWord# (indexWord8OffAddr# address index))

-- | Unpack a NUL-terminated Latin-1 C string.
unpackCString# :: Addr# -> [Char]
unpackCString# address = unpackLatin1From address 0#

unpackLatin1From :: Addr# -> Int# -> [Char]
unpackLatin1From address index =
  case byteAt address index of
    0# -> []
    code -> unsafeChr (I# code) : unpackLatin1From address (index +# 1#)

-- | Unpack a NUL-terminated UTF-8 C string.
unpackCStringUtf8# :: Addr# -> [Char]
unpackCStringUtf8# address = unpackUtf8From address 0#

unpackUtf8From :: Addr# -> Int# -> [Char]
unpackUtf8From address index =
  case byteAt address index of
    0# -> []
    leading ->
      unsafeChr (I# (utf8CodePoint address index leading))
        : unpackUtf8From address (index +# utf8Width leading)

-- | Give the byte count of one UTF-8 sequence from its first byte.
utf8Width :: Int# -> Int#
utf8Width leading =
  case isTrue# (leading <# 192#) of
    True -> 1#
    False ->
      case isTrue# (leading <# 224#) of
        True -> 2#
        False ->
          case isTrue# (leading <# 240#) of
            True -> 3#
            False -> 4#

-- | Decode one UTF-8 sequence that starts at the given index.
utf8CodePoint :: Addr# -> Int# -> Int# -> Int#
utf8CodePoint address index leading =
  case isTrue# (leading <# 192#) of
    True -> leading
    False ->
      case isTrue# (leading <# 224#) of
        True -> utf8Continue address index 1# 1# (leading -# 192#)
        False ->
          case isTrue# (leading <# 240#) of
            True -> utf8Continue address index 1# 2# (leading -# 224#)
            False -> utf8Continue address index 1# 3# (leading -# 240#)

-- | Add the remaining UTF-8 continuation bytes to a partial code point.
utf8Continue :: Addr# -> Int# -> Int# -> Int# -> Int# -> Int#
utf8Continue address index offset remaining accumulated =
  case remaining of
    0# -> accumulated
    _ ->
      case byteAt address (index +# offset) of
        continuation ->
          utf8Continue
            address
            index
            (offset +# 1#)
            (remaining -# 1#)
            (accumulated *# 64# +# (continuation -# 128#))

-- | Fold over the characters of a NUL-terminated Latin-1 C string.
unpackFoldrCString# :: Addr# -> (Char -> a -> a) -> a -> a
unpackFoldrCString# address = foldrLatin1From address 0#

foldrLatin1From :: Addr# -> Int# -> (Char -> a -> a) -> a -> a
foldrLatin1From address index combine initial =
  case byteAt address index of
    0# -> initial
    code -> combine (unsafeChr (I# code)) (foldrLatin1From address (index +# 1#) combine initial)

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
