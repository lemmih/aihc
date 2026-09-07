{-# LANGUAGE MagicHash #-}

-- | Unpacking of NUL-terminated C string literals. String literals desugar
-- to calls of these functions on an @Addr#@ literal. A literal made only of
-- characters between @\\1@ and @\\127@ is stored as Latin-1 and read by
-- 'unpackCString#'; any other literal is stored as modified UTF-8, where the
-- NUL character is encoded as the overlong two-byte sequence @0xC0 0x80@ so
-- that it never terminates the string, and read by 'unpackCStringUtf8#'.
module GHC.CString
  ( unpackCString#,
    unpackAppendCString#,
    unpackFoldrCString#,
    unpackCStringUtf8#,
    unpackAppendCStringUtf8#,
    unpackFoldrCStringUtf8#,
    unpackNBytes#,
    cstringLength#,
  )
where

import GHC.Prim
import GHC.Types (Bool (..), Char (..), isTrue#)

-- | Read one byte of a C string as an integer.
byteAt :: Addr# -> Int# -> Int#
byteAt address index = word2Int# (word8ToWord# (indexWord8OffAddr# address index))

-- | The character with the given code point.
chrAt :: Int# -> Char
chrAt code = C# (chr# code)

-- | Unpack a NUL-terminated Latin-1 C string.
unpackCString# :: Addr# -> [Char]
unpackCString# address = unpackLatin1From address 0#

unpackLatin1From :: Addr# -> Int# -> [Char]
unpackLatin1From address index =
  case byteAt address index of
    0# -> []
    code -> chrAt code : unpackLatin1From address (index +# 1#)

-- | Unpack a NUL-terminated Latin-1 C string in front of a list.
unpackAppendCString# :: Addr# -> [Char] -> [Char]
unpackAppendCString# address = unpackFoldrCString# address (:)

-- | Fold over the characters of a NUL-terminated Latin-1 C string.
unpackFoldrCString# :: Addr# -> (Char -> a -> a) -> a -> a
unpackFoldrCString# address = foldrLatin1From address 0#

foldrLatin1From :: Addr# -> Int# -> (Char -> a -> a) -> a -> a
foldrLatin1From address index combine initial =
  case byteAt address index of
    0# -> initial
    code -> combine (chrAt code) (foldrLatin1From address (index +# 1#) combine initial)

-- | Unpack a NUL-terminated modified UTF-8 C string.
unpackCStringUtf8# :: Addr# -> [Char]
unpackCStringUtf8# address = unpackUtf8From address 0#

unpackUtf8From :: Addr# -> Int# -> [Char]
unpackUtf8From address index =
  case byteAt address index of
    0# -> []
    leading ->
      chrAt (utf8CodePoint address index leading)
        : unpackUtf8From address (index +# utf8Width leading)

-- | Unpack a NUL-terminated modified UTF-8 C string in front of a list.
unpackAppendCStringUtf8# :: Addr# -> [Char] -> [Char]
unpackAppendCStringUtf8# address = unpackFoldrCStringUtf8# address (:)

-- | Fold over the characters of a NUL-terminated modified UTF-8 C string.
unpackFoldrCStringUtf8# :: Addr# -> (Char -> a -> a) -> a -> a
unpackFoldrCStringUtf8# address = foldrUtf8From address 0#

foldrUtf8From :: Addr# -> Int# -> (Char -> a -> a) -> a -> a
foldrUtf8From address index combine initial =
  case byteAt address index of
    0# -> initial
    leading ->
      combine
        (chrAt (utf8CodePoint address index leading))
        (foldrUtf8From address (index +# utf8Width leading) combine initial)

-- | Unpack the given number of Latin-1 bytes, which need not end in NUL.
unpackNBytes# :: Addr# -> Int# -> [Char]
unpackNBytes# address = unpackNBytesFrom address 0#

unpackNBytesFrom :: Addr# -> Int# -> Int# -> [Char]
unpackNBytesFrom address index count =
  case isTrue# (index >=# count) of
    True -> []
    False -> chrAt (byteAt address index) : unpackNBytesFrom address (index +# 1#) count

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
