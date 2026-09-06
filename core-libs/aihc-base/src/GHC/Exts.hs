{-# LANGUAGE MagicHash #-}

module GHC.Exts
  ( module GHC.Prim,
    IsList (..),
    Item,
    IsString (..),
    Char (..),
    Int8 (..),
    Int16 (..),
    Int32 (..),
    Int64 (..),
    FunPtr (..),
    Constraint,
    coerce,
    lazy,
    inline,
    oneShot,
    runRW#,
    build,
    augment,
    Addr#,
    ByteArray#,
    copyAddrToByteArray#,
    Int#,
    MutableByteArray#,
    RealWorld,
    StablePtr#,
    Word#,
    RuntimeRep (..),
    Levity (..),
    TYPE,
    Ptr (..),
    Int (..),
    Float (..),
    Double (..),
    Word (..),
    Word8 (..),
    Word16 (..),
    Word32 (..),
    Word64 (..),
    isTrue#,
    and#,
    getSizeofMutableByteArray#,
    indexAddrArray#,
    indexDoubleArray#,
    indexFloatArray#,
    indexInt8Array#,
    indexInt16Array#,
    indexInt32Array#,
    indexInt64Array#,
    indexIntArray#,
    indexStablePtrArray#,
    indexWideCharArray#,
    indexWord8Array#,
    indexWord16Array#,
    indexWord32Array#,
    indexWord64Array#,
    indexWordArray#,
    int2Word#,
    neWord#,
    newByteArray#,
    nullAddr#,
    or#,
    readAddrArray#,
    readDoubleArray#,
    readFloatArray#,
    readInt8Array#,
    readInt16Array#,
    readInt32Array#,
    readInt64Array#,
    readIntArray#,
    readStablePtrArray#,
    readWideCharArray#,
    readWord8Array#,
    readWord16Array#,
    readWord32Array#,
    readWord64Array#,
    readWordArray#,
    sameMutableByteArray#,
    setByteArray#,
    sizeofByteArray#,
    uncheckedIShiftL#,
    uncheckedIShiftRA#,
    uncheckedIShiftRL#,
    uncheckedShiftL#,
    unsafeCoerce#,
    unsafeFreezeByteArray#,
    word2Int#,
    word8ToWord#,
    writeAddrArray#,
    writeDoubleArray#,
    writeFloatArray#,
    writeInt8Array#,
    writeInt16Array#,
    writeInt32Array#,
    writeInt64Array#,
    writeIntArray#,
    writeStablePtrArray#,
    writeWideCharArray#,
    writeWord8Array#,
    writeWord16Array#,
    writeWord32Array#,
    writeWord64Array#,
    writeWordArray#,
    xor#,
  )
where

import Data.Coerce (coerce)
import Data.String (IsString (..))
import GHC.Base (augment, build)
import GHC.Int (Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Internal.Char (Char (..))
import GHC.IsList (IsList (..))
import GHC.Prim
import GHC.Ptr (FunPtr (..), Ptr (..))
import GHC.Types (Bool (..), Constraint, Double (..), Float (..), Int (..), Levity (..), RuntimeRep (..), TYPE, isTrue#)
import GHC.Word (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))

-- | The value is returned unchanged. Strictness analysis does not apply.
lazy :: a -> a
lazy value = value

-- | The value is returned unchanged. Inlining hints do not apply.
inline :: a -> a
inline value = value

-- | The function is returned unchanged. Arity hints do not apply.
oneShot :: (a -> b) -> a -> b
oneShot function = function
