{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foreign.C.Types
  ( CBool (..),
    CChar (..),
    CClock (..),
    CDouble (..),
    CFile,
    CFloat (..),
    CFpos,
    CInt (..),
    CIntMax (..),
    CIntPtr (..),
    CJmpBuf,
    CLLong (..),
    CLong (..),
    CPtrdiff (..),
    CSChar (..),
    CSUSeconds (..),
    CShort (..),
    CSigAtomic (..),
    CSize (..),
    CTime (..),
    CUChar (..),
    CUInt (..),
    CUIntMax (..),
    CUIntPtr (..),
    CULLong (..),
    CULong (..),
    CUSeconds (..),
    CUShort (..),
    CWchar (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types.Repr
  ( CIntPtrRep,
    CLongRep,
    CPtrdiffRep,
    CSizeRep,
    CUIntPtrRep,
    CULongRep,
  )
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Float ()
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Real (Integral (..), Real (..))
import GHC.Types (Double, Float)

newtype CBool = CBool Word8
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CChar = CChar Int8
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CClock = CClock Word64
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CDouble = CDouble Double
  deriving newtype (Eq, Ord, Num)

data CFile = CFile

newtype CFloat = CFloat Float
  deriving newtype (Eq, Ord, Num)

data CFpos = CFpos

newtype CInt = CInt Int32
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CIntMax = CIntMax Int64
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CIntPtr = CIntPtr CIntPtrRep
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

data CJmpBuf = CJmpBuf

newtype CLLong = CLLong Int64
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CLong = CLong CLongRep
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CPtrdiff = CPtrdiff CPtrdiffRep
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CSChar = CSChar Int8
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CSUSeconds = CSUSeconds Int32
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CShort = CShort Int16
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CSigAtomic = CSigAtomic Int32
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CSize = CSize CSizeRep
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CTime = CTime Int64
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CUChar = CUChar Word8
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CUInt = CUInt Word32
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CUIntMax = CUIntMax Word64
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CUIntPtr = CUIntPtr CUIntPtrRep
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CULLong = CULLong Word64
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CULong = CULong CULongRep
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CUSeconds = CUSeconds Word32
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CUShort = CUShort Word16
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)

newtype CWchar = CWchar Int32
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral)
