module Foreign.C.Types.Repr
  ( CIntPtrRep,
    CLongRep,
    CPtrdiffRep,
    CSizeRep,
    CUIntPtrRep,
    CULongRep,
  )
where

import Data.Int (Int64)
import Data.Word (Word64)

-- | Representation of pointer-width C types on an LP64 platform.
type CIntPtrRep = Int64

type CLongRep = Int64

type CPtrdiffRep = Int64

type CSizeRep = Word64

type CUIntPtrRep = Word64

type CULongRep = Word64
