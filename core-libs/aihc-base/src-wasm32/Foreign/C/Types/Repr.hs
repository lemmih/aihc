module Foreign.C.Types.Repr
  ( CIntPtrRep,
    CLongRep,
    CPtrdiffRep,
    CSizeRep,
    CUIntPtrRep,
    CULongRep,
  )
where

import Data.Int (Int32)
import Data.Word (Word32)

-- | Representation of pointer-width C types on wasm32 ILP32.
type CIntPtrRep = Int32

type CLongRep = Int32

type CPtrdiffRep = Int32

type CSizeRep = Word32

type CUIntPtrRep = Word32

type CULongRep = Word32
