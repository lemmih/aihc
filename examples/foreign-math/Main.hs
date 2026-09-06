{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Int (Int32 (..))
import Foreign.C.Types (CDouble (..), CInt (..))
import GHC.Int (Int (..))
import GHC.Prim
import GHC.Ptr (Ptr (..))
import GHC.Types (Double (..))
import System.IO (hPutBuf, stdout)

foreign import ccall unsafe "sin" c_sin :: CDouble -> CDouble

foreign import ccall unsafe "pow" c_pow :: CDouble -> CDouble -> CDouble

-- The classes of ldexp interleave, so it passes its double in the first
-- float register and its int in the first integer one.
foreign import ccall unsafe "ldexp" c_ldexp :: CDouble -> CInt -> CDouble

-- The values are whole numbers, so every result here is exact and the checks
-- need no tolerance.
double :: Int# -> CDouble
double value = CDouble (D# (int2Double# value))

integer :: CDouble -> Int
integer (CDouble (D# value)) = I# (double2Int# value)

main :: IO ()
main =
  if integer (c_sin (double 0#)) == 0
    && integer (c_pow (double 2#) (double 10#)) == 1024
    && integer (c_ldexp (double 3#) (CInt (I32# 4#Int32))) == 48
    then hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) 3
    else hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) 5
