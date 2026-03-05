{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeFrtypeOnly where
foreign import ccall "get_errno" getErrno :: Int
