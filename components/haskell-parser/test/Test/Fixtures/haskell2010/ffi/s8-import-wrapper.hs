{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportWrapper where
foreign import ccall "wrapper" wrapFun :: (Int -> IO Int) -> IO (Ptr (Int -> IO Int))
