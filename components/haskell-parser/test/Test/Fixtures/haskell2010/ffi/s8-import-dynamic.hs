{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportDynamic where
foreign import ccall "dynamic" mkFun :: Ptr (Int -> IO Int) -> (Int -> IO Int)
