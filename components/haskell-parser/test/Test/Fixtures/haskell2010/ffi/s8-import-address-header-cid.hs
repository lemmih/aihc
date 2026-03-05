{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportAddressHeaderCid where
foreign import ccall "errno.h &errno" errnoPtr :: Ptr Int
