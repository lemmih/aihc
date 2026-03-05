{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportAddressOnly where
foreign import ccall "&" errnoPtr :: Ptr Int
