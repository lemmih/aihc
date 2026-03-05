{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportCcallBasic where
foreign import ccall "puts" c_puts :: String -> IO Int
