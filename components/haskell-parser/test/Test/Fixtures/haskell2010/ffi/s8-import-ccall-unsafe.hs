{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportCcallUnsafe where
foreign import ccall unsafe "puts" c_puts_unsafe :: String -> IO Int
