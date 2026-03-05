{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportCcallSafe where
foreign import ccall safe "puts" c_puts_safe :: String -> IO Int
