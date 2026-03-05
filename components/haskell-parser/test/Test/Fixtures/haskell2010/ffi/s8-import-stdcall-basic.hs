{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStdcallBasic where
foreign import stdcall "puts" s_puts :: String -> IO Int
