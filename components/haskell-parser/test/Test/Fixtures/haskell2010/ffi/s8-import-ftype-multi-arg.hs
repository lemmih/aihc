{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeMultiArg where
foreign import ccall "plus" plus :: Int -> Int -> IO Int
