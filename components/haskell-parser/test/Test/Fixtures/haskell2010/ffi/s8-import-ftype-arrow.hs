{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportFtypeArrow where
foreign import ccall "plus1" plus1 :: Int -> IO Int
