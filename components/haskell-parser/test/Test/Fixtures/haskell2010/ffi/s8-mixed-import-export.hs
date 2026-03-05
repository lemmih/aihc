{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8MixedImportExport where
foreign import ccall "atoi" c_atoi :: String -> IO Int
inc :: Int -> Int
inc n = n + 1
foreign export ccall "inc" inc :: Int -> Int
