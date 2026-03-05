{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ExportCcallNamed where
addInt :: Int -> Int -> Int
addInt a b = a + b
foreign export ccall "addInt" addInt :: Int -> Int -> Int
