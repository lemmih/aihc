{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ExportStdcallNamed where
mulInt :: Int -> Int -> Int
mulInt a b = a * b
foreign export stdcall "mulInt" mulInt :: Int -> Int -> Int
