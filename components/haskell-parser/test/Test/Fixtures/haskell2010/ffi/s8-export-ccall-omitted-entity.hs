{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ExportCcallOmittedEntity where
addOne :: Int -> Int
addOne n = n + 1
foreign export ccall addOne :: Int -> Int
