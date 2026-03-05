{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticHeaderCid where
foreign import ccall "static math.h sin" c_sin :: Double -> Double
