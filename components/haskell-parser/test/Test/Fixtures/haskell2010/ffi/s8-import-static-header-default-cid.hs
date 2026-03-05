{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticHeaderDefaultCid where
foreign import ccall "static math.h" c_cos :: Double -> Double
