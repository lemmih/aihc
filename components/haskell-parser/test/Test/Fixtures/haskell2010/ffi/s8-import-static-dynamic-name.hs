{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticDynamicName where
foreign import ccall "static dynamic" dynamicFn :: IO Int
