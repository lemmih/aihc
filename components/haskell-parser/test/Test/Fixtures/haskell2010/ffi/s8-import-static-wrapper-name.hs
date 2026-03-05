{-# LANGUAGE ForeignFunctionInterface #-}
module FfiS8ImportStaticWrapperName where
foreign import ccall "static wrapper" wrapperFn :: IO Int
