{-# LANGUAGE CPP #-}
module Repro where
#if defined(mingw32_HOST_OS)
type PlatformPath = Int
#else
type PlatformPath = Bool
#endif
