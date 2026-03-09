{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Repro where

#if defined(mingw32_HOST_OS)
x = 1
#else
x = 2
#endif
