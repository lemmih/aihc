{-# LANGUAGE CPP #-}
module Repro where

xs =
  [ "a"
#if defined(FOO)
  , "b"
#endif
  ]
