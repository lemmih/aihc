{-# LANGUAGE CPP #-}
module Repro where

#if defined(GHC_MAKE)
import qualified System.OsPath.Windows.Internal as AFP_W
#else
import qualified System.OsPath.Windows as AFP_W
#endif
