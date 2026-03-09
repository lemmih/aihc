{-# LANGUAGE CPP #-}
module Repro where
#if __GLASGOW_HASKELL__ >= 914
import Language.Haskell.TH.Lift (Lift(..), lift)
#else
import Language.Haskell.TH.Syntax (Lift(..), lift)
#endif
