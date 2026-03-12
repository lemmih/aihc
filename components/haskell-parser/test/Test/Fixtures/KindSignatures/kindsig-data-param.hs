{-# LANGUAGE KindSignatures #-}

module KindSignaturesDataParam where

import Data.Kind (Type)

data Proxy (a :: Type) = Proxy
