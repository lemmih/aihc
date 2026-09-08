{-# LANGUAGE PolyKinds #-}

module Data.Proxy (Proxy (..)) where

data Proxy (a :: k) = Proxy
