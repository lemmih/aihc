{-# LANGUAGE NoImplicitPrelude #-}

-- | A module with the name of an aihc-base module, so that an import of
-- @System.IO@ is ambiguous when this package is selected.
module System.IO (duplicate) where

duplicate :: a -> a
duplicate x = x
