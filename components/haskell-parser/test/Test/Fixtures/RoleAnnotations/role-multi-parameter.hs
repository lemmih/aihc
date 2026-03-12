{-# LANGUAGE RoleAnnotations #-}

module RoleMultiParameter where

type role MultiBox representational phantom
data MultiBox a b = MultiBox a
