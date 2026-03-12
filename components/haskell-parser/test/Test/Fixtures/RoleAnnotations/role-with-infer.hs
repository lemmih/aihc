{-# LANGUAGE RoleAnnotations #-}

module RoleWithInfer where

type role InferBox _ nominal
data InferBox a b = InferBox b
