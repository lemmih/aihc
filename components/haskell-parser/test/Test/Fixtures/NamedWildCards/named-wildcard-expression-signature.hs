{-# LANGUAGE NamedWildCards #-}

module NamedWildcardExpressionSignature where

identityExpr :: Int
identityExpr = (id :: _b -> _b) 7
