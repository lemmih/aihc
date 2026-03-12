{-# LANGUAGE NamedWildCards #-}

module NamedWildcardMultiHole where

pair :: _x -> _y -> (_x, _y)
pair x y = (x, y)
