{-# LANGUAGE TupleSections #-}

module TupleSectionNested where

nested :: Int -> ((Int, Int), Int)
nested = ((,3),)
