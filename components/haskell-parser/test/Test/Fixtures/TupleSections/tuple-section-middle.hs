{-# LANGUAGE TupleSections #-}

module TupleSectionMiddle where

fillMiddle :: Int -> (Int, Int, Int)
fillMiddle = (,2,)
