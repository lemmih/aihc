{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsRecordUnidirectional where

data Point = MkPoint Int Int

pattern PointU :: Int -> Int -> Point
pattern PointU {xOnly, yOnly} <- MkPoint xOnly yOnly

xValue :: Point -> Int
xValue PointU {xOnly} = xOnly
