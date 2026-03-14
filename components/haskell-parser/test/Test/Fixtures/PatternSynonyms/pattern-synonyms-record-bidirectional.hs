{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsRecordBidirectional where

data Point = MkPoint Int Int

pattern Point :: Int -> Int -> Point
pattern Point {xCoord, yCoord} = MkPoint xCoord yCoord

origin :: Point
origin = Point {xCoord = 0, yCoord = 0}

shiftX :: Int -> Point -> Point
shiftX dx p = p {xCoord = xCoord p + dx}
