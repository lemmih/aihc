{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

closeDouble :: Double -> Double -> Bool
closeDouble left right = abs (left - right) < 1.0e-9

closeFloat :: Float -> Float -> Bool
closeFloat left right = abs (left - right) < 1.0e-4

angle :: Double
angle = 0.75

unitCircle :: Double
unitCircle = sin angle * sin angle + cos angle * cos angle

halfFloat :: Float
halfFloat = 0.5

checks :: Bool
checks =
  closeDouble (sqrt 2.0 * sqrt 2.0) 2.0
    && closeDouble (exp (log 5.0)) 5.0
    && closeDouble unitCircle 1.0
    && closeDouble (2.0 ** 10.0) 1024.0
    && closeDouble (logBase 2.0 8.0) 3.0
    && closeDouble pi 3.141592653589793
    && closeDouble (1.0 / 4.0) 0.25
    && closeDouble (negate 2.5 + 2.5) 0.0
    && closeDouble 1.25e2 125.0
    && closeDouble (atan 1.0 * 4.0) pi
    && closeFloat (halfFloat + 0.25) 0.75
    && closeFloat (sqrt 2.0) 1.4142135

main :: IO ()
main =
  if checks
    then hPutBuf stdout (Ptr "ok\n"# :: Ptr ()) 3
    else hPutBuf stdout (Ptr "fail\n"# :: Ptr ()) 5
