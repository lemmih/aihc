module Main where

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = BS.putStrLn (BS.reverse (BS.pack "gnirtsetyb"))
