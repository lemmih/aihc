module Main (main) where

import Test.Resolver (resolverTests)
import Test.Tasty (defaultMain)

main :: IO ()
main = resolverTests >>= defaultMain
