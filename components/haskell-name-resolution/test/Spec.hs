module Main (main) where

import Test.Progress (progressTests)
import Test.Resolver (resolverTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  resolver <- resolverTests
  progress <- progressTests
  defaultMain (testGroup "aihc-name-resolution" [resolver, progress])
