module Main (main) where

import Test.Haddock.Fixtures qualified as Fixtures
import Test.Haddock.Units qualified as Units
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  fixtures <- Fixtures.tests
  defaultMain (testGroup "aihc-haddock" [Units.tests, fixtures])
