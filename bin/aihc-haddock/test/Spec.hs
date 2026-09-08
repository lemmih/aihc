module Main (main) where

import Data.Proxy (Proxy (..))
import Test.Haddock.Fixtures qualified as Fixtures
import Test.Haddock.Units qualified as Units
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
import Test.Tasty.Hedgehog (HedgehogReplay, HedgehogShrinkLimit, HedgehogTestLimit)
import Test.Tasty.Options (OptionDescription (..))

main :: IO ()
main = do
  fixtures <- Fixtures.tests
  defaultMainWithIngredients
    ( includingOptions
        [ Option (Proxy :: Proxy HedgehogTestLimit),
          Option (Proxy :: Proxy HedgehogShrinkLimit),
          Option (Proxy :: Proxy HedgehogReplay)
        ]
        : defaultIngredients
    )
    (testGroup "aihc-haddock" [Units.tests, fixtures])
