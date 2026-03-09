{-# LANGUAGE LambdaCase #-}

module LambdaCaseNested where

choose :: Either Int String -> Int
choose = \case
  Left n -> n
  Right _ ->
    (\case
        Just k -> k
        Nothing -> 0
    )
      (Just 1)
