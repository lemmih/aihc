module T13 where
withDefault :: Maybe a -> a -> a
withDefault mx fallback = case mx of
  Just x -> x
  Nothing -> fallback
