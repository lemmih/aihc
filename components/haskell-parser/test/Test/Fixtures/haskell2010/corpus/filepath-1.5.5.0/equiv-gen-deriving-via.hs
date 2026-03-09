{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Repro where

class AltShow a where
  altShow :: a -> String

instance {-# OVERLAPPABLE #-} Show a => AltShow a where
  altShow = show
