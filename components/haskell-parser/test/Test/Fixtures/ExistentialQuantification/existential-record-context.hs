{-# LANGUAGE ExistentialQuantification #-}

module ExistentialRecordContext where

data Packed = forall a. Eq a => Packed {leftValue :: a, rightValue :: a}

same :: Packed -> Bool
same (Packed x y) = x == y
