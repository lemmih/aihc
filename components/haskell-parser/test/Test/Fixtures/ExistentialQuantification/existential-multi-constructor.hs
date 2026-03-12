{-# LANGUAGE ExistentialQuantification #-}

module ExistentialMultiConstructor where

data Some = forall a. Some a | forall b. Eq b => EqSome b b

isEqual :: Some -> Bool
isEqual (Some _) = False
isEqual (EqSome x y) = x == y
