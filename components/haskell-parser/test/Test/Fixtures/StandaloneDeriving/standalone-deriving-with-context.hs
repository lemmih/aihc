{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingWithContext where

data PairBox a b = PairBox a b

deriving instance (Eq a, Eq b) => Eq (PairBox a b)
