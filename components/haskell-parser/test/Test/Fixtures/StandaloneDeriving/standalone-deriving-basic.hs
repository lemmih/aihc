{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingBasic where

data Box a = Box a

deriving instance Eq a => Eq (Box a)
