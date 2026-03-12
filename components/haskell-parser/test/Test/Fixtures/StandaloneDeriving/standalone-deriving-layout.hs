{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingLayout where

data W a = W a

deriving instance
  Show a => Show (W a)
