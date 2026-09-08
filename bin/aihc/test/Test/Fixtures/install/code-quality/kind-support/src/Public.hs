module Public (f) where

import Hidden

f :: Proxy (a :: K) -> Proxy a
f x = x
