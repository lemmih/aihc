{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}

module EmptyDataDeclsWithKind where

import Data.Kind (Type)

data Tagged (a :: Type)
