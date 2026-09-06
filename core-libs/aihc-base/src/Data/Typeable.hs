module Data.Typeable
  ( Typeable (..),
    TypeRep,
    TyCon,
    cast,
    eqTypeRep,
    typeOf,
    typeRep,
    typeRepArgs,
    typeRepTyCon,
    tyConName,
    rnfTyCon,
    rnfTypeRep,
  )
where

import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (..))
import GHC.Types (Bool (..))
import Type.Reflection
  ( SomeTypeRep (..),
    TyCon,
    Typeable (..),
    eqTypeRep,
    rnfSomeTypeRep,
    rnfTyCon,
    splitApps,
    tyConName,
    typeOf,
    typeRep,
    typeRepTyCon,
  )
import Unsafe.Coerce (unsafeCoerce)

type TypeRep = SomeTypeRep

rnfTypeRep :: TypeRep -> ()
rnfTypeRep = rnfSomeTypeRep

-- | The arguments of a type.
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (SomeTypeRep rep) =
  case splitApps rep of
    (_, arguments) -> arguments

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast value = castWith value Proxy

castWith :: (Typeable a, Typeable b) => a -> Proxy b -> Maybe b
castWith value target =
  if eqTypeRep (typeOf value) (typeRep target)
    then Just (unsafeCoerce value)
    else Nothing
