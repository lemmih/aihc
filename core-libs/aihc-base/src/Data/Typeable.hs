{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Typeable
  ( Typeable (..),
    TypeRep,
    TyCon,
    Proxy (..),
    cast,
    gcast,
    gcast1,
    gcast2,
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

import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (..))
import GHC.Types (Bool (..), Type)
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

-- | Cast a value under a type constructor.
gcast :: forall (c :: Type -> Type) a b. (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast value = gcastWith value Proxy Proxy

gcastWith :: forall (c :: Type -> Type) a b. (Typeable a, Typeable b) => c a -> Proxy a -> Proxy b -> Maybe (c b)
gcastWith value source target =
  if eqTypeRep (typeRep source) (typeRep target)
    then Just (unsafeCoerce value)
    else Nothing

-- | Cast a value over a unary type constructor. Typeable is not kind
-- polymorphic here, so the cast has no evidence for the type constructors and
-- always fails; a 'Data.Data.dataCast1' method defined with it is a no-op.
gcast1 :: forall (c :: Type -> Type) (t :: Type -> Type) (t' :: Type -> Type) a. c (t a) -> Maybe (c (t' a))
gcast1 _ = Nothing

-- | Cast a value over a binary type constructor. See 'gcast1'.
gcast2 :: forall (c :: Type -> Type) (t :: Type -> Type -> Type) (t' :: Type -> Type -> Type) a b. c (t a b) -> Maybe (c (t' a b))
gcast2 _ = Nothing
