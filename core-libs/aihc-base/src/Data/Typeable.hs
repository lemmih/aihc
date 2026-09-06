{-# LANGUAGE KindSignatures #-}

module Data.Typeable
  ( Typeable (..),
    TypeRep,
    TyCon,
    cast,
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

-- | Cast over a type constructor with one argument.
--
-- The type checker has no kind polymorphism, so Typeable evidence exists
-- only for types of kind Type. The cast cannot compare the constructors and
-- always fails.
gcast1 :: forall (c :: Type -> Type) (t :: Type -> Type) (t' :: Type -> Type) a. c (t a) -> Maybe (c (t' a))
gcast1 _ = Nothing

-- | Cast over a type constructor with two arguments. See 'gcast1'.
gcast2 :: forall (c :: Type -> Type) (t :: Type -> Type -> Type) (t' :: Type -> Type -> Type) a b. c (t a b) -> Maybe (c (t' a b))
gcast2 _ = Nothing
