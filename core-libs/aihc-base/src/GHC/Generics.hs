{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GHC.Generics
  ( V1,
    U1 (..),
    Par1 (..),
    Rec1 (..),
    K1 (..),
    M1 (..),
    D1,
    C1,
    S1,
    (:+:) (..),
    (:*:) (..),
    (:.:) (..),
    URec (..),
    D,
    C,
    S,
    R,
    Generic (..),
    Generic1 (..),
    Rep,
    Rep1,
  )
where

import Data.Kind (Type)

data V1 p

data U1 p = U1

newtype Par1 p = Par1 {unPar1 :: p}

newtype Rec1 (f :: Type -> Type) p = Rec1 {unRec1 :: f p}

newtype K1 i c p = K1 {unK1 :: c}

newtype M1 i c (f :: Type -> Type) p = M1 {unM1 :: f p}

infixr 5 :+:

data (:+:) (f :: Type -> Type) (g :: Type -> Type) p = L1 (f p) | R1 (g p)

infixr 6 :*:

data (:*:) (f :: Type -> Type) (g :: Type -> Type) p = f p :*: g p

infixr 7 :.:

newtype (:.:) (f :: Type -> Type) (g :: Type -> Type) p = Comp1 {unComp1 :: f (g p)}

data family URec a p

data D

data C

data S

data R

type D1 = M1 D

type C1 = M1 C

type S1 = M1 S

type family Rep a :: Type -> Type

type family Rep1 (f :: Type -> Type) :: Type -> Type

class Generic a where
  from :: a -> Rep a x
  to :: Rep a x -> a

class Generic1 (f :: Type -> Type) where
  from1 :: f a -> Rep1 f a
  to1 :: Rep1 f a -> f a
