{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Data.Functor.Identity
  ( Identity (..),
  )
where

import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..))
import Prelude

newtype Identity (a :: Type) = Identity {runIdentity :: a}
  deriving newtype (Eq, Ord, Bounded, Enum, Monoid)

instance (Read a) => Read (Identity a) where
  readsPrec precedence = readParen (precedence > 10) readIdentity

instance (Show a) => Show (Identity a) where
  showsPrec precedence (Identity value) =
    showParen (precedence > 10) (showString "Identity " . showsPrec 11 value)

instance Functor Identity where
  fmap f (Identity value) = Identity (f value)

instance Foldable Identity where
  foldMap f (Identity value) = f value
  foldr f initial (Identity value) = f value initial
  foldl f initial (Identity value) = f initial value
  null _ = False
  length _ = 1

instance Traversable Identity where
  traverse f (Identity value) = fmap Identity (f value)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity value = Identity (f value)

instance Monad Identity where
  Identity value >>= next = next value
  Identity _ >> next = next
  return = Identity

instance (Semigroup a) => Semigroup (Identity a) where
  Identity left <> Identity right = Identity (left <> right)

readIdentity :: (Read a) => ReadS (Identity a)
readIdentity input =
  case lex input of
    (constructor, afterConstructor) : _ ->
      case constructor == "Identity" of
        True -> wrapIdentityReads (readsPrec 11 afterConstructor)
        False -> []
    [] -> []

wrapIdentityReads :: [(a, String)] -> [(Identity a, String)]
wrapIdentityReads [] = []
wrapIdentityReads ((value, rest) : results) = (Identity value, rest) : wrapIdentityReads results
