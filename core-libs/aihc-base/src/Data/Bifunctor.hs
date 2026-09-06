{-# LANGUAGE KindSignatures #-}

module Data.Bifunctor
  ( Bifunctor (..),
  )
where

import Control.Applicative (Const (..))
import Data.Kind (Type)
import Prelude (Either (..), id, (.))

{- HLINT ignore "Use bimap" -}
{- HLINT ignore "Use first" -}
{- HLINT ignore "Use second" -}
class Bifunctor (p :: Type -> Type -> Type) where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  first :: (a -> b) -> p a c -> p b c
  second :: (b -> c) -> p a b -> p a c

  bimap onFirst onSecond = first onFirst . second onSecond
  first onFirst = bimap onFirst id
  second = bimap id

instance Bifunctor (,) where
  bimap onFirst onSecond (valueOne, valueTwo) = (onFirst valueOne, onSecond valueTwo)

instance Bifunctor Either where
  bimap onFirst _ (Left value) = Left (onFirst value)
  bimap _ onSecond (Right value) = Right (onSecond value)

instance Bifunctor ((,,) x) where
  bimap onFirst onSecond (x, valueOne, valueTwo) = (x, onFirst valueOne, onSecond valueTwo)

instance Bifunctor ((,,,) x y) where
  bimap onFirst onSecond (x, y, valueOne, valueTwo) = (x, y, onFirst valueOne, onSecond valueTwo)

instance Bifunctor Const where
  bimap onFirst _ (Const value) = Const (onFirst value)
