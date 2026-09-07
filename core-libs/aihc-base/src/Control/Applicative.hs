{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

module Control.Applicative
  ( Applicative (..),
    Alternative (..),
    Const (..),
    ZipList (..),
    liftA,
    liftA2,
    liftA3,
    (<$>),
    (<**>),
  )
where

import Data.Monoid (Monoid (..))
import Prelude (Applicative (..), Functor (..), Maybe (..), (++), (<$>))

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA = fmap

liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\value function -> function value)

infixl 4 <**>

newtype Const a b = Const {getConst :: a}

instance Functor (Const a) where
  fmap _ (Const value) = Const value

instance (Monoid a) => Applicative (Const a) where
  pure _ = Const mempty
  Const left <*> Const right = Const (left `mappend` right)

newtype ZipList a = ZipList {getZipList :: [a]}
  deriving newtype (Functor)

instance Applicative ZipList where
  pure value = ZipList (repeatZipList value)
  ZipList functions <*> ZipList values = ZipList (applyZipList functions values)

class (Applicative f) => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

  some value = fmap prepend value <*> many value
  many value = some value <|> pure []

infixl 3 <|>

prepend :: a -> [a] -> [a]
prepend value values = value : values

repeatZipList :: a -> [a]
repeatZipList value = value : repeatZipList value

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList [] _ = []
applyZipList _ [] = []
applyZipList (f : functions) (value : values) = f value : applyZipList functions values

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> value = value
  value <|> _ = value
