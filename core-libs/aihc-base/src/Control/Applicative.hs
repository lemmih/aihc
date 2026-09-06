{-# LANGUAGE PolyKinds #-}

module Control.Applicative
  ( Applicative (..),
    Alternative (..),
    Const (..),
    ZipList (..),
    liftA,
    liftA2,
    (<**>),
  )
where

import Data.Monoid (Monoid (..))
import Prelude (Applicative (..), Functor (..), Maybe (..), (++), (<$>))

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA = fmap

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 function left right = function <$> left <*> right

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

instance Functor ZipList where
  fmap f (ZipList values) = ZipList (mapZipList f values)

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

mapZipList :: (a -> b) -> [a] -> [b]
mapZipList _ [] = []
mapZipList f (value : values) = f value : mapZipList f values

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
