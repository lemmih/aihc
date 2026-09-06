{-# LANGUAGE KindSignatures #-}

-- | The Foldable class and its instances for the core types. This module
-- does not import Prelude, so Prelude can export the class methods.
module GHC.Internal.Foldable
  ( Foldable (..),
  )
where

import Data.Bool (Bool (..), (&&), (||))
import Data.Either (Either (..))
import Data.Kind (Type)
import Data.Semigroup.Internal (Monoid (..), Semigroup (..))
import GHC.Base (Maybe (..), id, (++), (.))
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import GHC.Num (Num (..))

class Foldable (t :: Type -> Type) where
  fold :: (Monoid m) => t m -> m
  foldMap :: (Monoid m) => (a -> m) -> t a -> m
  foldMap' :: (Monoid m) => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: (Eq a) => a -> t a -> Bool
  maximum :: (Ord a) => t a -> a
  minimum :: (Ord a) => t a -> a
  sum :: (Num a) => t a -> a
  product :: (Num a) => t a -> a

  fold = foldMap identityFoldable
  foldMap f = foldr (\value rest -> f value <> rest) mempty
  foldMap' f = foldl' (\rest value -> rest <> f value) mempty
  foldr f initial structure = applyEndo (foldMap (Endo . f) structure) initial
  foldr' f initial structure = foldl strictRightStep id structure initial
    where
      strictRightStep continuation value rest =
        case f value rest of
          result -> continuation result
  foldl f initial structure = foldr leftStep id structure initial
    where
      leftStep value continuation rest = continuation (f rest value)
  foldl' f initial structure = foldr strictLeftStep id structure initial
    where
      strictLeftStep value continuation rest =
        case f rest value of
          result -> continuation result
  foldr1 f structure = fromMaybeFoldable emptyStructure (foldr rightStep Nothing structure)
    where
      rightStep value Nothing = Just value
      rightStep value (Just rest) = Just (f value rest)
  foldl1 f structure = fromMaybeFoldable emptyStructure (foldl leftStep Nothing structure)
    where
      leftStep Nothing value = Just value
      leftStep (Just rest) value = Just (f rest value)
  toList = foldr (:) []
  null = foldr (\_ _ -> False) True
  length = foldl' (\count _ -> count + 1) 0
  elem target = foldr (\value rest -> value == target || rest) False
  maximum = foldr1 maximumValue
  minimum = foldr1 minimumValue
  sum = foldl' (+) 0
  product = foldl' (*) 1

infix 4 `elem`

identityFoldable :: a -> a
identityFoldable value = value

fromMaybeFoldable :: a -> Maybe a -> a
fromMaybeFoldable fallback Nothing = fallback
fromMaybeFoldable _ (Just value) = value

maximumValue :: (Ord a) => a -> a -> a
maximumValue = max

minimumValue :: (Ord a) => a -> a -> a
minimumValue = min

newtype Endo a = Endo (a -> a)

applyEndo :: Endo a -> a -> a
applyEndo (Endo f) = f

instance Semigroup (Endo a) where
  Endo left <> Endo right = Endo (left . right)

instance Monoid (Endo a) where
  mempty = Endo id

emptyStructure :: a
emptyStructure = emptyStructure

instance Foldable [] where
  foldr _ initial [] = initial
  foldr f initial (value : values) = f value (foldr f initial values)

  foldl _ initial [] = initial
  foldl f initial (value : values) = foldl f (f initial value) values

  foldl' _ initial [] = initial
  foldl' f initial (value : values) =
    case f initial value of
      result -> foldl' f result values

  null [] = True
  null (_ : _) = False

instance Foldable Maybe where
  foldr _ initial Nothing = initial
  foldr f initial (Just value) = f value initial

  foldl _ initial Nothing = initial
  foldl f initial (Just value) = f initial value

  null Nothing = True
  null (Just _) = False

instance Foldable (Either e) where
  foldr _ initial (Left _) = initial
  foldr f initial (Right value) = f value initial

  foldl _ initial (Left _) = initial
  foldl f initial (Right value) = f initial value

  null (Left _) = True
  null (Right _) = False

instance Foldable ((,) e) where
  foldr f initial (_, value) = f value initial
  foldl f initial (_, value) = f initial value
  null _ = False

instance Foldable NonEmpty where
  foldr f initial (value :| values) = f value (foldr f initial values)
  foldl f initial (value :| values) = foldl f (f initial value) values
  toList (value :| values) = value : values
  null _ = False
