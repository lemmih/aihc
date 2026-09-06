{-# LANGUAGE KindSignatures #-}

module Data.Bifoldable
  ( Bifoldable (..),
    bifoldr',
    bifoldl',
    bitraverse_,
    bifor_,
    bisequence_,
    biList,
    binull,
    bilength,
    bielem,
    bisum,
    biproduct,
    biconcat,
    biconcatMap,
    biand,
    bior,
    biany,
    biall,
  )
where

import Control.Applicative (Const (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Prelude

-- | A functor of two arguments whose values fold to a monoid.
class Bifoldable (p :: Type -> Type -> Type) where
  bifold :: (Monoid m) => p m m -> m
  bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c

  bifold = bifoldMap id id
  bifoldMap f g = bifoldr (\left rest -> f left `mappend` rest) (\right rest -> g right `mappend` rest) mempty
  bifoldr f g initial structure = runStep (bifoldMap (Step . f) (Step . g) structure) initial
  bifoldl f g initial structure =
    bifoldr (\left continue acc -> continue (f acc left)) (\right continue acc -> continue (g acc right)) id structure initial

-- | An endomorphism monoid for the right fold default.
newtype Step c = Step (c -> c)

runStep :: Step c -> c -> c
runStep (Step f) = f

instance Semigroup (Step c) where
  Step f <> Step g = Step (f . g)

instance Monoid (Step c) where
  mempty = Step id

instance Bifoldable (,) where
  bifoldMap f g (left, right) = f left `mappend` g right

instance Bifoldable Either where
  bifoldMap f _ (Left left) = f left
  bifoldMap _ g (Right right) = g right

instance Bifoldable Const where
  bifoldMap f _ (Const value) = f value

bifoldr' :: (Bifoldable t) => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g initial structure =
  bifoldl (\continue left acc -> continue $! f left acc) (\continue right acc -> continue $! g right acc) id structure initial

bifoldl' :: (Bifoldable t) => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g initial structure =
  bifoldr (\left continue acc -> continue $! f acc left) (\right continue acc -> continue $! g acc right) id structure initial

bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr (\left rest -> f left *> rest) (\right rest -> g right *> rest) (pure ())

bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ structure f g = bitraverse_ f g structure

bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = bitraverse_ id id

biList :: (Bifoldable t) => t a a -> [a]
biList = bifoldr (:) (:) []

binull :: (Bifoldable t) => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True

bilength :: (Bifoldable t) => t a b -> Int
bilength = bifoldl' (\count _ -> count + 1) (\count _ -> count + 1) 0

bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem value = biany (== value) (== value)

bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = bifoldl' (+) (+) 0

biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = bifoldl' (*) (*) 1

biconcat :: (Bifoldable t) => t [a] [a] -> [a]
biconcat = bifold

biconcatMap :: (Bifoldable t) => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap

biand :: (Bifoldable t) => t Bool Bool -> Bool
biand = biall id id

bior :: (Bifoldable t) => t Bool Bool -> Bool
bior = biany id id

biany :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany f g = bifoldr (\left rest -> f left || rest) (\right rest -> g right || rest) False

biall :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall f g = bifoldr (\left rest -> f left && rest) (\right rest -> g right && rest) True
