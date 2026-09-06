{-# LANGUAGE KindSignatures #-}

module Data.Bifoldable
  ( Bifoldable (..),
    bifoldr',
    bifoldrM,
    bifoldl',
    bifoldlM,
    bitraverse_,
    bifor_,
    bimapM_,
    biforM_,
    bisequenceA_,
    bisequence_,
    biasum,
    biList,
    binull,
    bilength,
    bielem,
    bimaximum,
    biminimum,
    bisum,
    biproduct,
    biconcat,
    biconcatMap,
    biand,
    bior,
    biany,
    biall,
    bimaximumBy,
    biminimumBy,
    binotElem,
    bifind,
  )
where

import Control.Applicative (Alternative (..), Const (..))
import Data.Kind (Type)
import Data.Monoid (All (..), Any (..), Dual (..), Monoid (..), Product (..), Sum (..))
import Data.Semigroup (Semigroup (..))
import Prelude

class Bifoldable (p :: Type -> Type -> Type) where
  bifold :: (Monoid m) => p m m -> m
  bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c

  bifold = bifoldMap id id
  bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty
  bifoldr f g initial structure = runBiEndo (bifoldMap (BiEndo . f) (BiEndo . g) structure) initial
  bifoldl f g initial structure =
    runBiEndo (getDual (bifoldMap (Dual . BiEndo . flip f) (Dual . BiEndo . flip g) structure)) initial

-- | A function composition monoid; Data.Monoid does not export one.
newtype BiEndo a = BiEndo (a -> a)

runBiEndo :: BiEndo a -> a -> a
runBiEndo (BiEndo f) = f

instance Semigroup (BiEndo a) where
  BiEndo f <> BiEndo g = BiEndo (f . g)

instance Monoid (BiEndo a) where
  mempty = BiEndo id

instance Bifoldable (,) where
  bifoldMap f g (left, right) = f left `mappend` g right

instance Bifoldable Const where
  bifoldMap f _ (Const value) = f value

instance Bifoldable Either where
  bifoldMap f _ (Left value) = f value
  bifoldMap _ g (Right value) = g value

instance Bifoldable ((,,) x) where
  bifoldMap f g (_, left, right) = f left `mappend` g right

bifoldr' :: (Bifoldable t) => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g initial structure = bifoldl (strictLeft f) (strictLeft g) id structure initial
  where
    strictLeft step continuation value rest = continuation $! step value rest

bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g initial structure = bifoldl (bindLeft f) (bindLeft g) return structure initial
  where
    bindLeft step continuation value rest = step value rest >>= continuation

bifoldl' :: (Bifoldable t) => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g initial structure = bifoldr (strictRight f) (strictRight g) id structure initial
  where
    strictRight step value continuation rest = continuation $! step rest value

bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM f g initial structure = bifoldr (bindRight f) (bindRight g) return structure initial
  where
    bindRight step value continuation rest = step rest value >>= continuation

bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr (thenApplicative . f) (thenApplicative . g) (pure ())

thenApplicative :: (Applicative f) => f a -> f b -> f b
thenApplicative = (*>)

bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ structure f g = bitraverse_ f g structure

bimapM_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bimapM_ = bitraverse_

biforM_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
biforM_ = bifor_

bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bifoldr thenApplicative thenApplicative (pure ())

bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = bisequenceA_

biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = bifoldr (<|>) (<|>) empty

biList :: (Bifoldable t) => t a a -> [a]
biList = bifoldr (:) (:) []

binull :: (Bifoldable t) => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True

bilength :: (Bifoldable t) => t a b -> Int
bilength = bifoldl' (\count _ -> count + 1) (\count _ -> count + 1) 0

bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem value = biany (== value) (== value)

biconcat :: (Bifoldable t) => t [a] [a] -> [a]
biconcat = bifold

bimaximum :: (Bifoldable t, Ord a) => t a a -> a
bimaximum = bifoldr1 max

biminimum :: (Bifoldable t, Ord a) => t a a -> a
biminimum = bifoldr1 min

bifoldr1 :: (Bifoldable t) => (a -> a -> a) -> t a a -> a
bifoldr1 combine structure =
  case bifoldr step step Nothing structure of
    Nothing -> errorWithoutStackTrace "bifoldr1: empty structure"
    Just result -> result
  where
    step value Nothing = Just value
    step value (Just rest) = Just (combine value rest)

bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = getSum . bifoldMap Sum Sum

biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = getProduct . bifoldMap Product Product

biconcatMap :: (Bifoldable t) => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap

biand :: (Bifoldable t) => t Bool Bool -> Bool
biand = getAll . bifoldMap All All

bior :: (Bifoldable t) => t Bool Bool -> Bool
bior = getAny . bifoldMap Any Any

biany :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny . bifoldMap (Any . p) (Any . q)

biall :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll . bifoldMap (All . p) (All . q)

bimaximumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
bimaximumBy cmp = bifoldr1 pick
  where
    pick left right =
      case cmp left right of
        GT -> left
        _ -> right

biminimumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
biminimumBy cmp = bifoldr1 pick
  where
    pick left right =
      case cmp left right of
        GT -> right
        _ -> left

binotElem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
binotElem value = not . bielem value

bifind :: (Bifoldable t) => (a -> Bool) -> t a a -> Maybe a
bifind p = bifoldr step step Nothing
  where
    step value rest =
      if p value
        then Just value
        else rest
