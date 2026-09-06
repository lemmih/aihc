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
    bimsum,
    biList,
    binull,
    bilength,
    bielem,
    binotElem,
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
    bifind,
  )
where

import Control.Applicative (Alternative (..), Const (..))
import Data.Kind (Type)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Prelude

-- | 'Bifoldable' identifies foldable structures with two different varieties
-- of elements.
--
-- Instances define either 'bifoldMap' or 'bifoldr'.
class Bifoldable (p :: Type -> Type -> Type) where
  -- | Combines the elements of a structure using a monoid.
  bifold :: (Monoid m) => p m m -> m
  bifold = bifoldMap id id

  -- | Combines the elements of a structure, given ways of mapping them to a
  -- common monoid.
  bifoldMap :: (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
  bifoldMap f g = bifoldr (\a m -> f a <> m) (\b m -> g b <> m) mempty

  -- | Combines the elements of a structure in a right associative manner.
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldr f g z t = applyEndoB (bifoldMap (EndoB . f) (EndoB . g) t) z

  -- | Combines the elements of a structure in a left associative manner.
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldl f g z t = applyEndoB (getDualB (bifoldMap (DualB . EndoB . flip f) (DualB . EndoB . flip g) t)) z

instance Bifoldable (,) where
  bifoldMap f g (a, b) = f a <> g b

instance Bifoldable Const where
  bifoldMap f _ (Const a) = f a

instance Bifoldable ((,,) x) where
  bifoldMap f g (_, a, b) = f a <> g b

instance Bifoldable ((,,,) x y) where
  bifoldMap f g (_, _, a, b) = f a <> g b

instance Bifoldable Either where
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b

-- | As 'bifoldr', but strict in the result of the reduction functions at each step.
bifoldr' :: (Bifoldable t) => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g z0 xs = bifoldl f' g' id xs z0
  where
    f' k x z = case f x z of result -> k result
    g' k x z = case g x z of result -> k result

-- | Right associative monadic bifold over a structure.
bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g z0 xs = bifoldl f' g' return xs z0
  where
    f' k x z = f x z >>= k
    g' k x z = g x z >>= k

-- | As 'bifoldl', but strict in the result of the reduction functions at each step.
bifoldl' :: (Bifoldable t) => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g z0 xs = bifoldr f' g' id xs z0
  where
    f' x k z = case f z x of result -> k result
    g' x k z = case g z x of result -> k result

-- | Left associative monadic bifold over a structure.
bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM f g z0 xs = bifoldr f' g' return xs z0
  where
    f' x k z = f z x >>= k
    g' x k z = g z x >>= k

-- | Map each element of a structure using one of two actions, evaluate these
-- actions from left to right, and ignore the results.
bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr (\a rest -> f a *> rest) (\b rest -> g b *> rest) (pure ())

-- | As 'bitraverse_', but with the structure as the primary argument.
bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ t f g = bitraverse_ f g t

-- | Alias for 'bitraverse_'.
bimapM_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bimapM_ = bitraverse_

-- | Alias for 'bifor_'.
biforM_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
biforM_ = bifor_

-- | Evaluate each action in the structure from left to right, and ignore the results.
bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bitraverse_ id id

-- | Alias for 'bisequenceA_'.
bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequence_ = bisequenceA_

-- | The sum of a collection of actions, generalizing 'biconcat'.
biasum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
biasum = bifoldr (<|>) (<|>) empty

-- | Alias for 'biasum'.
bimsum :: (Bifoldable t, Alternative f) => t (f a) (f a) -> f a
bimsum = biasum

-- | Collects the list of elements of a structure, from left to right.
biList :: (Bifoldable t) => t a a -> [a]
biList = bifoldr (:) (:) []

-- | Test whether the structure is empty.
binull :: (Bifoldable t) => t a b -> Bool
binull = bifoldr (\_ _ -> False) (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.
bilength :: (Bifoldable t) => t a b -> Int
bilength = bifoldl' (\c _ -> c + 1) (\c _ -> c + 1) 0

-- | Does the element occur in the structure?
bielem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
bielem x = biany (== x) (== x)

-- | The negation of 'bielem'.
binotElem :: (Bifoldable t, Eq a) => a -> t a a -> Bool
binotElem x = not . bielem x

-- | Reduces a structure of lists to the concatenation of those lists.
biconcat :: (Bifoldable t) => t [a] [a] -> [a]
biconcat = bifold

-- | The largest element of a non-empty structure.
bimaximum :: (Bifoldable t, Ord a) => t a a -> a
bimaximum = fromMaybeBifoldable (bifoldr maxStep maxStep Nothing)
  where
    maxStep x Nothing = Just x
    maxStep x (Just y) = Just (max x y)

-- | The least element of a non-empty structure.
biminimum :: (Bifoldable t, Ord a) => t a a -> a
biminimum = fromMaybeBifoldable (bifoldr minStep minStep Nothing)
  where
    minStep x Nothing = Just x
    minStep x (Just y) = Just (min x y)

-- | The 'bisum' function computes the sum of the numbers of a structure.
bisum :: (Bifoldable t, Num a) => t a a -> a
bisum = bifoldl' (+) (+) 0

-- | The 'biproduct' function computes the product of the numbers of a structure.
biproduct :: (Bifoldable t, Num a) => t a a -> a
biproduct = bifoldl' (*) (*) 1

-- | Given a means of mapping the elements of a structure to lists,
-- computes the concatenation of all such lists in order.
biconcatMap :: (Bifoldable t) => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap

-- | 'biand' returns the conjunction of a container of Bools.
biand :: (Bifoldable t) => t Bool Bool -> Bool
biand = bifoldr (&&) (&&) True

-- | 'bior' returns the disjunction of a container of Bools.
bior :: (Bifoldable t) => t Bool Bool -> Bool
bior = bifoldr (||) (||) False

-- | Determines whether any element of the structure satisfies its
-- appropriate predicate argument.
biany :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = bifoldr (\a rest -> p a || rest) (\b rest -> q b || rest) False

-- | Determines whether all elements of the structure satisfy their
-- appropriate predicate argument.
biall :: (Bifoldable t) => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = bifoldr (\a rest -> p a && rest) (\b rest -> q b && rest) True

-- | The largest element of a non-empty structure with respect to the given
-- comparison function.
bimaximumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
bimaximumBy cmp = fromMaybeBifoldable (bifoldr step step Nothing)
  where
    step x Nothing = Just x
    step x (Just y) = Just (case cmp x y of GT -> x; _ -> y)

-- | The least element of a non-empty structure with respect to the given
-- comparison function.
biminimumBy :: (Bifoldable t) => (a -> a -> Ordering) -> t a a -> a
biminimumBy cmp = fromMaybeBifoldable (bifoldr step step Nothing)
  where
    step x Nothing = Just x
    step x (Just y) = Just (case cmp x y of GT -> y; _ -> x)

-- | The 'bifind' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
bifind :: (Bifoldable t) => (a -> Bool) -> t a a -> Maybe a
bifind p = bifoldr step step Nothing
  where
    step x rest = if p x then Just x else rest

fromMaybeBifoldable :: (t a a -> Maybe a) -> t a a -> a
fromMaybeBifoldable f t =
  case f t of
    Just x -> x
    Nothing -> errorWithoutStackTrace "bimaximum/biminimum: empty structure"

newtype EndoB a = EndoB (a -> a)

applyEndoB :: EndoB a -> a -> a
applyEndoB (EndoB f) = f

instance Semigroup (EndoB a) where
  EndoB f <> EndoB g = EndoB (f . g)

instance Monoid (EndoB a) where
  mempty = EndoB id

newtype DualB a = DualB {getDualB :: a}

instance (Semigroup a) => Semigroup (DualB a) where
  DualB a <> DualB b = DualB (b <> a)

instance (Monoid a) => Monoid (DualB a) where
  mempty = DualB mempty
