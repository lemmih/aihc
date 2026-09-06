{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use foldr" #-}

module Data.List.NonEmpty
  ( NonEmpty (..),
    map,
    intersperse,
    scanl,
    scanr,
    scanl1,
    scanr1,
    transpose,
    sortBy,
    sortWith,
    length,
    compareLength,
    head,
    tail,
    last,
    init,
    singleton,
    (<|),
    cons,
    uncons,
    unfoldr,
    sort,
    sortOn,
    reverse,
    inits,
    inits1,
    tails,
    tails1,
    append,
    appendList,
    prependList,
    iterate,
    repeat,
    cycle,
    unfold,
    insert,
    some1,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    span,
    break,
    filter,
    partition,
    group,
    groupBy,
    groupWith,
    groupAllWith,
    group1,
    groupBy1,
    groupWith1,
    groupAllWith1,
    permutations,
    permutations1,
    isPrefixOf,
    nub,
    nubBy,
    (!!),
    zip,
    zipWith,
    unzip,
    fromList,
    toList,
    nonEmpty,
    xor,
  )
where

import Control.Applicative (Alternative (many))
import Data.Foldable (Foldable (..))
import Data.Semigroup (Semigroup (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import Prelude
  ( Applicative (..),
    Bool (..),
    Eq (..),
    Functor (..),
    Int,
    Maybe (..),
    Monad (..),
    Num (..),
    Ord (..),
    Ordering (..),
    Show (..),
    Traversable (..),
    showParen,
    showString,
    (++),
    (.),
  )

infixr 5 <|

instance (Eq a) => Eq (NonEmpty a) where
  left == right = toList left == toList right
  left /= right = toList left /= toList right

instance (Ord a) => Ord (NonEmpty a) where
  compare left right = compare (toList left) (toList right)
  left < right = toList left < toList right
  left <= right = toList left <= toList right
  left > right = toList left > toList right
  left >= right = toList left >= toList right
  min left right = fromList (min (toList left) (toList right))
  max left right = fromList (max (toList left) (toList right))

instance (Show a) => Show (NonEmpty a) where
  showsPrec precedence ((:|) value values) =
    showParen
      (precedence > 5)
      (showsPrec 6 value . showString " :| " . showsPrec 6 values)

instance Foldable NonEmpty where
  foldr f initial (value :| values) = f value (foldr f initial values)
  foldMap f (value :| values) = f value <> foldMap f values
  null _ = False

instance Functor NonEmpty where
  fmap f ((:|) value values) = f value :| mapList f values

instance Traversable NonEmpty where
  traverse f (value :| values) = liftA2 (:|) (f value) (traverse f values)

instance Applicative NonEmpty where
  pure = singleton
  functions <*> values = bindNonEmpty functions (applyTo values)

instance Monad NonEmpty where
  values >>= next = bindNonEmpty values next
  values >> next = bindNonEmpty values (keepValue next)
  return = singleton

instance Semigroup (NonEmpty a) where
  (<>) = append

applyTo :: NonEmpty a -> (a -> b) -> NonEmpty b
applyTo values f = fmap f values

keepValue :: a -> b -> a
keepValue value _ = value

bindNonEmpty :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
bindNonEmpty ((:|) value values) next =
  case next value of
    (:|) first rest -> first :| (rest ++ nonEmptyBindTail values next)

nonEmptyBindTail :: [a] -> (a -> NonEmpty b) -> [b]
nonEmptyBindTail [] _ = []
nonEmptyBindTail (value : values) next = toList (next value) ++ nonEmptyBindTail values next

length :: NonEmpty a -> Int
length ((:|) _ values) = 1 + lengthList values

compareLength :: NonEmpty a -> Int -> Ordering
compareLength values target =
  case target < 1 of
    True -> GT
    False -> compareListLength (toList values) target

compareListLength :: [a] -> Int -> Ordering
compareListLength [] remaining =
  case remaining > 0 of
    True -> LT
    False -> EQ
compareListLength (_ : values) remaining =
  case remaining > 0 of
    True -> compareListLength values (remaining - 1)
    False -> GT

xor :: NonEmpty Bool -> Bool
xor ((:|) value values) = foldrList xorValue value values
  where
    xorValue True rest = notBool rest
    xorValue False rest = rest

unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfold = unfoldr

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (value : values) = Just (value :| values)

uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons ((:|) value values) = (value, nonEmpty values)

unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr step seed =
  case step seed of
    (value, next) -> value :| unfoldTail step next

unfoldTail :: (a -> (b, Maybe a)) -> Maybe a -> [b]
unfoldTail _ Nothing = []
unfoldTail step (Just seed) =
  case step seed of
    (value, next) -> value : unfoldTail step next

head :: NonEmpty a -> a
head ((:|) value _) = value

tail :: NonEmpty a -> [a]
tail ((:|) _ values) = values

last :: NonEmpty a -> a
last ((:|) value values) = lastWithDefault value values

lastWithDefault :: a -> [a] -> a
lastWithDefault value [] = value
lastWithDefault _ (value : values) = lastWithDefault value values

init :: NonEmpty a -> [a]
init ((:|) value values) = initWithHead value values

initWithHead :: a -> [a] -> [a]
initWithHead _ [] = []
initWithHead value (next : values) = value : initWithHead next values

singleton :: a -> NonEmpty a
singleton value = value :| []

(<|) :: a -> NonEmpty a -> NonEmpty a
value <| values = value :| toList values

cons :: a -> NonEmpty a -> NonEmpty a
cons = (<|)

sort :: (Ord a) => NonEmpty a -> NonEmpty a
sort values = sortByListToNonEmpty compareValues (toList values)

sortOn :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn projection = sortBy (compareOn projection)

sortWith :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty a
sortWith projection = sortBy (compareOn projection)

compareOn :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareOn projection left right = compare (projection left) (projection right)

fromList :: [a] -> NonEmpty a
fromList [] = emptyNonEmpty
fromList (value : values) = value :| values

emptyNonEmpty :: NonEmpty a
emptyNonEmpty = emptyNonEmpty

toList :: NonEmpty a -> [a]
toList ((:|) value values) = value : values

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f ((:|) value values) = f value :| mapList f values

inits :: [a] -> NonEmpty [a]
inits values = [] :| nonEmptyInits (foldableToList values)

nonEmptyInits :: [a] -> [[a]]
nonEmptyInits [] = []
nonEmptyInits (value : values) = [value] : mapList (prependValue value) (nonEmptyInits values)

inits1 :: NonEmpty a -> NonEmpty (NonEmpty a)
inits1 ((:|) value values) = singleton value :| mapList fromList (nonEmptyInitsFrom value values)

nonEmptyInitsFrom :: a -> [a] -> [[a]]
nonEmptyInitsFrom _ [] = []
nonEmptyInitsFrom value (next : values) =
  [value, next] : mapList (prependValue value) (nonEmptyInitsFrom next values)

tails :: [a] -> NonEmpty [a]
tails values =
  case tailsList (foldableToList values) of
    first : rest -> first :| rest
    [] -> [] :| []

tails1 :: NonEmpty a -> NonEmpty (NonEmpty a)
tails1 values = values :| mapList fromList (nonEmptyTails (tail values))

nonEmptyTails :: [a] -> [[a]]
nonEmptyTails [] = []
nonEmptyTails values@(_ : rest) = values : nonEmptyTails rest

insert :: (Ord a) => a -> [a] -> NonEmpty a
insert value values = fromList (insertByList compareValues value (foldableToList values))

some1 :: (Alternative f) => f a -> f (NonEmpty a)
some1 values = fmap makeNonEmpty values <*> many values
  where
    makeNonEmpty value rest = value :| rest

scanl :: (b -> a -> b) -> b -> [a] -> NonEmpty b
scanl f initial values = initial :| scanlTail f initial (foldableToList values)

scanlTail :: (b -> a -> b) -> b -> [a] -> [b]
scanlTail _ _ [] = []
scanlTail f initial (value : values) =
  case f initial value of
    result -> result : scanlTail f result values

scanr :: (a -> b -> b) -> b -> [a] -> NonEmpty b
scanr f initial values = fromList (scanrList f initial (foldableToList values))

scanrList :: (a -> b -> b) -> b -> [a] -> [b]
scanrList _ initial [] = [initial]
scanrList f initial (value : values) =
  case scanrList f initial values of
    result : results -> f value result : result : results
    [] -> [initial]

scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 f ((:|) value values) = value :| scanlTail f value values

scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr1 f values = fromList (scanr1List f (toList values))

scanr1List :: (a -> a -> a) -> [a] -> [a]
scanr1List _ [] = []
scanr1List _ [value] = [value]
scanr1List f (value : values) =
  case scanr1List f values of
    result : results -> f value result : result : results
    [] -> [value]

intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse separator ((:|) value values) = value :| intersperseTail separator values

intersperseTail :: a -> [a] -> [a]
intersperseTail _ [] = []
intersperseTail separator (value : values) = separator : value : intersperseTail separator values

iterate :: (a -> a) -> a -> NonEmpty a
iterate f value = value :| iterateList f (f value)

iterateList :: (a -> a) -> a -> [a]
iterateList f value = value : iterateList f (f value)

cycle :: NonEmpty a -> NonEmpty a
cycle ((:|) value values) = value :| repeatedTail
  where
    repeatedTail = values ++ (value : repeatedTail)

reverse :: NonEmpty a -> NonEmpty a
reverse values = fromList (reverseList (toList values))

repeat :: a -> NonEmpty a
repeat value = value :| repeatList value

repeatList :: a -> [a]
repeatList value = value : repeatList value

take :: Int -> NonEmpty a -> [a]
take count values = takeList count (toList values)

drop :: Int -> NonEmpty a -> [a]
drop count values = dropList count (toList values)

splitAt :: Int -> NonEmpty a -> ([a], [a])
splitAt count values = splitAtList count (toList values)

takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile predicate values = takeWhileList predicate (toList values)

dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile predicate values = dropWhileList predicate (toList values)

span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span predicate values = spanList predicate (toList values)

break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break predicate = span (notBool . predicate)

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter predicate values = filterList predicate (toList values)

partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition predicate values = partitionList predicate (toList values)

group :: (Eq a) => [a] -> [NonEmpty a]
group values = groupByList equalValues (foldableToList values)

groupBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupBy predicate values = groupByList predicate (foldableToList values)

groupWith :: (Eq b) => (a -> b) -> [a] -> [NonEmpty a]
groupWith projection = groupBy (equalOn projection)

equalOn :: (Eq b) => (a -> b) -> a -> a -> Bool
equalOn projection left right = projection left == projection right

equalValues :: (Eq a) => a -> a -> Bool
equalValues left right = left == right

compareValues :: (Ord a) => a -> a -> Ordering
compareValues left right = compare left right

groupAllWith :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith projection values = groupWith projection (sortByList (compareOn projection) values)

group1 :: (Eq a) => NonEmpty a -> NonEmpty (NonEmpty a)
group1 values = groupBy1 equalValues values

groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 predicate values = fromList (groupByList predicate (toList values))

groupWith1 :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith1 projection = groupBy1 (equalOn projection)

groupAllWith1 :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith1 projection values = fromList (groupAllWith projection (toList values))

permutations :: [a] -> NonEmpty [a]
permutations values = fromList (permutationsList values)

permutations1 :: NonEmpty a -> NonEmpty (NonEmpty a)
permutations1 values = fromList (mapList fromList (permutationsList (toList values)))

isPrefixOf :: (Eq a) => [a] -> NonEmpty a -> Bool
isPrefixOf prefix values = isPrefixOfList prefix (toList values)

nub :: (Eq a) => NonEmpty a -> NonEmpty a
nub values = nubBy equalValues values

nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
nubBy predicate values = fromList (nubByList predicate (toList values))

(!!) :: NonEmpty a -> Int -> a
values !! index = indexList (toList values) index

infixl 9 !!

zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zip = zipWith makePair

makePair :: a -> b -> (a, b)
makePair left right = (left, right)

zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f ((:|) left lefts) ((:|) right rights) = f left right :| zipWithList f lefts rights

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip values = (fmap first values, fmap second values)
  where
    first (left, _) = left
    second (_, right) = right

transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose rows = fromList (mapList fromList (transposeLists (mapList toList (toList rows))))

sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy compareValues values = fromList (sortByList compareValues (toList values))

sortByListToNonEmpty :: (a -> a -> Ordering) -> [a] -> NonEmpty a
sortByListToNonEmpty compareItems values = fromList (sortByList compareItems values)

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append ((:|) value values) right = value :| (values ++ toList right)

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList ((:|) value values) suffix = value :| (values ++ suffix)

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList [] values = values
prependList (value : values) suffix = value :| (values ++ toList suffix)

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (value : values) = f value : mapList f values

prependValue :: a -> [a] -> [a]
prependValue value values = value : values

foldableToList :: [a] -> [a]
foldableToList values = values

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList _ initial [] = initial
foldrList f initial (value : values) = f value (foldrList f initial values)

foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList _ initial [] = initial
foldlList f initial (value : values) = foldlList f (f initial value) values

foldlStrictList :: (b -> a -> b) -> b -> [a] -> b
foldlStrictList _ initial [] = initial
foldlStrictList f initial (value : values) =
  case f initial value of
    result -> foldlStrictList f result values

lengthList :: [a] -> Int
lengthList [] = 0
lengthList (_ : values) = 1 + lengthList values

reverseList :: [a] -> [a]
reverseList = reverseOnto []

reverseOnto :: [a] -> [a] -> [a]
reverseOnto result [] = result
reverseOnto result (value : values) = reverseOnto (value : result) values

takeList :: Int -> [a] -> [a]
takeList count values =
  case count <= 0 of
    True -> []
    False ->
      case values of
        [] -> []
        value : rest -> value : takeList (count - 1) rest

dropList :: Int -> [a] -> [a]
dropList count values =
  case count <= 0 of
    True -> values
    False ->
      case values of
        [] -> []
        _ : rest -> dropList (count - 1) rest

splitAtList :: Int -> [a] -> ([a], [a])
splitAtList count values = (takeList count values, dropList count values)

takeWhileList :: (a -> Bool) -> [a] -> [a]
takeWhileList _ [] = []
takeWhileList predicate (value : values) =
  case predicate value of
    True -> value : takeWhileList predicate values
    False -> []

dropWhileList :: (a -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList predicate values@(value : rest) =
  case predicate value of
    True -> dropWhileList predicate rest
    False -> values

spanList :: (a -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([], [])
spanList predicate values@(value : rest) =
  case predicate value of
    False -> ([], values)
    True ->
      case spanList predicate rest of
        (prefix, suffix) -> (value : prefix, suffix)

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList predicate (value : values) =
  case predicate value of
    True -> value : filterList predicate values
    False -> filterList predicate values

partitionList :: (a -> Bool) -> [a] -> ([a], [a])
partitionList _ [] = ([], [])
partitionList predicate (value : values) =
  case partitionList predicate values of
    (matches, misses) ->
      case predicate value of
        True -> (value : matches, misses)
        False -> (matches, value : misses)

groupByList :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByList _ [] = []
groupByList predicate (value : values) =
  case spanList (predicate value) values of
    (matches, rest) -> (value :| matches) : groupByList predicate rest

isPrefixOfList :: (Eq a) => [a] -> [a] -> Bool
isPrefixOfList [] _ = True
isPrefixOfList (_ : _) [] = False
isPrefixOfList (left : lefts) (right : rights) =
  andBool (left == right) (isPrefixOfList lefts rights)

nubByList :: (a -> a -> Bool) -> [a] -> [a]
nubByList _ [] = []
nubByList predicate (value : values) = value : nubByList predicate (filterList (notEqualTo predicate value) values)

notEqualTo :: (a -> a -> Bool) -> a -> a -> Bool
notEqualTo predicate left right = notBool (predicate left right)

indexList :: [a] -> Int -> a
indexList values index =
  case index < 0 of
    True -> indexError
    False ->
      case values of
        [] -> indexError
        value : rest ->
          case index == 0 of
            True -> value
            False -> indexList rest (index - 1)

indexError :: a
indexError = indexError

zipWithList :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithList _ [] _ = []
zipWithList _ _ [] = []
zipWithList f (left : lefts) (right : rights) = f left right : zipWithList f lefts rights

transposeLists :: [[a]] -> [[a]]
transposeLists rows =
  case collectColumn rows of
    ([], []) -> []
    (column, rest) -> column : transposeLists rest

collectColumn :: [[a]] -> ([a], [[a]])
collectColumn [] = ([], [])
collectColumn ([] : rows) = collectColumn rows
collectColumn ((value : values) : rows) =
  case collectColumn rows of
    (column, rest) -> (value : column, values : rest)

sortByList :: (a -> a -> Ordering) -> [a] -> [a]
sortByList compareValues values = mergeAll compareValues (mapList singletonList values)

singletonList :: a -> [a]
singletonList value = [value]

mergeAll :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeAll _ [] = []
mergeAll _ [values] = values
mergeAll compareValues runs = mergeAll compareValues (mergePairs compareValues runs)

mergePairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
mergePairs _ [] = []
mergePairs _ [values] = [values]
mergePairs compareValues (left : remaining) =
  case remaining of
    right : runs -> mergeLists compareValues left right : mergePairs compareValues runs
    [] -> [left]

mergeLists :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeLists _ [] right = right
mergeLists _ left [] = left
mergeLists compareValues left@(x : xs) right@(y : ys) =
  case compareValues x y of
    GT -> y : mergeLists compareValues left ys
    _ -> x : mergeLists compareValues xs right

insertByList :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertByList _ value [] = [value]
insertByList compareValues value values@(next : rest) =
  case compareValues value next of
    GT -> next : insertByList compareValues value rest
    _ -> value : values

tailsList :: [a] -> [[a]]
tailsList values@(_ : rest) = values : tailsList rest
tailsList [] = [[]]

permutationsList :: [a] -> [[a]]
permutationsList [] = [[]]
permutationsList (value : values) = concatMapList (insertEverywhere value) (permutationsList values)

concatMapList :: (a -> [b]) -> [a] -> [b]
concatMapList _ [] = []
concatMapList f (value : values) = f value ++ concatMapList f values

insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere value values = (value : values) : insertAfterPrefixes value values

insertAfterPrefixes :: a -> [a] -> [[a]]
insertAfterPrefixes _ [] = []
insertAfterPrefixes value (first : rest) =
  mapList (prependValue first) (insertEverywhere value rest)

notBool :: Bool -> Bool
notBool True = False
notBool False = True

andBool :: Bool -> Bool -> Bool
andBool True value = value
andBool False _ = False
