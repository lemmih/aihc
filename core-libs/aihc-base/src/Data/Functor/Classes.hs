{-# LANGUAGE KindSignatures #-}

-- | Liftings of the Prelude classes to unary and binary type constructors.
-- The classes carry no quantified superclass constraints, unlike base.
module Data.Functor.Classes
  ( -- * Liftings of Prelude classes

    -- ** For unary constructors
    Eq1 (..),
    eq1,
    Ord1 (..),
    compare1,
    Read1 (..),
    readsPrec1,
    readPrec1,
    liftReadListDefault,
    liftReadListPrecDefault,
    Show1 (..),
    showsPrec1,

    -- ** For binary constructors
    Eq2 (..),
    eq2,
    Ord2 (..),
    compare2,
    Read2 (..),
    readsPrec2,
    readPrec2,
    liftReadList2Default,
    liftReadListPrec2Default,
    Show2 (..),
    showsPrec2,

    -- * Helper functions
    readsData,
    readData,
    readsUnaryWith,
    readUnaryWith,
    readsBinaryWith,
    readBinaryWith,
    showsUnaryWith,
    showsBinaryWith,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Text.ParserCombinators.ReadPrec (ReadPrec, minPrec, readPrec_to_S, readS_to_Prec)
import Prelude

class Eq1 (f :: Type -> Type) where
  liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = liftEq (==)

class (Eq1 f) => Ord1 (f :: Type -> Type) where
  liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

compare1 :: (Ord1 f, Ord a) => f a -> f a -> Ordering
compare1 = liftCompare compare

class Read1 (f :: Type -> Type) where
  liftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
  liftReadList :: (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
  liftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)
  liftReadListPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec [f a]

  liftReadsPrec readsPrecA readListA = readPrec_to_S (liftReadPrec (readS_to_Prec readsPrecA) (readS_to_Prec (const readListA)))
  liftReadList readsPrecA readListA = readListWith (liftReadsPrec readsPrecA readListA 0)
  liftReadPrec readPrecA readListPrecA = readS_to_Prec (liftReadsPrec (readPrec_to_S readPrecA) (readPrec_to_S readListPrecA 0))
  liftReadListPrec readPrecA readListPrecA = readS_to_Prec (const (liftReadList (readPrec_to_S readPrecA) (readPrec_to_S readListPrecA 0)))

readsPrec1 :: (Read1 f, Read a) => Int -> ReadS (f a)
readsPrec1 = liftReadsPrec readsPrec readList

readPrec1 :: (Read1 f, Read a) => ReadPrec (f a)
readPrec1 = liftReadPrec readPrec readListPrec

liftReadListDefault :: (Read1 f) => (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
liftReadListDefault readsPrecA readListA = readPrec_to_S (liftReadListPrec (readS_to_Prec readsPrecA) (readS_to_Prec (const readListA))) 0

liftReadListPrecDefault :: (Read1 f) => ReadPrec a -> ReadPrec [a] -> ReadPrec [f a]
liftReadListPrecDefault readPrecA readListPrecA = readS_to_Prec (const (readListWith (readPrec_to_S (liftReadPrec readPrecA readListPrecA) 0)))

class Show1 (f :: Type -> Type) where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
  liftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
  liftShowList showsPrecA showListA = showListWith (liftShowsPrec showsPrecA showListA 0)

showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = liftShowsPrec showsPrec showList

class Eq2 (f :: Type -> Type -> Type) where
  liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool

eq2 :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = liftEq2 (==) (==)

class (Eq2 f) => Ord2 (f :: Type -> Type -> Type) where
  liftCompare2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) -> f a c -> f b d -> Ordering

compare2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Ordering
compare2 = liftCompare2 compare compare

class Read2 (f :: Type -> Type -> Type) where
  liftReadsPrec2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> Int -> ReadS (f a b)
  liftReadList2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
  liftReadPrec2 :: ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec (f a b)
  liftReadListPrec2 :: ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]

  liftReadsPrec2 readsPrecA readListA readsPrecB readListB =
    readPrec_to_S
      ( liftReadPrec2
          (readS_to_Prec readsPrecA)
          (readS_to_Prec (const readListA))
          (readS_to_Prec readsPrecB)
          (readS_to_Prec (const readListB))
      )
  liftReadList2 readsPrecA readListA readsPrecB readListB =
    readListWith (liftReadsPrec2 readsPrecA readListA readsPrecB readListB 0)
  liftReadPrec2 readPrecA readListPrecA readPrecB readListPrecB =
    readS_to_Prec
      ( liftReadsPrec2
          (readPrec_to_S readPrecA)
          (readPrec_to_S readListPrecA 0)
          (readPrec_to_S readPrecB)
          (readPrec_to_S readListPrecB 0)
      )
  liftReadListPrec2 readPrecA readListPrecA readPrecB readListPrecB =
    readS_to_Prec
      ( const
          ( liftReadList2
              (readPrec_to_S readPrecA)
              (readPrec_to_S readListPrecA 0)
              (readPrec_to_S readPrecB)
              (readPrec_to_S readListPrecB 0)
          )
      )

readsPrec2 :: (Read2 f, Read a, Read b) => Int -> ReadS (f a b)
readsPrec2 = liftReadsPrec2 readsPrec readList readsPrec readList

readPrec2 :: (Read2 f, Read a, Read b) => ReadPrec (f a b)
readPrec2 = liftReadPrec2 readPrec readListPrec readPrec readListPrec

liftReadList2Default :: (Read2 f) => (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
liftReadList2Default readsPrecA readListA readsPrecB readListB =
  readPrec_to_S
    ( liftReadListPrec2
        (readS_to_Prec readsPrecA)
        (readS_to_Prec (const readListA))
        (readS_to_Prec readsPrecB)
        (readS_to_Prec (const readListB))
    )
    0

liftReadListPrec2Default :: (Read2 f) => ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]
liftReadListPrec2Default readPrecA readListPrecA readPrecB readListPrecB =
  readS_to_Prec (const (readListWith (readPrec_to_S (liftReadPrec2 readPrecA readListPrecA readPrecB readListPrecB) 0)))

class Show2 (f :: Type -> Type -> Type) where
  liftShowsPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> f a b -> ShowS
  liftShowList2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> [f a b] -> ShowS
  liftShowList2 showsPrecA showListA showsPrecB showListB =
    showListWith (liftShowsPrec2 showsPrecA showListA showsPrecB showListB 0)

showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = liftShowsPrec2 showsPrec showList showsPrec showList

-- Instances for Prelude types

instance Eq1 Maybe where
  liftEq _ Nothing Nothing = True
  liftEq _ Nothing (Just _) = False
  liftEq _ (Just _) Nothing = False
  liftEq eq (Just left) (Just right) = eq left right

instance Ord1 Maybe where
  liftCompare _ Nothing Nothing = EQ
  liftCompare _ Nothing (Just _) = LT
  liftCompare _ (Just _) Nothing = GT
  liftCompare cmp (Just left) (Just right) = cmp left right

instance Show1 Maybe where
  liftShowsPrec _ _ _ Nothing = showString "Nothing"
  liftShowsPrec showsPrecA _ precedence (Just value) = showsUnaryWith showsPrecA "Just" precedence value

instance Read1 Maybe where
  liftReadsPrec readsPrecA _ precedence input =
    readNothing input ++ readsData (readsUnaryWith readsPrecA "Just" Just) precedence input
    where
      readNothing text =
        [(Nothing, rest) | ("Nothing", rest) <- lex text]

instance Eq1 List where
  liftEq _ [] [] = True
  liftEq _ [] (_ : _) = False
  liftEq _ (_ : _) [] = False
  liftEq eq (left : lefts) (right : rights) = eq left right && liftEq eq lefts rights

instance Ord1 List where
  liftCompare _ [] [] = EQ
  liftCompare _ [] (_ : _) = LT
  liftCompare _ (_ : _) [] = GT
  liftCompare cmp (left : lefts) (right : rights) =
    case cmp left right of
      EQ -> liftCompare cmp lefts rights
      result -> result

instance Show1 List where
  liftShowsPrec _ showListA _ = showListA

instance Read1 List where
  liftReadsPrec _ readListA _ = readListA

instance Eq1 Identity where
  liftEq eq (Identity left) (Identity right) = eq left right

instance Ord1 Identity where
  liftCompare cmp (Identity left) (Identity right) = cmp left right

instance Show1 Identity where
  liftShowsPrec showsPrecA _ precedence (Identity value) = showsUnaryWith showsPrecA "Identity" precedence value

instance Read1 Identity where
  liftReadsPrec readsPrecA _ = readsData (readsUnaryWith readsPrecA "Identity" Identity)

instance (Eq a) => Eq1 (Either a) where
  liftEq = liftEq2 (==)

instance (Ord a) => Ord1 (Either a) where
  liftCompare = liftCompare2 compare

instance (Show a) => Show1 (Either a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Read a) => Read1 (Either a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

instance Eq2 Either where
  liftEq2 eqLeft _ (Left left) (Left right) = eqLeft left right
  liftEq2 _ _ (Left _) (Right _) = False
  liftEq2 _ _ (Right _) (Left _) = False
  liftEq2 _ eqRight (Right left) (Right right) = eqRight left right

instance Ord2 Either where
  liftCompare2 cmpLeft _ (Left left) (Left right) = cmpLeft left right
  liftCompare2 _ _ (Left _) (Right _) = LT
  liftCompare2 _ _ (Right _) (Left _) = GT
  liftCompare2 _ cmpRight (Right left) (Right right) = cmpRight left right

instance Show2 Either where
  liftShowsPrec2 showsPrecA _ _ _ precedence (Left value) = showsUnaryWith showsPrecA "Left" precedence value
  liftShowsPrec2 _ _ showsPrecB _ precedence (Right value) = showsUnaryWith showsPrecB "Right" precedence value

instance Read2 Either where
  liftReadsPrec2 readsPrecA _ readsPrecB _ precedence input =
    readsData (readsUnaryWith readsPrecA "Left" Left) precedence input
      ++ readsData (readsUnaryWith readsPrecB "Right" Right) precedence input

instance (Eq a) => Eq1 ((,) a) where
  liftEq = liftEq2 (==)

instance (Ord a) => Ord1 ((,) a) where
  liftCompare = liftCompare2 compare

instance (Show a) => Show1 ((,) a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq2 (,) where
  liftEq2 eqA eqB (leftA, leftB) (rightA, rightB) = eqA leftA rightA && eqB leftB rightB

instance Ord2 (,) where
  liftCompare2 cmpA cmpB (leftA, leftB) (rightA, rightB) =
    case cmpA leftA rightA of
      EQ -> cmpB leftB rightB
      result -> result

instance Show2 (,) where
  liftShowsPrec2 showsPrecA _ showsPrecB _ _ (valueA, valueB) =
    showChar '(' . showsPrecA 0 valueA . showChar ',' . showsPrecB 0 valueB . showChar ')'

-- Helper functions

-- | Parse a data value: try each of the given constructor parsers, after
-- reading the surrounding parentheses.
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader precedence = readParen (precedence > 10) (\input -> [result | (token, rest) <- lex input, result <- reader token rest])

readData :: ReadPrec a -> ReadPrec a
readData = id

-- | Parse a unary constructor application, given the constructor name.
readsUnaryWith :: (Int -> ReadS a) -> String -> (a -> t) -> String -> ReadS t
readsUnaryWith readsPrecA name construct token input =
  if token == name
    then [(construct value, rest) | (value, rest) <- readsPrecA 11 input]
    else []

readUnaryWith :: ReadPrec a -> String -> (a -> t) -> ReadPrec t
readUnaryWith readPrecA name construct =
  readS_to_Prec (readsData (readsUnaryWith (readPrec_to_S readPrecA) name construct))

-- | Parse a binary constructor application, given the constructor name.
readsBinaryWith :: (Int -> ReadS a) -> (Int -> ReadS b) -> String -> (a -> b -> t) -> String -> ReadS t
readsBinaryWith readsPrecA readsPrecB name construct token input =
  if token == name
    then [(construct valueA valueB, rest) | (valueA, afterA) <- readsPrecA 11 input, (valueB, rest) <- readsPrecB 11 afterA]
    else []

readBinaryWith :: ReadPrec a -> ReadPrec b -> String -> (a -> b -> t) -> ReadPrec t
readBinaryWith readPrecA readPrecB name construct =
  readS_to_Prec (readsData (readsBinaryWith (readPrec_to_S readPrecA) (readPrec_to_S readPrecB) name construct))

-- | Show a unary constructor application.
showsUnaryWith :: (Int -> a -> ShowS) -> String -> Int -> a -> ShowS
showsUnaryWith showsPrecA name precedence value =
  showParen (precedence > 10) (showString name . showChar ' ' . showsPrecA 11 value)

-- | Show a binary constructor application.
showsBinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> String -> Int -> a -> b -> ShowS
showsBinaryWith showsPrecA showsPrecB name precedence valueA valueB =
  showParen (precedence > 10) (showString name . showChar ' ' . showsPrecA 11 valueA . showChar ' ' . showsPrecB 11 valueB)

readListWith :: ReadS a -> ReadS [a]
readListWith reader input =
  [(values, rest) | ("[", afterOpen) <- lex input, (values, rest) <- readElements afterOpen]
  where
    readElements text =
      [([], rest) | ("]", rest) <- lex text]
        ++ [(value : values, rest) | (value, afterValue) <- reader text, (values, rest) <- readMore afterValue]
    readMore text =
      [([], rest) | ("]", rest) <- lex text]
        ++ [(value : values, rest) | (",", afterComma) <- lex text, (value, afterValue) <- reader afterComma, (values, rest) <- readMore afterValue]

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] = showString "[]"
showListWith shower (value : values) = showChar '[' . shower value . go values
  where
    go [] = showChar ']'
    go (next : rest) = showChar ',' . shower next . go rest
