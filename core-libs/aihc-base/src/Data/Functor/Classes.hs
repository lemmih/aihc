{-# LANGUAGE KindSignatures #-}

-- | Lifted versions of the Eq, Ord, Read, and Show classes for unary and
-- binary type constructors.
module Data.Functor.Classes
  ( Eq1 (..),
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
    readsData,
    readData,
    readsUnaryWith,
    readUnaryWith,
    readsBinaryWith,
    readBinaryWith,
    showsUnaryWith,
    showsBinaryWith,
    readsUnary,
    readsUnary1,
    readsBinary1,
    showsUnary,
    showsUnary1,
    showsBinary1,
  )
where

import Data.Functor.Identity (Identity (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import Data.Kind (Type)
import GHC.Read (expectP, list, parens)
import GHC.Read.Lex (Lexeme (..))
import GHC.Show (showListWith)
import Prelude

-- | Equality that takes the equality of the element type.
class Eq1 (f :: Type -> Type) where
  liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = liftEq (==)

-- | Order that takes the order of the element type.
class (Eq1 f) => Ord1 (f :: Type -> Type) where
  liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

compare1 :: (Ord1 f, Ord a) => f a -> f a -> Ordering
compare1 = liftCompare compare

-- | Parsing that takes the parser of the element type.
class Read1 (f :: Type -> Type) where
  liftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
  liftReadList :: (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
  liftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)
  liftReadListPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec [f a]

  liftReadsPrec readElement readElements =
    readPrec_to_S (liftReadPrec (readS_to_Prec readElement) (readS_to_Prec (\_ -> readElements)))
  liftReadList readElement readElements =
    readPrec_to_S (list (liftReadPrec (readS_to_Prec readElement) (readS_to_Prec (\_ -> readElements)))) 0
  liftReadPrec readElement readElements =
    readS_to_Prec (liftReadsPrec (readPrec_to_S readElement) (readPrec_to_S readElements 0))
  liftReadListPrec readElement readElements =
    readS_to_Prec (\_ -> liftReadList (readPrec_to_S readElement) (readPrec_to_S readElements 0))

readsPrec1 :: (Read1 f, Read a) => Int -> ReadS (f a)
readsPrec1 = liftReadsPrec readsPrec readList

readPrec1 :: (Read1 f, Read a) => ReadPrec (f a)
readPrec1 = liftReadPrec readPrec readListPrec

liftReadListDefault :: (Read1 f) => (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
liftReadListDefault readElement readElements =
  readPrec_to_S (liftReadListPrec (readS_to_Prec readElement) (readS_to_Prec (\_ -> readElements))) 0

liftReadListPrecDefault :: (Read1 f) => ReadPrec a -> ReadPrec [a] -> ReadPrec [f a]
liftReadListPrecDefault readElement readElements = list (liftReadPrec readElement readElements)

-- | Showing that takes the shower of the element type.
class Show1 (f :: Type -> Type) where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
  liftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
  liftShowList showElement showElements = showListWith (liftShowsPrec showElement showElements 0)

showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = liftShowsPrec showsPrec showList

-- | Equality for binary type constructors.
class Eq2 (f :: Type -> Type -> Type) where
  liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool

eq2 :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = liftEq2 (==) (==)

-- | Order for binary type constructors.
class (Eq2 f) => Ord2 (f :: Type -> Type -> Type) where
  liftCompare2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) -> f a c -> f b d -> Ordering

compare2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Ordering
compare2 = liftCompare2 compare compare

-- | Parsing for binary type constructors.
class Read2 (f :: Type -> Type -> Type) where
  liftReadsPrec2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> Int -> ReadS (f a b)
  liftReadList2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
  liftReadPrec2 :: ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec (f a b)
  liftReadListPrec2 :: ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]

  liftReadsPrec2 readLeft readLefts readRight readRights =
    readPrec_to_S
      ( liftReadPrec2
          (readS_to_Prec readLeft)
          (readS_to_Prec (\_ -> readLefts))
          (readS_to_Prec readRight)
          (readS_to_Prec (\_ -> readRights))
      )
  liftReadList2 readLeft readLefts readRight readRights =
    readPrec_to_S
      ( list
          ( liftReadPrec2
              (readS_to_Prec readLeft)
              (readS_to_Prec (\_ -> readLefts))
              (readS_to_Prec readRight)
              (readS_to_Prec (\_ -> readRights))
          )
      )
      0
  liftReadPrec2 readLeft readLefts readRight readRights =
    readS_to_Prec
      ( liftReadsPrec2
          (readPrec_to_S readLeft)
          (readPrec_to_S readLefts 0)
          (readPrec_to_S readRight)
          (readPrec_to_S readRights 0)
      )
  liftReadListPrec2 readLeft readLefts readRight readRights =
    readS_to_Prec
      ( \_ ->
          liftReadList2
            (readPrec_to_S readLeft)
            (readPrec_to_S readLefts 0)
            (readPrec_to_S readRight)
            (readPrec_to_S readRights 0)
      )

readsPrec2 :: (Read2 f, Read a, Read b) => Int -> ReadS (f a b)
readsPrec2 = liftReadsPrec2 readsPrec readList readsPrec readList

readPrec2 :: (Read2 f, Read a, Read b) => ReadPrec (f a b)
readPrec2 = liftReadPrec2 readPrec readListPrec readPrec readListPrec

liftReadList2Default :: (Read2 f) => (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
liftReadList2Default readLeft readLefts readRight readRights =
  readPrec_to_S
    ( liftReadListPrec2
        (readS_to_Prec readLeft)
        (readS_to_Prec (\_ -> readLefts))
        (readS_to_Prec readRight)
        (readS_to_Prec (\_ -> readRights))
    )
    0

liftReadListPrec2Default :: (Read2 f) => ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]
liftReadListPrec2Default readLeft readLefts readRight readRights =
  list (liftReadPrec2 readLeft readLefts readRight readRights)

-- | Showing for binary type constructors.
class Show2 (f :: Type -> Type -> Type) where
  liftShowsPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> f a b -> ShowS
  liftShowList2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> [f a b] -> ShowS
  liftShowList2 showLeft showLefts showRight showRights =
    showListWith (liftShowsPrec2 showLeft showLefts showRight showRights 0)

showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = liftShowsPrec2 showsPrec showList showsPrec showList

-- Instances for the core types.

instance Eq1 Maybe where
  liftEq _ Nothing Nothing = True
  liftEq _ Nothing (Just _) = False
  liftEq _ (Just _) Nothing = False
  liftEq eq (Just left) (Just right) = eq left right

instance Ord1 Maybe where
  liftCompare _ Nothing Nothing = EQ
  liftCompare _ Nothing (Just _) = LT
  liftCompare _ (Just _) Nothing = GT
  liftCompare comp (Just left) (Just right) = comp left right

instance Read1 Maybe where
  liftReadsPrec readElement _ precedence input =
    readParen False readNothing input ++ readsData (readsUnaryWith readElement "Just" Just) precedence input
    where
      readNothing text = [(Nothing, rest) | (keyword, rest) <- lex text, keyword == "Nothing"]

instance Show1 Maybe where
  liftShowsPrec _ _ _ Nothing = showString "Nothing"
  liftShowsPrec showElement _ precedence (Just value) = showsUnaryWith showElement "Just" precedence value

instance Eq1 [] where
  liftEq _ [] [] = True
  liftEq _ [] (_ : _) = False
  liftEq _ (_ : _) [] = False
  liftEq eq (left : lefts) (right : rights) = eq left right && liftEq eq lefts rights

instance Ord1 [] where
  liftCompare _ [] [] = EQ
  liftCompare _ [] (_ : _) = LT
  liftCompare _ (_ : _) [] = GT
  liftCompare comp (left : lefts) (right : rights) =
    case comp left right of
      EQ -> liftCompare comp lefts rights
      result -> result

instance Read1 [] where
  liftReadsPrec _ readElements _ = readElements

instance Show1 [] where
  liftShowsPrec _ showElements _ = showElements

instance Eq1 Identity where
  liftEq eq (Identity left) (Identity right) = eq left right

instance Ord1 Identity where
  liftCompare comp (Identity left) (Identity right) = comp left right

instance Read1 Identity where
  liftReadsPrec readElement _ = readsData (readsUnaryWith readElement "Identity" Identity)

instance Show1 Identity where
  liftShowsPrec showElement _ precedence (Identity value) = showsUnaryWith showElement "Identity" precedence value

instance Eq1 NonEmpty where
  liftEq eq (left :| lefts) (right :| rights) = eq left right && liftEq eq lefts rights

instance Ord1 NonEmpty where
  liftCompare comp (left :| lefts) (right :| rights) =
    case comp left right of
      EQ -> liftCompare comp lefts rights
      result -> result

instance Read1 NonEmpty where
  liftReadsPrec readElement readElements precedence =
    readParen
      (precedence > 5)
      ( \input ->
          [ (value :| values, rest)
          | (value, afterValue) <- readElement 6 input,
            (operator, afterOperator) <- lex afterValue,
            operator == ":|",
            (values, rest) <- readElements afterOperator
          ]
      )

instance Show1 NonEmpty where
  liftShowsPrec showElement showElements precedence (value :| values) =
    showParen (precedence > 5) (showElement 6 value . showString " :| " . showElements values)

instance Eq2 (,) where
  liftEq2 eqLeft eqRight (left1, right1) (left2, right2) = eqLeft left1 left2 && eqRight right1 right2

instance Ord2 (,) where
  liftCompare2 compLeft compRight (left1, right1) (left2, right2) =
    case compLeft left1 left2 of
      EQ -> compRight right1 right2
      result -> result

instance Read2 (,) where
  liftReadsPrec2 readLeft _ readRight _ _ =
    readParen
      False
      ( \input ->
          [ ((left, right), rest)
          | (open, afterOpen) <- lex input,
            open == "(",
            (left, afterLeft) <- readLeft 0 afterOpen,
            (comma, afterComma) <- lex afterLeft,
            comma == ",",
            (right, afterRight) <- readRight 0 afterComma,
            (close, rest) <- lex afterRight,
            close == ")"
          ]
      )

instance Show2 (,) where
  liftShowsPrec2 showLeft _ showRight _ _ (left, right) =
    showChar '(' . showLeft 0 left . showChar ',' . showRight 0 right . showChar ')'

instance (Eq a) => Eq1 ((,) a) where
  liftEq = liftEq2 (==)

instance (Ord a) => Ord1 ((,) a) where
  liftCompare = liftCompare2 compare

instance (Read a) => Read1 ((,) a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Show a) => Show1 ((,) a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq2 Either where
  liftEq2 eqLeft _ (Left left1) (Left left2) = eqLeft left1 left2
  liftEq2 _ _ (Left _) (Right _) = False
  liftEq2 _ _ (Right _) (Left _) = False
  liftEq2 _ eqRight (Right right1) (Right right2) = eqRight right1 right2

instance Ord2 Either where
  liftCompare2 compLeft _ (Left left1) (Left left2) = compLeft left1 left2
  liftCompare2 _ _ (Left _) (Right _) = LT
  liftCompare2 _ _ (Right _) (Left _) = GT
  liftCompare2 _ compRight (Right right1) (Right right2) = compRight right1 right2

instance Read2 Either where
  liftReadsPrec2 readLeft _ readRight _ precedence input =
    readsData (readsUnaryWith readLeft "Left" Left) precedence input
      ++ readsData (readsUnaryWith readRight "Right" Right) precedence input

instance Show2 Either where
  liftShowsPrec2 showLeft _ _ _ precedence (Left value) = showsUnaryWith showLeft "Left" precedence value
  liftShowsPrec2 _ _ showRight _ precedence (Right value) = showsUnaryWith showRight "Right" precedence value

instance (Eq a) => Eq1 (Either a) where
  liftEq = liftEq2 (==)

instance (Ord a) => Ord1 (Either a) where
  liftCompare = liftCompare2 compare

instance (Read a) => Read1 (Either a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Show a) => Show1 (Either a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

-- Building blocks for instances.

-- | Parse a constructor application. The reader gets the constructor name
-- and the text after it.
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader precedence =
  readParen (precedence > 10) (\input -> [result | (keyword, rest) <- lex input, result <- reader keyword rest])

readData :: ReadPrec a -> ReadPrec a
readData reader = parens (prec 10 reader)

readsUnaryWith :: (Int -> ReadS a) -> String -> (a -> t) -> String -> ReadS t
readsUnaryWith readArgument name construct keyword input =
  [(construct value, rest) | keyword == name, (value, rest) <- readArgument 11 input]

readUnaryWith :: ReadPrec a -> String -> (a -> t) -> ReadPrec t
readUnaryWith readArgument name construct = do
  expectP (Ident name)
  value <- step readArgument
  return (construct value)

readsBinaryWith :: (Int -> ReadS a) -> (Int -> ReadS b) -> String -> (a -> b -> t) -> String -> ReadS t
readsBinaryWith readFirst readSecond name construct keyword input =
  [ (construct first second, rest)
  | keyword == name,
    (first, afterFirst) <- readFirst 11 input,
    (second, rest) <- readSecond 11 afterFirst
  ]

readBinaryWith :: ReadPrec a -> ReadPrec b -> String -> (a -> b -> t) -> ReadPrec t
readBinaryWith readFirst readSecond name construct = do
  expectP (Ident name)
  first <- step readFirst
  second <- step readSecond
  return (construct first second)

showsUnaryWith :: (Int -> a -> ShowS) -> String -> Int -> a -> ShowS
showsUnaryWith showArgument name precedence value =
  showParen (precedence > 10) (showString name . showChar ' ' . showArgument 11 value)

showsBinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> String -> Int -> a -> b -> ShowS
showsBinaryWith showFirst showSecond name precedence first second =
  showParen (precedence > 10) (showString name . showChar ' ' . showFirst 11 first . showChar ' ' . showSecond 11 second)

readsUnary :: (Read a) => String -> (a -> t) -> String -> ReadS t
readsUnary name construct = readsUnaryWith readsPrec name construct

readsUnary1 :: (Read1 f, Read a) => String -> (f a -> t) -> String -> ReadS t
readsUnary1 name construct = readsUnaryWith readsPrec1 name construct

readsBinary1 :: (Read1 f, Read1 g, Read a) => String -> (f a -> g a -> t) -> String -> ReadS t
readsBinary1 name construct = readsBinaryWith readsPrec1 readsPrec1 name construct

showsUnary :: (Show a) => String -> Int -> a -> ShowS
showsUnary name = showsUnaryWith showsPrec name

showsUnary1 :: (Show1 f, Show a) => String -> Int -> f a -> ShowS
showsUnary1 name = showsUnaryWith showsPrec1 name

showsBinary1 :: (Show1 f, Show1 g, Show a) => String -> Int -> f a -> g a -> ShowS
showsBinary1 name = showsBinaryWith showsPrec1 showsPrec1 name
