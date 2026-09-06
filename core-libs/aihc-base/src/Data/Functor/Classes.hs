{-# LANGUAGE KindSignatures #-}

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

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import GHC.Read (expectP, list, paren, parens)
import Text.ParserCombinators.ReadPrec (ReadPrec, prec, readPrec_to_S, readS_to_Prec, step, (+++))
import Text.Read.Lex (Lexeme (..))
import Text.Show (showListWith)
import Prelude

-- | Lifting of the 'Eq' class to unary type constructors.
class Eq1 (f :: Type -> Type) where
  liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = liftEq (==)

-- | Lifting of the 'Ord' class to unary type constructors.
class (Eq1 f) => Ord1 (f :: Type -> Type) where
  liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
compare1 :: (Ord1 f, Ord a) => f a -> f a -> Ordering
compare1 = liftCompare compare

-- | Lifting of the 'Read' class to unary type constructors.
--
-- Instances define either 'liftReadsPrec' or 'liftReadPrec'.
class Read1 (f :: Type -> Type) where
  liftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
  liftReadsPrec rp rl =
    readPrec_to_S (liftReadPrec (readS_to_Prec rp) (readS_to_Prec (const rl)))

  liftReadList :: (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
  liftReadList rp rl =
    readPrec_to_S (list (liftReadPrec (readS_to_Prec rp) (readS_to_Prec (const rl)))) 0

  liftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)
  liftReadPrec rp rl = readS_to_Prec (liftReadsPrec (readPrec_to_S rp) (readPrec_to_S rl 0))

  liftReadListPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec [f a]
  liftReadListPrec rp rl = readS_to_Prec (\_ -> liftReadList (readPrec_to_S rp) (readPrec_to_S rl 0))

-- | Lift the standard 'readsPrec' and 'readList' functions through the type constructor.
readsPrec1 :: (Read1 f, Read a) => Int -> ReadS (f a)
readsPrec1 = liftReadsPrec readsPrec readList

-- | Lift the standard 'readPrec' and 'readListPrec' functions through the type constructor.
readPrec1 :: (Read1 f, Read a) => ReadPrec (f a)
readPrec1 = liftReadPrec readPrec readListPrec

-- | A possible replacement definition for the 'liftReadList' method.
liftReadListDefault :: (Read1 f) => (Int -> ReadS a) -> ReadS [a] -> ReadS [f a]
liftReadListDefault rp rl =
  readPrec_to_S (liftReadListPrec (readS_to_Prec rp) (readS_to_Prec (const rl))) 0

-- | A possible replacement definition for the 'liftReadListPrec' method.
liftReadListPrecDefault :: (Read1 f) => ReadPrec a -> ReadPrec [a] -> ReadPrec [f a]
liftReadListPrecDefault rp rl = list (liftReadPrec rp rl)

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 (f :: Type -> Type) where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS

  liftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
  liftShowList sp sl = showListWith (liftShowsPrec sp sl 0)

-- | Lift the standard 'showsPrec' and 'showList' functions through the type constructor.
showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = liftShowsPrec showsPrec showList

-- | Lifting of the 'Eq' class to binary type constructors.
class Eq2 (f :: Type -> Type -> Type) where
  liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool

-- | Lift the standard @('==')@ function through the type constructor.
eq2 :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = liftEq2 (==) (==)

-- | Lifting of the 'Ord' class to binary type constructors.
class (Eq2 f) => Ord2 (f :: Type -> Type -> Type) where
  liftCompare2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) -> f a c -> f b d -> Ordering

-- | Lift the standard 'compare' function through the type constructor.
compare2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Ordering
compare2 = liftCompare2 compare compare

-- | Lifting of the 'Read' class to binary type constructors.
--
-- Instances define either 'liftReadsPrec2' or 'liftReadPrec2'.
class Read2 (f :: Type -> Type -> Type) where
  liftReadsPrec2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> Int -> ReadS (f a b)
  liftReadsPrec2 rp1 rl1 rp2 rl2 =
    readPrec_to_S
      ( liftReadPrec2
          (readS_to_Prec rp1)
          (readS_to_Prec (const rl1))
          (readS_to_Prec rp2)
          (readS_to_Prec (const rl2))
      )

  liftReadList2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
  liftReadList2 rp1 rl1 rp2 rl2 =
    readPrec_to_S
      ( list
          ( liftReadPrec2
              (readS_to_Prec rp1)
              (readS_to_Prec (const rl1))
              (readS_to_Prec rp2)
              (readS_to_Prec (const rl2))
          )
      )
      0

  liftReadPrec2 :: ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec (f a b)
  liftReadPrec2 rp1 rl1 rp2 rl2 =
    readS_to_Prec
      ( liftReadsPrec2
          (readPrec_to_S rp1)
          (readPrec_to_S rl1 0)
          (readPrec_to_S rp2)
          (readPrec_to_S rl2 0)
      )

  liftReadListPrec2 :: ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]
  liftReadListPrec2 rp1 rl1 rp2 rl2 =
    readS_to_Prec
      ( \_ ->
          liftReadList2
            (readPrec_to_S rp1)
            (readPrec_to_S rl1 0)
            (readPrec_to_S rp2)
            (readPrec_to_S rl2 0)
      )

-- | Lift the standard 'readsPrec' and 'readList' functions through the type constructor.
readsPrec2 :: (Read2 f, Read a, Read b) => Int -> ReadS (f a b)
readsPrec2 = liftReadsPrec2 readsPrec readList readsPrec readList

-- | Lift the standard 'readPrec' and 'readListPrec' functions through the type constructor.
readPrec2 :: (Read2 f, Read a, Read b) => ReadPrec (f a b)
readPrec2 = liftReadPrec2 readPrec readListPrec readPrec readListPrec

-- | A possible replacement definition for the 'liftReadList2' method.
liftReadList2Default ::
  (Read2 f) => (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> ReadS [f a b]
liftReadList2Default rp1 rl1 rp2 rl2 =
  readPrec_to_S
    ( liftReadListPrec2
        (readS_to_Prec rp1)
        (readS_to_Prec (const rl1))
        (readS_to_Prec rp2)
        (readS_to_Prec (const rl2))
    )
    0

-- | A possible replacement definition for the 'liftReadListPrec2' method.
liftReadListPrec2Default ::
  (Read2 f) => ReadPrec a -> ReadPrec [a] -> ReadPrec b -> ReadPrec [b] -> ReadPrec [f a b]
liftReadListPrec2Default rp1 rl1 rp2 rl2 = list (liftReadPrec2 rp1 rl1 rp2 rl2)

-- | Lifting of the 'Show' class to binary type constructors.
class Show2 (f :: Type -> Type -> Type) where
  liftShowsPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> f a b -> ShowS

  liftShowList2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> [f a b] -> ShowS
  liftShowList2 sp1 sl1 sp2 sl2 = showListWith (liftShowsPrec2 sp1 sl1 sp2 sl2 0)

-- | Lift the standard 'showsPrec' and 'showList' functions through the type constructor.
showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = liftShowsPrec2 showsPrec showList showsPrec showList

-- Instances for Prelude type constructors

instance Eq1 Maybe where
  liftEq _ Nothing Nothing = True
  liftEq _ Nothing (Just _) = False
  liftEq _ (Just _) Nothing = False
  liftEq eq (Just x) (Just y) = eq x y

instance Ord1 Maybe where
  liftCompare _ Nothing Nothing = EQ
  liftCompare _ Nothing (Just _) = LT
  liftCompare _ (Just _) Nothing = GT
  liftCompare comp (Just x) (Just y) = comp x y

instance Read1 Maybe where
  liftReadPrec rp _ =
    parens (readNothing +++ prec 10 (readUnaryWith rp "Just" Just))
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

readNothing :: ReadPrec (Maybe a)
readNothing = do
  expectP (Ident "Nothing")
  return Nothing

instance Show1 Maybe where
  liftShowsPrec _ _ _ Nothing = showString "Nothing"
  liftShowsPrec sp _ d (Just x) = showsUnaryWith sp "Just" d x

instance Eq1 [] where
  liftEq _ [] [] = True
  liftEq _ [] (_ : _) = False
  liftEq _ (_ : _) [] = False
  liftEq eq (x : xs) (y : ys) = eq x y && liftEq eq xs ys

instance Ord1 [] where
  liftCompare _ [] [] = EQ
  liftCompare _ [] (_ : _) = LT
  liftCompare _ (_ : _) [] = GT
  liftCompare comp (x : xs) (y : ys) =
    case comp x y of
      EQ -> liftCompare comp xs ys
      other -> other

instance Read1 [] where
  liftReadPrec _ rl = rl
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

instance Show1 [] where
  liftShowsPrec _ sl _ = sl

instance Eq1 NonEmpty where
  liftEq eq (a :| as) (b :| bs) = eq a b && liftEq eq as bs

instance Ord1 NonEmpty where
  liftCompare cmp (a :| as) (b :| bs) =
    case cmp a b of
      EQ -> liftCompare cmp as bs
      other -> other

instance Read1 NonEmpty where
  liftReadPrec rp rl =
    parens
      ( prec
          5
          ( do
              a <- step rp
              expectP (Symbol ":|")
              as <- step (liftReadPrec rp rl)
              return (a :| as)
          )
      )
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

instance Show1 NonEmpty where
  liftShowsPrec sp sl d (a :| as) =
    showParen (d > 5) (sp 6 a . showString " :| " . liftShowsPrec sp sl 6 as)

instance Eq2 (,) where
  liftEq2 e1 e2 (x1, y1) (x2, y2) = e1 x1 x2 && e2 y1 y2

instance Ord2 (,) where
  liftCompare2 comp1 comp2 (x1, y1) (x2, y2) =
    case comp1 x1 x2 of
      EQ -> comp2 y1 y2
      other -> other

instance Read2 (,) where
  liftReadPrec2 rp1 _ rp2 _ =
    parens
      ( paren
          ( do
              x <- rp1
              expectP (Punc ",")
              y <- rp2
              return (x, y)
          )
      )
  liftReadListPrec2 = liftReadListPrec2Default
  liftReadList2 = liftReadList2Default

instance Show2 (,) where
  liftShowsPrec2 sp1 _ sp2 _ _ (x, y) =
    showChar '(' . sp1 0 x . showChar ',' . sp2 0 y . showChar ')'

instance (Eq a) => Eq1 ((,) a) where
  liftEq = liftEq2 (==)

instance (Ord a) => Ord1 ((,) a) where
  liftCompare = liftCompare2 compare

instance (Read a) => Read1 ((,) a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

instance (Show a) => Show1 ((,) a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq2 Either where
  liftEq2 e1 _ (Left x) (Left y) = e1 x y
  liftEq2 _ _ (Left _) (Right _) = False
  liftEq2 _ _ (Right _) (Left _) = False
  liftEq2 _ e2 (Right x) (Right y) = e2 x y

instance Ord2 Either where
  liftCompare2 comp1 _ (Left x) (Left y) = comp1 x y
  liftCompare2 _ _ (Left _) (Right _) = LT
  liftCompare2 _ _ (Right _) (Left _) = GT
  liftCompare2 _ comp2 (Right x) (Right y) = comp2 x y

instance Read2 Either where
  liftReadPrec2 rp1 _ rp2 _ =
    readData (readUnaryWith rp1 "Left" Left +++ readUnaryWith rp2 "Right" Right)
  liftReadListPrec2 = liftReadListPrec2Default
  liftReadList2 = liftReadList2Default

instance Show2 Either where
  liftShowsPrec2 sp1 _ _ _ d (Left x) = showsUnaryWith sp1 "Left" d x
  liftShowsPrec2 _ _ sp2 _ d (Right x) = showsUnaryWith sp2 "Right" d x

instance (Eq a) => Eq1 (Either a) where
  liftEq = liftEq2 (==)

instance (Ord a) => Ord1 (Either a) where
  liftCompare = liftCompare2 compare

instance (Read a) => Read1 (Either a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

instance (Show a) => Show1 (Either a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq1 Identity where
  liftEq eq (Identity x) (Identity y) = eq x y

instance Ord1 Identity where
  liftCompare comp (Identity x) (Identity y) = comp x y

instance Read1 Identity where
  liftReadPrec rp _ = readData (readUnaryWith rp "Identity" Identity)
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

instance Show1 Identity where
  liftShowsPrec sp _ d (Identity x) = showsUnaryWith sp "Identity" d x

instance Eq2 Const where
  liftEq2 eq _ (Const x) (Const y) = eq x y

instance Ord2 Const where
  liftCompare2 comp _ (Const x) (Const y) = comp x y

instance Read2 Const where
  liftReadPrec2 rp _ _ _ = readData (readUnaryWith rp "Const" Const)
  liftReadListPrec2 = liftReadListPrec2Default
  liftReadList2 = liftReadList2Default

instance Show2 Const where
  liftShowsPrec2 sp _ _ _ d (Const x) = showsUnaryWith sp "Const" d x

instance (Eq a) => Eq1 (Const a) where
  liftEq = liftEq2 (==)

instance (Ord a) => Ord1 (Const a) where
  liftCompare = liftCompare2 compare

instance (Read a) => Read1 (Const a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

instance (Show a) => Show1 (Const a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq1 Proxy where
  liftEq _ _ _ = True

instance Ord1 Proxy where
  liftCompare _ _ _ = EQ

instance Show1 Proxy where
  liftShowsPrec _ _ _ _ = showString "Proxy"

instance Read1 Proxy where
  liftReadPrec _ _ = parens readProxy
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

readProxy :: ReadPrec (Proxy a)
readProxy = do
  expectP (Ident "Proxy")
  return Proxy

instance Eq1 Down where
  liftEq eq (Down x) (Down y) = eq x y

instance Ord1 Down where
  liftCompare comp (Down x) (Down y) =
    case comp x y of
      LT -> GT
      EQ -> EQ
      GT -> LT

instance Read1 Down where
  liftReadPrec rp _ = readData (readUnaryWith rp "Down" Down)
  liftReadListPrec = liftReadListPrecDefault
  liftReadList = liftReadListDefault

instance Show1 Down where
  liftShowsPrec sp _ d (Down x) = showsUnaryWith sp "Down" d x

-- Building blocks

-- | @'readsData' p d@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and passes
-- it to @p@.  Parsers for various constructors can be constructed with
-- 'readsUnaryWith' and 'readsBinaryWith', and combined with @mappend@
-- from the @Monoid@ class.
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader d =
  readParen (d > 10) (concatMap (uncurry reader) . lex)

-- | @'readData' p@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and passes
-- it to @p@.  Parsers for various constructors can be constructed with
-- 'readUnaryWith' and 'readBinaryWith', and combined with '+++'.
readData :: ReadPrec a -> ReadPrec a
readData reader = parens (prec 10 reader)

-- | @'readsUnaryWith' rp n c n'@ matches the name of a unary data constructor
-- and then parses its argument using @rp@.
readsUnaryWith :: (Int -> ReadS a) -> String -> (a -> t) -> String -> ReadS t
readsUnaryWith rp name cons keyword input =
  if keyword == name
    then mapReads cons (rp 11 input)
    else []

-- | @'readUnaryWith' rp n c'@ matches the name of a unary data constructor
-- and then parses its argument using @rp@.
readUnaryWith :: ReadPrec a -> String -> (a -> t) -> ReadPrec t
readUnaryWith rp name cons = do
  expectP (Ident name)
  x <- step rp
  return (cons x)

-- | @'readsBinaryWith' rp1 rp2 n c n'@ matches the name of a binary
-- data constructor and then parses its arguments using @rp1@ and @rp2@
-- respectively.
readsBinaryWith :: (Int -> ReadS a) -> (Int -> ReadS b) -> String -> (a -> b -> t) -> String -> ReadS t
readsBinaryWith rp1 rp2 name cons keyword input =
  if keyword == name
    then concatMap (\(x, rest) -> mapReads (cons x) (rp2 11 rest)) (rp1 11 input)
    else []

-- | @'readBinaryWith' rp1 rp2 n c'@ matches the name of a binary
-- data constructor and then parses its arguments using @rp1@ and @rp2@
-- respectively.
readBinaryWith :: ReadPrec a -> ReadPrec b -> String -> (a -> b -> t) -> ReadPrec t
readBinaryWith rp1 rp2 name cons = do
  expectP (Ident name)
  x <- step rp1
  y <- step rp2
  return (cons x y)

-- | @'showsUnaryWith' sp n d x@ produces the string representation of a
-- unary data constructor with name @n@ and argument @x@, in precedence
-- context @d@.
showsUnaryWith :: (Int -> a -> ShowS) -> String -> Int -> a -> ShowS
showsUnaryWith sp name d x = showParen (d > 10) (showString name . showChar ' ' . sp 11 x)

-- | @'showsBinaryWith' sp1 sp2 n d x y@ produces the string
-- representation of a binary data constructor with name @n@ and arguments
-- @x@ and @y@, in precedence context @d@.
showsBinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> String -> Int -> a -> b -> ShowS
showsBinaryWith sp1 sp2 name d x y =
  showParen (d > 10) (showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y)

mapReads :: (a -> b) -> [(a, String)] -> [(b, String)]
mapReads _ [] = []
mapReads f ((value, rest) : results) = (f value, rest) : mapReads f results
