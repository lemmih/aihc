{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

{-# HLINT ignore "Use sequence_" #-}

module Prelude
  ( Applicative (..),
    Bounded (..),
    Bool (..),
    Char (..),
    Either (..),
    Enum (..),
    Eq (..),
    Functor (..),
    Fractional (..),
    Floating (..),
    Float,
    Double,
    IO,
    Int,
    Integral (..),
    Integer,
    List (..),
    Maybe (..),
    Monad (..),
    MonadFail (..),
    Num (..),
    Ord (..),
    Ordering (..),
    Rational,
    Ratio,
    Real (..),
    RealFrac (..),
    RealFloat (..),
    Read (..),
    ReadS,
    Show (..),
    ShowS,
    String,
    FilePath,
    (&&),
    ($),
    ($!),
    (<$>),
    (*>),
    (.),
    (++),
    Foldable (elem, foldMap, foldl, foldl1, foldr, foldr1, length, maximum, minimum, null, product, sum),
    Traversable (traverse, sequenceA, mapM, sequence),
    map,
    sequence_,
    zip,
    and,
    all,
    any,
    concat,
    concatMap,
    filter,
    head,
    tail,
    last,
    init,
    reverse,
    lookup,
    zipWith,
    unzip,
    break,
    dropWhile,
    replicate,
    fst,
    snd,
    span,
    take,
    drop,
    takeWhile,
    maybe,
    either,
    mapM_,
    flip,
    error,
    (=<<),
    (/=),
    (==),
    id,
    even,
    fromIntegral,
    gcd,
    lcm,
    not,
    odd,
    numerator,
    denominator,
    otherwise,
    print,
    putChar,
    putStr,
    putStrLn,
    showChar,
    showParen,
    shows,
    showString,
    read,
    reads,
    readParen,
    lex,
    seq,
    realToFrac,
    (%),
    (^),
    (^^),
    (||),
    Semigroup ((<>)),
    Monoid (..),
    Word,
    IOError,
    ioError,
    userError,
    undefined,
    errorWithoutStackTrace,
    const,
    curry,
    uncurry,
    until,
    asTypeOf,
    subtract,
    splitAt,
    lines,
    unlines,
    words,
    unwords,
    iterate,
    repeat,
    cycle,
    scanl,
    scanl1,
    scanr,
    scanr1,
    notElem,
    or,
    (!!),
    zip3,
    zipWith3,
    unzip3,
  )
where

import Data.Bool (Bool (..), not, otherwise, (&&), (||))
import Data.Either (Either (..), either)
import Data.Maybe (maybe)
import Data.Semigroup.Internal (Monoid (..), Semigroup (..))
import GHC.Base (Applicative (..), Functor (..), List (..), Maybe (..), Monad (..), String, const, flip, id, ($), (++), (.))
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Err (error, errorWithoutStackTrace, undefined)
import GHC.Float (Double, Float, Floating (..), RealFloat (..))
import GHC.IO (FilePath, IO (..))
import GHC.IO.Exception (IOError, ioError, userError)
import GHC.IO.Handle.Text (hPutStr)
import GHC.IO.StdHandles (stdout)
import GHC.Int (Int (..))
import GHC.Integer (Integer)
import GHC.Internal.Char (Char (..))
import GHC.Internal.Classes (Eq (..), Ord (..), Ordering (..))
import GHC.Internal.Foldable (Foldable (..))
import GHC.Internal.Integer (Integer (..), compareInteger#, eqInteger#, integerAbs, integerQuotRemWord#)
import GHC.Internal.Traversable (Traversable (..))
import GHC.Num (Num (..))
import GHC.Prim (Int#, Word#, chr#, eqWord#, int2Word#, minusWord#, ord#, quotRemWord#, seq, word2Int#, word8ToWord#, (+#), (<#), (==#))
import GHC.Real
  ( Fractional (..),
    Integral (..),
    Ratio,
    Rational,
    Real (..),
    RealFrac (..),
    denominator,
    even,
    fromIntegral,
    gcd,
    lcm,
    numerator,
    odd,
    realToFrac,
    (%),
    (^),
    (^^),
  )
import GHC.Show (Show (..), ShowS, showChar, showParen, showString, shows)
import GHC.Tuple ()
import GHC.Types (RuntimeRep, TYPE, Type)
import GHC.Word (Word (..), Word8 (..))
import Text.ParserCombinators.ReadPrec (Prec, ReadPrec, minPrec, readPrec_to_S, readS_to_Prec)

type ReadS a = String -> [(a, String)]

-- | Function application. The result type can have any runtime
-- representation, as in GHC. The definition returns the function itself, so
-- the value that the definition returns is always lifted.
($!) :: (a -> b) -> a -> b
function $! argument = argument `seq` function argument

infixr 0 $!

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 4 <$>

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map function (value : values) = function value : map function values

concat :: [[a]] -> [a]
concat = foldr (++) []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap function = concat . map function

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter predicate (value : values) =
  if predicate value
    then value : filter predicate values
    else filter predicate values

head :: [a] -> a
head [] = error "Prelude.head: empty list"
head (value : _) = value

tail :: [a] -> [a]
tail [] = error "Prelude.tail: empty list"
tail (_ : values) = values

last :: [a] -> a
last [] = error "Prelude.last: empty list"
last (value : values) = lastWithDefault value values

lastWithDefault :: a -> [a] -> a
lastWithDefault value [] = value
lastWithDefault _ (value : values) = lastWithDefault value values

init :: [a] -> [a]
init [] = error "Prelude.init: empty list"
init [_] = []
init (value : values) = value : init values

reverse :: [a] -> [a]
reverse = reverseOnto []

reverseOnto :: [a] -> [a] -> [a]
reverseOnto = foldl (flip (:))

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup key ((candidate, value) : values) =
  if key == candidate
    then Just value
    else lookup key values

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith function (left : lefts) (right : rights) = function left right : zipWith function lefts rights

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((left, right) : values) =
  case unzip values of
    (lefts, rights) -> (left : lefts, right : rights)

zip :: [a] -> [b] -> [(a, b)]
zip (left : lefts) (right : rights) = (left, right) : zip lefts rights
zip _ _ = []

and :: [Bool] -> Bool
and = foldr (&&) True

all :: (a -> Bool) -> [a] -> Bool
all predicate = foldr (\value result -> predicate value && result) True

any :: (a -> Bool) -> [a] -> Bool
any predicate = foldr (\value result -> predicate value || result) False

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break predicate values@(value : rest) =
  if predicate value
    then ([], values)
    else case break predicate rest of
      (prefix, suffix) -> (value : prefix, suffix)

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile predicate values@(value : rest) =
  if predicate value
    then dropWhile predicate rest
    else values

replicate :: Int -> a -> [a]
replicate count value =
  if count <= 0
    then []
    else value : replicate (count - 1) value

fst :: (a, b) -> a
fst (left, _) = left

snd :: (a, b) -> b
snd (_, right) = right

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span predicate values@(value : rest) =
  if predicate value
    then case span predicate rest of
      (prefix, suffix) -> (value : prefix, suffix)
    else ([], values)

take :: Int -> [a] -> [a]
take count values =
  if count <= 0
    then []
    else case values of
      [] -> []
      value : rest -> value : take (count - 1) rest

drop :: Int -> [a] -> [a]
drop count values =
  if count <= 0
    then values
    else case values of
      [] -> []
      _ : rest -> drop (count - 1) rest

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile predicate (value : values) =
  if predicate value
    then value : takeWhile predicate values
    else []

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ _ [] = return ()
mapM_ function (value : values) = function value >> mapM_ function values

sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = foldr (>>) (return ())

class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]
  readPrec :: ReadPrec a
  readListPrec :: ReadPrec [a]

  readsPrec = readPrec_to_S readPrec
  readList = readPrec_to_S readListPrec minPrec
  readPrec = readS_to_Prec readsPrec
  readListPrec = readS_to_Prec defaultReadListParser

defaultReadListParser :: (Read a) => Prec -> ReadS [a]
defaultReadListParser _ = readList

reads :: (Read a) => ReadS a
reads = readsPrec minPrec

read :: (Read a) => String -> a
read input =
  case completePreludeReads (reads input) of
    [value] -> value
    [] -> error "Prelude.read: no parse"
    _ -> error "Prelude.read: ambiguous parse"

completePreludeReads :: [(a, String)] -> [a]
completePreludeReads [] = []
completePreludeReads ((value, rest) : results) =
  case lex rest of
    [([], [])] -> value : completePreludeReads results
    _ -> completePreludeReads results

readParen :: Bool -> ReadS a -> ReadS a
readParen required parser =
  case required of
    True -> mandatory
    False -> optional
  where
    optional input = parser input ++ mandatory input
    mandatory input = bindPreludeReadS (matchPreludeLexeme "(" input) afterOpen
    afterOpen _ input = bindPreludeReadS (optional input) afterValue
    afterValue value input = bindPreludeReadS (matchPreludeLexeme ")" input) (afterClose value)
    afterClose value _ rest = [(value, rest)]

bindPreludeReadS :: [(a, String)] -> (a -> String -> [(b, String)]) -> [(b, String)]
bindPreludeReadS [] _ = []
bindPreludeReadS ((value, rest) : results) next =
  next value rest ++ bindPreludeReadS results next

matchPreludeLexeme :: String -> ReadS String
matchPreludeLexeme expected input =
  case lex input of
    (actual, rest) : _ ->
      case actual == expected of
        True -> [(actual, rest)]
        False -> []
    _ -> []

lex :: ReadS String
lex input =
  case skipPreludeReadSpaces input of
    [] -> [([], [])]
    char : rest ->
      case isPreludeReadPunctuation char of
        True -> [([char], rest)]
        False ->
          case takePreludeReadToken (char : rest) of
            (token, remaining) -> [(token, remaining)]

skipPreludeReadSpaces :: String -> String
skipPreludeReadSpaces [] = []
skipPreludeReadSpaces (' ' : rest) = skipPreludeReadSpaces rest
skipPreludeReadSpaces ('\t' : rest) = skipPreludeReadSpaces rest
skipPreludeReadSpaces ('\n' : rest) = skipPreludeReadSpaces rest
skipPreludeReadSpaces ('\r' : rest) = skipPreludeReadSpaces rest
skipPreludeReadSpaces input = input

takePreludeReadToken :: String -> (String, String)
takePreludeReadToken [] = ([], [])
takePreludeReadToken input@(char : rest) =
  case isPreludeReadDelimiter char of
    True -> ([], input)
    False ->
      case takePreludeReadToken rest of
        (token, remaining) -> (char : token, remaining)

isPreludeReadDelimiter :: Char -> Bool
isPreludeReadDelimiter ' ' = True
isPreludeReadDelimiter '\t' = True
isPreludeReadDelimiter '\n' = True
isPreludeReadDelimiter '\r' = True
isPreludeReadDelimiter char = isPreludeReadPunctuation char

isPreludeReadPunctuation :: Char -> Bool
isPreludeReadPunctuation '(' = True
isPreludeReadPunctuation ')' = True
isPreludeReadPunctuation '[' = True
isPreludeReadPunctuation ']' = True
isPreludeReadPunctuation '{' = True
isPreludeReadPunctuation '}' = True
isPreludeReadPunctuation ',' = True
isPreludeReadPunctuation ';' = True
isPreludeReadPunctuation _ = False

instance (Ord a) => Ord [a] where
  compare = compareList
  xs < ys = lessBy compareList xs ys
  xs <= ys = lessOrEqualBy compareList xs ys
  xs > ys = greaterBy compareList xs ys
  xs >= ys = greaterOrEqualBy compareList xs ys
  max = maxBy compareList
  min = minBy compareList

instance Ord () where
  compare () () = EQ
  () < () = False
  () <= () = True
  () > () = False
  () >= () = True
  max () () = ()
  min () () = ()

instance (Ord a) => Ord (Maybe a) where
  compare = compareMaybe
  x < y = lessBy compareMaybe x y
  x <= y = lessOrEqualBy compareMaybe x y
  x > y = greaterBy compareMaybe x y
  x >= y = greaterOrEqualBy compareMaybe x y
  max = maxBy compareMaybe
  min = minBy compareMaybe

instance (Ord a, Ord b) => Ord (Either a b) where
  compare = compareEither
  x < y = lessBy compareEither x y
  x <= y = lessOrEqualBy compareEither x y
  x > y = greaterBy compareEither x y
  x >= y = greaterOrEqualBy compareEither x y
  max = maxBy compareEither
  min = minBy compareEither

instance (Ord a, Ord b) => Ord (a, b) where
  compare = comparePair
  left < right = lessBy comparePair left right
  left <= right = lessOrEqualBy comparePair left right
  left > right = greaterBy comparePair left right
  left >= right = greaterOrEqualBy comparePair left right
  max = maxBy comparePair
  min = minBy comparePair

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  compare = compareTriple
  left < right = lessBy compareTriple left right
  left <= right = lessOrEqualBy compareTriple left right
  left > right = greaterBy compareTriple left right
  left >= right = greaterOrEqualBy compareTriple left right
  max = maxBy compareTriple
  min = minBy compareTriple

compareList :: (Ord a) => [a] -> [a] -> Ordering
compareList [] [] = EQ
compareList [] (_ : _) = LT
compareList (_ : _) [] = GT
compareList (x : xs) (y : ys) =
  case compare x y of
    LT -> LT
    EQ -> compareList xs ys
    GT -> GT

compareMaybe :: (Ord a) => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing Nothing = EQ
compareMaybe Nothing (Just _) = LT
compareMaybe (Just _) Nothing = GT
compareMaybe (Just x) (Just y) = compare x y

compareEither :: (Ord a, Ord b) => Either a b -> Either a b -> Ordering
compareEither (Left x) (Left y) = compare x y
compareEither (Left _) (Right _) = LT
compareEither (Right _) (Left _) = GT
compareEither (Right x) (Right y) = compare x y

comparePair :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
comparePair (leftA, leftB) (rightA, rightB) =
  case compare leftA rightA of
    EQ -> compare leftB rightB
    result -> result

compareTriple :: (Ord a, Ord b, Ord c) => (a, b, c) -> (a, b, c) -> Ordering
compareTriple (leftA, leftB, leftC) (rightA, rightB, rightC) =
  case compare leftA rightA of
    EQ ->
      case compare leftB rightB of
        EQ -> compare leftC rightC
        result -> result
    result -> result

lessBy :: (a -> a -> Ordering) -> a -> a -> Bool
lessBy cmp x y =
  case cmp x y of
    LT -> True
    _ -> False

lessOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
lessOrEqualBy cmp x y =
  case cmp x y of
    GT -> False
    _ -> True

greaterBy :: (a -> a -> Ordering) -> a -> a -> Bool
greaterBy cmp x y =
  case cmp x y of
    GT -> True
    _ -> False

greaterOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
greaterOrEqualBy cmp x y =
  case cmp x y of
    LT -> False
    _ -> True

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y =
  case cmp x y of
    GT -> x
    _ -> y

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y =
  case cmp x y of
    GT -> y
    _ -> x

instance (Show a) => Show (Ratio a) where
  showsPrec precedence value =
    showParen
      (precedence > 7)
      (showsPrec 8 (numerator value) . showString " % " . showsPrec 8 (denominator value))

instance Show Char where
  showsPrec _ char = showChar '\'' . showLitChar char . showChar '\''
  showList chars = showChar '"' . showLitString chars . showChar '"'

instance (Show a) => Show [a] where
  showsPrec _ = showList

instance (Show a, Show b) => Show (Either a b) where
  showsPrec precedence (Left value) =
    showParen (precedence > 10) (showString "Left " . showsPrec 11 value)
  showsPrec precedence (Right value) =
    showParen (precedence > 10) (showString "Right " . showsPrec 11 value)

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (first, second) =
    showChar '(' . shows first . showChar ',' . shows second . showChar ')'

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (first, second, third) =
    showChar '('
      . shows first
      . showChar ','
      . shows second
      . showChar ','
      . shows third
      . showChar ')'

showLitString :: String -> ShowS
showLitString [] = id
showLitString ('"' : chars) = showString "\\\"" . showLitString chars
showLitString ('\'' : chars) = showChar '\'' . showLitString chars
showLitString (char : chars) = showLitChar char . showLitString chars

showLitChar :: Char -> ShowS
showLitChar '\a' = showString "\\a"
showLitChar '\b' = showString "\\b"
showLitChar '\f' = showString "\\f"
showLitChar '\n' = showString "\\n"
showLitChar '\r' = showString "\\r"
showLitChar '\t' = showString "\\t"
showLitChar '\v' = showString "\\v"
showLitChar '\\' = showString "\\\\"
showLitChar '\'' = showString "\\'"
showLitChar char@(C# value) =
  case ord# value of
    code -> showLitCode char code

showLitCode :: Char -> Int# -> ShowS
showLitCode char code =
  case (<#) code 32# of
    1# -> showChar '\\' . showString (asciiControlName code)
    _ ->
      case (==#) code 127# of
        1# -> showString "\\DEL"
        _ ->
          case (<#) code 128# of
            1# -> showChar char
            _ ->
              case (<#) code 160# of
                1# -> showNumericEscape code
                _ -> showChar char

asciiControlName :: Int# -> String
asciiControlName code =
  case code of
    0# -> "NUL"
    1# -> "SOH"
    2# -> "STX"
    3# -> "ETX"
    4# -> "EOT"
    5# -> "ENQ"
    6# -> "ACK"
    7# -> "BEL"
    8# -> "BS"
    9# -> "HT"
    10# -> "LF"
    11# -> "VT"
    12# -> "FF"
    13# -> "CR"
    14# -> "SO"
    15# -> "SI"
    16# -> "DLE"
    17# -> "DC1"
    18# -> "DC2"
    19# -> "DC3"
    20# -> "DC4"
    21# -> "NAK"
    22# -> "SYN"
    23# -> "ETB"
    24# -> "CAN"
    25# -> "EM"
    26# -> "SUB"
    27# -> "ESC"
    28# -> "FS"
    29# -> "GS"
    30# -> "RS"
    _ -> "US"

showNumericEscape :: Int# -> ShowS
showNumericEscape value suffix =
  showChar '\\' (shows (IS value) (protectNumericEscape suffix))

protectNumericEscape :: String -> String
protectNumericEscape [] = []
protectNumericEscape chars@('0' : _) = '\\' : '&' : chars
protectNumericEscape chars@('1' : _) = '\\' : '&' : chars
protectNumericEscape chars@('2' : _) = '\\' : '&' : chars
protectNumericEscape chars@('3' : _) = '\\' : '&' : chars
protectNumericEscape chars@('4' : _) = '\\' : '&' : chars
protectNumericEscape chars@('5' : _) = '\\' : '&' : chars
protectNumericEscape chars@('6' : _) = '\\' : '&' : chars
protectNumericEscape chars@('7' : _) = '\\' : '&' : chars
protectNumericEscape chars@('8' : _) = '\\' : '&' : chars
protectNumericEscape chars@('9' : _) = '\\' : '&' : chars
protectNumericEscape chars = chars

putChar :: Char -> IO ()
putChar character = putStr [character]

putStr :: String -> IO ()
putStr = hPutStr stdout

putStrLn :: String -> IO ()
putStrLn characters = do
  putStr characters
  putChar '\n'

print :: (Show a) => a -> IO ()
print value = putStrLn (show value)

instance Functor List where
  fmap = fmapList

instance Functor Maybe where
  fmap f mx =
    case mx of
      Nothing -> Nothing
      Just x -> Just (f x)

instance Functor (Either e) where
  fmap f mx =
    case mx of
      Left e -> Left e
      Right x -> Right (f x)

instance Functor ((,) a) where
  fmap f (first, second) = (first, f second)

instance Traversable List where
  traverse _ [] = pure []
  traverse f (value : values) = fmap (:) (f value) <*> traverse f values

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just value) = fmap Just (f value)

instance Traversable (Either e) where
  traverse _ (Left value) = pure (Left value)
  traverse f (Right value) = fmap Right (f value)

instance Functor ((->) r) where
  fmap f g x = f (g x)

instance Applicative ((->) r) where
  pure x _ = x
  (f <*> g) x = f x (g x)

instance Monad ((->) r) where
  return x _ = x
  (f >>= k) x = k (f x) x
  (_ >> g) x = g x

instance Applicative List where
  pure x = [x]

  fs <*> xs = applyList fs xs

instance Applicative Maybe where
  pure = Just

  mf <*> mx =
    case mf of
      Nothing -> Nothing
      Just f ->
        case mx of
          Nothing -> Nothing
          Just x -> Just (f x)

instance Applicative (Either e) where
  pure = Right

  mf <*> mx =
    case mf of
      Left e -> Left e
      Right f ->
        case mx of
          Left e -> Left e
          Right x -> Right (f x)

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
f =<< mx = mx >>= f

infixr 1 =<<

instance Monad List where
  xs >>= k = bindList xs k

  xs >> ys = thenList xs ys
  return x = [x]

-- | The monads that can report a failed pattern match in @do@ notation.
class (Monad m) => MonadFail m where
  fail :: String -> m a

instance MonadFail IO where
  fail = error

instance MonadFail List where
  fail _ = []

instance MonadFail Maybe where
  fail _ = Nothing

instance Monad Maybe where
  mx >>= k = bindMaybe mx k

  mx >> my =
    case mx of
      Nothing -> Nothing
      Just _ -> my
  return = Just

instance Monad (Either e) where
  mx >>= k =
    case mx of
      Left e -> Left e
      Right x -> k x

  mx >> my =
    case mx of
      Left e -> Left e
      Right _ -> my
  return = Right

fmapList :: (a -> b) -> [a] -> [b]
fmapList _ [] = []
fmapList f (x : xs) = f x : fmapList f xs

applyList :: [a -> b] -> [a] -> [b]
applyList [] _ = []
applyList (f : fs) xs = fmapList f xs ++ applyList fs xs

bindList :: [a] -> (a -> [b]) -> [b]
bindList [] _ = []
bindList (x : xs) k = k x ++ bindList xs k

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) k = k x

thenList :: [a] -> [b] -> [b]
thenList [] _ = []
thenList (_ : xs) ys = ys ++ thenList xs ys

curry :: ((a, b) -> c) -> a -> b -> c
curry function left right = function (left, right)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry function (left, right) = function left right

until :: (a -> Bool) -> (a -> a) -> a -> a
until done next value =
  if done value
    then value
    else until done next (next value)

asTypeOf :: a -> a -> a
asTypeOf = const

subtract :: (Num a) => a -> a -> a
subtract left right = right - left

splitAt :: Int -> [a] -> ([a], [a])
splitAt count values = (take count values, drop count values)

lines :: String -> [String]
lines [] = []
lines text =
  case break (== '\n') text of
    (line, rest) ->
      line
        : ( case rest of
              [] -> []
              (_ : remaining) -> lines remaining
          )

unlines :: [String] -> String
unlines = concatMap (++ "\n")

words :: String -> [String]
words text =
  case dropWhile isSpaceChar text of
    [] -> []
    trimmed ->
      case break isSpaceChar trimmed of
        (word, rest) -> word : words rest

isSpaceChar :: Char -> Bool
isSpaceChar character =
  character
    == ' '
    || character
    == '\t'
    || character
    == '\n'
    || character
    == '\r'
    || character
    == '\f'
    || character
    == '\v'
    || character
    == '\160'

unwords :: [String] -> String
unwords [] = []
unwords [word] = word
unwords (word : rest) = word ++ (' ' : unwords rest)

iterate :: (a -> a) -> a -> [a]
iterate next value = value : iterate next (next value)

repeat :: a -> [a]
repeat value = value : repeat value

cycle :: [a] -> [a]
cycle [] = errorWithoutStackTrace "Prelude.cycle: empty list"
cycle values = values ++ cycle values

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl combine initial values =
  initial
    : ( case values of
          [] -> []
          (value : rest) -> scanl combine (combine initial value) rest
      )

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 _ [] = []
scanl1 combine (value : values) = scanl combine value values

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ initial [] = [initial]
scanr combine initial (value : values) =
  case scanr combine initial values of
    results@(result : _) -> combine value result : results
    [] -> [initial]

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 _ [] = []
scanr1 _ [value] = [value]
scanr1 combine (value : values) =
  case scanr1 combine values of
    results@(result : _) -> combine value result : results
    [] -> [value]

notElem :: (Eq a) => a -> [a] -> Bool
notElem value values = not (value `elem` values)

or :: [Bool] -> Bool
or = any id

(!!) :: [a] -> Int -> a
(!!) values index =
  if index < 0
    then errorWithoutStackTrace "Prelude.!!: negative index"
    else indexList values index

indexList :: [a] -> Int -> a
indexList [] _ = errorWithoutStackTrace "Prelude.!!: index too large"
indexList (value : values) index =
  if index == 0
    then value
    else indexList values (index - 1)

infixl 9 !!

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 tripleOf

tripleOf :: a -> b -> c -> (a, b, c)
tripleOf valueOne valueTwo valueThree = (valueOne, valueTwo, valueThree)

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 combine (valueOne : onesRest) (valueTwo : twosRest) (valueThree : threesRest) =
  combine valueOne valueTwo valueThree : zipWith3 combine onesRest twosRest threesRest
zipWith3 _ _ _ _ = []

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 = foldr unzip3Step ([], [], [])

unzip3Step :: (a, b, c) -> ([a], [b], [c]) -> ([a], [b], [c])
unzip3Step (valueOne, valueTwo, valueThree) (ones, twos, threes) =
  (valueOne : ones, valueTwo : twos, valueThree : threes)
