{- HLINT ignore "Use camelCase" -}

-- | Precedence parsers. 'Prelude' builds its 'Read' class on this module.
module Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    Prec,
    minPrec,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import Data.Bool (Bool (..))
import GHC.Base (Applicative (..), Functor (..), List (..), Monad (..), String, foldr, (++))
import GHC.Int (Int)
import GHC.Internal.Classes (Ord (..))
import GHC.Num (Num (..))
import GHC.Types (Char)

-- | The same synonym as @Prelude.ReadS@, which imports this module.
type ReadS a = String -> [(a, String)]

type Prec = Int

minPrec :: Prec
minPrec = 0

newtype ReadPrec a = ReadPrec (Prec -> ReadS a)

instance Functor ReadPrec where
  fmap f (ReadPrec parser) =
    ReadPrec (\precedence input -> mapReadResults f (parser precedence input))

instance Applicative ReadPrec where
  pure value = ReadPrec (\_ input -> [(value, input)])

  ReadPrec functionParser <*> ReadPrec valueParser =
    ReadPrec
      ( \precedence input ->
          applyReadResults precedence valueParser (functionParser precedence input)
      )

instance Monad ReadPrec where
  ReadPrec parser >>= next =
    ReadPrec
      ( \precedence input ->
          bindReadResults precedence next (parser precedence input)
      )

  ReadPrec first >> ReadPrec second =
    ReadPrec
      ( \precedence input ->
          thenReadResults precedence second (first precedence input)
      )

  return = pure

mapReadResults :: (a -> b) -> [(a, String)] -> [(b, String)]
mapReadResults _ [] = []
mapReadResults f ((value, rest) : results) = (f value, rest) : mapReadResults f results

applyReadResults :: Prec -> (Prec -> ReadS a) -> [(a -> b, String)] -> [(b, String)]
applyReadResults _ _ [] = []
applyReadResults precedence parser ((f, rest) : results) =
  mapReadResults f (parser precedence rest) ++ applyReadResults precedence parser results

bindReadResults :: Prec -> (a -> ReadPrec b) -> [(a, String)] -> [(b, String)]
bindReadResults _ _ [] = []
bindReadResults precedence next ((value, rest) : results) =
  runReadPrec (next value) precedence rest ++ bindReadResults precedence next results

thenReadResults :: Prec -> (Prec -> ReadS b) -> [(a, String)] -> [(b, String)]
thenReadResults _ _ [] = []
thenReadResults precedence parser ((_, rest) : results) =
  parser precedence rest ++ thenReadResults precedence parser results

runReadPrec :: ReadPrec a -> Prec -> ReadS a
runReadPrec (ReadPrec parser) = parser

readPrec_to_S :: ReadPrec a -> Prec -> ReadS a
readPrec_to_S = runReadPrec

readS_to_Prec :: (Prec -> ReadS a) -> ReadPrec a
readS_to_Prec = ReadPrec

prec :: Prec -> ReadPrec a -> ReadPrec a
prec required parser =
  ReadPrec
    ( \context input ->
        case context <= required of
          True -> runReadPrec parser required input
          False -> []
    )

step :: ReadPrec a -> ReadPrec a
step parser = ReadPrec (\context -> runReadPrec parser (context + 1))

reset :: ReadPrec a -> ReadPrec a
reset parser = ReadPrec (\_ -> runReadPrec parser minPrec)

get :: ReadPrec Char
get =
  ReadPrec
    ( \_ input ->
        case input of
          [] -> []
          char : rest -> [(char, rest)]
    )

look :: ReadPrec String
look = ReadPrec (\_ input -> [(input, input)])

(+++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
ReadPrec left +++ ReadPrec right =
  ReadPrec (\precedence input -> left precedence input ++ right precedence input)

(<++) :: ReadPrec a -> ReadPrec a -> ReadPrec a
ReadPrec left <++ ReadPrec right =
  ReadPrec
    ( \precedence input ->
        case left precedence input of
          [] -> right precedence input
          results -> results
    )

pfail :: ReadPrec a
pfail = ReadPrec (\_ _ -> [])

choice :: [ReadPrec a] -> ReadPrec a
choice = foldr (+++) pfail
