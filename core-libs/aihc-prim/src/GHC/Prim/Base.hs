{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim.Base
  ( List (..),
    String,
    Maybe (..),
    Applicative (..),
    Functor (..),
    Monad (..),
    IO (..),
    divInt#,
    bindIO,
    returnIO,
    thenIO,
  )
where

import GHC.Prim (RealWorld, State#, divInt#)
import GHC.Prim.IO (IO (..))
import GHC.Types (Char, List (..), Type)

type String = [Char]

data Maybe a = Nothing | Just a

class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  value <$ functor = fmap (\_ -> value) functor

infixl 4 <$

{- HLINT ignore "Use const" -}
class (Functor f) => Applicative (f :: Type -> Type) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  first *> second = fmap (\_ value -> value) first <*> second
  liftA2 function left right = fmap function left <*> right
  first <* second = fmap (\value _ -> value) first <*> second

infixl 4 <*>, *>, <*

{- HLINT ignore "Use >>" -}
class (Applicative m) => Monad (m :: Type -> Type) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  action >> next = action >>= \_ -> next
  return = pure

infixl 1 >>=, >>

instance Functor IO where
  fmap f (IO action) =
    IO
      ( \state ->
          case action state of
            (# nextState, value #) -> (# nextState, f value #)
      )

instance Applicative IO where
  pure = returnIO

  IO function <*> IO argument =
    IO
      ( \state ->
          case function state of
            (# functionState, f #) ->
              case argument functionState of
                (# resultState, value #) -> (# resultState, f value #)
      )

instance Monad IO where
  (>>=) = bindIO
  (>>) = thenIO
  return = returnIO

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO action) next =
  IO
    ( \state ->
        case action state of
          (# nextState, value #) ->
            case next value of
              IO nextAction -> nextAction nextState
    )

thenIO :: IO a -> IO b -> IO b
thenIO (IO action) (IO nextAction) =
  IO
    ( \state ->
        case action state of
          (# nextState, _ #) -> nextAction nextState
    )

returnIO :: a -> IO a
returnIO value = IO (returnIOState value)

returnIOState :: a -> State# RealWorld -> (# State# RealWorld, a #)
returnIOState value state = (# state, value #)
