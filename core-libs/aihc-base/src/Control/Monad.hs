module Control.Monad
  ( Functor (..),
    Applicative (..),
    Monad (..),
    MonadFail (..),
    MonadPlus (..),
    ap,
    liftM,
    liftM2,
    (=<<),
    (>=>),
    (<=<),
    (<$!>),
    mapM,
    mapM_,
    forM,
    forM_,
    sequence,
    sequence_,
    when,
    unless,
    foldM,
    foldM_,
    forever,
    void,
    join,
    replicateM,
    replicateM_,
    zipWithM,
    zipWithM_,
    filterM,
    guard,
    mzero,
    mplus,
    msum,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad.Fail (MonadFail (..))
import Prelude
  ( Applicative (..),
    Bool (..),
    Functor (..),
    Int,
    Maybe,
    Monad (..),
    Num (..),
    Ord (..),
    Traversable (..),
    const,
    flip,
    foldr,
    id,
    mapM_,
    seq,
    sequence_,
    (<$>),
    (=<<),
  )

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap function argument = do
  selected <- function
  selected <$> argument

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM function action = function <$> action

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 function left right = do
  leftValue <- left
  function leftValue <$> right

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

  mzero = empty
  mplus = (<|>)

instance MonadPlus []

instance MonadPlus Maybe

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) first second value = first value >>= second

infixr 1 >=>

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = flip (>=>)

infixr 1 <=<

(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
function <$!> action = do
  value <- action
  let result = function value
  result `seq` return result

infixl 4 <$!>

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

when :: (Applicative f) => Bool -> f () -> f ()
when True action = action
when False _ = pure ()

unless :: (Applicative f) => Bool -> f () -> f ()
unless True _ = pure ()
unless False action = action

foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM _ initial [] = return initial
foldM combine initial (value : values) = do
  next <- combine initial value
  foldM combine next values

{- HLINT ignore foldM_ "Use foldM_" -}
foldM_ :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
foldM_ combine initial values = void (foldM combine initial values)

forever :: (Monad m) => m a -> m b
forever action = action >> forever action

void :: (Functor f) => f a -> f ()
void = fmap (const ())

join :: (Monad m) => m (m a) -> m a
join action = action >>= id

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM count action =
  if count <= 0
    then return []
    else do
      value <- action
      values <- replicateM (count - 1) action
      return (value : values)

replicateM_ :: (Monad m) => Int -> m a -> m ()
replicateM_ count action =
  if count <= 0
    then return ()
    else action >> replicateM_ (count - 1) action

zipWithM :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM combine (left : lefts) (right : rights) = do
  value <- combine left right
  values <- zipWithM combine lefts rights
  return (value : values)
zipWithM _ _ _ = return []

{- HLINT ignore zipWithM_ "Use zipWithM_" -}
zipWithM_ :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ combine lefts rights = void (zipWithM combine lefts rights)

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM keep (value : values) = do
  selected <- keep value
  rest <- filterM keep values
  return (if selected then value : rest else rest)

guard :: (Alternative f) => Bool -> f ()
guard True = pure ()
guard False = empty

msum :: (MonadPlus m) => [m a] -> m a
msum = foldr mplus mzero
