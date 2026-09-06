module Data.Functor
  ( Functor (..),
    (<$>),
    (<&>),
    ($>),
    void,
  )
where

import Prelude (Functor (..), (<$>))

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
value <&> function = fmap function value

infixl 1 <&>

($>) :: (Functor f) => f a -> b -> f b
($>) functor value = fmap (\_ -> value) functor

infixl 4 $>

void :: (Functor f) => f a -> f ()
void = fmap (\_ -> ())
