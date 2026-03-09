module X where

#ifdef HASHABLE_RANDOM_SEED
x :: Int
x = 1
#else
x :: Int
x = 2
#endif
