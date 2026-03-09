module X where

#if WORD_SIZE_IN_BITS == 64
x :: Int
x = 64
#else
x :: Int
x = 32
#endif
