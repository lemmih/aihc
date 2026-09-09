module Demo where

#include <ghcautoconf.h>
#include "MachDeps.h"

#if WORD_SIZE_IN_BITS != 64 || SIZEOF_HSWORD != 8
#error compiler configuration changed the Haskell word representation
#endif

data Token = Token
