module Demo where

#include "MachDeps.h"

#ifndef GHCPLATFORM_H
#error MachDeps.h must include ghcplatform.h
#endif

#if WORD_SIZE_IN_BITS != 64 || SIZEOF_HSWORD != 8 || SIZEOF_HSINT != 8
#error compiler headers changed the Haskell word representation
#endif

#if SIZEOF_HSCHAR != 4 || ALIGNMENT_HSWORD != 8 || SIZEOF_HSDOUBLE != 8
#error compiler headers have incorrect Haskell size or alignment facts
#endif

data Token = Token
