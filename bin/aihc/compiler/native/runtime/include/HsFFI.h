#ifndef HSFFI_H
#define HSFFI_H

#include <stdint.h>

#include "MachDeps.h"

#if SIZEOF_VOID_P == 8
typedef int64_t HsInt;
typedef uint64_t HsWord;
#else
typedef int32_t HsInt;
typedef uint32_t HsWord;
#endif

typedef int8_t HsInt8;
typedef int16_t HsInt16;
typedef int32_t HsInt32;
typedef int64_t HsInt64;
typedef uint8_t HsWord8;
typedef uint16_t HsWord16;
typedef uint32_t HsWord32;
typedef uint64_t HsWord64;
typedef float HsFloat;
typedef double HsDouble;
typedef int HsBool;
typedef uint32_t HsChar;
typedef void *HsPtr;
typedef void (*HsFunPtr)(void);
typedef void *HsStablePtr;

#endif
