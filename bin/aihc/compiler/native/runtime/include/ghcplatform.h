#ifndef GHCPLATFORM_H
#define GHCPLATFORM_H

#if defined(__APPLE__)
#define darwin_HOST_OS 1
#elif defined(__linux__)
#define linux_HOST_OS 1
#elif defined(__wasi__)
#define wasi_HOST_OS 1
#endif

#if defined(__aarch64__)
#define aarch64_HOST_ARCH 1
#elif defined(__x86_64__)
#define x86_64_HOST_ARCH 1
#elif defined(__i386__)
#define i386_HOST_ARCH 1
#elif defined(__wasm32__)
#define wasm32_HOST_ARCH 1
#endif

#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define WORDS_BIGENDIAN 1
#endif

#if defined(__LP64__) || defined(__x86_64__) || defined(__aarch64__)
#define SIZEOF_VOID_P 8
#define SIZEOF_UNSIGNED_LONG 8
#else
#define SIZEOF_VOID_P 4
#define SIZEOF_UNSIGNED_LONG 4
#endif

#define SIZEOF_UNSIGNED_LONG_LONG 8

#endif
