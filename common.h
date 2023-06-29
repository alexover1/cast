#pragma once

#include <stdio.h>
#include <stdlib.h> // for abort?
#include <stdint.h>

#define xx (void*)

// INTEGER TYPES

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

#include "vendor/context_alloc.h"
#include "vendor/stb_ds.h"

#include "string_builder.h"

// RUNTIME PANIC

#define UNIMPLEMENTED \
    do { \
        fprintf(stderr, "%s:%d: %s is not implemented yet\n", \
                __FILE__, __LINE__, __func__); \
        abort(); \
    } while(0)

#define UNREACHABLE \
    do { \
        fprintf(stderr, "%s:%d: unreachable\n",  __FILE__, __LINE__); \
        abort(); \
    } while(0)

#define UNUSED(x) (void)(x)

// GENERIC MATH FUNCTIONS

#define Swap(T, a, b) do { T t = a; a = b; b = t; } while (0)
#define Sign(T, x)    ((T)((x) > 0) - (T)((x) < 0))
#define Abs(T, x)     (Sign(T, x)*(x))

#define Max(T, a, b) ((a > b) ? a : b)
#define Min(T, a, b) ((a > b) ? b : a)

// AUTOMATIC ITERATION

#define For(xs) for (ptrdiff_t it = 0; it < arrlen((xs)); ++it)

// BITWISE FLAGS

#define flag_has(x, flag) (((x) & (flag)) != 0)

// COMMAND-LINE ARGUMENTS

char *shift_args(int *argc, char ***argv);
