#pragma once

#include <stdio.h>
#include <stdlib.h> // for abort?

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

// C INHERITANCE CASTING

// For some reason this works
#define Down(x) (void*)(x)
#define Base(x) (&(x)->base)

#define For(xs) for (ptrdiff_t it = 0; it < arrlen((xs)); ++it)

// BITWISE FLAGS

#define flag_has(x, flag) (((x) & (flag)) != 0)
