#ifndef CONTEXT_ALLOC_H_
#define CONTEXT_ALLOC_H_

#include <stdarg.h>

#include "vendor/arena.h"

extern Arena *context_arena;

void *context_alloc(size_t size);
void *context_realloc(void *oldptr, size_t oldsz, size_t newsz);

// Temporary allocation makes a lot of stuff much simpler.

extern Arena temporary_arena;

char *tprint(const char *fmt, ...);
char *vtprint(const char *fmt, va_list args);

#include <stdio.h>
#include <stdlib.h> // for abort?

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

#define Swap(T, a, b) do { T t = a; a = b; b = t; } while (0)
#define Sign(T, x)    ((T)((x) > 0) - (T)((x) < 0))
#define Abs(T, x)     (Sign(T, x)*(x))

 // For some reason this works
#define Down(x) (void*)(x)
#define Base(x) (&(x)->base)

#include "vendor/stb_ds.h"
#define For(xs) for (ptrdiff_t it = 0; it < arrlen((xs)); ++it)

#endif // CONTEXT_ALLOC_H_



#ifdef CONTEXT_ALLOC_IMPLEMENTATION

#ifndef CONTEXT_ALLOC_C_
#define CONTEXT_ALLOC_C_

void *context_alloc(size_t size)
{
    assert(context_arena);
    void *result = arena_alloc(context_arena, size);
    memset(result, 0, size);
    return result;
}

void *context_realloc(void *oldptr, size_t oldsz, size_t newsz)
{
    assert(context_arena);
    return arena_realloc(context_arena, oldptr, oldsz, newsz);
}

#include "vendor/stb_sprintf.h"

char *tprint(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    // Find the length needed and allocate it.
    int n = stbsp_vsnprintf(NULL, 0, fmt, args) + 1;
    char *buf = arena_alloc(&temporary_arena, n);

    // Actually write the string.
    va_start(args, fmt);
    stbsp_vsnprintf(buf, n, fmt, args);
    va_end(args);

    return buf;
}

char *vtprint(const char *fmt, va_list args0)
{
    va_list args1;
    va_copy(args1, args0); // args1 = args0
    
    int n = stbsp_vsnprintf(NULL, 0, fmt, args0) + 1;
    char *buf = arena_alloc(&temporary_arena, n);
    stbsp_vsnprintf(buf, n, fmt, args1);
    va_end(args1);
    return buf;
}

#endif // CONTEXT_ALLOC_C_
#endif // CONTEXT_ALLOC_IMPLEMENTATION
