#ifndef CONTEXT_ALLOC_H_
#define CONTEXT_ALLOC_H_

#include "vendor/arena.h"

extern Arena *context_arena;

void *context_alloc(size_t size);
void *context_realloc(void *oldptr, size_t oldsz, size_t newsz);

// Temporary allocation makes a lot of stuff much simpler.

extern Arena temporary_arena;

char *tprint(const char *fmt, ...);
char *vtprint(const char *fmt, va_list args);

#endif // CONTEXT_ALLOC_H_

#ifdef CONTEXT_ALLOC_IMPLEMENTATION

#ifndef CONTEXT_ALLOC_C_
#define CONTEXT_ALLOC_C_

void *context_alloc(size_t size)
{
    assert(context_arena);
    return arena_alloc(context_arena, size);
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