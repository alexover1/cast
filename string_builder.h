#ifndef STRING_BUILDER_H_
#define STRING_BUILDER_H_

#include <stddef.h>

typedef struct {
    char *data;
    size_t capacity;
    size_t count;
} String_Builder;

#ifndef SB_INIT_CAP
#define SB_INIT_CAP 128
#endif

void sb_append(String_Builder *sb, const char *items, size_t items_count);
void sb_append_cstr(String_Builder *sb, const char *cstr);
void sb_print(String_Builder *sb, const char *fmt, ...);

#endif // STRING_BUILDER_H_

#ifdef STRING_BUILDER_IMPLEMENTATION
#ifndef STRING_BUILDER_C_
#define STRING_BUILDER_C_

#include <string.h> // memcpy and strlen

#include "vendor/context_alloc.h"

static void sb_maybe_grow(String_Builder *sb, size_t new_items_count)
{
    size_t n = sb->count + new_items_count;
    size_t oldsz = sb->capacity;

    if (n > sb->capacity) {
        if (sb->capacity == 0) sb->capacity = SB_INIT_CAP;
        while (n > sb->capacity) {
            sb->capacity *= 2;
        }
        sb->data = context_realloc(sb->data, oldsz, sb->capacity);
        assert(sb->data != NULL && "Ran out of memory");
    }
}

void sb_append(String_Builder *b, const char *items, size_t items_count)
{
    sb_maybe_grow(b, items_count);
    if (items != NULL) memcpy(b->data + b->count, items, items_count*sizeof(*b->data));
    b->count += items_count;
}

inline void sb_append_cstr(String_Builder *sb, const char *cstr)
{
    sb_append(sb, cstr, strlen(cstr));
}

void sb_print(String_Builder *sb, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    sb_append_cstr(sb, vtprint(fmt, args));
    va_end(args);
}

#endif // STRING_BUILDER_C_
#endif // STRING_BUILDER_IMPLEMENTATION
