#include <assert.h>

#include "ast.h"
#include "type_info.h"
#include "vendor/stb_ds.h"

Type parse_literal_type(const char *lit, size_t n)
{
    if (n == 3) {
        if      (strcmp(lit, "int") == 0) return &type_info_int;
        else if (strcmp(lit, "s64") == 0) return &type_info_s64;
    }
    else if (n == 4) {
        if (strcmp(lit, "bool") == 0)      return &type_info_bool;
        else if (strcmp(lit, "void") == 0) return &type_info_void;
        else if (strcmp(lit, "Type") == 0) return &type_info_type;
    }  
    else if (strcmp(lit, "float") == 0)   return &type_info_float;
    else if (strcmp(lit, "float64") == 0) return &type_info_float64;
    else if (strcmp(lit, "string") == 0)  return &type_info_string;
    return NULL;
}

bool types_are_equal(Type a, Type b)
{
    if (a == b) return true;
    if (a->tag != b->tag) return false;

    if (a->tag == TYPE_POINTER) {
        return a->pointer.element_type == b->pointer.element_type;
    }

    switch (a->tag) {
        // We check runtime_size here, which we know MUST be set for literal types.
        // However, it is not set for user-defined types, so be careful!
        case TYPE_INTEGER:
            return (a->runtime_size == b->runtime_size) && (a->integer.sign == b->integer.sign);
        case TYPE_FLOAT:
            return a->runtime_size == b->runtime_size;

        case TYPE_STRING:
        case TYPE_BOOL:
        case TYPE_VOID:
            return true;
        
        case TYPE_PROCEDURE:
            if (a->procedure.parameter_count != b->procedure.parameter_count) return false;
            if (!types_are_equal(a->procedure.return_type, b->procedure.return_type)) {
                return false;
            }
            for (size_t i = 0; i < a->procedure.parameter_count; ++i) {
                if (!types_are_equal(a->procedure.parameters[i], b->procedure.parameters[i])) return false;
            }
            return true;

        case TYPE_STRUCT:
            // We don't do duck typing or any of that blasphemy.
            // If the pointers aren't equal, the types aren't equal.
            return false;

        default: assert(0 && "unreachable");
    }
}

#define TYPE_INFO_INTEGER(bytes, signed) { \
    .tag = TYPE_INTEGER,                   \
    .runtime_size = (bytes),               \
    .integer = { .sign = (signed) }        \
}

#define TYPE_INFO(tag_, size) { \
    .tag = (tag_),              \
    .runtime_size = (size),     \
}

// int :: #distinct s64;
const Type_Info type_info_int = TYPE_INFO_INTEGER(8, true);
const Type_Info type_info_s64     = TYPE_INFO_INTEGER(8, true);
const Type_Info type_info_float   = TYPE_INFO(TYPE_FLOAT, 4);
const Type_Info type_info_float64 = TYPE_INFO(TYPE_FLOAT, 8);
const Type_Info type_info_string  = TYPE_INFO(TYPE_STRING, 2*8);
const Type_Info type_info_bool    = TYPE_INFO(TYPE_BOOL, 1);
const Type_Info type_info_void    = TYPE_INFO(TYPE_VOID, 0);
const Type_Info type_info_type    = TYPE_INFO(TYPE_TYPE, 8);

const Type_Info type_info_comptime_int = TYPE_INFO_INTEGER(8, true);
const Type_Info type_info_comptime_float = TYPE_INFO(TYPE_FLOAT, 4);
const Type_Info type_info_comptime_string = TYPE_INFO(TYPE_STRING, 2*8);
