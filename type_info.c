#include <assert.h>
#include <stdlib.h> // For realloc

#include "common.h"
#include "ast.h"
#include "type_info.h"
#include "interp.h"
#include "vendor/stb_ds.h"

#define TYPE_INFO(tag_, size) { \
    .tag = (tag_),              \
    .runtime_size = (size),     \
}

#define TYPE_INFO_INTEGER(size, signed) {  \
    .info = TYPE_INFO(TYPE_INTEGER, size), \
    .sign = (signed)                       \
}

Type_Table type_table_init(void)
{
    Type_Table table;
    table.arena = (Arena){0};
    table.types = NULL;

    Type_Info_Integer type_info_s64 = TYPE_INFO_INTEGER(8, true);
    Type_Info type_info_float       = TYPE_INFO(TYPE_FLOAT, 4);
    Type_Info type_info_float64     = TYPE_INFO(TYPE_FLOAT, 8);
    Type_Info type_info_bool        = TYPE_INFO(TYPE_BOOL, 1);
    Type_Info type_info_void        = TYPE_INFO(TYPE_VOID, 0);
    Type_Info type_info_string      = TYPE_INFO(TYPE_STRING, 2*8);
    Type_Info type_info_type        = TYPE_INFO(TYPE_TYPE, 8);

    table.INT = type_table_append(&table, &type_info_s64, sizeof(type_info_s64));
    table.FLOAT = type_table_append(&table, &type_info_float, sizeof(type_info_float));
    table.BOOL = type_table_append(&table, &type_info_bool, sizeof(type_info_bool));
    table.VOID = type_table_append(&table, &type_info_void, sizeof(type_info_void));
    table.STRING = type_table_append(&table, &type_info_string, sizeof(type_info_string));
    table.TYPE = type_table_append(&table, &type_info_type, sizeof(type_info_type));
    
    // TODO: add other sized types
    table.s64 = type_table_append(&table, &type_info_s64, sizeof(type_info_s64));
    table.float64 = type_table_append(&table, &type_info_float64, sizeof(type_info_float64));

    table.comptime_int = type_table_append(&table, &type_info_s64, sizeof(type_info_s64));
    table.comptime_float = type_table_append(&table, &type_info_float, sizeof(type_info_float));
    table.comptime_string = type_table_append(&table, &type_info_string, sizeof(type_info_string));

    return table;
}

Type parse_literal_type(const Type_Table *table, const char *lit, size_t n)
{
    if (n == 3) {
        if      (strcmp(lit, "int") == 0) return table->INT;
        else if (strcmp(lit, "s64") == 0) return table->s64;
    }
    else if (n == 4) {
        if (strcmp(lit, "bool") == 0)      return table->BOOL;
        else if (strcmp(lit, "void") == 0) return table->VOID;
        else if (strcmp(lit, "Type") == 0) return table->TYPE;
    }  
    else if (strcmp(lit, "float") == 0)   return table->FLOAT;
    else if (strcmp(lit, "float64") == 0)   return table->float64;
    else if (strcmp(lit, "string") == 0)  return table->STRING;
    return NULL;
}

#define TABLE_INIT_CAP 512

Type type_table_append(Type_Table *table, void *item, size_t item_size)
{
    assert(item_size >= sizeof(Type_Info));
    Type_Info *data = arena_alloc(&table->arena, item_size);
    memcpy(data, item, item_size);
    data->type_table_index = arrlen(table->types);
    arrput(table->types, (Type)data);
    return (Type)data;
}

bool types_are_equal(const Type_Table *table, const Type_Info *a, const Type_Info *b)
{
    if (a == b) return true;
    if (a->tag != b->tag) return false;

    switch (a->tag) {
        case TYPE_POINTER: {
            const Type_Info_Pointer *ap = Down(a);
            const Type_Info_Pointer *bp = Down(b);
            return ap->element_type == bp->element_type;
        }

        case TYPE_ARRAY: {
        const Type_Info_Array *aa = Down(a);
        const Type_Info_Array *ba = Down(b);
        return types_are_equal(table, aa->element_type, ba->element_type) &&
            aa->element_count == ba->element_count;
        }
        
        case TYPE_PROCEDURE: {
            const Type_Info_Procedure *ap = Down(a);
            const Type_Info_Procedure *bp = Down(b);
            if (ap->parameter_count != bp->parameter_count) return false;
            if (!types_are_equal(table, ap->return_type, bp->return_type)) {
                return false;
            }
            for (size_t i = 0; i < ap->parameter_count; ++i) {
                if (!types_are_equal(table, ap->parameters[i], bp->parameters[i])) return false;
            }
            return true;
        }

        default:
            return false;
    }
}
