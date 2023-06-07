#include <assert.h>
#include <stdlib.h> // For realloc

#include "common.h"
#include "ast.h"
#include "type_info.h"
#include "interp.h"
#include "vendor/stb_ds.h"

#define xx (void*)

#define TYPE_INFO(tag_, size) { \
    .tag = (tag_),              \
    .runtime_size = (size),     \
}

#define TYPE_INFO_INTEGER(size, signed) (Type_Info_Integer) {  \
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
    Type_Info type_info_code        = TYPE_INFO(TYPE_CODE, 8);

    table.INT = type_table_append(&table, &type_info_s64, sizeof(type_info_s64));
    table.FLOAT = type_table_append(&table, &type_info_float, sizeof(type_info_float));
    table.BOOL = type_table_append(&table, &type_info_bool, sizeof(type_info_bool));
    table.VOID = type_table_append(&table, &type_info_void, sizeof(type_info_void));
    table.STRING = type_table_append(&table, &type_info_string, sizeof(type_info_string));
    table.TYPE = type_table_append(&table, &type_info_type, sizeof(type_info_type));
    table.CODE = type_table_append(&table, &type_info_code, sizeof(type_info_code));
    table.null = type_table_append(&table, &type_info_void, sizeof(type_info_void));
    
    table.s8  = type_table_append(&table, &TYPE_INFO_INTEGER(1, true),  sizeof(Type_Info_Integer));
    table.s16 = type_table_append(&table, &TYPE_INFO_INTEGER(2, true),  sizeof(Type_Info_Integer));
    table.s32 = type_table_append(&table, &TYPE_INFO_INTEGER(4, true),  sizeof(Type_Info_Integer));
    table.s64 = type_table_append(&table, &type_info_s64,              sizeof(Type_Info_Integer));
    table.u8  = type_table_append(&table, &TYPE_INFO_INTEGER(1, false), sizeof(Type_Info_Integer));
    table.u16 = type_table_append(&table, &TYPE_INFO_INTEGER(2, false), sizeof(Type_Info_Integer));
    table.u32 = type_table_append(&table, &TYPE_INFO_INTEGER(4, false), sizeof(Type_Info_Integer));
    table.u64 = type_table_append(&table, &TYPE_INFO_INTEGER(8, false), sizeof(Type_Info_Integer));

    table.float64 = type_table_append(&table, &type_info_float64, sizeof(type_info_float64));

    table.comptime_int = type_table_append(&table, &type_info_s64, sizeof(type_info_s64));
    table.comptime_float = type_table_append(&table, &type_info_float, sizeof(type_info_float));
    table.comptime_string = type_table_append(&table, &type_info_string, sizeof(type_info_string));

    return table;
}

Type parse_literal_type(const Type_Table *table, String_View lit)
{
    switch (lit.count) {
    case 3:
        if (sv_eq(lit, SV("int"))) return table->INT;
        if (sv_eq(lit, SV("s64"))) return table->s64;
        break;
    case 4:
        if (sv_eq(lit, SV("bool"))) return table->BOOL;
        if (sv_eq(lit, SV("void"))) return table->VOID;
        if (sv_eq(lit, SV("Type"))) return table->TYPE;
        break;
    case 5:
        if (sv_eq(lit, SV("float"))) return table->FLOAT;
        break;
    case 6:
        if (sv_eq(lit, SV("string"))) return table->STRING; break;
    case 7:
        if (sv_eq(lit, SV("float64"))) return table->float64; break;
    }
    return NULL;
}

#define TABLE_INIT_CAP 512

inline Type type_table_append_pointer_to(Type_Table *table, Type element_type)
{
    if (element_type->tag == TYPE_POINTER) {
        Type_Info_Pointer copy = *(Type_Info_Pointer *)element_type;
        copy.pointer_level += 1;
        return type_table_append(table, &copy, sizeof(copy));
    }
    
    Type_Info_Pointer pointer;
    pointer.info.tag = TYPE_POINTER;
    pointer.info.runtime_size = 8;
    pointer.element_type = element_type;
    pointer.pointer_level = 1;
    return type_table_append(table, &pointer, sizeof(pointer));
}

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
            const Type_Info_Pointer *ap = xx a;
            const Type_Info_Pointer *bp = xx b;
            return ap->pointer_level == bp->pointer_level &&
                types_are_equal(table, ap->element_type, bp->element_type);
        }

        case TYPE_ARRAY: {
        const Type_Info_Array *aa = xx a;
        const Type_Info_Array *ba = xx b;
        return types_are_equal(table, aa->element_type, ba->element_type) &&
            aa->element_count == ba->element_count;
        }
        
        case TYPE_PROCEDURE: {
            const Type_Info_Procedure *ap = xx a;
            const Type_Info_Procedure *bp = xx b;
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
