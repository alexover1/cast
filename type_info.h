#pragma once

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

typedef enum {
    TYPE_INTEGER = 1,
    TYPE_FLOAT = 2,
    TYPE_BOOL = 3,
    TYPE_STRING = 4,
    TYPE_VOID = 5,
    TYPE_PROCEDURE = 6,
    TYPE_STRUCT = 7,
    TYPE_POINTER = 8,
    TYPE_ARRAY = 9,
    TYPE_TYPE = 10,
} Type_Info_Tag;

typedef struct {
    Type_Info_Tag tag;
    int32_t runtime_size;
    int64_t type_table_index;
} Type_Info;

typedef const Type_Info *Type;

typedef struct {
    Type_Info info;
    bool sign;
} Type_Info_Integer;

typedef struct {
    Type_Info info; // @using
    Type *parameters; // @array
    size_t parameter_count;
    Type return_type;
} Type_Info_Procedure;

typedef struct {
    Type type;
    const char *name;
    int64_t offset;
} Type_Info_Struct_Field;

typedef struct {
    Type_Info info; // @using
    Type_Info_Struct_Field *field_data;
    size_t field_count;
} Type_Info_Struct;

typedef struct {
    Type_Info info; // @using
    Type element_type;
} Type_Info_Pointer;

typedef struct {
    Type_Info info; // @using
    Type element_type;
    int64_t element_count; // -1 for slice
} Type_Info_Array;

typedef struct {
    Arena arena;
    Type *types; // @malloced with stb_ds

    Type INT, FLOAT, BOOL, VOID, STRING;
    Type TYPE, CODE;
    Type s8, s16, s32, s64;
    Type u8, u16, u32, u64;
    Type comptime_int, comptime_float, comptime_string; // TODO: maybe add an AUTOCAST type?
    Type float64, anyptr;
} Type_Table;

Type_Table type_table_init(void);
Type parse_literal_type(const Type_Table *table, String_View lit);
Type type_table_append(Type_Table *table, void *item, size_t item_size);
const char *type_to_string(const Type_Table *table, Type type);
bool types_are_equal(const Type_Table *table, Type a, Type b);
