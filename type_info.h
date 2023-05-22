#pragma once

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct Type_Info Type_Info;
typedef const Type_Info *Type;

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
    bool sign;
} Type_Info_Integer;

typedef void Type_Info_Bool;
typedef void Type_Info_Float;
typedef void Type_Info_String;
typedef void Type_Info_Void;

typedef struct {
    Type *parameters; // @array
    size_t parameter_count;
    Type return_type;
} Type_Info_Procedure;

typedef struct {
    size_t field_count;
    Type *types; // @soa
    const char **names;
    size_t *offsets;
} Type_Info_Struct;

typedef struct {
    Type element_type;
} Type_Info_Pointer;

typedef struct {
    Type element_type;
    int64_t element_count; // -1 for slice
} Type_Info_Array;

struct Type_Info {
    Type_Info_Tag tag;
    int32_t runtime_size;
   
    union {
        Type_Info_Integer integer;
        // Type_Info_Bool _bool;
        // Type_Info_Float _float;
        // Type_Info_String _string;
        // Type_Info_Void _void;
        Type_Info_Procedure procedure;
        Type_Info_Struct structure;
        Type_Info_Pointer pointer;
        Type_Info_Array array;
        // Type_Info_Type _type;
    };
};

const char *type_to_string(Type type);
Type parse_literal_type(const char *lit, size_t n);
bool types_are_equal(Type a, Type b);

// Thread-safe because we never write:
extern const Type_Info type_info_int;
extern const Type_Info type_info_s64;
extern const Type_Info type_info_float;
extern const Type_Info type_info_float64;
extern const Type_Info type_info_string;
extern const Type_Info type_info_bool;
extern const Type_Info type_info_void;
extern const Type_Info type_info_type;

extern const Type_Info type_info_comptime_int;
extern const Type_Info type_info_comptime_float;
extern const Type_Info type_info_comptime_string;
