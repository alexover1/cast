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
    TYPE_TYPE = 8,
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

typedef void Type_Info_Comptime;

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
        // Type_Info_Comptime _comptime;
    };
};

const char *type_to_string(Type type);
Type parse_literal_type(const char *lit, size_t n);
bool types_are_equal(Type a, Type b);

#include "ast.h"
Type infer_ast_type(const Ast *ast);

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
