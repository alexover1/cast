#pragma once

#include <stddef.h>

typedef struct Type_Info Type_Info;
typedef const Type_Info *Type;

typedef enum {
    TYPE_INTEGER = 1,
    TYPE_FLOAT = 2,
    TYPE_BOOL = 3,
    TYPE_STRING = 4,
    TYPE_VOID = 5,
    TYPE_PROCEDURE = 6,
} Type_Info_Tag;

typedef struct {
    Type *parameters;
    size_t parameters_count;
    Type return_type;
} Type_Info_Procedure;

struct Type_Info {
    Type_Info_Tag tag;
    int32_t runtime_size;
   
    union {
        bool signed_integer;
        Type_Info_Procedure procedure;
    };
};

typedef struct {
    Type s64;
    Type f32;
    Type string;
    Type bool_type;
    Type void_type;
    
    Type_Info *internal_array;
} Type_Table;