#pragma once

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "parser.h"

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
    TYPE_CODE = 11,
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
    size_t pointer_level;
} Type_Info_Pointer;

typedef struct {
    Type_Info info; // @using
    Type element_type;
    int64_t element_count; // -1 for slice
} Type_Info_Array;

bool types_are_equal(Ast_Type_Definition *x, Ast_Type_Definition *y);
bool pointer_types_are_equal(Ast_Type_Definition *x, Ast_Type_Definition *y);

void typecheck_literal(Workspace *w, Ast_Literal *literal);
void typecheck_literal_as_type(Workspace *w, Ast_Literal *literal, Ast_Type_Definition *type_def);
void typecheck_identifier(Workspace *w, Ast_Ident *ident);
void typecheck_unary_operator(Workspace *w, Ast_Unary_Operator *unary);
void typecheck_binary_operator(Workspace *w, Ast_Binary_Operator *binary);
void typecheck_ast(Workspace *w, Ast *ast);

void report_error(Ast *ast, const char *format, ...);
