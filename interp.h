#pragma once

#include "workspace.h"

typedef struct {
    Ast_Literal *literal_true;
    Ast_Literal *literal_false;
    Ast_Literal *literal_null;

    Ast_Type_Definition *type_def_int;
    Ast_Type_Definition *type_def_float;
    Ast_Type_Definition *type_def_bool;
    Ast_Type_Definition *type_def_void;
    Ast_Type_Definition *type_def_string;

    Ast_Type_Definition *type_def_u8;
    Ast_Type_Definition *type_def_u16;
    Ast_Type_Definition *type_def_u32;
    Ast_Type_Definition *type_def_u64;
    Ast_Type_Definition *type_def_s8;
    Ast_Type_Definition *type_def_s16;
    Ast_Type_Definition *type_def_s32;
    Ast_Type_Definition *type_def_s64;

    Ast_Type_Definition *type_def_float32;
    Ast_Type_Definition *type_def_float64;
    Ast_Type_Definition *type_def_void_pointer;
    Ast_Type_Definition *type_def_type;
} Axe_Interp;

Axe_Interp *interp_init(void);
