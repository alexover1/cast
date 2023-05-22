#pragma once

#include "ast.h"
#include "type_info.h"

typedef struct {
    const Ast_Type_Definition *key;
    Type value;
} Type_Table_Entry;

Type typecheck_ast(const Ast *ast); // Or NULL.

typedef struct Object Object;

typedef struct {
    Object *object_table;
    Type_Table_Entry *type_table;
} Interp;

Interp init_interp(void);

void interp_add_scope(Interp *interp, const Ast_Block *scope);
Type interp_create_type(Interp *interp, const Ast_Type_Definition *defn);
void interp_run_main_loop(Interp *interp);
