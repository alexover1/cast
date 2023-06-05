#pragma once

#include "ast.h"
#include "type_info.h"

// typedef struct {
//     const Ast_Type_Definition *key;
//     ptrdiff_t value;
// } Type_Table_Entry;

typedef struct Object Object;

typedef struct {
    Object *objects;
    ptrdiff_t *queue;
    // Type_Table_Entry *type_table;
    Type_Table type_table;
} Interp;

Interp init_interp(void);

void interp_add_scope(Interp *interp, const Ast_Block *scope);
Type interp_get_type(Interp *interp, const Ast_Type_Definition *defn);
void interp_run_main_loop(Interp *interp);

Type typecheck_ast(const Interp *interp, const Ast *ast);
