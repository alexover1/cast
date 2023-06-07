#pragma once

#include "ast.h"
#include "type_info.h"
#include "object.h"
#include "parser.h"

#include <llvm-c/Core.h>
#include <llvm-c/Target.h>

typedef struct {
    const Ast_Type_Definition *key;
    const Type_Info *value;
} Type_Definition;

typedef struct {
    const char *workspace_name;
    Arena arena;

    Parser parser;
    Object *objects;
    Type_Definition *type_definitions;
    ptrdiff_t *queue;
    Type_Table type_table;

    LLVMContextRef llvm_context;
    LLVMModuleRef llvm_module;
    LLVMBuilderRef llvm_builder;
} Interp;

Interp init_interp(const char *workspace_name);

void interp_add_scope(Interp *interp, const Ast_Block *scope);
Type interp_get_type(Interp *interp, const Ast_Type_Definition *defn);
void interp_run_main_loop(Interp *interp);

void interp_prepare_llvm_context(Interp *interp);

bool try_autocast_ast_to_type(Interp *interp, const Ast *ast, Type type);
Type typecheck_expression(Interp *interp, const Ast *ast);
