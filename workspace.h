#pragma once

#include <llvm-c/Core.h>
#include <llvm-c/Target.h>

#include "parser.h"
#include "typecheck.h"

typedef struct {
    LLVMContextRef context;
    LLVMModuleRef module;
    LLVMBuilderRef builder;
} Llvm;

struct Workspace {
    const char *name;
    Llvm llvm;
    Ast_Declaration **declarations;

    Source_File *files;

    Ast_Type_Definition *type_def_int;
    Ast_Type_Definition *type_def_u8;
    Ast_Type_Definition *type_def_u16;
    Ast_Type_Definition *type_def_u32;
    Ast_Type_Definition *type_def_u64;
    Ast_Type_Definition *type_def_s8;
    Ast_Type_Definition *type_def_s16;
    Ast_Type_Definition *type_def_s32;
    Ast_Type_Definition *type_def_s64;
    Ast_Type_Definition *type_def_float;
    Ast_Type_Definition *type_def_float32;
    Ast_Type_Definition *type_def_float64;
    Ast_Type_Definition *type_def_bool;
    Ast_Type_Definition *type_def_void;
    Ast_Type_Definition *type_def_void_pointer;
    Ast_Type_Definition *type_def_string;
    Ast_Type_Definition *type_def_type;
};

void workspace_init(Workspace *w, const char *name);
void workspace_add_file(Workspace *w, const char *path_as_cstr);
void workspace_add_string(Workspace *w, String_View input);
void workspace_typecheck(Workspace *w);
void workspace_llvm(Workspace *w);

void report_error(Workspace *workspace, Source_Location location, const char *format, ...);

// LLVM stuff:

void workspace_setup_llvm(Workspace *w);
void workspace_dispose_llvm(Workspace *w);

LLVMValueRef llvm_get_named_value(LLVMValueRef function, const char *name);
LLVMTypeRef llvm_get_type(Workspace *w, const Ast_Type_Definition *type_def);
LLVMOpcode llvm_get_opcode(int operator_type, Ast_Type_Definition *defn, LLVMIntPredicate *int_predicate, LLVMRealPredicate *real_predicate);

LLVMValueRef llvm_const_string(Llvm llvm, const char *data, size_t count);
LLVMValueRef llvm_build_expression(Workspace *w, Ast_Expression *expr);
void llvm_build_statement(Workspace *w, LLVMValueRef function, Ast_Statement *stmt);
void llvm_build_declaration(Workspace *w, Ast_Declaration *decl);

// Random helper functions:

Ast_Literal *make_literal(Ast_Type_Definition *default_type, unsigned long value);
Ast_Literal *make_integer_literal(Ast_Type_Definition *int_type, unsigned long value);
Ast_Literal *make_float_literal(Ast_Type_Definition *float_type, double value);
Ast_Literal *make_string_literal(Ast_Type_Definition *string_type, String_View value);
