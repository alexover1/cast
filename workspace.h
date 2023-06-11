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
    Ast **infer_queue;

    Ast_Literal *literal_true;
    Ast_Literal *literal_false;
    Ast_Literal *literal_null;

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

Workspace create_workspace(const char *name);
void workspace_add_file(Workspace *w, const char *path_as_cstr);
void workspace_add_string(Workspace *w, String_View input);
void workspace_run(Workspace *w);

// Messaging:

typedef enum {
    MESSAGE_END = 0,
    MESSAGE_TYPECHECKED = 1,
    MESSAGE_BYTECODED = 2,
    MESSAGE_LLVMED = 3,
} Compiler_Message_Type;

typedef struct {
    Ast *ast;
} Compiler_Message_Typechecked;

typedef struct {
    Ast *ast;
    // Bytecode bytecode;
} Compiler_Message_Bytecoded;

typedef struct {
    Ast *ast;
    LLVMTypeRef type;
    LLVMValueRef value;
    // Bytecode bytecode;
} Compiler_Message_Llvmed;

typedef struct {
    Compiler_Message_Type type;
    union {
        Compiler_Message_Typechecked typechecked;
        Compiler_Message_Bytecoded bytecoded;
        Compiler_Message_Llvmed llvmed;
    };
} Compiler_Message;

Compiler_Message compiler_next_message(Workspace *workspace);

// Type checking:

void flatten_for_typechecking(Workspace *w, Ast *ast);

// LLVM stuff:

void workspace_setup_llvm(Workspace *w);
LLVMValueRef llvm_const_string(Llvm llvm, const char *data, size_t count);
LLVMTypeRef llvm_create_type(Llvm llvm, Ast_Type_Definition *type_def);
LLVMValueRef llvm_create_value(Llvm llvm, LLVMTypeRef type, const Ast *ast);

// Random helper functions

Ast_Literal *make_integer_literal(Ast_Type_Definition *type_def, unsigned long value);
Ast_Literal *make_float_literal(Ast_Type_Definition *type_def, double value);
Ast_Literal *make_string_literal(Workspace *w, String_View value);
