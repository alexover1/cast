#pragma once

#include <llvm-c/Core.h>

#include "ast.h"
#include "type_info.h"
#include "ir_builder.h"

// Really, this needs to be renamed. It's a single "thing"
// that we push through all steps of compilation.

typedef struct Object Object;

struct Object {
    const Ast_Declaration *key;
    const Type_Info *inferred_type;

    union {
        LLVMValueRef llvm_value;
        Type type_value;
        // Inst_Addr instruction_address;
    };
};

Object init_object(const Ast_Declaration *declaration);
