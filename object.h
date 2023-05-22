#pragma once

#include "ast.h"
#include "type_info.h"
#include "ir_builder.h"

// Really, this needs to be renamed. It's a single "thing"
// that we push through all steps of compilation.

typedef struct Object Object;

struct Object {
    const Ast_Declaration *key;
    Type inferred_type;
    ptrdiff_t index_within_type_table; // positive only if this is a typedef
    Ir_Index ir_index;
};

Object init_object(const Ast_Declaration *declaration);
