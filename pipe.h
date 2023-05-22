#pragma once

#include "object.h"

typedef struct {
    const Ast_Type_Definition *key;
    Type value;
} Type_Entry;

typedef struct {
    const Ast_Declaration *key;
    Object value;
} Object_Entry;

typedef struct {
    Object_Entry *objects;
    Type_Entry *types;

    // Ir_Builder ir_table;
} Pipe;

Pipe init_pipe(void);
void pipe_add_scope(Pipe *pipe, const Ast_Block *scope);
void pipe_forward(Pipe *pipe);
