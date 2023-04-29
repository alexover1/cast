#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <stdbool.h>

#include "type_info.c"
// #include "error_reporting.c"
#include "program_printing.c"

Arena temporary_arena = {0};
Arena *context_arena = &temporary_arena;

Type_Table type_table = {0};

int main()
{
    add_preloaded_types_to_table(&type_table);
   
    // First, create the identifier "print".

    Ast_Node ident = {
        .kind = AST_IDENTIFIER,
        .identifier = {
            // .declaration = &my_print,
            .text = "print",
        },
    };

    // Then setup the arguments and create the call.

    Ast_Node hello_world = {
        .kind = AST_STRING,
        .string = "Hello, World",
    };

    Ast_Node arguments[] = {hello_world};
    size_t arguments_count = 1;

    Ast_Node call = {
        .kind = AST_PROCEDURE_CALL,
        .procedure_call = {
            .procedure = &ident,
            .arguments = arguments,
            .arguments_count = arguments_count,
        },
    };

    infer_types(&hello_world);
    printf("type_of(%s) is %s\n", ast_to_string(hello_world), type_to_string(hello_world.inferred_type));

    arena_free(&temporary_arena);
    return 0;
}

#define CONTEXT_ALLOC_IMPLEMENTATION
#include "context_alloc.h"
#define STRING_BUILDER_IMPLEMENTATION
#include "string_builder.h"
#define STB_DS_IMPLEMENTATION
#include "vendor/stb_ds.h"
#define SV_IMPLEMENTATION
#include "vendor/sv.h"
#define ARENA_IMPLEMENTATION
#include "vendor/arena.h"
#define STB_SPRINTF_IMPLEMENTATION
#include "vendor/stb_sprintf.h"
