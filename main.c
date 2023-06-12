#include <errno.h>
#include <string.h> // strerror
#include <stdlib.h> // exit

#include "parser.h"
#include "workspace.h"
#include "typecheck.h"

Arena temporary_arena = {0};
Arena general_arena = {0};
Arena *context_arena = &general_arena;

Ast_Ident make_identifier(const char *name, Ast_Block *enclosing_block);

int main(int argc, char **argv)
{   
    const char *program = shift_args(&argc, &argv);

    if (!argc) {
        fprintf(stderr, "Usage: %s [input_file]\n", program);
        fprintf(stderr, "... expected at least one input file\n");
        exit(1);
    }

    const char *input_path = shift_args(&argc, &argv);

    Workspace w0 = create_workspace("My Program");
    workspace_add_file(&w0, input_path);
    workspace_typecheck(&w0);
    workspace_setup_llvm(&w0);
    workspace_llvm(&w0);

    LLVMDumpModule(w0.llvm.module);
    workspace_dispose_llvm(&w0);

    arena_free(&temporary_arena);
    return 0;
}

char *shift_args(int *argc, char ***argv)
{
    assert(*argc > 0);
    char *result = **argv;
    *argv += 1;
    *argc -= 1;
    return result;
}

#define STRING_BUILDER_IMPLEMENTATION
#include "string_builder.h"
#define CONTEXT_ALLOC_IMPLEMENTATION
#include "vendor/context_alloc.h"
#define STB_DS_IMPLEMENTATION
#include "vendor/stb_ds.h"
#define SV_IMPLEMENTATION
#include "vendor/sv.h"
#define ARENA_IMPLEMENTATION
#include "vendor/arena.h"
#define STB_SPRINTF_IMPLEMENTATION
#include "vendor/stb_sprintf.h"

// TODO: It's really easy to check for a procedure when parsing.
//       Right after we parse a parentheses expression, just check
//       if the next token is a "->" token. (or check for "{")
