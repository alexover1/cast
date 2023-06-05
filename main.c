#include <errno.h>
#include <string.h> // strerror
#include <stdlib.h> // exit

#include "common.h"
#include "vendor/arena.h"
#include "vendor/stb_ds.h"
#include "ast.h"
#include "parser.h"
#include "interp.h"

#include "object.h"

Arena temporary_arena = {0};
Arena general_arena = {0};
Arena *context_arena = &general_arena;
fprintf_t context_logger = fprintf;

Ast_Ident make_identifier(const char *name, Ast_Block *enclosing_block);

int main(int argc, char **argv)
{   
    const char *program = shift_args(&argc, &argv);

    if (!argc) {
        fprintf(stderr, "Usage: %s [input_file]\n", program);
        fprintf(stderr, "... expected at least one input file\n");
        exit(1);
    }

    String_View input_file_path = sv_from_cstr(shift_args(&argc, &argv));

    if (!path_file_exist(input_file_path.data)) {
        fprintf(stderr, "File '%s' is not a regular file or does not exist\n", input_file_path.data);
        exit(1);
    }

    String_View input;
    int err = arena_slurp_file(context_arena, input_file_path, &input);
    if (err != 0) {
        fprintf(stderr, "System error while reading input file '%s': %s\n", input_file_path.data, strerror(errno));
        exit(1);
    }

    Arena arena = {0};

    Parser parser;
    parser.arena = &arena;
    parser_init(&parser, input, path_get_file_name(input_file_path.data), input_file_path);

    // while (1) {
    //     Token token = eat_next_token(&parser);
    //     if (token.type == TOKEN_END_OF_INPUT) break;
    //     printf(Loc_Fmt": %s (end = %d)\n", SV_Arg(input_file_path), Loc_Arg(token.location), token_type_to_string(token.type), token.location.c1);
    // }

    Ast_Block *block = parse_toplevel(&parser);
    printf("--- The block has %ld statement(s). ---\n", arrlen(block->statements));
    For (block->statements) {
        printf("%s\n", ast_to_string(block->statements[it]));
    }

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
