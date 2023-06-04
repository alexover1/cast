#include <errno.h>
#include <string.h> // strerror
#include <stdlib.h> // exit

#include "common.h"
#include "vendor/arena.h"
#include "vendor/stb_ds.h"
#include "ast.h"
#include "lexer.h"
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

    while (1) {
        Token token = eat_next_token(&parser);
        printf("%s\n", token_type_to_string(token.type));
        if (token.type == TOKEN_END_OF_INPUT) break;
    }

    arena_free(&temporary_arena);
    return 0;
    
    Ast_Block *file_scope = context_alloc(sizeof(Ast_Block));
    file_scope->base.type = AST_BLOCK;

    Ast_Declaration *decl = context_alloc(sizeof(Ast_Declaration));
    decl->base.type = AST_DECLARATION;
    decl->ident =  (Ast_Ident) {
        .base = { .type = AST_IDENT },
        .name = "foo",
        .enclosing_block = file_scope,
    };
    decl->flags |= DECLARATION_IS_COMPTIME;
    {
        Ast_Struct *struct_desc = context_alloc(sizeof(Ast_Struct));
        struct_desc->base.type = AST_STRUCT;
        struct_desc->scope.base.type = AST_BLOCK;
        struct_desc->scope.parent = file_scope;

        // position := START;
        Ast_Declaration *member_decl = context_alloc(sizeof(Ast_Declaration));
        member_decl->base.type = AST_DECLARATION;
        member_decl->ident =  (Ast_Ident) {
            .base = { .type = AST_IDENT },
            .name = "position",
            .enclosing_block = file_scope,
        };
        member_decl->flags |= DECLARATION_IS_STRUCT_FIELD;
        {
            Ast_Literal *lit = context_alloc(sizeof(Ast_Literal));
            lit->base.type = AST_LITERAL;
            lit->kind = LITERAL_INT;
            lit->int_value = 5;
            member_decl->expression = Base(lit);
        }
        arrput(struct_desc->scope.statements, Base(member_decl));

        Ast_Type_Definition *defn = context_alloc(sizeof(Ast_Type_Definition));
        defn->base.type = AST_TYPE_DEFINITION;
        defn->struct_desc = struct_desc;

        decl->expression = Base(defn);
    }
    arrput(file_scope->statements, Base(decl));

    Interp interp = init_interp();
    interp_add_scope(&interp, file_scope);
    interp_run_main_loop(&interp);

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
