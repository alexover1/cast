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
    parser.block = NULL;

    lexer_init(&parser.lexer, input);
    parser.lexer.path_name = input_file_path;
    parser.lexer.file_name = path_get_file_name(input_file_path.data);

    Ast *ast = parse_statement(&parser);
    printf("%s\n", ast_to_string(ast));

    // Lexer lexer;
    // lexer_init(&lexer, input);
    // lexer.path_name = input_file_path;
    // lexer.file_name = path_get_file_name(input_file_path.data);

    // Token token = lexer_next_token(&lexer);
    // while (token.type != TOKEN_END_OF_INPUT) {
    //     token = lexer_next_token(&lexer);
    //     lexer_report_error(&lexer, position_from_token(token), "This is illegal.");
    // }

    return 0;
    
    Ast_Block *file_scope = context_alloc(sizeof(Ast_Block));
    file_scope->base.type = AST_BLOCK;
    
    // START :: (34 + 35);
    Ast_Declaration *start_decl = context_alloc(sizeof(Ast_Declaration));
    start_decl->base.type = AST_DECLARATION;
    start_decl->ident = make_identifier("START", file_scope);
    start_decl->flags |= DECLARATION_IS_COMPTIME;
    {
        Ast_Binary_Operator *bin = context_alloc(sizeof(Ast_Binary_Operator));
        bin->base.type = AST_BINARY_OPERATOR;
        bin->operator_type = '+';
        {
            Ast_Literal *left = context_alloc(sizeof(Ast_Literal));
            left->base.type = AST_LITERAL;
            left->kind = LITERAL_INT;
            left->int_value = 30;

            Ast_Literal *right = context_alloc(sizeof(Ast_Literal));
            right->base.type = AST_LITERAL;
            right->kind = LITERAL_INT;
            right->int_value = 35;
    
            bin->left = Base(left);
            bin->right = Base(right);
        }
        start_decl->expression = Base(bin);
    }
    arrput(file_scope->statements, Base(start_decl));

    // Monster :: struct { e: Entity; strength: int; };
    Ast_Declaration *monster_decl = context_alloc(sizeof(Ast_Declaration));
    monster_decl->base.type = AST_DECLARATION;
    monster_decl->ident = make_identifier("Monster", file_scope);
    monster_decl->flags |= DECLARATION_IS_COMPTIME;
    {
        Ast_Type_Definition *defn = context_alloc(sizeof(Ast_Type_Definition));
        defn->base.type = AST_TYPE_DEFINITION;

        Ast_Struct *struct_desc = context_alloc(sizeof(Ast_Struct));
        struct_desc->base.type = AST_STRUCT;
        struct_desc->scope.base.type = AST_BLOCK;
        struct_desc->scope.parent = file_scope;

        defn->struct_desc = struct_desc;
        monster_decl->expression = Base(defn);

        // e: Entity;
        // Which gets translated into:
        // e := Entity{};
        Ast_Declaration *member_decl = context_alloc(sizeof(Ast_Declaration));
        member_decl->base.type = AST_DECLARATION;
        member_decl->ident = make_identifier("e", &struct_desc->scope);
        member_decl->flags |= DECLARATION_IS_STRUCT_FIELD;
        {
            Ast_Type_Definition *defn = context_alloc(sizeof(Ast_Type_Definition));
            defn->base.type = AST_TYPE_DEFINITION;
            defn->type_name = context_alloc(sizeof(Ast_Ident));
            *defn->type_name = make_identifier("Entity", &struct_desc->scope);
            
            Ast_Type_Instantiation *inst = context_alloc(sizeof(Ast_Type_Instantiation));
            inst->base.type = AST_TYPE_INSTANTIATION;
            inst->type_definition = defn;
            inst->argument_list = NULL;
            inst->flags |= INSTANTIATION_IS_IMPLICIT;

            member_decl->expression = Base(inst);
        }
        arrput(struct_desc->scope.statements, Base(member_decl));

        Ast_Declaration *strength_decl = context_alloc(sizeof(Ast_Declaration));
        strength_decl->base.type = AST_DECLARATION;
        strength_decl->ident = make_identifier("strength", &struct_desc->scope);
        strength_decl->flags |= DECLARATION_IS_STRUCT_FIELD;
        {
            Ast_Type_Definition *defn = context_alloc(sizeof(Ast_Type_Definition));
            defn->base.type = AST_TYPE_DEFINITION;
            defn->literal_name = "int";
            
            Ast_Type_Instantiation *inst = context_alloc(sizeof(Ast_Type_Instantiation));
            inst->base.type = AST_TYPE_INSTANTIATION;
            inst->type_definition = defn;
            inst->argument_list = NULL;
            inst->flags |= INSTANTIATION_IS_IMPLICIT;

            strength_decl->expression = Base(inst);
        }
        arrput(struct_desc->scope.statements, Base(strength_decl));
    }
    arrput(file_scope->statements, Base(monster_decl));
   
    // Entity :: struct { position := (34 + 35); };
    Ast_Declaration *entity_decl = context_alloc(sizeof(Ast_Declaration));
    entity_decl->base.type = AST_DECLARATION;
    entity_decl->ident = make_identifier("Entity", file_scope);
    entity_decl->flags |= DECLARATION_IS_COMPTIME;
    {
        Ast_Struct *struct_desc = context_alloc(sizeof(Ast_Struct));
        struct_desc->base.type = AST_STRUCT;
        struct_desc->scope.base.type = AST_BLOCK;
        struct_desc->scope.parent = file_scope;

        // position := START;
        Ast_Declaration *member_decl = context_alloc(sizeof(Ast_Declaration));
        member_decl->base.type = AST_DECLARATION;
        member_decl->ident = make_identifier("position", &struct_desc->scope);
        member_decl->flags |= DECLARATION_IS_STRUCT_FIELD;
        {
            Ast_Ident *ident = context_alloc(sizeof(Ast_Ident));
            *ident = make_identifier("START", &struct_desc->scope);
            member_decl->expression = Base(ident);
        }
        arrput(struct_desc->scope.statements, Base(member_decl));

        Ast_Type_Definition *defn = context_alloc(sizeof(Ast_Type_Definition));
        defn->base.type = AST_TYPE_DEFINITION;
        defn->struct_desc = struct_desc;

        entity_decl->expression = Base(defn);
    }
    arrput(file_scope->statements, Base(entity_decl));

    // Copy :: Entity;
    Ast_Declaration *copy_decl = context_alloc(sizeof(Ast_Declaration));
    copy_decl->base.type = AST_DECLARATION;
    copy_decl->ident = make_identifier("Copy", file_scope);
    copy_decl->flags |= DECLARATION_IS_COMPTIME;
    {
        Ast_Ident *ident = context_alloc(sizeof(Ast_Ident));
        *ident = make_identifier("Entity", file_scope);
        copy_decl->expression = Base(ident);
    }
    arrput(file_scope->statements, Base(copy_decl));

    printf("{\n");
    For (file_scope->statements) {
        printf("    %s;\n", ast_to_string(file_scope->statements[it]));
    }
    printf("}\n");

    {
        Interp interp = init_interp();
        interp_add_scope(&interp, file_scope);
        interp_run_main_loop(&interp);
    }

    arena_free(&temporary_arena);
    return 0;
}

Ast_Ident make_identifier(const char *name, Ast_Block *enclosing_block)
{
    return (Ast_Ident) {
        .base = { .type = AST_IDENT },
        .name = name,
        .enclosing_block = enclosing_block,
    };
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
