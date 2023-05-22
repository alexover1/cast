#include "context_alloc.h"
#include "vendor/arena.h"
#include "vendor/stb_ds.h"
#include "ast.h"
#include "pipe.h"

Arena temporary_arena = {0};
Arena *context_arena = &temporary_arena;

int main(void)
{
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
        Pipe pipe = init_pipe();
        pipe_add_scope(&pipe, file_scope);
        pipe_forward(&pipe);
    }

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

// TODO: It's really easy to check for a procedure when parsing.
//       Right after we parse a parentheses expression, just check
//       if the next token is a "->" token. (or check for "{")
