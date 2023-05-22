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
    
    Ast_Binary_Operator *bin = context_alloc(sizeof(Ast_Binary_Operator));
    bin->base.type = AST_BINARY_OPERATOR;
    bin->operator_type = '+';
    {
        Ast_Literal *left = context_alloc(sizeof(Ast_Literal));
        left->base.type = AST_LITERAL;
        left->kind = LITERAL_INT;
        left->int_value = 34;

        Ast_Literal *right = context_alloc(sizeof(Ast_Literal));
        right->base.type = AST_LITERAL;
        right->kind = LITERAL_INT;
        right->int_value = 35;
    
        bin->left = Base(left);
        bin->right = Base(right);
    }

    Ast_Declaration *member_decl = context_alloc(sizeof(Ast_Declaration));
    member_decl->base.type = AST_DECLARATION;
    member_decl->ident.base.type = AST_IDENT; // TODO: this is hard to remember to set.
    member_decl->ident.name = "position";
    // member_decl->ident.enclosing_block = file_scope;
    member_decl->expression = Base(bin);
    // member_decl->flags |= DECLARATION_IS_COMPTIME;

    Ast_Declaration *decl = context_alloc(sizeof(Ast_Declaration));
    decl->base.type = AST_DECLARATION;
    decl->ident.base.type = AST_IDENT; // TODO: this is hard to remember to set.
    decl->ident.name = "Entity";
    decl->ident.enclosing_block = file_scope;
    decl->flags |= DECLARATION_IS_COMPTIME;
    
    {
        Ast_Struct *struct_desc = context_alloc(sizeof(Ast_Struct));
        struct_desc->base.type = AST_STRUCT;
        struct_desc->scope.base.type = AST_BLOCK;

        // Add member to parent scope
        arrput(struct_desc->scope.statements, Base(member_decl));
        member_decl->ident.enclosing_block = &struct_desc->scope;
        member_decl->flags |= DECLARATION_IS_STRUCT_FIELD;

        Ast_Type_Definition *defn = context_alloc(sizeof(Ast_Type_Definition));
        defn->base.type = AST_TYPE_DEFINITION;
        defn->struct_desc = struct_desc;

        decl->expression = Base(defn);
    }

    arrput(file_scope->statements, Base(decl));
    
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
