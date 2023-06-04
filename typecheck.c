#include "common.h"
#include "token.h"
#include "ast.h"
#include "type_info.h"
#include "interp.h"

const Ast_Declaration *find_declaration_or_null(const Ast_Ident *ident)
{
    const Ast_Block *block = ident->enclosing_block;
    while (block) {
        For (block->statements) {
            if (block->statements[it]->type != AST_DECLARATION) continue;

            const Ast_Declaration *decl = Down(block->statements[it]);
            if (strcmp(decl->ident.name, ident->name) == 0) return decl;
        }
        block = block->parent;
    }

    return NULL;
}

#define xx (const Type_Info *)

// TODO: use lexer_report_error
Type typecheck_ast(const Interp *interp, const Ast *ast)
{
    if (ast == NULL) return NULL;

    switch (ast->type) {       
    case AST_LITERAL: {
        const Ast_Literal *lit = Down(ast);
        if (lit->kind == LITERAL_INT)         return interp->type_table.comptime_int;
        else if (lit->kind == LITERAL_FLOAT)  return interp->type_table.comptime_float;
        else if (lit->kind == LITERAL_STRING) return interp->type_table.comptime_string;
        else if (lit->kind == LITERAL_BOOL)   return interp->type_table.BOOL;
        else if (lit->kind == LITERAL_NULL)   return interp->type_table.anyptr;
        UNREACHABLE;
    }
    
    case AST_BINARY_OPERATOR: {
        const Ast_Binary_Operator *bin = Down(ast);
        Type left = typecheck_ast(interp, bin->left);
        Type right = typecheck_ast(interp, bin->right);

        assert(left);
        assert(right);

        if (left != right) {
            if (left == interp->type_table.comptime_int) Swap(Type, left, right);
            else if (right == interp->type_table.comptime_float) Swap(Type, left, right);

            // comptime_int to any int
            if (left == interp->type_table.comptime_int && right->tag == TYPE_INTEGER) return right;

            // comptime_int to any float
            if (left == interp->type_table.comptime_int && right->tag == TYPE_FLOAT) return right;

            // comptime_float to any float
            if (left == interp->type_table.comptime_float && right->tag == TYPE_FLOAT) return right;

            // comptime_float to any int
            if (left == interp->type_table.comptime_float && right->tag == TYPE_INTEGER) return interp->type_table.FLOAT;

            fprintf(stderr, "error: The types on either side of the binary node must be the same.\n");
            fprintf(stderr, "info: Got types %s and %s\n",
                type_to_string(&interp->type_table, left),
                type_to_string(&interp->type_table, right));
            exit(1);
        }

        // Now we know that we have the same type on either side.

        const Type_Info *type = left;

        // For now, assume all operators work only on int and float.
        // TODO: logical operators and pointer math

        if (type->tag != TYPE_INTEGER && type->tag != TYPE_FLOAT) {
            fprintf(stderr, "error: Binary operator expects type int or float but got type %s\n", type_to_string(&interp->type_table, type));
            exit(1);
        }

        int c = bin->operator_type;
        if (c == TOKEN_ISEQUAL || c == TOKEN_ISNOTEQUAL || c == TOKEN_LESSEQUALS || c == TOKEN_GREATEREQUALS) return interp->type_table.BOOL;

        return type;
    }
    
    case AST_PROCEDURE_CALL: {
        const Ast_Procedure_Call *call = Down(ast);
        Type type = typecheck_ast(interp, call->procedure_expression);

        assert(type);

        if (type->tag != TYPE_PROCEDURE) {
            fprintf(stderr, "error: '%s' is not a procedure.\n", ast_to_string((const Ast *)call->procedure_expression));
            exit(1);
        }
        
        const Type_Info_Procedure *proc = Down(type);

        if (arrlenu(call->arguments) != proc->parameter_count) {
            fprintf(stderr, "error: Incorrect number of arguments to function '%s'.\n",
                ast_to_string((const Ast *)call->procedure_expression));
            fprintf(stderr, "info: Expected %zu arguments but got %zu\n", proc->parameter_count, arrlenu(call->arguments));
            exit(1);
        }

        For (call->arguments) {
            Type arg = typecheck_ast(interp, call->arguments[it]);
            Type par = proc->parameters[it];
            assert(arg);
            assert(par);
            if (!types_are_equal(&interp->type_table, par, arg)) {
                fprintf(stderr, "error: Parameter %ld of 'print' was expected to be %s but got %s.\n",
                    it + 1,
                    type_to_string(&interp->type_table, par),
                    type_to_string(&interp->type_table, arg));
                exit(1);
            }
        }

        return proc->return_type;
    }

    case AST_IDENT: {
        const Ast_Ident *ident = Down(ast);
        assert(ident->name);
        assert(ident->enclosing_block);
        const Ast_Declaration *decl = find_declaration_or_null(ident);
        assert(decl);
        assert(decl->expression);
        return typecheck_ast(interp, decl->expression);
    }

    case AST_DECLARATION:
    case AST_TYPE_DEFINITION:
    case AST_TYPE_INSTANTIATION:
        UNREACHABLE;

    default: UNIMPLEMENTED;
    }       
}

