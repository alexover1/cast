#include "context_alloc.h"
#include "type_info.h"
#include "vendor/stb_ds.h"

typedef Ast_Declaration *Decl;
typedef const Ast_Block *Scope;

Decl find_declaration_or_null(Scope scope, const char *name)
{
    while (scope) {
        For (scope->statements) {
            if (scope->statements[it]->type != AST_DECLARATION) continue;

            Decl decl = Down(scope->statements[it]);
            if (strcmp(decl->ident.name, name) == 0) return decl;
        }
        scope = scope->parent;
    }

    return NULL;
}

Type infer_ast_type(const Ast *ast)
{
    if (ast == NULL) return NULL;

    switch (ast->type) {
        case AST_LITERAL: {
            Ast_Literal *lit = Down(ast);
            if (lit->kind == LITERAL_INT)         return &type_info_comptime_int;
            else if (lit->kind == LITERAL_FLOAT)  return &type_info_comptime_float;
            else if (lit->kind == LITERAL_STRING) return &type_info_comptime_string;
            UNREACHABLE;
        }
        
        case AST_BINARY_OPERATOR: {
            Ast_Binary_Operator *bin = Down(ast);
            Type left = infer_ast_type(bin->left);
            Type right = infer_ast_type(bin->right);

            assert(left && right);

            if (left != right) {

                if (right == &type_info_comptime_int) Swap(Type, left, right);
                else if (right == &type_info_comptime_float) Swap(Type, left, right);

                // comptime_int to sized int
                if (left == &type_info_comptime_int && right->tag == TYPE_INTEGER) return right;

                // comptime_int to sized float
                if (left == &type_info_comptime_int && right->tag == TYPE_FLOAT) return right;

                // comptime_float to sized float
                if (left == &type_info_comptime_float && right->tag == TYPE_FLOAT) return right;

                // TODO: comptime_float + int

                // We do not allow comptime_float to sized int.
                // ex:    3.5 + (s64)149

                fprintf(stderr, "error: The types on either side of the binary node must be the same.\n");
                fprintf(stderr, "info: Got types %s and %s\n", type_to_string(left), type_to_string(right));
                exit(1);
            }

            // Now we know that we have the same type on either side.

            // For now, assume all operators work only on int and float.
            if (left->tag != TYPE_INTEGER && left->tag != TYPE_FLOAT) {
                fprintf(stderr, "error: Binary operator expects type int or float but got type %s\n", type_to_string(left));
                exit(1);
            }

            if (bin->operator_type == OPERATOR_DOUBLE_EQUALS) return &type_info_bool;

            return left;
        }
        
        case AST_PROCEDURE_CALL: {
            Ast_Procedure_Call *call = Down(ast);
            Type procedure_type = infer_ast_type(call->procedure_expression);

            assert(procedure_type);

            if (procedure_type->tag != TYPE_PROCEDURE) {
                fprintf(stderr, "error: '%s' is not a procedure.\n", ast_to_string((const Ast *)call->procedure_expression));
                exit(1);
            }
            
            Type_Info_Procedure proc = procedure_type->procedure;

            if (arrlenu(call->arguments) != proc.parameter_count) {
                fprintf(stderr, "error: Incorrect number of arguments to function '%s'.\n",
                    ast_to_string((const Ast *)call->procedure_expression));
                fprintf(stderr, "info: Expected %zu arguments but got %zu\n", proc.parameter_count, arrlenu(call->arguments));
                exit(1);
            }

            For (call->arguments) {
                Type arg = infer_ast_type(call->arguments[it]);
                Type par = proc.parameters[it];
                assert(arg && par);
                if (arg != par) {
                    fprintf(stderr, "error: Parameter %ld of 'print' was expected to be %s but got %s.\n",
                        it + 1,
                        type_to_string(par),
                        type_to_string(arg));
                    exit(1);
                }
            }

            return proc.return_type;
        }

        case AST_IDENT: {
            Ast_Ident *ident = Down(ast);
            assert(ident->name);
            assert(ident->enclosing_block);
            Ast_Declaration *decl = find_declaration_or_null(ident->enclosing_block, ident->name);
            assert(decl);
            assert(decl->expression);
            return infer_ast_type((const Ast *)decl->expression);
        }

        case AST_DECLARATION:
            return NULL;

        case AST_TYPE_DEFINITION:
            return &type_info_type;
        
        default: UNIMPLEMENTED;
    }
}

