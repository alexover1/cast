#include <stdio.h>

#include "context_alloc.h"
#include "string_builder.h"
#include "vendor/stb_ds.h"

#include "ast.h"
#include "type_info.h"

const char *type_to_string(Type type)
{
    if (type == NULL) return "**NULL**";
    
    // Builtin types
    if (type == &type_info_int)             return "int";
    if (type == &type_info_float)           return "float";
    if (type == &type_info_float64)         return "float64";
    if (type == &type_info_comptime_int)    return "comptime_int";
    if (type == &type_info_comptime_float)  return "comptime_float";
    if (type == &type_info_comptime_string) return "comptime_string";
    
    // Otherwise, construct the string dynamically
    switch (type->tag) {
        case TYPE_INTEGER:
            if (type->integer.sign) {
                return tprint("s%ld", type->runtime_size * 8);
            }
            return tprint("u%ld", type->runtime_size * 8);

        case TYPE_FLOAT:  return tprint("f%ld", type->runtime_size * 8);

        case TYPE_STRING: return "string";
        case TYPE_BOOL:   return "bool";
        case TYPE_VOID:   return "void";
        case TYPE_TYPE:   return "Type";

        case TYPE_PROCEDURE: {
            Arena *old = context_arena;
            context_arena = &temporary_arena;
            String_Builder sb = {0};

            sb_append_cstr(&sb, "(");
            for (size_t i = 0; i < type->procedure.parameter_count; ++i) {
                if (i > 0) sb_append_cstr(&sb, ", ");
                sb_append_cstr(&sb, type_to_string(type->procedure.parameters[i]));
            }
            sb_append_cstr(&sb, ") -> ");
            sb_append_cstr(&sb, type_to_string(type->procedure.return_type));

            context_arena = old;
            return sb.data;
        }

        case TYPE_STRUCT: {
            Arena *old = context_arena;
            context_arena = &temporary_arena;
            String_Builder sb = {0};

            sb_append_cstr(&sb, "{ ");
            for (size_t i = 0; i < type->structure.field_count; ++i) {
                sb_print(&sb, "%s: %s;", type->structure.names[i], type_to_string(type->structure.types[i]));
            }
            sb_append_cstr(&sb, " }");

            context_arena = old;
            return sb.data;
        }

        case TYPE_POINTER: return tprint("*%s", type_to_string(type->pointer.element_type));

        case TYPE_ARRAY: {
            if (type->array.element_count >= 0) {
                return tprint("[%ld]%s",
                    type->array.element_count,
                    type_to_string(type->array.element_type));
            }
            return tprint("[]%s", type_to_string(type->array.element_type));
        }

        default: return "**INVALID**";
    }
}

const char *operator_type_to_string(int operator_type)
{
    if (operator_type < 256) {
        return tprint("%c", (char)operator_type);
    }

    UNIMPLEMENTED;

    // switch (operator_type) {
    // case TOKEN_DOUBLE_EQUAL:  return "==";
    // case TOKEN_NOT_EQUAL:     return "!=";
    // case TOKEN_GREATER_EQUAL: return ">=";
    // case TOKEN_LESS_EQUAL:    return "<=";
    // default:                  return "(unknown operator)";
    // }
}

const char *ast_to_string(const Ast *ast)
{
    if (ast == NULL) return "**NULL**";
    
    #define xx (const Ast *)

    switch (ast->type) {
        case AST_UNINITIALIZED: return "**UNINITIALIZED**";
        
        case AST_LITERAL: {
            const Ast_Literal *lit = Down(ast);
            switch (lit->kind) {
            case LITERAL_INT:    return tprint("%llu", lit->int_value);
            case LITERAL_FLOAT:  return tprint("%f", lit->float_value);
            case LITERAL_STRING: return tprint("\"%*.s\"", lit->string_value.count, lit->string_value.data);
            }
            UNREACHABLE;
        }

        case AST_IDENT: return ((const Ast_Ident *)ast)->name;

        case AST_BINARY_OPERATOR: {
            const Ast_Binary_Operator *bin = Down(ast);
            return tprint("(%s %s %s)",
                ast_to_string(bin->left),
                operator_type_to_string(bin->operator_type),
                ast_to_string(bin->right));
        }
        
        case AST_PROCEDURE_CALL: {
            const Ast_Procedure_Call *call = Down(ast);

            Arena *saved = context_arena;
            context_arena = &temporary_arena;

            String_Builder sb = {0};
            sb_print(&sb, "%s(", ast_to_string(call->procedure_expression));

            For (call->arguments) {
                if (it > 0) sb_append_cstr(&sb, ", ");
                sb_append_cstr(&sb, ast_to_string(call->arguments[it]));
            }
            
            sb_append_cstr(&sb, ")");

            context_arena = saved;
            return sb.data;
        }

        case AST_TYPE_DEFINITION: {
            const Ast_Type_Definition *defn = Down(ast);

            if (defn->struct_desc) {
                return tprint("struct %s", ast_to_string(xx &defn->struct_desc->scope));
            }

            if (defn->enum_defn) {
                return tprint("enum %s", ast_to_string(xx &defn->enum_defn->scope));
            }

            if (defn->literal_name) return defn->literal_name;

            if (defn->array_element_type) {
                return tprint("[] %s", ast_to_string(xx defn->array_element_type));
            }

            if (defn->pointer_to) {
                assert(defn->pointer_level <= 10);
                return tprint("%.*s%s", defn->pointer_level, "**********", ast_to_string(xx defn->pointer_to));
            }

            assert(defn->lambda_return_type);
            
            Arena *saved = context_arena;
            context_arena = &temporary_arena;

            String_Builder sb = {0};
            sb_append_cstr(&sb, "(");

            For (defn->lambda_argument_types) {
                if (it > 0) sb_append_cstr(&sb, ", ");
                sb_append_cstr(&sb, ast_to_string(xx defn->lambda_argument_types[it]));
            }
            
            sb_print(&sb, ") -> %s", ast_to_string(xx defn->lambda_return_type));

            context_arena = saved;
            return sb.data;
        }

        case AST_BLOCK: {
            const Ast_Block *block = Down(ast);

            Arena *saved = context_arena;
            context_arena = &temporary_arena;

            String_Builder sb = {0};
            sb_append_cstr(&sb, "{ ");

            For (block->statements) {
                sb_print(&sb, "%s; ", ast_to_string(xx block->statements[it]));
            }
            
            sb_append_cstr(&sb, "}");

            context_arena = saved;
            return sb.data;
        }

        case AST_LAMBDA_BODY: {
            const Ast_Lambda_Body *body = Down(ast);
            return ast_to_string(&body->block.base);
        }

        case AST_LAMBDA: {
            const Ast_Lambda *lambda = Down(ast);
            return tprint("%s %s", ast_to_string(xx lambda->type_definition), ast_to_string(xx &lambda->body));
        }

        case AST_DECLARATION: {
            const Ast_Declaration *decl = Down(ast);
            if (decl->flags & DECLARATION_IS_COMPTIME) {
                return tprint("%s :: %s", ast_to_string(xx &decl->ident), ast_to_string(xx decl->expression));
            }
            return tprint("%s := %s", ast_to_string(xx &decl->ident), ast_to_string(xx decl->expression));
        }

        default: return "**INVALID**";
    }

    #undef xx
}
