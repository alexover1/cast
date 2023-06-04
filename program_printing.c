#include <stdio.h>

#include "common.h"
#include "token.h"
#include "ast.h"
#include "type_info.h"
#include "vendor/stb_ds.h"

const char *type_to_string(const Type_Table *table, const Type_Info *type)
{
    if (type == NULL) return "**NULL**";
    
    // Builtin types
    if (type == table->INT)             return "int";
    if (type == table->FLOAT)           return "float";
    if (type == table->BOOL)            return "bool";
    if (type == table->float64)         return "float64";
    if (type == table->anyptr)          return "anyptr";
    if (type == table->comptime_int)    return "comptime_int";
    if (type == table->comptime_float)  return "comptime_float";
    if (type == table->comptime_string) return "comptime_string";
    
    // Otherwise, construct the string dynamically
    switch (type->tag) {
        case TYPE_INTEGER:
            if (((const Type_Info_Integer *)type)->sign) {
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

            const Type_Info_Procedure *proc = (const Type_Info_Procedure *)type;

            sb_append_cstr(&sb, "(");
            for (size_t i = 0; i < proc->parameter_count; ++i) {
                if (i > 0) sb_append_cstr(&sb, ", ");
                sb_append_cstr(&sb, type_to_string(table, proc->parameters[i]));
            }
            sb_append_cstr(&sb, ") -> ");
            sb_append_cstr(&sb, type_to_string(table, proc->return_type));

            context_arena = old;
            return sb.data;
        }

        case TYPE_STRUCT: {
            Arena *old = context_arena;
            context_arena = &temporary_arena;
            String_Builder sb = {0};

            const Type_Info_Struct *struct_info = (const Type_Info_Struct *)type;

            sb_append_cstr(&sb, "{ ");
            for (size_t i = 0; i < struct_info->field_count; ++i) {
                Type_Info_Struct_Field *field = struct_info->field_data + i;
                sb_print(&sb, "%s: %s; ", field->name, type_to_string(table, field->type));
            }
            sb_append_cstr(&sb, "}");

            context_arena = old;
            return sb.data;
        }

        case TYPE_POINTER: {
            const Type_Info_Pointer *pointer = Down(type);
            return tprint("*%s", type_to_string(table, pointer->element_type));
        }

        case TYPE_ARRAY: {
            const Type_Info_Array *array = Down(type);
            if (array->element_count >= 0) {
                return tprint("[%ld]%s",
                    array->element_count,
                    type_to_string(table, array->element_type));
            }
            return tprint("[]%s", type_to_string(table, array->element_type));
        }

        default: return "**INVALID**";
    }
}

const char *token_type_to_string(int type)
{
    if (type < 256) {
        return tprint("%c", (char) type);
    }

    switch (type) {
    case TOKEN_IDENT: return "identifier";
    case TOKEN_NUMBER: return "number";
    case TOKEN_STRING: return "string";
    case TOKEN_PLUSEQUALS: return "+=";
    case TOKEN_MINUSEQUALS: return "-=";
    case TOKEN_TIMESEQUALS: return "*=";
    case TOKEN_DIVEQUALS: return "/=";
    case TOKEN_MODEQUALS: return "%=";
    case TOKEN_ISEQUAL: return "==";
    case TOKEN_ISNOTEQUAL: return "!=";
    case TOKEN_LOGICAL_AND: return "&&";
    case TOKEN_LOGICAL_OR: return "||";
    case TOKEN_LESSEQUALS: return "<=";
    case TOKEN_GREATEREQUALS: return ">=";

    case TOKEN_RIGHT_ARROW: return "->";
    case TOKEN_DOUBLE_DOT: return "..";

    case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT: return "<<";
    case TOKEN_SHIFT_RIGHT: return ">>";
    case TOKEN_BITWISE_AND_EQUALS: return "&=";
    case TOKEN_BITWISE_OR_EQUALS: return "|=";
    case TOKEN_BITWISE_XOR_EQUALS: return "^=";
    
    case TOKEN_KEYWORD_IF: return "if";
    case TOKEN_KEYWORD_THEN: return "then";
    case TOKEN_KEYWORD_ELSE: return "else";
    // case TOKEN_KEYWORD_CASE: return "case";
    case TOKEN_KEYWORD_RETURN: return "return";
    case TOKEN_KEYWORD_STRUCT: return "struct";
    case TOKEN_KEYWORD_WHILE: return "while";
    case TOKEN_KEYWORD_BREAK: return "break";
    case TOKEN_KEYWORD_CONTINUE: return "continue";
    case TOKEN_KEYWORD_USING: return "using";

    case TOKEN_KEYWORD_DEFER: return "defer";
    case TOKEN_KEYWORD_SIZE_OF: return "size_of";
    case TOKEN_KEYWORD_TYPE_OF: return "type_of";
    case TOKEN_KEYWORD_INITIALIZER_OF: return "initializer_of";
    case TOKEN_KEYWORD_TYPE_INFO: return "type_info";
    case TOKEN_KEYWORD_NULL: return "null";

    case TOKEN_KEYWORD_ENUM: return "enum";
    case TOKEN_KEYWORD_TRUE: return "true";
    case TOKEN_KEYWORD_FALSE: return "false";
    case TOKEN_KEYWORD_UNION: return "union";

    case TOKEN_NOTE: return "note";
    case TOKEN_END_OF_INPUT: return "end of input";

    case TOKEN_ERROR: return "error";

    default: return "**INVALID**";
    }
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
            case LITERAL_BOOL:   return lit->bool_value ? "true" : "false";
            case LITERAL_NULL:   return "null";
            }
            UNREACHABLE;
        }

        case AST_IDENT: return ((const Ast_Ident *)ast)->name;

        case AST_BINARY_OPERATOR: {
            const Ast_Binary_Operator *bin = Down(ast);
            return tprint("(%s %s %s)",
                ast_to_string(bin->left),
                token_type_to_string(bin->operator_type),
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

            if (defn->type_name) return ast_to_string(xx defn->type_name);

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

        case AST_TYPE_INSTANTIATION: {
            const Ast_Type_Instantiation *inst= Down(ast);
            // TODO: print values
            return tprint("%s{}", ast_to_string(xx inst->type_definition));
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

        case AST_IF: {
            const Ast_If *if_stmt = Down(ast);
            if (if_stmt->else_statement != NULL) {
                return tprint("if %s then %s else %s",
                    ast_to_string(if_stmt->condition_expression),
                    ast_to_string(if_stmt->then_statement),
                    ast_to_string(if_stmt->else_statement));
            }
            return tprint("if %s then %s",
                ast_to_string(if_stmt->condition_expression),
                ast_to_string(if_stmt->then_statement));
        }

        case AST_WHILE: {
            const Ast_While *while_stmt = Down(ast);
            return tprint("while %s %s",
                ast_to_string(while_stmt->condition_expression),
                ast_to_string(while_stmt->then_statement));
        }

        case AST_LOOP_CONTROL: {
            const Ast_Loop_Control *loop_control = Down(ast);
            return token_type_to_string(loop_control->keyword_type);
        }

        case AST_RETURN: {
            const Ast_Return *ret = Down(ast);
            return tprint("return %s", ast_to_string(ret->subexpression));
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
