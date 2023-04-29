#include "context_alloc.h"
#include "string_builder.h"

const char *type_to_string(Type type)
{
    switch (type->tag) {
        case TYPE_INTEGER:
            if (type->signed_integer) {
                return tprint("s%ld", type->runtime_size * 8);
            }
            return tprint("u%ld", type->runtime_size * 8);

        case TYPE_FLOAT:  return tprint("f%ld", type->runtime_size * 8);

        case TYPE_STRING: return "string";
        case TYPE_BOOL:   return "bool";
        case TYPE_VOID:   return "void";

        case TYPE_PROCEDURE: {
            Arena *old = context_arena;
            context_arena = &temporary_arena;
            String_Builder sb = {0};

            sb_append_cstr(&sb, "(");
            for (size_t i = 0; i < type->procedure.parameters_count; ++i) {
                if (i > 0) sb_append_cstr(&sb, ", ");
                sb_append_cstr(&sb, type_to_string(type->procedure.parameters[i]));
            }
            sb_append_cstr(&sb, ") -> ");
            sb_append_cstr(&sb, type_to_string(type->procedure.return_type));

            context_arena = old;
            return sb.data;
        } break;

        default: return "(unknown type)";
    }
}

const char *token_kind_to_string(Token_Kind token_kind)
{
    if (token_kind < 256) {
        return tprint("%c", (char)token_kind);
    }

    switch (token_kind) {
    case TOKEN_DOUBLE_EQUAL:  return "==";
    case TOKEN_NOT_EQUAL:     return "!=";
    case TOKEN_GREATER_EQUAL: return ">=";
    case TOKEN_LESS_EQUAL:    return "<=";
    default:                  return "(unknown token)";
    }
}

const char *ast_to_string(Ast_Node node)
{
    switch (node.kind) {
        case AST_INTEGER:    return tprint("%llu", node.integer);
        case AST_FLOAT:      return tprint("%f", node.floating);
        case AST_STRING:     return tprint("\"%s\"", node.string);
        case AST_IDENTIFIER: return node.identifier.text;

        case AST_BINARY:
            return tprint("(%s %s %s)",
                ast_to_string(*node.binary.lhs),
                token_kind_to_string(node.binary.op),
                ast_to_string(*node.binary.rhs));
        
        case AST_PROCEDURE_CALL: {
            Arena *old = context_arena;
            context_arena = &temporary_arena;
            String_Builder sb = {0};

            sb_print(&sb, "%s(", ast_to_string(*node.procedure_call.procedure));
            for (size_t i = 0; i < node.procedure_call.arguments_count; ++i) {
                if (i > 0) sb_append_cstr(&sb, ", ");
                sb_append_cstr(&sb, ast_to_string(node.procedure_call.arguments[i]));
            }
            sb_append_cstr(&sb, ")");

            context_arena = old;
            return sb.data;
        } break;

        default:
            assert(0 && "not implemented yet");
    }
}