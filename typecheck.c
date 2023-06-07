#include "common.h"
#include "token.h"
#include "ast.h"
#include "type_info.h"
#include "interp.h"
#include "parser.h" // parser_report_error()

#define xx (void*)

static inline uint64_t log_base_2(uint64_t x)
{
    uint64_t count = 0;
    while (x >>= 1) {
        count += 1;
    }
    return count;
}

const Ast_Declaration *find_declaration_or_null(const Ast_Ident *ident)
{
    const Ast_Block *block = ident->enclosing_block;
    while (block) {
        For (block->statements) {
            if (block->statements[it]->type != AST_DECLARATION) continue;

            const Ast_Declaration *decl = xx block->statements[it];
            if (sv_eq(decl->ident->name, ident->name)) return decl;
        }
        block = block->parent;
    }

    return NULL;
}

// x: u8 = 3003;

static inline void report_type_mismatch(Interp *interp, Source_Location location, Type actual, Type expected)
{
    parser_report_error(&interp->parser, location, "Type mismatch: Wanted type %s but got type %s.",
        type_to_string(&interp->type_table, expected),
        type_to_string(&interp->type_table, actual));
}

#define INT_OR_FLOAT(tag) ((1<<(tag))&((1 << TYPE_INTEGER) | (1 << TYPE_FLOAT)))

static void check_for_number_literal_overflow(Interp *interp, const Ast_Literal *lit, Type type)
{
    if (lit->kind == LITERAL_INT) {
        uint64_t needed_bits = log_base_2(lit->int_value) + 1;

        printf("%lu needs at least %zu bits.\n", lit->int_value, needed_bits); // @nocheckin

        if (needed_bits > (uint64_t)type->runtime_size) {
            parser_report_error(&interp->parser, lit->base.location, "Number overflow: Cannot cast %lu into type %s (needs at least %zu bits).",
                type_to_string(&interp->type_table, type));
        }
        return;
    }

    UNIMPLEMENTED;
}

// Returns a replacement node if it is needed to cast the literal.
Ast *typecheck_literal(Interp *interp, const Ast_Literal *lit, Type type)
{
    switch (lit->kind) {
    case LITERAL_INT: {
        size_t mask = (1 << TYPE_INTEGER) | (1 << TYPE_FLOAT) | (1 << TYPE_BOOL);
        size_t flag = (1 << type->tag);

        if ((flag&mask) == 0) {
            report_type_mismatch(interp, lit->base.location, interp->type_table.INT, type);
            return NULL;
        }

        if (type->tag == TYPE_BOOL) {
            Ast_Literal *new_literal = ast_alloc(&interp->parser, lit->base.location, AST_LITERAL, sizeof(*new_literal));
            new_literal->kind = LITERAL_BOOL;
            new_literal->bool_value = lit->int_value != 0;
            return xx new_literal;
        }

        uint64_t needed_bits = log_base_2(lit->int_value) + 1;

        printf("%lu needs at least %zu bits.\n", lit->int_value, needed_bits); // @nocheckin

        if (needed_bits > (uint64_t)type->runtime_size) {
            parser_report_error(&interp->parser, lit->base.location, "Number overflow: Cannot cast %lu into type %s (needs at least %zu bits).",
                type_to_string(&interp->type_table, type));
        }
        return NULL;
    }
    case LITERAL_FLOAT:
        if (type != interp->type_table.FLOAT && type != interp->type_table.float64) {
            report_type_mismatch(interp, lit->base.location, interp->type_table.FLOAT, type);
        }
        return NULL;
    case LITERAL_STRING: {
        if (lit->string_value.count == 1 && type->tag == TYPE_INTEGER) {
            Ast_Literal *new_literal = ast_alloc(&interp->parser, lit->base.location, AST_LITERAL, sizeof(*new_literal));
            new_literal->kind = LITERAL_INT;
            new_literal->int_value = lit->string_value.data[0];
            return xx new_literal;
        }
        
        Type_Info_Array array;
        array.info.tag = TYPE_ARRAY;
        array.element_type = interp->type_table.u8;
        array.element_count = -1;
        if (type != interp->type_table.STRING && !types_are_equal(&interp->type_table, &array.info, type)) {
            report_type_mismatch(interp, lit->base.location, interp->type_table.STRING, type);
        }
        return NULL;
    }
    case LITERAL_BOOL:
        if (type != interp->type_table.BOOL) {
            report_type_mismatch(interp, lit->base.location, interp->type_table.BOOL, type);
        }
        return NULL;
    case LITERAL_NULL:
        return NULL; // Null is able to cast to any type.
    }
}

Type typecheck_expression(Interp *interp, const Ast *ast)
{
    assert(ast);

    switch (ast->type) {     
    case AST_LITERAL: {
        const Ast_Literal *lit = xx ast;
        switch (lit->kind) {
        case LITERAL_INT:    return interp->type_table.comptime_int;
        case LITERAL_FLOAT:  return interp->type_table.comptime_float;
        case LITERAL_STRING: return interp->type_table.comptime_string;
        case LITERAL_BOOL:   return interp->type_table.BOOL;
        case LITERAL_NULL:   return interp->type_table.null;
        }
    }

    case AST_UNARY_OPERATOR: {
        const Ast_Unary_Operator *unary = xx ast;
        Type element_type = typecheck_expression(interp, unary->subexpression);
        switch (unary->operator_type) {
        case '-':
            if (INT_OR_FLOAT(element_type->tag) == 0) {
                parser_report_error(&interp->parser, ast->location, "Type mismatch: Unary '-' only works on integer and floating-point types, but we got %s.",
                    type_to_string(&interp->type_table, element_type));
                return NULL;
            }
            if (element_type->tag == TYPE_INTEGER) {
                Type_Info_Integer *info = xx element_type;
                if (!info->sign) {
                    parser_report_error(&interp->parser, ast->location, "Sign mismatch: Unary '-' only works on signed integer types, but we got type '%s'.",
                        type_to_string(&interp->type_table, element_type));
                    return NULL;
                }
            }
            return element_type;
        case '!':
            if (element_type->tag != TYPE_BOOL) {
                parser_report_error(&interp->parser, ast->location, "Type mismatch: Unary '!' only works on boolean values, but we got %s.",
                    type_to_string(&interp->type_table, element_type));
                return NULL;
            }
            return element_type; // We know it is bool.
        case '*':
            return type_table_append_pointer_to(&interp->type_table, element_type);
        case TOKEN_BITWISE_NOT:
            if (element_type->tag != TYPE_INTEGER) {
                parser_report_error(&interp->parser, ast->location, "Type mismatch: Unary '^' only works on integer values, but we got %s.",
                    type_to_string(&interp->type_table, element_type));
                return NULL;
            }
            return element_type; // We know it is an integer.
        case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT: {
            if (element_type->tag != TYPE_POINTER) {
                parser_report_error(&interp->parser, ast->location, "Mismatching levels of indirection: Dereference of non-pointer type %s.",
                    type_to_string(&interp->type_table, element_type));
                return NULL;
            }
            Type_Info_Pointer *info = xx element_type;
            if (info->pointer_level > 1) {
                Type_Info_Pointer copy = *info;
                copy.pointer_level -= 1;
                return type_table_append(&interp->type_table, &copy, sizeof(copy));
            } else {
                return info->element_type;
            }
        }
        default:
            assert(0);
        }
    }
    
    case AST_BINARY_OPERATOR: {
        const Ast_Binary_Operator *bin = xx ast;
        Type left = typecheck_expression(interp, bin->left);
        Type right = typecheck_expression(interp, bin->right);

        assert(left);
        assert(right);

        if (left != right) {
            // Move all comptime types to the left so we can go fast by not having duplicate code.
            if (right == interp->type_table.comptime_int) Swap(Type, left, right);
            else if (right == interp->type_table.comptime_float) Swap(Type, left, right);

            if (left == interp->type_table.comptime_int && INT_OR_FLOAT(right->tag)) {
                assert(bin->left->type == AST_LITERAL); // Otherwise how is it comptime_int?
                check_for_number_literal_overflow(interp, xx bin->left, right);
                return right;
            }

            if (left == interp->type_table.comptime_float && INT_OR_FLOAT(right->tag)) return right;

            parser_report_error(&interp->parser, ast->location, "Type mismatch: The types on either side of the binary expression must be the same, but we got %s and %s.",
                type_to_string(&interp->type_table, left), type_to_string(&interp->type_table, right));
        }

        // Now we know that we have the same type on either side.

        const Type_Info *type = left;

        // For now, assume all operators work only on int and float.
        // TODO: logical operators and pointer math

        switch (bin->operator_type) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
        case TOKEN_BITWISE_AND:
        case TOKEN_BITWISE_OR:
        case TOKEN_BITWISE_XOR:
        case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT:
        case TOKEN_SHIFT_RIGHT:
            if (INT_OR_FLOAT(type->tag) == 0) {
                parser_report_error(&interp->parser, ast->location, "Type mismatch: The binary operator '%s' expects integer or floating-point types, but we got type %s.",
                    token_type_to_string(bin->operator_type), type_to_string(&interp->type_table, type));
            }
            return type;
        case '>':
        case '<':
        case TOKEN_ISEQUAL:
        case TOKEN_ISNOTEQUAL:
        case TOKEN_LESSEQUALS:
        case TOKEN_GREATEREQUALS:
            if (INT_OR_FLOAT(type->tag) == 0) {
                parser_report_error(&interp->parser, ast->location, "Type mismatch: The binary operator '%s' expects integer or floating-point types, but we got type %s.",
                    token_type_to_string(bin->operator_type), type_to_string(&interp->type_table, type));
            }
            return interp->type_table.BOOL;
        default:
            assert(0);
        }
    }
    
    case AST_PROCEDURE_CALL: {
        const Ast_Procedure_Call *call = xx ast;
        Type type = typecheck_expression(interp, call->procedure_expression);

        assert(type);

        if (type->tag != TYPE_PROCEDURE) {
            fprintf(stderr, "error: '%s' is not a procedure.\n", ast_to_string((const Ast *)call->procedure_expression));
            exit(1);
        }
        
        const Type_Info_Procedure *proc = xx type;

        if (arrlenu(call->arguments) != proc->parameter_count) {
            fprintf(stderr, "error: Incorrect number of arguments to function '%s'.\n",
                ast_to_string((const Ast *)call->procedure_expression));
            fprintf(stderr, "info: Expected %zu arguments but got %zu\n", proc->parameter_count, arrlenu(call->arguments));
            exit(1);
        }

        For (call->arguments) {
            Type arg = typecheck_expression(interp, call->arguments[it]);
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
        const Ast_Ident *ident = xx ast;
        assert(ident->enclosing_block);
        const Ast_Declaration *decl = find_declaration_or_null(ident);
        assert(decl);
        Object *object = hmgetp_null(interp->objects, decl);
        assert(object);
        return object->inferred_type; // Could be null.
    }

    case AST_TYPE_INSTANTIATION: {
        const Ast_Type_Instantiation *inst = xx ast;
        return interp_get_type(interp, inst->type_definition);
    }

    case AST_TYPE_DEFINITION:
        return interp->type_table.TYPE;

    case AST_DECLARATION:
        UNREACHABLE;

    default: UNIMPLEMENTED;
    }       
}

