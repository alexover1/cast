#include <stdarg.h>

#include "typecheck.h"
#include "workspace.h"

#define number_flags_is_int(nf) (((nf)&NUMBER_FLAGS_NUMBER) && !((nf)&NUMBER_FLAGS_FLOAT))
#define is_integer_type(defn) number_flags_is_int((defn)->number_flags)

#define Replace(ast) do { \
    while (ast->replacement) ast = ast->replacement; \
} while(0)

#define Wait_On(expr) if ((expr)->inferred_type == NULL) return

bool pointer_types_are_equal(Ast_Type_Definition *x, Ast_Type_Definition *y)
{
    if (x->pointer_level != y->pointer_level) return false;
    return types_are_equal(x->pointer_to, y->pointer_to);
}

bool types_are_equal(Ast_Type_Definition *x, Ast_Type_Definition *y)
{
    if (x == y) return true;

    if (x->pointer_to && y->pointer_to) return pointer_types_are_equal(x, y);

    if (x->array_element_type && y->array_element_type) {
        if (x->array_length != y->array_length) return false;
        return types_are_equal(x->array_element_type, y->array_element_type);
    }

    // All other types, such as structures and enumerations, can only be compared by pointer.
    // We do *NOT* do any duck typing or other functional programming strangeness. If you want
    // "duck" typing, you can use compile-time polymorphism or metaprogramming.

    return false;
}

void typecheck_literal(Workspace *w, Ast_Literal *literal)
{
    UNUSED(w);
    literal->base.inferred_type = literal->type;
}

void typecheck_literal_as_type(Workspace *w, Ast_Literal *literal, Ast_Type_Definition *type_def)
{
    if (literal->type == type_def) return;
    
    // Integer literals can cast to many other types.
    if (number_flags_is_int(literal->number_flags)) {
        // If we are inferred to be some kind of integer type, make sure we fit into that range!
        if (number_flags_is_int(type_def->number_flags)) {
            unsigned long low = type_def->number_literal_low->integer_value;
            unsigned long high = type_def->number_literal_high->integer_value;
            
            if (literal->integer_value > high) {
                report_error(&literal->base, "Numeric constant too big for type (max for %s is %lu).", type_def->literal_name, high);
            }
            if (literal->integer_value < low) {
                report_error(&literal->base, "Numeric constant too small for type (min for %s is %lu).", type_def->literal_name, low);
            }
            return;
        }

        if (type_def == w->type_def_bool) {
            if (literal->integer_value > 1) {
                report_error(&literal->base, "Numeric constant too big for type (max for bool is 1).");
            }
            return;
        }
    }

    // Strings can cast into array of u8, and integer if they are only 1 character.
    else if (literal->type == w->type_def_string) {
        if (is_integer_type(type_def)) {
            if (literal->string_value.count != 1) {
                report_error(&literal->base, "Type mismatch: Strings can only be converted into integers if they are exactly 1 character.");
            }
            literal->base.replacement = xx make_integer_literal(type_def, *literal->string_value.data);
            return;
        } else if (type_def->array_element_type) {
            if (type_def->array_element_type != w->type_def_u8) {
                report_error(&literal->base, "Type mismatch: Strings can only be converted into arrays of u8 (got %s).",
                    type_to_string(type_def->array_element_type));
            }
            if (type_def->array_length > 0 && type_def->array_length != (long long)literal->string_value.count) {
                report_error(&literal->base, "Incorrect array length for string literal (actual %zu).", literal->string_value.count);
            }
            return;
        }
    }

    else if (literal->type == w->type_def_float) {
        if (type_def == w->type_def_float32 || type_def == w->type_def_float64) return;
    }

    else if (literal->type == w->type_def_void_pointer) {
        if (type_def->pointer_to) return;
        report_error(&literal->base, "Type mismatch: 'null' can only be applied to pointer types (got %s).", type_to_string(type_def));
    }

    report_error(&literal->base, "Type mismatch: Wanted %s but got %s.", type_to_string(type_def), type_to_string(literal->type));
    return;
}

void typecheck_identifier(Workspace *w, Ast_Ident *ident)
{
    UNUSED(w);

    const Ast_Declaration *resolved_decl = find_declaration_if_exists(ident);
    if (!resolved_decl) {
        report_error(&ident->base, "Undeclared identifier '"SV_Fmt"'.", SV_Arg(ident->name));
    }

    Wait_On(&resolved_decl->base);

    ident->base.inferred_type = resolved_decl->base.inferred_type;
}

void typecheck_unary_operator(Workspace *w, Ast_Unary_Operator *unary)
{
    UNUSED(w);
    UNUSED(unary);
    UNIMPLEMENTED;
}

// @Cleanup: This whole function's error messages.
void typecheck_binary_operator(Workspace *w, Ast_Binary_Operator *binary)
{
    // TODO: If left and right are literals, replace us with a literal.
    // Technically, LLVM does this for us, but let's not rely on that.
    
    // Check for constant replacement.
    Ast *binary_left = binary->left;
    Ast *binary_right = binary->right;
    Replace(binary_left);
    Replace(binary_right);
    
    // Ensure that both sides of the tree have already been inferred before continuing.
    Ast_Type_Definition *left = binary_left->inferred_type;
    Ast_Type_Definition *right = binary_right->inferred_type;
    
    if (!left || !right) return; // Waiting...

    switch (binary->operator_type) {
    case TOKEN_ARRAY_SUBSCRIPT:
        UNIMPLEMENTED;
    case TOKEN_ISEQUAL:
    case TOKEN_ISNOTEQUAL:
        if (!types_are_equal(left, right)) {
            report_error(&binary->base, "Type mismatch: Cannot compare values of different types (got %s and %s).",
                type_to_string(left), type_to_string(right));
        }
        break;
    case '>':
    case '<':
    case TOKEN_GREATEREQUALS:
    case TOKEN_LESSEQUALS:
        if (left != right) {
            report_error(&binary->base, "Type mismatch: Cannot compare values of different types (got %s and %s).",
                type_to_string(left), type_to_string(right));
        }
        if ((left->number_flags&NUMBER_FLAGS_NUMBER) == 0) {
            report_error(binary->left, "Type mismatch: Operator '%s' only works on number types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(left));
        }
        break;
    case TOKEN_LOGICAL_AND:
    case TOKEN_LOGICAL_OR:
        if (left != w->type_def_bool) {
            report_error(binary->left, "Type mismatch: Operator '%s' only works on boolean types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(left));
        }
        if (right != w->type_def_bool) {
            report_error(binary->right, "Type mismatch: Operator '%s' only works on boolean types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(right));
        }
        break;
    case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT:
    case TOKEN_SHIFT_RIGHT:
        if (!number_flags_is_int(left->number_flags)) {
            report_error(binary->left, "Type mismatch: Bit shift operators only work on integer types (got %s).", type_to_string(left));
        }
        if (!number_flags_is_int(right->number_flags)) {
            report_error(binary->right, "Type mismatch: Bit shift operators only work on integer types (got %s).", type_to_string(right));
        }
        break;
    default:       
        // We let the left type be the determinant.
        binary->base.inferred_type = left;

        if (left->number_flags & NUMBER_FLAGS_NUMBER) {
            if (left != right) {
                report_error(&binary->base, "Type mismatch: Types on either side of '%s' must be the same (got %s and %s).",
                    token_type_to_string(binary->operator_type), type_to_string(left), type_to_string(right));
            }
            return;
        }

        if (left->pointer_to) {
            if (binary->operator_type != '+' && binary->operator_type != '-') {
                report_error(&binary->base, "Type mismatch: Operator '%s' does not work on pointers.", token_type_to_string(binary->operator_type));
            }

            // Two possibilities, pointer-pointer and pointer-int.
            if (right->pointer_to) {
                if (!pointer_types_are_equal(left, right)) {
                    report_error(&binary->base, "Type mismatch: Pointer types on either side of '%s' must be the same (got %s and %s).",
                        token_type_to_string(binary->operator_type), type_to_string(left), type_to_string(right));
                }
            } else if (!number_flags_is_int(right->number_flags)) {
                if (binary->operator_type == '+') {
                    report_error(&binary->base, "Type mismatch: Can only add integer types to pointers (got %s)", type_to_string(right));
                } else if (binary->operator_type == '-') {
                    report_error(&binary->base, "Type mismatch: Can only subtract integer types to pointers (got %s)", type_to_string(right));
                }
            }
            return;
        }

        report_error(&binary->base, "Type mismatch: Operator '%s' only works on literal types (got %s).",
            token_type_to_string(binary->operator_type), type_to_string(left));
    }

    binary->base.inferred_type = left;
}

void typecheck_lambda(Workspace *w, Ast_Lambda *lambda)
{
    lambda->base.inferred_type = lambda->type_definition;
    UNUSED(w);
}

bool typecheck_procedure_call(Workspace *w, Ast_Procedure_Call *call)
{
    UNUSED(w);
    UNUSED(call);
    UNIMPLEMENTED;
}

bool typecheck_while(Workspace *w, Ast_While *while_stmt)
{
    UNUSED(w);
    UNUSED(while_stmt);
    UNIMPLEMENTED;
}

void typecheck_if(Workspace *w, Ast_If *if_stmt)
{
    Wait_On(if_stmt->condition_expression);

    // TODO: implicit bool conversions.

    if (if_stmt->condition_expression->inferred_type != w->type_def_bool) {
        report_error(if_stmt->condition_expression, "Condition of 'if' statement must result in a boolean value.");
    }

    if_stmt->base.inferred_type = w->type_def_void;
}

void typecheck_return(Workspace *w, Ast_Return *ret)
{
    Wait_On(ret->subexpression);
    
    Ast_Type_Definition *expected_type = ret->lambda_i_belong_to->type_definition->lambda_return_type;
    Ast_Type_Definition *actual_type = ret->subexpression->inferred_type;

    if (!types_are_equal(expected_type, actual_type)) {
        report_error(&ret->base, "Return type mismatch: Wanted %s but got %s.",
            type_to_string(expected_type), type_to_string(actual_type));
    }

    ret->base.inferred_type = w->type_def_void;
}

void typecheck_definition(Workspace *w, Ast_Type_Definition *defn)
{
    defn->base.inferred_type = w->type_def_type;
}

void typecheck_instantiation(Workspace *w, Ast_Type_Instantiation *inst)
{
    Wait_On(inst->initializer_expression);

    // The declaration `x := 5` will produce an initializer expression with no type definition,
    // in which case we just infer the type from the initializer.

    if (!inst->type_definition) {
        inst->type_definition = inst->initializer_expression->inferred_type;
    } else {
        if (!types_are_equal(inst->initializer_expression->inferred_type, inst->type_definition)) {
            report_error(inst->initializer_expression, "Type mismatch: Wanted %s but got %s.",
                type_to_string(inst->type_definition), type_to_string(inst->initializer_expression->inferred_type));
        }
        inst->base.inferred_type = inst->type_definition;
    }

    UNUSED(w);
}

void typecheck_enum(Workspace *w, Ast_Enum *enum_defn)
{
    enum_defn->base.inferred_type = w->type_def_type;
}

void typecheck_struct(Workspace *w, Ast_Struct *struct_desc)
{
    struct_desc->base.inferred_type = w->type_def_type;
}

void typecheck_using(Workspace *w, Ast_Using *using)
{
    UNUSED(w);
    UNUSED(using);
    UNIMPLEMENTED;
}

void typecheck_declaration(Workspace *w, Ast_Declaration *decl)
{   
    if (decl->expression && !decl->expression->inferred_type) return; // Waiting...
    
    if (decl->my_type) {
        if (decl->expression) {
            if (decl->my_type == w->type_def_void) {
                report_error(&decl->base, "Cannot assign to void.");
            }

            // Do constant replacement.
            Ast *expr = decl->expression;
            while (expr->replacement) expr = expr->replacement;

            if (expr->type == AST_LITERAL) {
                typecheck_literal_as_type(w, xx expr, decl->my_type);
            } else {
                // Make sure the value fits into the type.

                if (!types_are_equal(decl->my_type, expr->inferred_type)) {
                    report_error(&decl->base, "Type mismatch: Wanted %s but got %s.",
                        type_to_string(decl->my_type), type_to_string(expr->inferred_type));
                }
            }
            
            decl->expression = expr;
            decl->expression->inferred_type = decl->my_type;
            decl->base.inferred_type = decl->my_type;
            return;
        }

        // No default value, so create it here.
        if (decl->my_type->number_flags & NUMBER_FLAGS_NUMBER) {
            if (decl->my_type->number_flags & NUMBER_FLAGS_FLOAT) {
                decl->expression = xx make_float_literal(decl->my_type, 0.0);
            } else {
                decl->expression = xx make_integer_literal(decl->my_type, 0);
            }
        } else if (decl->my_type == w->type_def_bool) {
            decl->expression = xx make_integer_literal(decl->my_type, 0);
        } else if (decl->my_type == w->type_def_string) {
            decl->expression = xx make_string_literal(w, sv_from_parts(NULL, 0));
        } else if (decl->my_type->pointer_to) {
            decl->expression = xx make_integer_literal(decl->my_type, 0);
        } else {
            assert(0 && "Default values for complex types are not implemented yet.");
        }
        
        decl->base.inferred_type = decl->my_type;
        return;
    }

    if (!decl->expression) {
        report_error(&decl->base, "Can't have a declaration without a type or a value.");
    }

    // Do constant replacement.
    Ast *expr = decl->expression;
    while (expr->replacement) expr = expr->replacement;
    
    decl->expression = expr;
    decl->base.inferred_type = expr->inferred_type;
}

void typecheck_cast(Workspace *w, Ast_Cast *cast)
{
    UNUSED(w);
    cast->base.inferred_type = cast->type;
}

void typecheck_ast(Workspace *w, Ast *ast)
{
    while (ast->replacement) ast = ast->replacement;
    if (ast->inferred_type) return;
    switch (ast->type) {
    case AST_UNINITIALIZED:      assert(0);
    case AST_BLOCK:              assert(0);
    case AST_LITERAL:            typecheck_literal(w, xx ast);         break;
    case AST_IDENT:              typecheck_identifier(w, xx ast);      break;
    case AST_UNARY_OPERATOR:     typecheck_unary_operator(w, xx ast);  break;
    case AST_BINARY_OPERATOR:    typecheck_binary_operator(w, xx ast); break;
    case AST_LAMBDA:             typecheck_lambda(w, xx ast);          break;
    case AST_PROCEDURE_CALL:     typecheck_procedure_call(w, xx ast);  break;
    case AST_WHILE:              typecheck_while(w, xx ast);           break;
    case AST_IF:                 typecheck_if(w, xx ast);              break;
    case AST_LOOP_CONTROL:       assert(0);
    case AST_RETURN:             typecheck_return(w, xx ast);          break;
    case AST_TYPE_DEFINITION:    typecheck_definition(w, xx ast);      break;
    case AST_TYPE_INSTANTIATION: typecheck_instantiation(w, xx ast);   break;
    case AST_ENUM:               typecheck_enum(w, xx ast);            break;
    case AST_STRUCT:             typecheck_struct(w, xx ast);          break;
    case AST_USING:              typecheck_using(w, xx ast);           break;
    case AST_DECLARATION:        typecheck_declaration(w, xx ast);     break;
    case AST_CAST:               typecheck_cast(w, xx ast);            break;
    }
}

void flatten_for_typechecking(Workspace *w, Ast *ast)
{
    if (ast == NULL) return;
    switch (ast->type) {
    case AST_BLOCK: {
        Ast_Block *block = xx ast;
        For (block->statements) flatten_for_typechecking(w, block->statements[it]);
        For (block->declarations) flatten_for_typechecking(w, xx block->declarations[it]);
        return; // So we don't get added.
    }
    case AST_LITERAL:
        return; // No need to typecheck.
    case AST_IDENT:
        break;
    case AST_UNARY_OPERATOR: {
        Ast_Unary_Operator *unary = xx ast;
        flatten_for_typechecking(w, unary->subexpression);
        break;
    }
    case AST_BINARY_OPERATOR: {
        Ast_Binary_Operator *binary = xx ast;
        flatten_for_typechecking(w, binary->left);
        flatten_for_typechecking(w, binary->right);
        break;
    }
    case AST_LAMBDA: {
        Ast_Lambda *lambda = xx ast;
        flatten_for_typechecking(w, xx lambda->type_definition);
        flatten_for_typechecking(w, xx lambda->block);
        break;
    }
    case AST_PROCEDURE_CALL: {
        Ast_Procedure_Call *call = xx ast;
        flatten_for_typechecking(w, call->procedure_expression);
        For (call->arguments) flatten_for_typechecking(w, call->arguments[it]);
        break;
    }
    case AST_WHILE: {
        Ast_While *while_stmt = xx ast;
        flatten_for_typechecking(w, while_stmt->condition_expression);
        flatten_for_typechecking(w, while_stmt->then_statement);
        break;
    }
    case AST_IF: {
        Ast_If *if_stmt = xx ast;
        flatten_for_typechecking(w, if_stmt->condition_expression);
        flatten_for_typechecking(w, if_stmt->then_statement);
        if (if_stmt->else_statement) flatten_for_typechecking(w, if_stmt->else_statement);
        break;
    }
    case AST_LOOP_CONTROL:
        break;
    case AST_RETURN: {
        Ast_Return *ret = xx ast;
        flatten_for_typechecking(w, ret->subexpression);
        break;
    }
    case AST_TYPE_DEFINITION: {
        Ast_Type_Definition *defn = xx ast;
        if (defn->struct_desc)        flatten_for_typechecking(w, xx defn->struct_desc);
        if (defn->enum_defn)          flatten_for_typechecking(w, xx defn->enum_defn);
        if (defn->type_name)          flatten_for_typechecking(w, xx defn->type_name);
        if (defn->struct_call)        flatten_for_typechecking(w, xx defn->struct_call);
        if (defn->array_element_type) flatten_for_typechecking(w, xx defn->array_element_type);
        if (defn->pointer_to)         flatten_for_typechecking(w, xx defn->pointer_to);
        if (defn->lambda_return_type) {
            flatten_for_typechecking(w, xx defn->lambda_return_type);
            For (defn->lambda_argument_types) flatten_for_typechecking(w, xx defn->lambda_argument_types[it]);
        }
        return; // Don't add ourselves.
    }
    case AST_TYPE_INSTANTIATION: {
        Ast_Type_Instantiation *inst = xx ast;
        flatten_for_typechecking(w, xx inst->type_definition);
        flatten_for_typechecking(w, inst->initializer_expression);
        break;
    }
    case AST_ENUM: {
        Ast_Enum *enum_defn = xx ast;
        // flatten_for_typechecking(array, xx enum_defn->underlying_int_type);
        flatten_for_typechecking(w, xx enum_defn->block);
        break;
    }
    case AST_STRUCT: {
        Ast_Struct *struct_desc = xx ast;
        flatten_for_typechecking(w, xx struct_desc->block);
        break;
    }
    case AST_DECLARATION: {
        Ast_Declaration *decl = xx ast;
        arrput(w->declarations, decl);
        flatten_for_typechecking(w, xx decl->my_type);
        flatten_for_typechecking(w, xx decl->expression);
        break;
    }
    case AST_USING: {
        Ast_Using *using = xx ast;
        flatten_for_typechecking(w, using->subexpression);
        break;
    }
    default: UNREACHABLE;
    }
    arrput(w->typecheck_queue, ast);
}

void report_error(Ast *ast, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    fprintf(stderr, Loc_Fmt ": Error: ", SV_Arg(ast->file_name), Loc_Arg(ast->location));
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);
    exit(1);
}
