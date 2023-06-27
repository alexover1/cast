#include <stdarg.h>
#include <math.h>

#include "typecheck.h"
#include "workspace.h"

#define Replace(ast) do { \
    while (ast->replacement) ast = ast->replacement; \
} while(0)

#define Replace_Type(type) do { \
    while (type->_expression.replacement) type = xx type->_expression.replacement; \
} while(0)

// Be sure to call Replace() on this first to replace constant identifiers.
static bool expression_is_lvalue(Ast_Expression *expr)
{
    if (expr->kind == AST_IDENT) {
        Ast_Ident *ident = xx expr;
        return !(ident->resolved_declaration->flags & DECLARATION_IS_CONSTANT);
    }
    if (expr->kind == AST_SELECTOR) {
        Ast_Selector *selector = xx expr;
        return expression_is_lvalue(selector->namespace_expression);
    }
    if (expr->kind == AST_UNARY_OPERATOR) {
        Ast_Unary_Operator *unary = xx expr;
        if (unary->operator_type != TOKEN_POINTER_DEREFERENCE) return false;
        return expression_is_lvalue(unary->subexpression);
    }
    if (expr->kind == AST_BINARY_OPERATOR) {
        Ast_Binary_Operator *binary = xx expr;
        if (binary->operator_type != TOKEN_ARRAY_SUBSCRIPT) return false; // What about pointer arithmetic?
        return expression_is_lvalue(binary->left);
    }
    return false;
}

static Ast_Type_Definition *make_pointer_type(Ast_Type_Definition *element_type)
{
    Ast_Type_Definition *type = context_alloc(sizeof(*type));
    type->_expression.kind = AST_TYPE_DEFINITION;
    type->_expression.location = element_type->_expression.location;
    type->_expression.inferred_type = element_type->_expression.inferred_type; // This is a sneaky trick so we don't have to pass the Workspace.
    type->kind = TYPE_DEF_POINTER;
    type->pointer_to = element_type;
    return type;
}

void typecheck_declaration(Workspace *w, Ast_Declaration *decl)
{
#if 0
    String_Builder sb = {0};
    sb_append_cstr(&sb, ">>> ");
    print_decl_to_builder(&sb, decl);
    sb_append_cstr(&sb, "\n");
    printf(SV_Fmt, SV_Arg(sb));
#endif
    
    // Note: None of this gets set for non-constants, which is totally fine.
    while (decl->typechecking_position < arrlenu(decl->flattened)) {
        Ast_Node *node = &decl->flattened[decl->typechecking_position];
        if (node->expression) {
            typecheck_expression(w, node->expression);
            Replace(node->expression);
            if (node->expression->inferred_type) {
                decl->typechecking_position += 1;
            } else {
                // Hit a roadblock.
#if 0
                Source_Location loc = node->expression->location;
                String_View name = decl->ident ? decl->ident->name : sv_from_cstr("<unnamed>");
                printf(Loc_Fmt": WAITING: Declaration '"SV_Fmt"' is waiting on: %s\n",
                    SV_Arg(w->files[loc.fid].path),
                    Loc_Arg(loc),
                    SV_Arg(name),
                    expr_to_string(node->expression));
#endif
                return;
            }
        }
        if (node->statement) {
            typecheck_statement(w, node->statement);
            if (node->statement->typechecked) {
                decl->typechecking_position += 1;
            } else {
                // Currently this can never happen because we can never wait on statements.
                // Their inner expressions are typechecked before they are.
                printf("$$$ %s\n", stmt_to_string(node->statement));
                return;
            }
        }
    }

    // We are done typechecking our members.

    if (decl->flags & DECLARATION_IS_PROCEDURE_BODY) goto done;

    if (decl->flags & DECLARATION_IS_CONSTANT) {
        if (!decl->root_expression) {
            report_error(w, decl->location, "Constant declarations must have a value (this is an internal error).");
        }
    }

    if (!decl->my_type) {
        if (!decl->root_expression) {
            report_error(w, decl->location, "We have a non-constant declaration with no type and no value (this is an internal error because this shouldn't have parsed correctly).");
        }

        Replace(decl->root_expression);
        assert(decl->root_expression->inferred_type);

        if (decl->root_expression->kind == AST_NUMBER) {
            ((Ast_Number *)decl->root_expression)->inferred_type_is_final = true;
        }

        decl->my_type = decl->root_expression->inferred_type;
        decl->flags |= DECLARATION_TYPE_WAS_INFERRED_FROM_EXPRESSION;
        goto done;
    }

    // So we know we have a type now.
    Replace_Type(decl->my_type);

    if (decl->my_type == w->type_def_void) {
        report_error(w, decl->location, "Cannot have a declaration with void type.");
    }

    if (!decl->root_expression) {
#if 0
        printf("Default value for: %s\n", type_to_string(decl->my_type));
#endif
        // We're definitely not a constant (this error is checked above).
        // So we just set the default value for the type.
        decl->root_expression = generate_default_value_for_type(w, decl->my_type);
        decl->root_expression->location = decl->location;
        decl->root_expression->inferred_type = decl->my_type;
        decl->flags |= DECLARATION_VALUE_WAS_INFERRED_FROM_TYPE;
        goto done;       
    }

    Replace(decl->root_expression);

    if (decl->root_expression->kind == AST_NUMBER) {
        if (decl->flags & DECLARATION_IS_ENUM_VALUE) {
            assert(decl->my_type->kind == TYPE_DEF_ENUM); // Because it was set in parse_enum_defn().
            typecheck_number(w, xx decl->root_expression, decl->my_type->enum_defn->underlying_int_type);
        } else {
            typecheck_number(w, xx decl->root_expression, decl->my_type);
        }
    } else {
        // Check that we're not doing something like `x: bool = 12345`.
        if (!types_are_equal(decl->my_type, decl->root_expression->inferred_type)) {
            report_error(w, decl->root_expression->location, "Type mismatch: Wanted %s but got %s.",
                type_to_string(decl->my_type), type_to_string(decl->root_expression->inferred_type));
        }
    }

done:
    decl->flags |= DECLARATION_HAS_BEEN_TYPECHECKED;
}

Ast_Expression *generate_default_value_for_type(Workspace *w, Ast_Type_Definition *type)
{
    switch (type->kind) {
    case TYPE_DEF_NUMBER:
        return xx make_number(0);
    case TYPE_DEF_LITERAL:
        return xx make_literal(type->literal);
    case TYPE_DEF_STRUCT: {
        Ast_Type_Instantiation *inst = context_alloc(sizeof(*inst));
        inst->_expression.kind = AST_TYPE_INSTANTIATION;
        inst->type_definition = type;
        For (type->struct_desc->block->declarations) {
            Ast_Declaration *field = type->struct_desc->block->declarations[it];
            if (!(field->flags & DECLARATION_IS_STRUCT_FIELD)) continue;
            assert(field->flags & DECLARATION_HAS_BEEN_TYPECHECKED);
            arrput(inst->arguments, field->root_expression);
        }           
        return xx inst;
    }
    case TYPE_DEF_ENUM:
        UNIMPLEMENTED;
    case TYPE_DEF_IDENT:
        assert(0);
        return generate_default_value_for_type(w, xx type->type_name->resolved_declaration->root_expression);
    case TYPE_DEF_STRUCT_CALL:
        UNIMPLEMENTED;
    case TYPE_DEF_POINTER:
        return xx make_literal(LITERAL_NULL);
    case TYPE_DEF_ARRAY: {
        // Fill in default values.
        Ast_Type_Instantiation *inst = context_alloc(sizeof(*inst));
        inst->_expression.kind = AST_TYPE_INSTANTIATION;
        inst->type_definition = type;
        return xx inst;
    }
    case TYPE_DEF_LAMBDA:
        return xx make_literal(LITERAL_NULL);
    }
}

Ast_Expression *autocast_to_bool(Workspace *w, Ast_Expression *expr)
{
    Ast_Type_Definition *defn = expr->inferred_type;
    Replace_Type(defn);
    switch (defn->kind) {
    case TYPE_DEF_NUMBER: {
        // TODO: This might be able to be checked at compile-time if the expression is constant.
        Ast_Binary_Operator *binary = context_alloc(sizeof(*binary));
        binary->_expression.kind = AST_BINARY_OPERATOR;
        binary->_expression.location = expr->location;
        binary->_expression.inferred_type = w->type_def_bool;
        binary->left = expr;
        binary->operator_type = TOKEN_ISNOTEQUAL;
        binary->right = xx make_number(0);
        return xx binary;
    }
    case TYPE_DEF_LITERAL:
        // TODO: These are both considered literals. Define "literal" more seriously.
        if (defn == w->type_def_void) return NULL;
        if (defn == w->type_def_type) return NULL;

        switch (defn->literal) {
        case LITERAL_STRING: {
            Ast_Selector *selector = context_alloc(sizeof(*selector));
            selector->_expression.kind = AST_SELECTOR;
            selector->_expression.location = expr->location;
            selector->_expression.inferred_type = w->type_def_int; // @Volatile: This assumes string.count is an int.
            selector->namespace_expression = expr;
            selector->ident = context_alloc(sizeof(*selector->ident));
            selector->ident->_expression.kind = AST_IDENT;
            selector->ident->_expression.location = expr->location;
            selector->ident->_expression.inferred_type = w->type_def_int;
            selector->ident->name = sv_from_cstr("count");
            selector->ident->enclosing_block = NULL;
            selector->struct_field_index = 1; // @Volatile: This assume string.count is the second field.
            return xx selector;
        }
        case LITERAL_BOOL: return expr;
        case LITERAL_NULL: {
            Ast_Literal *literal = make_literal(LITERAL_BOOL);
            literal->_expression.location = expr->location;
            literal->_expression.inferred_type = defn;
            literal->bool_value = false;
            return xx literal;
        }
        }
    case TYPE_DEF_STRUCT:
    case TYPE_DEF_ENUM:
        return NULL;
    case TYPE_DEF_IDENT:
        UNREACHABLE; // Because we called Replace_Type.
    case TYPE_DEF_STRUCT_CALL:
        return NULL;
    case TYPE_DEF_POINTER: {
        Ast_Literal *literal = make_literal(LITERAL_NULL);
        literal->_expression.location = expr->location;
        literal->_expression.inferred_type = defn;
        
        Ast_Binary_Operator *binary = context_alloc(sizeof(*binary));
        binary->_expression.kind = AST_BINARY_OPERATOR;
        binary->_expression.location = expr->location;
        binary->_expression.inferred_type = w->type_def_bool;
        binary->left = expr;
        binary->operator_type = TOKEN_ISNOTEQUAL;
        binary->right = xx literal;
        return xx binary;
    }
    case TYPE_DEF_ARRAY: {
        if (defn->array.kind != ARRAY_KIND_FIXED) {
            Ast_Selector *selector = context_alloc(sizeof(*selector));
            selector->_expression.kind = AST_SELECTOR;
            selector->_expression.location = expr->location;
            selector->_expression.inferred_type = w->type_def_int; // @Volatile: This assumes arary.count is an int.
            selector->namespace_expression = expr;
            selector->ident = context_alloc(sizeof(*selector->ident));
            selector->ident->_expression.kind = AST_IDENT;
            selector->ident->_expression.location = expr->location;
            selector->ident->_expression.inferred_type = w->type_def_int;
            selector->ident->name = sv_from_cstr("count");
            selector->ident->enclosing_block = NULL;
            selector->struct_field_index = 1; // @Volatile: This assumes array.count is the second field.

            Ast_Binary_Operator *binary = context_alloc(sizeof(*binary));
            binary->_expression.kind = AST_BINARY_OPERATOR;
            binary->_expression.location = expr->location;
            binary->_expression.inferred_type = w->type_def_bool;
            binary->left = xx selector;
            binary->operator_type = TOKEN_ISNOTEQUAL;
            binary->right = xx make_number(0);
            return xx binary;
        }

        Ast_Literal *literal = make_literal(LITERAL_BOOL);
        literal->_expression.location = expr->location;
        literal->_expression.inferred_type = w->type_def_bool;
        literal->bool_value = defn->array.length != 0;
        return xx literal;
    }
    case TYPE_DEF_LAMBDA:
        return NULL;
    }
}

void typecheck_number(Workspace *w, Ast_Number *number, Ast_Type_Definition *supplied_type)
{
    if (!supplied_type) {
        // Ast_Type_Definition *defn = context_alloc(sizeof(*defn));
        // defn->_expression.kind = AST_TYPE_DEFINITION;
        // defn->_expression.location = number->_expression.location;
        // defn->_expression.inferred_type = w->type_def_type;
        // defn->name = "<any number>";
        // defn->kind = TYPE_DEF_ANY;

        if (number->flags & NUMBER_FLAGS_FLOAT64)    number->_expression.inferred_type = w->type_def_float64;
        else if (number->flags & NUMBER_FLAGS_FLOAT) number->_expression.inferred_type = w->type_def_float;
        else                                         number->_expression.inferred_type = w->type_def_int;
        return;
    }

    if (supplied_type->kind != TYPE_DEF_NUMBER) {
        report_error(w, number->_expression.location, "Type mismatch: Wanted %s but got a number literal.", type_to_string(supplied_type));
    }

    if (number->flags & NUMBER_FLAGS_FLOAT) {
        if (!(supplied_type->number.flags & NUMBER_FLAGS_FLOAT)) {
            report_error(w, number->_expression.location, "Cannot use float literal as type %s.", type_to_string(supplied_type));
        }
        if (number->flags & NUMBER_FLAGS_FLOAT64 && !(supplied_type->number.flags & NUMBER_FLAGS_FLOAT64)) {
            report_error(w, number->_expression.location, "Precision loss when casting to %s.", type_to_string(supplied_type));
        }
        goto done;
    }

    // Number literal with no fraction can be int or float.

    if (supplied_type->number.flags & NUMBER_FLAGS_FLOAT) {
        goto done;
    }

    // We know the type is an integer, so let's compare the range.

    assert(supplied_type->name); // If we are trying to instantiate a numeric type with a number literal, the type must be compiler-defined.

    if (supplied_type->number.flags & NUMBER_FLAGS_SIGNED) {
        signed long low = supplied_type->number.literal_low->as.integer;
        signed long high = supplied_type->number.literal_high->as.integer;
        signed long value = number->as.integer;
        if (value > high) {
            report_error(w, number->_expression.location, "Numeric constant too big for type (max for %s is %lu).", supplied_type->name, high);
        }
        if (value < low) {
            report_error(w, number->_expression.location, "Numeric constant too small for type (min for %s is %lu).", supplied_type->name, low);
        }
        goto done;
    }
    
    unsigned long low = supplied_type->number.literal_low->as.integer;
    unsigned long high = supplied_type->number.literal_high->as.integer;
    
    if (number->as.integer > high) {
        report_error(w, number->_expression.location, "Numeric constant too big for type (max for %s is %lu).", supplied_type->name, high);
    }
    if (number->as.integer < low) {
        report_error(w, number->_expression.location, "Numeric constant too small for type (min for %s is %lu).", supplied_type->name, low);
    }
    
done:
    number->_expression.inferred_type = supplied_type;
}

void typecheck_literal(Workspace *w, Ast_Literal *literal)
{
    switch (literal->kind) {
    case LITERAL_BOOL:   literal->_expression.inferred_type = w->type_def_bool;   break;
    case LITERAL_STRING: literal->_expression.inferred_type = w->type_def_string; break;
    case LITERAL_NULL:   literal->_expression.inferred_type = w->type_def_void;   break;
    }
}

void typecheck_identifier(Workspace *w, Ast_Ident *ident)
{
    // TODO: We should have a separate phase where we check for circular dependencies and unresolved identifiers.
    if (!ident->resolved_declaration) {
        ident->resolved_declaration = find_declaration_from_identifier(ident);
        if (!ident->resolved_declaration) {
            report_error(w, ident->_expression.location, "Undeclared identifier '"SV_Fmt"'.", SV_Arg(ident->name));
        }

        For (ident->resolved_declaration->flattened) {
            if (ident->resolved_declaration->flattened == xx ident) {
                report_error(w, ident->_expression.location, "Circular depedency detected: '"SV_Fmt"'.", SV_Arg(ident->name));
            }
        }
    }

    Ast_Declaration *decl = ident->resolved_declaration;

    // This is where we might hit a roadblock.

    if (!(decl->flags & DECLARATION_HAS_BEEN_TYPECHECKED)) {
        if (!(decl->flags & DECLARATION_IS_CONSTANT)) {
            report_error(w, ident->_expression.location, "Cannot use variable '"SV_Fmt"' before it is defined.", SV_Arg(ident->name));
        }
        // Otherwise we must wait for the constant to come in.
        return;
    }

    // If the declaration has been typechecked, we can typecheck ourselves. 

    assert(decl->my_type);

    if (decl->flags & DECLARATION_IS_CONSTANT) {
        // TODO: does this work for lambdas?
        ident->_expression.replacement = decl->root_expression;
        // TODO: Because we replace the expression, the debug location information gets messed up.
    }

    ident->_expression.inferred_type = decl->my_type;

    UNUSED(w);
}

void typecheck_unary_operator(Workspace *w, Ast_Unary_Operator *unary)
{
    Replace(unary->subexpression);

    switch (unary->operator_type) {
    case '*':
        if (!expression_is_lvalue(unary->subexpression)) {
            report_error(w, unary->_expression.location, "Can only take a pointer to an lvalue."); // TODO: This error mesage.
        }
        unary->_expression.inferred_type = make_pointer_type(unary->subexpression->inferred_type);
        break;
    case '!': {   
        Ast_Expression *expr = autocast_to_bool(w, unary->subexpression);
        if (expr) {
            unary->subexpression = expr;
        } else {
            report_error(w, unary->subexpression->location, "Type mismatch: Wanted bool but got %s.",
                type_to_string(unary->subexpression->inferred_type));
        }
        unary->_expression.inferred_type = w->type_def_bool;
        break;
    }
    case TOKEN_POINTER_DEREFERENCE:
        if (unary->subexpression->inferred_type->kind != TYPE_DEF_POINTER) {
            report_error(w, unary->_expression.location, "Attempt to dereference a non-pointer (got type %s).",
                type_to_string(unary->subexpression->inferred_type));
        }
        unary->_expression.inferred_type = unary->subexpression->inferred_type->pointer_to;
        break;
    default:
        UNIMPLEMENTED;
    }   
}

inline Ast_Literal *make_literal(Literal_Kind kind)
{
    Ast_Literal *lit = context_alloc(sizeof(*lit));
    lit->_expression.kind = AST_LITERAL;
    lit->kind = kind;
    return lit;
}

inline Ast_Literal *make_boolean(Workspace *w, Source_Location loc, bool value)
{
    Ast_Literal *lit = make_literal(LITERAL_BOOL);
    lit->bool_value = value;
    lit->_expression.location = loc;
    lit->_expression.inferred_type = w->type_def_bool;
    return lit;
}

inline Ast_Number *make_float_or_float64(Workspace *w, Source_Location loc, double value, bool use_float64)
{
    Ast_Number *res = make_number_float(value);
    res->_expression.location = loc;
    if (use_float64) {
        res->flags |= NUMBER_FLAGS_FLOAT64;
        res->_expression.inferred_type = w->type_def_float64;
    } else {
        res->_expression.inferred_type = w->type_def_float;
    }
    return res;
}

inline Ast_Number *make_integer(Workspace *w, Source_Location loc, unsigned long value, bool is_signed)
{
    Ast_Number *res = make_number(value);
    res->_expression.location = loc;
    res->_expression.inferred_type = w->type_def_int;
    if (is_signed) res->flags |= NUMBER_FLAGS_SIGNED;
    return res;
}

Ast_Expression *constant_arithmetic_or_comparison(Workspace *w, Ast_Binary_Operator *binary)
{
    assert(binary->left->kind == AST_NUMBER);
    assert(binary->right->kind == AST_NUMBER);
    Ast_Number *left = xx binary->left;
    Ast_Number *right = xx binary->right;
    
    Source_Location loc = left->_expression.location;

    // If either is float, we are float.
    if ((left->flags & NUMBER_FLAGS_FLOAT) || (right->flags & NUMBER_FLAGS_FLOAT)) {
        bool use_float64 = (left->flags & NUMBER_FLAGS_FLOAT64) || (right->flags & NUMBER_FLAGS_FLOAT64);
        switch (binary->operator_type) {
        case '+':                 return xx make_float_or_float64(w, loc, left->as.real + right->as.real, use_float64);
        case '-':                 return xx make_float_or_float64(w, loc, left->as.real - right->as.real, use_float64);
        case '*':                 return xx make_float_or_float64(w, loc, left->as.real * right->as.real, use_float64);
        case '/':                 return xx make_float_or_float64(w, loc, left->as.real / right->as.real, use_float64);
        case '%':                 return xx make_float_or_float64(w, loc, fmod(left->as.real, right->as.real), use_float64);
        case '>':                 return xx make_boolean(w, loc, left->as.real > right->as.real);
        case '<':                 return xx make_boolean(w, loc, left->as.real < right->as.real);
        case TOKEN_GREATEREQUALS: return xx make_boolean(w, loc, left->as.real >= right->as.real);
        case TOKEN_LESSEQUALS:    return xx make_boolean(w, loc, left->as.real <= right->as.real);
        default:  assert(0);
        }
    }

    // If either is signed, we are signed.
    if ((left->flags & NUMBER_FLAGS_SIGNED) || (right->flags & NUMBER_FLAGS_SIGNED)) {
        signed long l = left->as.integer;
        signed long r = right->as.integer;
        switch (binary->operator_type) {
        case '+':                 return xx make_integer(w, loc, l + r, true);
        case '-':                 return xx make_integer(w, loc, l - r, true);
        case '*':                 return xx make_integer(w, loc, l * r, true);
        case '/':                 return xx make_integer(w, loc, l / r, true);
        case '%':                 return xx make_integer(w, loc, l % r, true);
        case '>':                 return xx make_boolean(w, loc, l >  r);
        case '<':                 return xx make_boolean(w, loc, l <  r);
        case TOKEN_GREATEREQUALS: return xx make_boolean(w, loc, l >= r);
        case TOKEN_LESSEQUALS:    return xx make_boolean(w, loc, l <= r);
        default:  assert(0);
        }
    }
    
    unsigned long l = left->as.integer;
    unsigned long r = right->as.integer;
    switch (binary->operator_type) {
    case '+':                 return xx make_integer(w, loc, l +  r, false);
    case '-':                 return xx make_integer(w, loc, l -  r, false);
    case '*':                 return xx make_integer(w, loc, l *  r, false);
    case '/':                 return xx make_integer(w, loc, l /  r, false);
    case '%':                 return xx make_integer(w, loc, l %  r, false);
    case '>':                 return xx make_boolean(w, loc, l >  r);
    case '<':                 return xx make_boolean(w, loc, l <  r);
    case TOKEN_GREATEREQUALS: return xx make_boolean(w, loc, l >= r);
    case TOKEN_LESSEQUALS:    return xx make_boolean(w, loc, l <= r);
    default:  assert(0);
    }
}

// Checks that the types of the binary operator match and are integers.
Ast_Type_Definition *typecheck_binary_int_operator(Workspace *w, Ast_Binary_Operator *binary)
{
    // Currently, the left hand side is the determinant.
    Ast_Type_Definition *defn = binary->left->inferred_type;

    if (defn->kind != TYPE_DEF_NUMBER) {
        report_error(w, binary->_expression.location, "Type mismatch: Operator '%s' does not work on non-number types (got %s).",
            token_type_to_string(binary->operator_type), type_to_string(defn));
    }

    if (defn->number.flags & NUMBER_FLAGS_FLOAT) {
        report_error(w, binary->_expression.location, "Type mismatch: Operator '%s' does not work on floating-point types (got %s).",
            token_type_to_string(binary->operator_type), type_to_string(defn));
    }

    if (!check_that_types_match(w, binary->right, binary->left->inferred_type)) {
        report_error(w, binary->_expression.location, "Type mismatch: Types on either side of '%s' must be the same (got %s and %s).",
            token_type_to_string(binary->operator_type), type_to_string(binary->left->inferred_type), type_to_string(binary->right->inferred_type));
    }

    return defn;
}

Ast_Type_Definition *typecheck_binary_arithmetic(Workspace *w, Ast_Binary_Operator *binary)
{
    // Check for pointer arithmetic.
    if (binary->left->inferred_type->kind == TYPE_DEF_POINTER) {
        if (binary->operator_type != '+' && binary->operator_type != '-') {
            report_error(w, binary->_expression.location, "Type mismatch: Pointer arithmetic is only supported by the '+' or '-' operators.");
        }
        if (binary->right->inferred_type->kind == TYPE_DEF_POINTER) {
            // Pointer and pointer.
            if (!types_are_equal(binary->left->inferred_type->pointer_to, binary->right->inferred_type->pointer_to)) {
                report_error(w, binary->_expression.location, "Type mismatch: Cannot perform pointer arithmetic on points of different types (got %s and %s).",
                    type_to_string(binary->left->inferred_type), type_to_string(binary->right->inferred_type));
            }
        } else {
            // Pointer and integer.
            if (binary->right->inferred_type->kind != TYPE_DEF_NUMBER && (binary->right->inferred_type->number.flags & NUMBER_FLAGS_FLOAT)) {
                report_error(w, binary->right->location, "Type mismatch: Pointer arithmetic operand must be a number (got %s).",
                    type_to_string(binary->right->inferred_type));
            }
        }
        return binary->left->inferred_type;
    }

    // The types must be equal, and they also must be numbers.

    Ast_Type_Definition *defn = binary->left->inferred_type;

    if (defn->kind != TYPE_DEF_NUMBER) {
        report_error(w, binary->_expression.location, "Type mismatch: Operator '%s' does not work on non-number types (got %s).",
            token_type_to_string(binary->operator_type), type_to_string(defn));
    }

    if (!check_that_types_match(w, binary->right, defn)) {
        report_error(w, binary->_expression.location,
            "Type mismatch: Types on either side of '%s' must be the same (got %s and %s).",
            token_type_to_string(binary->operator_type),
            type_to_string(binary->left->inferred_type),
            type_to_string(binary->right->inferred_type));
    }

    return defn;
}

void typecheck_binary_comparison(Workspace *w, Ast_Binary_Operator *binary)
{
    if (!check_that_types_match(w, binary->right, binary->left->inferred_type)) {
        report_error(w, binary->_expression.location,
            "Type mismatch: Types on either side of '%s' must be the same (got %s and %s).",
            token_type_to_string(binary->operator_type),
            type_to_string(binary->left->inferred_type),
            type_to_string(binary->right->inferred_type));
    }

    Ast_Type_Definition *defn = binary->left->inferred_type; // Now they are the same.

    if (defn->kind == TYPE_DEF_POINTER) return;

    if (defn->kind != TYPE_DEF_NUMBER) {
        report_error(w, binary->_expression.location, "Type mismatch: Operator '%s' does not work on non-number types (got %s).",
            token_type_to_string(binary->operator_type), type_to_string(defn));
    }
}

// @Cleanup: This whole function's error messages.
void typecheck_binary_operator(Workspace *w, Ast_Binary_Operator *binary)
{
    Replace(binary->left);
    Replace(binary->right);

    switch (binary->operator_type) {
    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
        // If left and right are literals, replace us with a literal.
        // Technically, LLVM does this for us, but let's not rely on that.
        if (binary->left->kind == AST_NUMBER && binary->right->kind == AST_NUMBER) {
            binary->_expression.replacement = constant_arithmetic_or_comparison(w, binary);
            binary->_expression.replacement->inferred_type = binary->left->inferred_type; // TODO: check this.
            break;
        }
        binary->_expression.inferred_type = typecheck_binary_arithmetic(w, binary);
        break;
        
    case TOKEN_ISEQUAL:
    case TOKEN_ISNOTEQUAL:
        if (binary->left->kind == AST_NUMBER && binary->right->kind == AST_NUMBER) {
            binary->_expression.replacement = constant_arithmetic_or_comparison(w, binary);
            break;
        }
        if (!check_that_types_match(w, binary->right, binary->left->inferred_type)) {
            report_error(w, binary->_expression.location, "Type mismatch: Cannot compare values of different types (got %s and %s).",
                type_to_string(binary->left->inferred_type), type_to_string(binary->right->inferred_type));
        }
        binary->_expression.inferred_type = w->type_def_bool;
        // TODO: constant_comparison
        break;
        
    case '>':
    case '<':
    case TOKEN_GREATEREQUALS:
    case TOKEN_LESSEQUALS:
        // If left and right are literals, replace us with a literal.
        // Technically, LLVM does this for us, but let's not rely on that.
        if (binary->left->kind == AST_NUMBER && binary->right->kind == AST_NUMBER) {
            binary->_expression.replacement = constant_arithmetic_or_comparison(w, binary);
            break;
        }
        typecheck_binary_comparison(w, binary);
        binary->_expression.inferred_type = w->type_def_bool;
        break;
        
    case TOKEN_LOGICAL_AND:
    case TOKEN_LOGICAL_OR: {
        // TODO: check for constant.
            
        Ast_Expression *left = autocast_to_bool(w, binary->left);
        if (!left) {
            report_error(w, binary->left->location, "Type mismatch: Operator '%s' only works on boolean types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(binary->left->inferred_type));
        }
            
        Ast_Expression *right = autocast_to_bool(w, binary->right);
        if (!right) {
            report_error(w, binary->right->location, "Type mismatch: Operator '%s' only works on boolean types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(binary->right->inferred_type));
        }
            
        binary->left = left;
        binary->right = right;
        binary->_expression.inferred_type = w->type_def_bool;
        break;
    }
        
    case TOKEN_SHIFT_LEFT:
    case TOKEN_SHIFT_RIGHT: {
        binary->_expression.inferred_type = typecheck_binary_int_operator(w, binary);

        // Check for constants.
        if (binary->left->kind == AST_NUMBER && binary->right->kind == AST_NUMBER) {
            unsigned long left, right, result;
            left  = ((Ast_Number *)binary->left)->as.integer;
            right = ((Ast_Number *)binary->right)->as.integer;
            if (binary->operator_type == TOKEN_SHIFT_LEFT) {
                result = left << right;
            } else {
                result = left >> right;
            }
            Ast_Number *number = make_number(result);
            number->_expression.inferred_type = w->type_def_int; // Because this can be changed at any point.
            number->_expression.location = binary->_expression.location;

            binary->_expression.replacement = xx number;
        }
        break;
    }

    case TOKEN_BITWISE_AND:
    case TOKEN_BITWISE_OR: {
        // TODO: Constants.
        binary->_expression.inferred_type = typecheck_binary_int_operator(w, binary);
        break;
    }
        
    case TOKEN_ARRAY_SUBSCRIPT:
        if (binary->left->inferred_type->kind != TYPE_DEF_ARRAY) {
            report_error(w, binary->left->location, "Type mismatch: Wanted an array but got %s.",
                type_to_string(binary->left->inferred_type));
        }
        if (binary->right->inferred_type->kind != TYPE_DEF_NUMBER && (binary->right->inferred_type->number.flags & NUMBER_FLAGS_FLOAT)) {
            report_error(w, binary->left->location, "Type mismatch: Array subscript must be an integer (got %s).",
                type_to_string(binary->right->inferred_type));
        }
        binary->_expression.inferred_type = binary->left->inferred_type->array.element_type;
        break;

    case TOKEN_DOUBLE_DOT:
        // So we must be inside of a range-based for loop.
        binary->_expression.inferred_type = typecheck_binary_int_operator(w, binary);
        break;
        
    default:
        printf(">>> %s\n", token_type_to_string(binary->operator_type));
        UNIMPLEMENTED;
    }
}

void typecheck_lambda(Workspace *w, Ast_Lambda *lambda)
{
    lambda->_expression.inferred_type = lambda->type_definition;
    UNUSED(w);
    // UNUSED(lambda);
    // UNIMPLEMENTED;
}

void typecheck_procedure_call(Workspace *w, Ast_Procedure_Call *call)
{
    if (call->procedure_expression->inferred_type->kind != TYPE_DEF_LAMBDA) {
        report_error(w, call->procedure_expression->location, "Type mismatch: Wanted a procedure but got %s.",
            type_to_string(call->procedure_expression->inferred_type));
    }

    Ast_Type_Definition *proc = call->procedure_expression->inferred_type;

    size_t n = arrlenu(call->arguments);
    size_t m = arrlenu(proc->lambda.argument_types);
    
    if (n < m) report_error(w, call->_expression.location, "Not enough arguments for procedure call (wanted %zu but got %zu).", m, n);

    if (n > m && !proc->lambda.variadic) {
        report_error(w, call->_expression.location, "Too many arguments for procedure call (wanted %zu but got %zu).", m, n);
    }

    // Note: We iterate up to m here because if we are variadic, there may be more arguments passed than the function takes.
    for (size_t i = 0; i < m; ++i) {
        if (!check_that_types_match(w, call->arguments[i], proc->lambda.argument_types[i])) {
            report_error(w, call->arguments[i]->location, "Argument type mismatch: Wanted %s but got %s.",
                type_to_string(proc->lambda.argument_types[i]), type_to_string(call->arguments[i]->inferred_type));
        }
        Replace(call->arguments[i]);
        
        // if (!types_are_equal(call->arguments[i]->inferred_type, proc->lambda.argument_types[i])) {
        // }
    }

    // @Incomplete: This doesn't work for multiple return values.
    call->_expression.inferred_type = proc->lambda.return_type;
}

inline void typecheck_definition(Workspace *w, Ast_Type_Definition *defn)
{   
    if (defn->kind == TYPE_DEF_STRUCT) {
        if (!defn->struct_desc->block->_statement.typechecked) return; // @Cleanup can we assert this now
        For (defn->struct_desc->block->declarations) {
            Ast_Declaration *member = defn->struct_desc->block->declarations[it];
            if (member->flags & DECLARATION_IS_STRUCT_FIELD) {
                assert(member->my_type);
                arrput(defn->struct_desc->field_types, member->my_type);
            }
        }
        defn->size = 0;
        For (defn->struct_desc->field_types) {
            defn->size += defn->struct_desc->field_types[it]->size;
        }
    }

    if (defn->kind == TYPE_DEF_IDENT) {
        Ast_Declaration *decl = defn->type_name->resolved_declaration;

        if (decl->my_type != w->type_def_type) {
            report_error(w, defn->_expression.location, "Type mismatch: Wanted Type but got %s.",
                type_to_string(decl->my_type));
        }

        defn->_expression.replacement = xx decl->root_expression;
    }

    defn->_expression.inferred_type = w->type_def_type;
}

void typecheck_cast(Workspace *w, Ast_Cast *cast)
{
    UNUSED(w);
    cast->_expression.inferred_type = cast->type;
}

void typecheck_selector_on_string(Workspace *w, Ast_Selector *selector)
{
    if (sv_eq(selector->ident->name, sv_from_cstr("data"))) {
        selector->struct_field_index = 0;
        selector->_expression.inferred_type = make_pointer_type(w->type_def_u8);
        return;
    }
    
    if (sv_eq(selector->ident->name, sv_from_cstr("count"))) {
        selector->struct_field_index = 1;
        selector->_expression.inferred_type = w->type_def_int;
        return;
    }
    
    report_error(w, selector->_expression.location, "String type has no member '"SV_Fmt"'.", SV_Arg(selector->ident->name));
}

void typecheck_selector_on_array(Workspace *w, Ast_Selector *selector, Ast_Type_Definition *defn)
{
    if (defn->array.kind != ARRAY_KIND_FIXED) {
        if (sv_eq(selector->ident->name, sv_from_cstr("data"))) {
            selector->struct_field_index = 0;
            selector->_expression.inferred_type = make_pointer_type(defn->array.element_type);
            return;
        }
    
        if (sv_eq(selector->ident->name, sv_from_cstr("count"))) {
            selector->struct_field_index = 1;
            selector->_expression.inferred_type = w->type_def_int;
            return;
        }
        
        if (defn->array.kind == ARRAY_KIND_DYNAMIC && sv_eq(selector->ident->name, sv_from_cstr("capacity"))) {
            selector->struct_field_index = 1;
            selector->_expression.inferred_type = w->type_def_int;
            return;
        }

        report_error(w, selector->_expression.location, "Array type has no member '"SV_Fmt"'.", SV_Arg(selector->ident->name));
    }
   
    if (sv_eq(selector->ident->name, sv_from_cstr("data"))) {
        // TODO: If we are selecting the '.data' field of a fixed-sized array, nothing happens in bytecode.
        assert(0 && "Selecting the data field from a fixed-size array is not implemented yet, (just use a cast, as the memory representation is the same).");
        selector->struct_field_index = 0;
        selector->_expression.inferred_type = make_pointer_type(defn->array.element_type);
        return;
    }

    if (sv_eq(selector->ident->name, sv_from_cstr("count"))) {
        // Replace the selector with a constant.
        selector->_expression.replacement = xx make_number(defn->array.length);
        return;
    }

    assert(0);
}

void typecheck_selector(Workspace *w, Ast_Selector *selector)
{
    Replace(selector->namespace_expression);

    Source_Location site = selector->_expression.location;

    Ast_Type_Definition *defn = selector->namespace_expression->inferred_type;
    Replace_Type(defn);

    // Since we may be waiting for this declaration, we will try to be typechecked multiple times.
    // We cache the resolved_declaration for that reason.
    if (selector->ident->resolved_declaration) {
        Ast_Declaration *decl = selector->ident->resolved_declaration;

        if (!(decl->flags & DECLARATION_HAS_BEEN_TYPECHECKED)) return;

        // Otherwise, we're done.
        selector->_expression.inferred_type = decl->my_type;
        if (decl->flags & DECLARATION_IS_CONSTANT) {
            selector->_expression.replacement = decl->root_expression;
        } else if (decl->flags & DECLARATION_IS_STRUCT_FIELD) {
            selector->struct_field_index = decl->struct_field_index;
        }
        return;
    }
    
    // Otherwise we actually need to do a member lookup.

    if (defn == w->type_def_type) {
        assert(selector->namespace_expression->kind == AST_TYPE_DEFINITION);
        defn = xx selector->namespace_expression;

        // TODO: Handle struct.constant
        if (defn->kind == TYPE_DEF_ENUM) {
            Ast_Declaration *decl = find_declaration_in_block(defn->enum_defn->block, selector->ident->name);
            if (!decl) {
                report_error(w, site, "Enum has no member '"SV_Fmt"'.", SV_Arg(selector->ident->name));
            }

            // Cache this in case we can't proceed and need to return here later.
            selector->ident->resolved_declaration = decl;

            if (!(decl->flags & DECLARATION_HAS_BEEN_TYPECHECKED)) return;

            assert(decl->flags & DECLARATION_IS_CONSTANT);
            selector->_expression.inferred_type = decl->my_type;
            selector->_expression.replacement = decl->root_expression;
            return;
        }

        report_error(w, selector->namespace_expression->location, "Attempt to dereference a non-namespaced type (got type %s).",
            type_to_string(defn));
    }

    switch (defn->kind) {
    case TYPE_DEF_IDENT:
        UNREACHABLE; // We called Replace_Type already.
    case TYPE_DEF_LITERAL:
        if (defn->literal == LITERAL_STRING) {
            typecheck_selector_on_string(w, selector);
        } else {
            report_error(w, site, "Attempt to dereference a non-namespaced type (got type %s).",
                type_to_string(defn));
        }
        break;
    // @Copypasta between struct and enum.
    case TYPE_DEF_STRUCT: {
        Ast_Declaration *decl = find_declaration_in_block(defn->struct_desc->block, selector->ident->name);
        if (!decl) {
            report_error(w, site, "Struct has no member '"SV_Fmt"'.", SV_Arg(selector->ident->name));
        }

        // Cache this in case we can't proceed and need to return here later.
        selector->ident->resolved_declaration = decl;

        if (!(decl->flags & DECLARATION_HAS_BEEN_TYPECHECKED)) return;

        // @Copypasta
        selector->_expression.inferred_type = decl->my_type;
        if (decl->flags & DECLARATION_IS_CONSTANT) {
            selector->_expression.replacement = decl->root_expression;
        } else if (decl->flags & DECLARATION_IS_STRUCT_FIELD) {
            selector->struct_field_index = decl->struct_field_index;
        }
        break;
    }
    case TYPE_DEF_ENUM: {
        Ast_Declaration *decl = find_declaration_in_block(defn->enum_defn->block, selector->ident->name);
        if (!decl) {
            report_error(w, site, "Enum has no member '"SV_Fmt"'.", SV_Arg(selector->ident->name));
        }

        // Cache this in case we can't proceed and need to return here later.
        selector->ident->resolved_declaration = decl;

        if (!(decl->flags & DECLARATION_HAS_BEEN_TYPECHECKED)) return;

        assert(decl->flags & DECLARATION_IS_CONSTANT);
        selector->_expression.inferred_type = decl->my_type;
        selector->_expression.replacement = decl->root_expression;
        break;
    }
    case TYPE_DEF_ARRAY:
        typecheck_selector_on_array(w, selector, defn);
        break;
    case TYPE_DEF_STRUCT_CALL:
        UNIMPLEMENTED;
    case TYPE_DEF_POINTER: {
        report_error(w, site, "Dereferencing members through a pointer type is currently not implemented (this is an internal error).");
    }
    case TYPE_DEF_NUMBER:
    case TYPE_DEF_LAMBDA:
        report_error(w, selector->namespace_expression->location, "Attempt to dereference a non-namespaced type (got type %s).",
            type_to_string(defn));
    }
}

void typecheck_instantiation(Workspace *w, Ast_Type_Instantiation *inst)
{
    Replace_Type(inst->type_definition);

    Ast_Type_Definition *defn = inst->type_definition;
    Source_Location site = inst->_expression.location;

    if (arrlenu(inst->arguments) == 0) {
        inst->_expression.replacement = generate_default_value_for_type(w, defn);
        inst->_expression.replacement->inferred_type = defn;
        return;
    }

    switch (defn->kind) {
    case TYPE_DEF_NUMBER: {
        if (arrlenu(inst->arguments) != 1) {
            report_error(w, site, "Can only instantiate literal types with 1 argument.");
        }

        Ast_Expression *value = inst->arguments[0];

        if (!check_that_types_match(w, value, defn)) {
            report_error(w, value->location, "Type mismatch: Wanted %s but got %s.",
                type_to_string(defn), type_to_string(value->inferred_type));
        }
            
        inst->_expression.replacement = value;
        break;
    }
    case TYPE_DEF_LITERAL: {
        if (arrlenu(inst->arguments) != 1) {
            report_error(w, site, "Can only instantiate literal types with 1 argument.");
        }
        Ast_Expression *value = inst->arguments[0];
        if (!types_are_equal(value->inferred_type, defn)) {
            report_error(w, value->location, "Type mismatch: Wanted %s but got %s.",
                type_to_string(defn), type_to_string(value->inferred_type));
        }
        inst->_expression.replacement = value;
        break;
    }
    case TYPE_DEF_POINTER: {
        if (arrlenu(inst->arguments) != 1) {
            report_error(w, site, "Can only instantiate pointer types with 1 argument.");
        }
        Ast_Expression *value = inst->arguments[0];
        if (!types_are_equal(value->inferred_type, defn)) {
            report_error(w, value->location, "Type mismatch: Wanted %s but got %s.",
                type_to_string(defn), type_to_string(value->inferred_type));
        }
        inst->_expression.replacement = value;
        break;
    }
    case TYPE_DEF_ARRAY: {
        int n = arrlen(inst->arguments);
            
        switch (defn->array.kind) {
        case ARRAY_KIND_FIXED: {
            int m = defn->array.length;
            if (n != m) {
                report_error(w, site, "Incorrect number of arguments for array literal (wanted %d but got %d).", m, n);
            }
            for (int i = 0; i < n; ++i) {
                Ast_Expression *arg = inst->arguments[i];
                if (!check_that_types_match(w, arg, defn->array.element_type)) {
                    report_error(w, arg->location, "Argument type mismatch: Wanted %s but got %s.",
                        type_to_string(defn->array.element_type), type_to_string(arg->inferred_type));
                }
            }
            break;
        }
        case ARRAY_KIND_SLICE: {
            if (n != 2) {
                report_error(w, site, "Incorrect number of arguments for slice literal (wanted 2 but got %d.)", n);
            }
            Ast_Type_Definition *data_field = context_alloc(sizeof(*data_field));
            data_field->kind = TYPE_DEF_POINTER;
            data_field->pointer_to = defn->array.element_type;
            if (!check_that_types_match(w, inst->arguments[0], data_field)) {
                report_error(w, inst->arguments[0]->location, "Field type mismatch: Wanted %s but got %s.",
                    type_to_string(data_field), type_to_string(inst->arguments[0]->inferred_type));
            }
            if (!check_that_types_match(w, inst->arguments[1], w->type_def_int)) {
                report_error(w, inst->arguments[1]->location, "Field type mismatch: Wanted int but got %s.",
                    type_to_string(inst->arguments[1]->inferred_type));
            }
            break;
        }
        case ARRAY_KIND_DYNAMIC: {
            if (n != 3) {
                report_error(w, site, "Incorrect number of arguments for dynamic array literal (wanted 3 but got %d.)", n);
            }
            Ast_Type_Definition *data_field = context_alloc(sizeof(*data_field));
            data_field->kind = TYPE_DEF_POINTER;
            data_field->pointer_to = defn->array.element_type;
            if (!check_that_types_match(w, inst->arguments[0], data_field)) {
                report_error(w, inst->arguments[0]->location, "Field type mismatch: Wanted %s but got %s.",
                    type_to_string(inst->arguments[0]->inferred_type));
            }
            if (!check_that_types_match(w, inst->arguments[1], w->type_def_int)) {
                report_error(w, inst->arguments[1]->location, "Field type mismatch: Wanted int but got %s.",
                    type_to_string(inst->arguments[1]->inferred_type));
            }
            if (!check_that_types_match(w, inst->arguments[2], w->type_def_int)) {
                report_error(w, inst->arguments[2]->location, "Field type mismatch: Wanted int but got %s.",
                    type_to_string(inst->arguments[2]->inferred_type));
            }
            break;
        }
        }
            
        break;
    }
    case TYPE_DEF_STRUCT: {
        int n = arrlen(inst->arguments);
        int m = defn->struct_desc->field_count;
        if (n != m) {
            report_error(w, site, "Incorrect number of arguments to instantiate struct type (wanted %d but got %d).", m, n);
        }
        for (int i = 0; i < n; ++i) {
            Ast_Expression *arg = inst->arguments[i];
            Ast_Type_Definition *expected = defn->struct_desc->field_types[i];

            if (!check_that_types_match(w, arg, expected)) {
                report_error(w, inst->arguments[i]->location, "Field type mismatch: Wanted %s but got %s.",
                    type_to_string(expected), type_to_string(arg->inferred_type));
            }
        }
        break;
    }
    case TYPE_DEF_ENUM:
        report_error(w, site, "Currently, yuo cannot instantiate an enum using an initializer list.");
    case TYPE_DEF_IDENT:
        UNREACHABLE; // Because we called Replace_Type.
    case TYPE_DEF_STRUCT_CALL:
        UNIMPLEMENTED;
    case TYPE_DEF_LAMBDA:
        report_error(w, site, "Currently, you cannot instantiate a function pointer using an initializer list.");
    }

    inst->_expression.inferred_type = defn;
}

void typecheck_expression(Workspace *w, Ast_Expression *expr)
{
    if (expr->inferred_type) return; // TODO: replace this with an assert and see if this ever happens.
    switch (expr->kind) {
    case AST_NUMBER:             typecheck_number(w, xx expr, NULL);    break;
    case AST_LITERAL:            typecheck_literal(w, xx expr);         break;
    case AST_IDENT:              typecheck_identifier(w, xx expr);      break;
    case AST_UNARY_OPERATOR:     typecheck_unary_operator(w, xx expr);  break;
    case AST_BINARY_OPERATOR:    typecheck_binary_operator(w, xx expr); break;
    case AST_LAMBDA:             typecheck_lambda(w, xx expr);          break;
    case AST_PROCEDURE_CALL:     typecheck_procedure_call(w, xx expr);  break;
    case AST_TYPE_DEFINITION:    typecheck_definition(w, xx expr);      break;
    case AST_CAST:               typecheck_cast(w, xx expr);            break;
    case AST_SELECTOR:           typecheck_selector(w, xx expr);        break;
    case AST_TYPE_INSTANTIATION: typecheck_instantiation(w, xx expr);   break;
    }
}

void typecheck_while(Workspace *w, Ast_While *while_stmt)
{
    Replace(while_stmt->condition_expression);

    if (while_stmt->condition_expression->inferred_type != w->type_def_bool) {
        Ast_Expression *expr = autocast_to_bool(w, while_stmt->condition_expression);
        if (expr) {
            while_stmt->condition_expression = expr;
        } else {
            report_error(w, while_stmt->condition_expression->location, "Condition of 'while' statement must result in a boolean value (got %s).",
                type_to_string(while_stmt->condition_expression->inferred_type));
        }
    }
}

void typecheck_if(Workspace *w, Ast_If *if_stmt)
{
    Replace(if_stmt->condition_expression);

    if (if_stmt->condition_expression->inferred_type != w->type_def_bool) {
        Ast_Expression *expr = autocast_to_bool(w, if_stmt->condition_expression);
        if (expr) {
            // if_stmt->condition_expression->replacement = expr;
            if_stmt->condition_expression = expr;
        } else {
            report_error(w, if_stmt->condition_expression->location, "Condition of 'if' statement must result in a boolean value (got %s).",
                type_to_string(if_stmt->condition_expression->inferred_type));
        }
    }
}

void typecheck_for(Workspace *w, Ast_For *for_stmt)
{
    Replace(for_stmt->range_expression);

    // typecheck_declaration(w, for_stmt->iterator_declaration);
    // assert(for_stmt->iterator_declaration->flags & DECLARATION_HAS_BEEN_TYPECHECKED);

    if (for_stmt->range_expression->kind == AST_BINARY_OPERATOR) {
        Ast_Binary_Operator *binary = xx for_stmt->range_expression;
        if (binary->operator_type == TOKEN_DOUBLE_DOT) return;
    }

    if (for_stmt->range_expression->inferred_type->kind == TYPE_DEF_ARRAY) return;

    report_error(w, for_stmt->range_expression->location, "Expected an array but got %s.",
        type_to_string(for_stmt->range_expression->inferred_type));
}

void typecheck_return(Workspace *w, Ast_Return *ret)
{
    UNUSED(w);

    Ast_Type_Definition *expected_type = ret->lambda_i_belong_to->type_definition->lambda.return_type;

    if (!check_that_types_match(w, ret->subexpression, expected_type)) {
        report_error(w, ret->subexpression->location, "Return type mismatch: Wanted %s but got %s.",
            type_to_string(expected_type), type_to_string(ret->subexpression->inferred_type));
    }
}

void typecheck_using(Workspace *w, Ast_Using *using)
{
    UNUSED(w);
    UNUSED(using);
    UNIMPLEMENTED;
}

inline void typecheck_variable(Workspace *w, Ast_Variable *var)
{
    typecheck_declaration(w, var->declaration);
}

void typecheck_assignment(Workspace *w, Ast_Assignment *assign)
{
    Replace(assign->pointer);
    Replace(assign->value);
    
    if (assign->pointer->kind == AST_IDENT) {
        Ast_Ident *ident = xx assign->pointer;
        if (ident->resolved_declaration->flags & DECLARATION_IS_CONSTANT) {
            report_error(w, ident->_expression.location, "Cannot assign to constant.");
        }
        if (ident->resolved_declaration->flags & DECLARATION_IS_FOR_LOOP_ITERATOR) {
            report_error(w, ident->_expression.location, "Cannot assign to iterator.");
        }

        goto end;
    }

    // Here we must be assigning to a selector or a pointer, and we must not be constant
    // otherwise we *should* have been replaced in typecheck_selector.

    Ast_Type_Definition *defn = assign->pointer->inferred_type;

    // TODO: @Cleanup: How we determine what is an "lvalue".
    if (assign->pointer->kind != AST_SELECTOR && defn->kind != TYPE_DEF_POINTER) {
        if (assign->pointer->kind == AST_BINARY_OPERATOR && ((Ast_Binary_Operator *)(assign->pointer))->operator_type == TOKEN_ARRAY_SUBSCRIPT) {
            goto end;
        }
        report_error(w, assign->pointer->location, "Cannot assign to non-lvalue.");
    }
    
end:
    if (!check_that_types_match(w, assign->value, assign->pointer->inferred_type)) {
        report_error(w, assign->value->location, "Type mismatch: Wanted %s but got %s.",
            type_to_string(assign->pointer->inferred_type), type_to_string(assign->value->inferred_type));
    }
}

void typecheck_statement(Workspace *w, Ast_Statement *stmt)
{
    if (stmt->typechecked) return;
    switch (stmt->kind) {
    case AST_BLOCK:                break; // Do nothing, our members should have compiled first.
    case AST_WHILE:                typecheck_while(w, xx stmt); break;
    case AST_IF:                   typecheck_if(w, xx stmt); break;
    case AST_FOR:                  typecheck_for(w, xx stmt); break;
    case AST_LOOP_CONTROL:         break;
    case AST_RETURN:               typecheck_return(w, xx stmt); break;
    case AST_USING:                typecheck_using(w, xx stmt); break;
    case AST_IMPORT:               break;
    case AST_EXPRESSION_STATEMENT: break;
    case AST_VARIABLE:             typecheck_variable(w, xx stmt); break;
    case AST_ASSIGNMENT:           typecheck_assignment(w, xx stmt); break;
    }
    stmt->typechecked = true;
}

void flatten_expr_for_typechecking(Ast_Declaration *root, Ast_Expression *expr)
{
    if (expr == NULL) return; // This can happen if flatten_stmt_for_typechecking adds us when we don't exist.

    switch (expr->kind) {
    case AST_NUMBER:
    case AST_LITERAL:
    case AST_IDENT:
        break;
    case AST_UNARY_OPERATOR: {
        Ast_Unary_Operator *unary = xx expr;
        flatten_expr_for_typechecking(root, unary->subexpression);
        break;
    }
    case AST_BINARY_OPERATOR: {
        Ast_Binary_Operator *binary = xx expr;
        flatten_expr_for_typechecking(root, binary->left);
        flatten_expr_for_typechecking(root, binary->right);
        break;
    }
    case AST_LAMBDA: {
        Ast_Lambda *lambda = xx expr;
        flatten_expr_for_typechecking(root, xx lambda->type_definition);
        // Notice how we don't do anything about lambda->my_body_declaration.
        // It will get async processing on its own.
        break;
    }
    case AST_PROCEDURE_CALL: {
        Ast_Procedure_Call *call = xx expr;
        flatten_expr_for_typechecking(root, call->procedure_expression);
        For (call->arguments) flatten_expr_for_typechecking(root, call->arguments[it]);
        break;
    }
    case AST_TYPE_DEFINITION: {
        Ast_Type_Definition *defn = xx expr;
        switch (defn->kind) {
        // TODO: When enum->underlying_int_type can be an alias, it needs to be added here.
        case TYPE_DEF_POINTER:
            flatten_expr_for_typechecking(root, xx defn->type_name);
            break;
        case TYPE_DEF_STRUCT:
            flatten_stmt_for_typechecking(root, xx defn->struct_desc->block);
            break;
        case TYPE_DEF_IDENT:
            flatten_expr_for_typechecking(root, xx defn->type_name);                
            break;
        case TYPE_DEF_LAMBDA:
            For (defn->lambda.argument_types) flatten_expr_for_typechecking(root, xx defn->lambda.argument_types[it]);
            flatten_expr_for_typechecking(root, xx defn->lambda.return_type);
            break;
        default:
            break;
        }
        break;
    }
    case AST_CAST: {
        Ast_Cast *cast = xx expr;
        flatten_expr_for_typechecking(root, cast->subexpression);
        break;
    }
    case AST_SELECTOR: {
        Ast_Selector *selector = xx expr;
        flatten_expr_for_typechecking(root, selector->namespace_expression);
        // flatten_expr_for_typechecking(root, xx selector->ident);
        break;
    }
    case AST_TYPE_INSTANTIATION: {
        Ast_Type_Instantiation *inst = xx expr;
        flatten_expr_for_typechecking(root, xx inst->type_definition);
        For (inst->arguments) {
            flatten_expr_for_typechecking(root, inst->arguments[it]);
        }
        break;
    }
    }

    Ast_Node node;
    node.expression = expr;
    node.statement = NULL;
    arrput(root->flattened, node);
}

void flatten_stmt_for_typechecking(Ast_Declaration *root, Ast_Statement *stmt)
{
    switch (stmt->kind) {
    case AST_BLOCK: {
        Ast_Block *block = xx stmt;
        For (block->statements) flatten_stmt_for_typechecking(root, block->statements[it]);
        For (block->declarations) {
            Ast_Declaration *decl = block->declarations[it];
            flatten_expr_for_typechecking(root, decl->root_expression);
            if (decl->my_block) {
                flatten_stmt_for_typechecking(root, xx decl->my_block);
            }
        }
        break;
    }
    case AST_WHILE: {
        Ast_While *while_stmt = xx stmt;
        flatten_expr_for_typechecking(root, while_stmt->condition_expression);
        flatten_stmt_for_typechecking(root, while_stmt->then_statement);
        break;
    }
    case AST_IF: {
        Ast_If *if_stmt = xx stmt;
        flatten_expr_for_typechecking(root, if_stmt->condition_expression);
        flatten_stmt_for_typechecking(root, if_stmt->then_statement);
        if (if_stmt->else_statement) flatten_stmt_for_typechecking(root, if_stmt->else_statement);
        break;
    }
    case AST_FOR: {
        Ast_For *for_stmt = xx stmt;
        flatten_expr_for_typechecking(root, for_stmt->range_expression);
        flatten_stmt_for_typechecking(root, for_stmt->then_statement);
        break;
    }
    case AST_LOOP_CONTROL:
        break;
    case AST_RETURN: {
        Ast_Return *ret = xx stmt;
        flatten_expr_for_typechecking(root, ret->subexpression);
        break;
    }
    case AST_USING: {
        Ast_Using *using = xx stmt;
        flatten_expr_for_typechecking(root, using->subexpression);
        break;
    }
    case AST_IMPORT:
        break;
    case AST_EXPRESSION_STATEMENT: {
        Ast_Expression_Statement *expr = xx stmt;
        flatten_expr_for_typechecking(root, expr->subexpression);
        break;
    }
    case AST_VARIABLE: {
        Ast_Variable *var = xx stmt;
        // TODO: Is this right?
        flatten_expr_for_typechecking(root, xx var->declaration->my_type);
        flatten_expr_for_typechecking(root, var->declaration->root_expression);
        break;
    }
    case AST_ASSIGNMENT: {
        Ast_Assignment *assign = xx stmt;
        // TODO: Check the order on this.
        flatten_expr_for_typechecking(root, assign->value);
        flatten_expr_for_typechecking(root, assign->pointer);
        break;
    }
    }

    Ast_Node node;
    node.expression = NULL;
    node.statement = stmt;
    arrput(root->flattened, node);
}

void flatten_decl_for_typechecking(Ast_Declaration *decl)
{
    if (decl->flags & DECLARATION_IS_PROCEDURE_BODY) {
        if (decl->flags & DECLARATION_IS_FOREIGN) return; // Nothing to typecheck.
        flatten_stmt_for_typechecking(decl, xx decl->my_block->parent); // Arguments.
        flatten_stmt_for_typechecking(decl, xx decl->my_block);
        return;
    }

    if (decl->my_type) {
        flatten_expr_for_typechecking(decl, xx decl->my_type);
    }

    if (decl->root_expression) {
        flatten_expr_for_typechecking(decl, decl->root_expression);
    }
}

bool check_that_types_match(Workspace *w, Ast_Expression *expr, Ast_Type_Definition *type)
{
    Replace_Type(type);
    
    if (types_are_equal(expr->inferred_type, type)) return true;

    // Handle constant numeric values that can implicitly cast to most types.
    if (expr->kind == AST_NUMBER) {
        Ast_Number *number = xx expr;

        if (!number->inferred_type_is_final) {
            typecheck_number(w, number, type);
            return true;
        }
    }

    // Strings can be converted to integers if they are exactly one character.
    if (expr->kind == AST_LITERAL) {
        Ast_Literal *literal = xx expr;
        if (literal->kind == LITERAL_STRING && (type->kind == TYPE_DEF_NUMBER && !(type->number.flags & NUMBER_FLAGS_FLOAT))) {
            if (literal->string_value.count != 1) {
                report_error(w, expr->location, "Strings can only convert to integers if they are exactly one character.");
            }

            Ast_Number *char_literal = context_alloc(sizeof(*char_literal));
            char_literal->_expression.kind = AST_NUMBER;
            char_literal->_expression.location = expr->location;
            char_literal->_expression.inferred_type = type;
            char_literal->as.integer = *literal->string_value.data;

            expr->replacement = xx char_literal;
            return true;
        }
    }

    // Fixed-sized arrays can autocast to pointer & slice.
    if (expr->inferred_type->kind == TYPE_DEF_ARRAY && expr->inferred_type->array.kind == ARRAY_KIND_FIXED) {
        // if (type->kind == TYPE_DEF_POINTER) return types_are_equal(expr->inferred_type->array.element_type, type->pointer_to);

        if (type->kind == TYPE_DEF_ARRAY && type->array.kind == ARRAY_KIND_SLICE) {
            Ast_Type_Instantiation *inst = context_alloc(sizeof(*inst));
            inst->_expression.kind = AST_TYPE_INSTANTIATION;
            inst->_expression.location = expr->location;
            inst->_expression.inferred_type = type;
            inst->type_definition = type;

            {
                Ast_Number *index = make_number(0);
                index->_expression.location = expr->location;
                index->_expression.inferred_type = w->type_def_int;

                Ast_Binary_Operator *subscript = context_alloc(sizeof(*subscript));
                subscript->_expression.kind = AST_BINARY_OPERATOR;
                subscript->_expression.location = expr->location;
                subscript->_expression.inferred_type = expr->inferred_type->array.element_type;
                subscript->left = expr;
                subscript->operator_type = TOKEN_ARRAY_SUBSCRIPT;
                subscript->right = xx index;

                Ast_Type_Definition *pointer_type = context_alloc(sizeof(*pointer_type));
                pointer_type->_expression.kind = AST_TYPE_DEFINITION;
                pointer_type->_expression.location = expr->location;
                pointer_type->_expression.inferred_type = w->type_def_type;
                pointer_type->kind = TYPE_DEF_POINTER;
                pointer_type->pointer_to = expr->inferred_type->array.element_type;

                Ast_Unary_Operator *unary = context_alloc(sizeof(*unary));
                unary->_expression.kind = AST_UNARY_OPERATOR;
                unary->_expression.location = expr->location;
                unary->_expression.inferred_type = pointer_type;
                unary->operator_type = '*';
                unary->subexpression = xx subscript;

                arrput(inst->arguments, xx unary);
            }

            Ast_Number *number = make_number(expr->inferred_type->array.length);
            number->_expression.inferred_type = w->type_def_int;
            arrput(inst->arguments, xx number);

            // TODO: Wow, this line is the problem.
            expr->replacement = xx inst;
            return true;
        }
    }

    return false;
}

bool types_are_equal(Ast_Type_Definition *x, Ast_Type_Definition *y)
{
    if (x == y) return true;
    if (x->kind != y->kind) return false;

    switch (x->kind) {
    case TYPE_DEF_POINTER:
        return types_are_equal(x->pointer_to, y->pointer_to);
    case TYPE_DEF_ARRAY:
        // TODO: Is this the place to do implicit cast of array to slice?
        if (x->array.length != y->array.length) return false;
        return types_are_equal(x->array.element_type, y->array.element_type);
    case TYPE_DEF_LAMBDA:
        if (!types_are_equal(x->lambda.return_type, y->lambda.return_type)) return false;
        if (arrlenu(x->lambda.argument_types) != arrlenu(y->lambda.argument_types)) return false;
        For (x->lambda.argument_types) {
            if (!types_are_equal(x->lambda.argument_types[it], y->lambda.argument_types[it])) return false;
        }
        return true;
    default:
        // All other types, such as structures and enumerations, can only be compared by pointer.
        // We do *NOT* do any duck typing or other functional programming strangeness. If you want
        // "duck" typing, you can use compile-time polymorphism or metaprogramming.
        return false;
    }
}

#define RED   "\x1B[31m"
#define GRN   "\x1B[32m"
#define YEL   "\x1B[33m"
#define BLU   "\x1B[34m"
#define MAG   "\x1B[35m"
#define CYN   "\x1B[36m"
#define WHT   "\x1B[37m"
#define RESET "\x1B[0m"
#define TAB   "    "

void report_error(Workspace *workspace, Source_Location loc, const char *format, ...)
{
    va_list args;
    va_start(args, format);

    if (loc.l1 < 0) loc.l1 = loc.l0;
    if (loc.c1 < 0) loc.c1 = loc.c0;

    Source_File file = workspace->files[loc.fid];

    // Display the error message.
    fprintf(stderr, Loc_Fmt": Error: ", SV_Arg(file.path), Loc_Arg(loc));
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n\n");

    int ln = loc.l0;

    // Display the previous line if it exists.
    String_View prev = (ln > 0) ? file.lines[ln-1] : SV_NULL;
    String_View line = file.lines[ln];

    if (prev.count > 1) {
        size_t count = Min(size_t, prev.count, line.count);
        size_t n = 0;
        while (n < count && prev.data[n] == line.data[n] && isspace(prev.data[n])) {
            n += 1;
        }
        sv_chop_left(&prev, n);
        sv_chop_left(&line, n);

        fprintf(stderr, TAB CYN SV_Fmt RESET, SV_Arg(prev));

        loc.c0 -= n;
        loc.c1 -= n;
    } else {
        size_t n = 0;
        while (n < line.count && isspace(line.data[n])) {
            n += 1;
        }
        sv_chop_left(&line, n);
        loc.c0 -= n;
        loc.c1 -= n;
    }

    // Highlight the token in red.

    fprintf(stderr, TAB CYN SV_Fmt, loc.c0, line.data);
    fprintf(stderr,     RED SV_Fmt, loc.c1 - loc.c0, line.data + loc.c0);
    fprintf(stderr,     CYN SV_Fmt, (int)line.count - loc.c1, line.data + loc.c1);
    fprintf(stderr, "\n" RESET);

    va_end(args);
    exit(1);
}

void report_info(Workspace *workspace, Source_Location loc, const char *format, ...)
{
    va_list args;
    va_start(args, format);

    if (loc.l1 < 0) loc.l1 = loc.l0;
    if (loc.c1 < 0) loc.c1 = loc.c0;

    Source_File file = workspace->files[loc.fid];

    // Display the error message.
    fprintf(stderr, Loc_Fmt": Info: ", SV_Arg(file.path), Loc_Arg(loc));
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n\n");

    int ln = loc.l0;

    // Display the previous line if it exists.
    String_View prev = (ln > 0) ? file.lines[ln-1] : SV_NULL;
    String_View line = file.lines[ln];

    if (prev.count > 1) {
        size_t count = Min(size_t, prev.count, line.count);
        size_t n = 0;
        while (n < count && prev.data[n] == line.data[n] && isspace(prev.data[n])) {
            n += 1;
        }
        sv_chop_left(&prev, n);
        sv_chop_left(&line, n);

        fprintf(stderr, TAB CYN SV_Fmt RESET, SV_Arg(prev));

        loc.c0 -= n;
        loc.c1 -= n;
    } else {
        size_t n = 0;
        while (n < line.count && isspace(line.data[n])) {
            n += 1;
        }
        sv_chop_left(&line, n);
        loc.c0 -= n;
        loc.c1 -= n;
    }

    // Highlight the token in red.

    fprintf(stderr, TAB CYN SV_Fmt, loc.c0, line.data);
    fprintf(stderr,     RED SV_Fmt, loc.c1 - loc.c0, line.data + loc.c0);
    fprintf(stderr,     CYN SV_Fmt, (int)line.count - loc.c1, line.data + loc.c1);
    fprintf(stderr, "\n" RESET);

    va_end(args);
}

