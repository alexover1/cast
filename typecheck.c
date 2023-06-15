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

Ast_Expression *autocast_to_bool(Workspace *w, Ast_Expression *expr)
{
    Ast_Type_Definition *defn = expr->inferred_type;

    if (defn->literal_name) {
        if (defn->number_flags & NUMBER_FLAGS_NUMBER) {
            Ast_Binary_Operator *binary = context_alloc(sizeof(*binary));
            binary->_expression.location = expr->location;
            binary->_expression.inferred_type = w->type_def_bool;
            binary->left = expr;
            binary->operator_type = TOKEN_ISNOTEQUAL;
            binary->right = xx make_integer_literal(defn, 0);
            return xx binary;
        }
        if (defn == w->type_def_string) {
            // TODO: Create a selector and get the "count" field and then check if it's not zero.
            assert(0);
        }
        if (defn == w->type_def_void) {
            Ast_Literal *literal = context_alloc(sizeof(*literal));
            literal->_expression.location = expr->location;
            literal->_expression.inferred_type = w->type_def_bool;
            literal->default_type = w->type_def_bool;
            literal->integer_value = 0;
            return xx literal;
        }
        assert(0);
    }

    if (defn->pointer_to) {
        Ast_Binary_Operator *binary = context_alloc(sizeof(*binary));
        binary->_expression.location = expr->location;
        binary->_expression.inferred_type = w->type_def_bool;
        binary->left = expr;
        binary->operator_type = TOKEN_ISNOTEQUAL;
        binary->right = xx w->literal_null;
        return xx binary;
    }

    return NULL;
}

// Literals can't be inferred just from their parse tree.
// They need access to where they are used.
// For example, if I call a function that takes a float with the literal `0` that should work.
// However, if I say `x := 0`, I want the type of x to be integer.
// This is actually not that complicated because we set default_type on the literal.
void typecheck_literal(Workspace *w, Ast_Literal *literal, Ast_Type_Definition *supplied_type)
{
    if (!supplied_type) {
        literal->_expression.inferred_type = literal->default_type;
        return;
    }
    
    if (types_are_equal(supplied_type, literal->default_type)) {
        // No casting needed.
        return;
    }

    if (literal->number_flags & NUMBER_FLAGS_NUMBER) {
        if (literal->number_flags & NUMBER_FLAGS_FLOAT) {
            // Float literals only autocast to float, float32, and float64.
            if (supplied_type == w->type_def_float32 || supplied_type == w->type_def_float32) {
                literal->_expression.inferred_type = supplied_type;
                return;
            }
            goto error;
        }

        // We are some integer literal, so we can cast to integer or floating-point types.

        if (!(supplied_type->number_flags & NUMBER_FLAGS_NUMBER)) {
            goto error;
        }

        if (supplied_type->number_flags & NUMBER_FLAGS_FLOAT) {
            // For now, just safely assume the value can fit.
            // TODO: Fix this.
            literal->_expression.inferred_type = supplied_type;
            return;
        }

        // Note: It's safe now to assume we're a literal type because we have NUMBER_FLAGS_NUMBER.

        // int-to-int
        unsigned long low = supplied_type->number_literal_low->integer_value;
        unsigned long high = supplied_type->number_literal_high->integer_value;
        
        if (literal->integer_value > high) {
            report_error(w, literal->_expression.location, "Numeric constant too big for type (max for %s is %lu).", supplied_type->literal_name, high);
        }
        if (literal->integer_value < low) {
            report_error(w, literal->_expression.location, "Numeric constant too small for type (min for %s is %lu).", supplied_type->literal_name, low);
        }
        literal->_expression.inferred_type = supplied_type;
        return;
    }

    if (literal->default_type == w->type_def_string) {
        if (supplied_type->number_flags & NUMBER_FLAGS_NUMBER && !(supplied_type->number_flags & NUMBER_FLAGS_FLOAT)) {
            if (literal->string_value.count != 1) {
                report_error(w, literal->_expression.location, "String literals can only cast to int if they are exactly 1 character.");
            }
            
            Ast_Literal *char_literal = context_alloc(sizeof(*char_literal));
            char_literal->_expression.location = literal->_expression.location;
            char_literal->_expression.inferred_type = supplied_type;
            char_literal->default_type = supplied_type;
            char_literal->number_flags = NUMBER_FLAGS_NUMBER;
            char_literal->integer_value = *literal->string_value.data;

            literal->_expression.replacement = xx char_literal;
            return;
        }
        goto error;
    }

error:
    report_error(w, literal->_expression.location, "Type mismatch: Wanted %s but got %s.",
        type_to_string(supplied_type), type_to_string(literal->default_type));
}

void typecheck_identifier(Workspace *w, Ast_Ident *ident)
{
    if (!ident->resolved_declaration) {
        // TODO: We should have a separate phase where we check for circular dependencies and unresolved identifiers.
        ident->resolved_declaration = find_declaration_if_exists(ident);
        if (!ident->resolved_declaration) {
            report_error(w, ident->_expression.location, "Undeclared identifier '"SV_Fmt"'.", SV_Arg(ident->name));
        }
    }

    Ast_Declaration *decl = ident->resolved_declaration;

    // This is where we might hit a roadblock.

    if (!(decl->flags & DECLARATION_HAS_BEEN_TYPECHECKED)) {
        return;
    }

    // If the declaration has been typechecked, we can typecheck ourselves. 

    assert(decl->my_type);

    if (decl->flags & DECLARATION_IS_CONSTANT) {
        // TODO: does this work for lambdas?
        ident->_expression.replacement = decl->root_expression;
    }

    ident->_expression.inferred_type = decl->my_type;

    UNUSED(w);
}

void typecheck_unary_operator(Workspace *w, Ast_Unary_Operator *unary)
{
    UNUSED(w);
    UNUSED(unary);
    UNIMPLEMENTED;
}

Ast_Literal *do_binary_constant_literal_folding(Workspace *w, Ast_Type_Definition *defn, int operator_type, Ast_Literal *left, Ast_Literal *right)
{
    UNUSED(w);

    switch (operator_type) {
    case '+':
        if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
            return make_float_literal(defn, left->double_value + right->double_value);
        }
        return make_integer_literal(defn, left->integer_value + right->integer_value);
    case '-':
        if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
            return make_float_literal(defn, left->double_value - right->double_value);
        }
        return make_integer_literal(defn, left->integer_value - right->integer_value);
    default:
        UNIMPLEMENTED;
    }
}

// @Cleanup: This whole function's error messages.
void typecheck_binary_operator(Workspace *w, Ast_Binary_Operator *binary)
{
    // TODO: If left and right are literals, replace us with a literal.
    // Technically, LLVM does this for us, but let's not rely on that.
    
    // Check for constant replacement.
    Ast_Expression *binary_left = binary->left;
    Ast_Expression *binary_right = binary->right;
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
            report_error(w, binary->_expression.location, "Type mismatch: Cannot compare values of different types (got %s and %s).",
                type_to_string(left), type_to_string(right));
        }
        break;
    case '>':
    case '<':
    case TOKEN_GREATEREQUALS:
    case TOKEN_LESSEQUALS:
        if (left != right) {
            report_error(w, binary->_expression.location, "Type mismatch: Cannot compare values of different types (got %s and %s).",
                type_to_string(left), type_to_string(right));
        }
        if ((left->number_flags&NUMBER_FLAGS_NUMBER) == 0) {
            report_error(w, binary->left->location, "Type mismatch: Operator '%s' only works on number types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(left));
        }
        break;
    case TOKEN_LOGICAL_AND:
    case TOKEN_LOGICAL_OR:
        if (left != w->type_def_bool) {
            report_error(w, binary->left->location, "Type mismatch: Operator '%s' only works on boolean types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(left));
        }
        if (right != w->type_def_bool) {
            report_error(w, binary->right->location, "Type mismatch: Operator '%s' only works on boolean types (got %s).",
                token_type_to_string(binary->operator_type), type_to_string(right));
        }
        break;
    case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT:
    case TOKEN_SHIFT_RIGHT:
        if (!number_flags_is_int(left->number_flags)) {
            report_error(w, binary->left->location, "Type mismatch: Bit shift operators only work on integer types (got %s).", type_to_string(left));
        }
        if (!number_flags_is_int(right->number_flags)) {
            report_error(w, binary->right->location, "Type mismatch: Bit shift operators only work on integer types (got %s).", type_to_string(right));
        }
        break;
    default:       
        // We let the left type be the determinant.
        binary->_expression.inferred_type = left;

        if (left->number_flags & NUMBER_FLAGS_NUMBER) {
            if (left != right) {
                report_error(w, binary->_expression.location, "Type mismatch: Types on either side of '%s' must be the same (got %s and %s).",
                    token_type_to_string(binary->operator_type), type_to_string(left), type_to_string(right));
            }
            return;
        }

        if (left->pointer_to) {
            if (binary->operator_type != '+' && binary->operator_type != '-') {
                report_error(w, binary->_expression.location, "Type mismatch: Operator '%s' does not work on pointers.", token_type_to_string(binary->operator_type));
            }

            // Two possibilities, pointer-pointer and pointer-int.
            if (right->pointer_to) {
                if (!pointer_types_are_equal(left, right)) {
                    report_error(w, binary->_expression.location, "Type mismatch: Pointer types on either side of '%s' must be the same (got %s and %s).",
                        token_type_to_string(binary->operator_type), type_to_string(left), type_to_string(right));
                }
            } else if (!number_flags_is_int(right->number_flags)) {
                if (binary->operator_type == '+') {
                    report_error(w, binary->_expression.location, "Type mismatch: Can only add integer types to pointers (got %s)", type_to_string(right));
                } else if (binary->operator_type == '-') {
                    report_error(w, binary->_expression.location, "Type mismatch: Can only subtract integer types to pointers (got %s)", type_to_string(right));
                }
            }
            return;
        }

        report_error(w, binary->_expression.location, "Type mismatch: Operator '%s' only works on literal types (got %s).",
            token_type_to_string(binary->operator_type), type_to_string(left));
    }

    binary->_expression.inferred_type = left;

    if (binary_left->kind == AST_LITERAL && binary_right->kind == AST_LITERAL) {
        binary->_expression.replacement = xx do_binary_constant_literal_folding(w, left, binary->operator_type, xx binary_left, xx binary_right);
    }
}

void typecheck_lambda(Workspace *w, Ast_Lambda *lambda)
{
    lambda->_expression.inferred_type = lambda->type_definition;
    UNUSED(w);
    // UNUSED(lambda);
    // UNIMPLEMENTED;
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
        report_error(w, if_stmt->condition_expression->location, "Condition of 'if' statement must result in a boolean value.");
    }
}

void typecheck_return(Workspace *w, Ast_Return *ret)
{
    UNUSED(w);
    Wait_On(ret->subexpression);

    Ast_Type_Definition *expected_type = ret->lambda_i_belong_to->type_definition->lambda_return_type;
    Ast_Type_Definition *actual_type = ret->subexpression->inferred_type;

    if (!types_are_equal(expected_type, actual_type)) {
        report_error(w, ret->subexpression->location, "Return type mismatch: Wanted %s but got %s.",
            type_to_string(expected_type), type_to_string(actual_type));
    }
}

inline void typecheck_definition(Workspace *w, Ast_Type_Definition *defn)
{
    // @Cleanup: can this happen? (we don't add it in the flattening anymore)
    defn->_expression.inferred_type = w->type_def_type;
}

void typecheck_using(Workspace *w, Ast_Using *using)
{
    UNUSED(w);
    UNUSED(using);
    UNIMPLEMENTED;
}

void typecheck_cast(Workspace *w, Ast_Cast *cast)
{
    UNUSED(w);
    cast->_expression.inferred_type = cast->type;
}

void typecheck_declaration(Workspace *w, Ast_Declaration *decl)
{
    // Note: None of this gets set for non-constants, which is totally fine.
    while (decl->typechecking_position < arrlenu(decl->flattened)) {
        Ast_Node node = decl->flattened[decl->typechecking_position];
        if (node.expression) {
            typecheck_expression(w, node.expression);            
            if (node.expression->inferred_type) {
                decl->typechecking_position += 1;
            } else {
                // Hit a roadblock.
                break;
            }
        }
        if (node.statement) {
            typecheck_statement(w, node.statement);
            if (node.statement->typechecked) {
                decl->typechecking_position += 1;
            } else {
                // Hit a roadblock.
                break;
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
            report_error(w, decl->location, "We have a non-constant declaration with no type and no value (this is an internal error).");
        }

        if (decl->root_expression->kind == AST_LITERAL) {
            Ast_Literal *literal = xx decl->root_expression;
            decl->my_type = literal->default_type;
        } else {
            assert(decl->root_expression->inferred_type);
            decl->my_type = decl->root_expression->inferred_type;
        }

        decl->flags |= DECLARATION_TYPE_WAS_INFERRED_FROM_EXPRESSION;
        goto done;
    }

    // So we know we have a type now.

    if (!decl->root_expression) {
        // We're definitely not a constant (this error is checked above).
        // So we just set the default value for the type.
        if (!decl->my_type->is_leaf) {
            report_error(w, decl->location, "Support for default values for non-leaf types is not implemented yet (this is an internal error).");
        }

        if (decl->my_type->struct_desc) UNIMPLEMENTED;
        if (decl->my_type->enum_defn) UNIMPLEMENTED;
        if (decl->my_type->lambda_return_type) UNIMPLEMENTED; // TODO: What does this even mean? I think we should put null here.

        // Otherwise, we're a literal.
        assert(decl->my_type->literal_name);

        Ast_Literal *literal = context_alloc(sizeof(*literal));
        literal->_expression.kind = AST_LITERAL;
        literal->_expression.location = decl->location;
        literal->integer_value = 0; // Works for float, null, & bool also.
        literal->default_type = decl->my_type; // TODO: I think we need to remove default_type from literals.
        literal->number_flags = decl->my_type->number_flags;

        decl->root_expression = xx literal;
        decl->flags |= DECLARATION_VALUE_WAS_INFERRED_FROM_TYPE;
    }

    if (decl->root_expression->kind == AST_LITERAL) {
        typecheck_literal(w, xx decl->root_expression, decl->my_type);
        Replace(decl->root_expression);
    } else {
        // Check that we're not doing something like `x: bool = 12345`.
        if (!types_are_equal(decl->my_type, decl->root_expression->inferred_type)) {
            report_error(w, decl->location, "Type mismatch: Wanted %s but got %s.",
                type_to_string(decl->my_type), type_to_string(decl->root_expression->inferred_type));
        }
    }

done:
    decl->flags |= DECLARATION_HAS_BEEN_TYPECHECKED;
}

void typecheck_expression(Workspace *w, Ast_Expression *expr)
{
    while (expr->replacement) expr = expr->replacement;
    if (expr->inferred_type) return;
    switch (expr->kind) {
    case AST_LITERAL:            typecheck_literal(w, xx expr, NULL);   break;
    case AST_IDENT:              typecheck_identifier(w, xx expr);      break;
    case AST_UNARY_OPERATOR:     typecheck_unary_operator(w, xx expr);  break;
    case AST_BINARY_OPERATOR:    typecheck_binary_operator(w, xx expr); break;
    case AST_LAMBDA:             typecheck_lambda(w, xx expr);          break;
    case AST_PROCEDURE_CALL:     typecheck_procedure_call(w, xx expr);  break;
    case AST_TYPE_DEFINITION:    assert(0 && "This shouldn't have been added to the queue.");
    case AST_CAST:               typecheck_cast(w, xx expr);            break;
    }
}

inline void typecheck_variable(Workspace *w, Ast_Variable *var)
{
    typecheck_declaration(w, var->declaration);
}

void typecheck_assignment(Workspace *w, Ast_Assignment *assign)
{
    UNUSED(w);
    UNUSED(assign);
    UNIMPLEMENTED;
}

void typecheck_statement(Workspace *w, Ast_Statement *stmt)
{
    if (stmt->typechecked) return;
    switch (stmt->kind) {
    case AST_BLOCK: break; // Do nothing, our members should have compiled first.
    case AST_WHILE: typecheck_while(w, xx stmt); return;
    case AST_IF: typecheck_if(w, xx stmt); return;
    case AST_LOOP_CONTROL: break;
    case AST_RETURN: typecheck_return(w, xx stmt); return;
    case AST_USING: typecheck_using(w, xx stmt); return;
    case AST_EXPRESSION_STATEMENT: break;
    case AST_VARIABLE: typecheck_variable(w, xx stmt); break;
    case AST_ASSIGNMENT: typecheck_assignment(w, xx stmt); return;
    }
    stmt->typechecked = true;
}

void flatten_expr_for_typechecking(Ast_Declaration *root, Ast_Expression *expr)
{
    if (expr == NULL) return; // This can happen if flatten_stmt_for_typechecking adds us when we don't exist.

    switch (expr->kind) {
    case AST_LITERAL:
        break;
        // return; // We don't need to be typechecked.
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
    case AST_TYPE_DEFINITION:
        break;
    case AST_CAST: {
        Ast_Cast *cast = xx expr;
        flatten_expr_for_typechecking(root, cast->subexpression);
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
    // case AST_FOR:
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
    case AST_EXPRESSION_STATEMENT: {
        Ast_Expression_Statement *expr = xx stmt;
        flatten_expr_for_typechecking(root, expr->subexpression);
        break;
    }
    case AST_VARIABLE: {
        Ast_Variable *var = xx stmt;
        // TODO: Is this right?
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
        flatten_stmt_for_typechecking(decl, xx decl->my_block);
        return;
    }

    if (decl->root_expression) {
        flatten_expr_for_typechecking(decl, decl->root_expression);
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

    // TODO: I want to "normalize" the indentation.
    // When we add lines to the parser, we can add it
    // after it has been trimmed to remove leading spaces.
    // However, when printing the previous line, if they differ
    // in indentation I want to show that somehow.
    // Basically, if we are like 10 scopes deep in a function,
    // I want to only print indentation 1 level deeper or shallower
    // than the current line, so that when printing diagnostics we
    // don't end up printing like 50 spaces.

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
