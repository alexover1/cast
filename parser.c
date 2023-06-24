#include <assert.h>
#include <errno.h>
#include <string.h> // strerror
#include <stdio.h> // os_read_entire_file
#include <limits.h> // LLONG_MIN for defn->array.length

#include "common.h"
#include "parser.h"
#include "workspace.h"

// printf("%s:%d: >>> %p.\n", __FILE__, __LINE__, (void*)the_block);

#define Enter_Block(parser, the_block) do {        \
    (the_block)->parent = (parser)->current_block; \
    (parser)->current_block = (the_block);         \
} while (0);

#define Exit_Block(parser, the_block) do {                     \
    assert((parser)->current_block == the_block);              \
    (parser)->current_block = (parser)->current_block->parent; \
} while (0);

static inline Ast_Ident *make_identifier(Parser *p, Token token)
{
    // We copy the name but append a '\0' so it can be a c-string for LLVM.
    Ast_Ident *ident = ast_alloc(p, token.location, AST_IDENT, sizeof(*ident));
    ident->name.data = arena_sv_to_cstr(context_arena, token.string_value);
    ident->name.count = token.string_value.count;
    ident->enclosing_block = p->current_block;
    return ident;
}

static inline Ast_Declaration *make_declaration(Parser *p, Source_Location loc)
{
    Ast_Declaration *decl = arena_alloc(p->arena, sizeof(*decl));
    decl->location = loc;
    decl->serial = p->serial;
    p->serial += 1;
    arrput(p->workspace->declarations, decl);
    return decl;
}

static inline Ast_Type_Definition *make_type_definition(Parser *p, Source_Location loc, Ast_Type_Kind kind)
{
    Ast_Type_Definition *defn = ast_alloc(p, loc, AST_TYPE_DEFINITION, sizeof(*defn));
    defn->kind = kind;
    // defn->_expression.inferred_type = p->workspace->type_def_type;
    return defn;
}

static int operator_precedence_from_token_type(int type)
{
    switch (type) {
    case TOKEN_LOGICAL_OR: return 1;
    case TOKEN_LOGICAL_AND: return 2;

    case TOKEN_ISEQUAL:
    case TOKEN_ISNOTEQUAL:
    case TOKEN_LESSEQUALS:
    case TOKEN_GREATEREQUALS:
    case '<':
    case '>':
        return 3;

    case '+':
    case '-':
    case '|':
    case '^':
        return 4;

    case '*':
    case '/':
    case '%':
    case TOKEN_SHIFT_LEFT:
    case TOKEN_SHIFT_RIGHT:
    case '&':
        return 5;

    default:
        return 0;
    }
}

static inline void eat_semicolon(Parser *parser, Source_Location stmt_location)
{
    Token token = eat_next_token(parser);
    if (token.type != ';' && !parser->reported_error) {
        parser_report_error(parser, token.location, "Expected semicolon after statement.");
        parser_report_error(parser, stmt_location, "... the statement began here.");
    }
}

static inline Token eat_token_type(Parser *parser, Token_Type type, const char *error_message)
{
    Token token = eat_next_token(parser);
    // TODO: If we already reported an error, we just return the next token no matter what it is.
    // This can lead to some errors because you expect a token to be of a certain type and it may not be.
    if (token.type != (int)type && !parser->reported_error) {
        parser_report_error(parser, token.location, error_message);
    }
    return token;
}

static bool expression_requires_semicolon(Ast_Expression *expr)
{
    if (expr->kind == AST_TYPE_DEFINITION) {
        Ast_Type_Definition *defn = xx expr;
        switch (defn->kind) {
        case TYPE_DEF_STRUCT:
        case TYPE_DEF_ENUM:
            return false;
        case TYPE_DEF_NUMBER:
        case TYPE_DEF_IDENT:
        case TYPE_DEF_POINTER:
        case TYPE_DEF_ARRAY:
        case TYPE_DEF_LAMBDA:
        case TYPE_DEF_LITERAL:
        case TYPE_DEF_STRUCT_CALL:
            return true;
        }
    }
    if (expr->kind == AST_LAMBDA) return false;
    return true;
}

static bool declaration_requires_semicolon(Ast_Declaration *decl)
{
    if (decl->flags & DECLARATION_IS_PROCEDURE_BODY) return false;
    if (decl->root_expression) return expression_requires_semicolon(decl->root_expression);
    // Not sure when this can happen...
    return true;
}

static bool statement_requires_semicolon(Ast_Statement *stmt)
{
    switch (stmt->kind) {
    case AST_BLOCK: return false;
    case AST_WHILE: {
        Ast_While *while_stmt = xx stmt;
        return statement_requires_semicolon(while_stmt->then_statement);
    }
    case AST_IF: {
        Ast_If *if_stmt = xx stmt;
        if (if_stmt->else_statement) return statement_requires_semicolon(if_stmt->else_statement);
        return statement_requires_semicolon(if_stmt->then_statement);
    }
    case AST_LOOP_CONTROL:
    case AST_RETURN:
    case AST_USING:
    case AST_IMPORT:
        return true;
    case AST_EXPRESSION_STATEMENT: {
        Ast_Expression_Statement *expr = xx stmt;
        return expression_requires_semicolon(expr->subexpression);
    }
    case AST_VARIABLE: return true;
    case AST_ASSIGNMENT: return true;
    }
}

inline void *ast_alloc(Parser *p, Source_Location loc, unsigned int type, size_t size)
{
    Ast_Expression *ast = arena_alloc(p->arena, size);
    memset(ast, 0, size); // TODO: We could only memset the part AFTER Ast_Expression, but what about Ast_Statement...?
    ast->kind = type;
    ast->location = loc;
    return ast;
}

Ast_Expression *parse_binary_expression(Parser *p, Ast_Expression *left, int precedence)
{
    if (left == NULL) left = parse_unary_expression(p);

    Token token;
    Ast_Binary_Operator *bin;

    while (1) {
        token = peek_next_token(p);
        int token_precedence = operator_precedence_from_token_type(token.type);
        if (token_precedence < precedence || p->reported_error) {
            return left;
        }
        // TODO: Do we even check that it's a valid binary operator?
        eat_next_token(p);
        bin = ast_alloc(p, token.location, AST_BINARY_OPERATOR, sizeof(*bin));
        bin->left = left;
        bin->operator_type = token.type;
        bin->right = parse_binary_expression(p, NULL, token_precedence + 1);
        left = xx bin;
    }

    UNREACHABLE;
}

Ast_Expression *parse_unary_expression(Parser *p)
{
    Token token = peek_next_token(p);
    switch (token.type) {
    case '-':
    case '*':
    case '!':
    case TOKEN_BITWISE_NOT:
    {
        eat_next_token(p);
        Ast_Unary_Operator *unary = ast_alloc(p, token.location, AST_UNARY_OPERATOR, sizeof(*unary));
        unary->operator_type = token.type;
        unary->subexpression = parse_unary_expression(p);
        return xx unary;
    }
    default:
        return parse_primary_expression(p, NULL);
    }
}

Ast_Expression *parse_primary_expression(Parser *p, Ast_Expression *base)
{
    if (base == NULL) base = parse_base_expression(p);

    while (1) {
        if (p->reported_error) return base;
        
        Token token = peek_next_token(p);
        switch (token.type) {
        case '.': {
            eat_next_token(p);

            token = eat_next_token(p);

            switch (token.type) {
            case '*': {
                Ast_Selector *selector = ast_alloc(p, token.location, AST_SELECTOR, sizeof(*selector));
                selector->namespace_expression = base;
                selector->is_pointer_dereference = true;
                base = xx selector;
                break;
            }
            case '{': {
                Ast_Type_Instantiation *inst = ast_alloc(p, token.location, AST_TYPE_INSTANTIATION, sizeof(*inst));
                inst->type_definition = parse_type_definition(p, base);
                if (p->reported_error) return base;
                Ast_Expression *arg = parse_expression(p);
                arrput(inst->arguments, arg);
                while (peek_next_token(p).type == ',') {
                    if (p->reported_error) return xx inst;
                    eat_next_token(p); // eat comma
                    arg = parse_expression(p);
                    arrput(inst->arguments, arg);
                }
                eat_token_type(p, '}', "Missing closing curly brace around type instantiation arguments list.");
                base = xx inst;
                break;
            }
            case TOKEN_IDENT: {
                Ast_Selector *selector = ast_alloc(p, token.location, AST_SELECTOR, sizeof(*selector));
                selector->namespace_expression = base;
                selector->ident = make_identifier(p, token);
                base = xx selector;
                break;
            }
            default:
                parser_report_error(p, token.location, "Expected identifier after '.'.");
                break;
            }
            
            break;
        }
        case '[': // TODO: array subscript
            UNIMPLEMENTED;
        case '(': {
            eat_next_token(p);
            Ast_Procedure_Call *call = ast_alloc(p, token.location, AST_PROCEDURE_CALL, sizeof(*call));
            call->procedure_expression = base;

            if (peek_next_token(p).type == ')') {
                eat_next_token(p);
                base = xx call;
                break;
            }

            Ast_Expression *arg = parse_expression(p);
            arrput(call->arguments, arg);

            while (peek_next_token(p).type == ',') {
                eat_next_token(p);
                arg = parse_expression(p);
                if (p->reported_error) return xx call;
                arrput(call->arguments, arg);
            }
            eat_token_type(p, ')', "Expected closing parenthesis after procedure call argument list.");
            
            base = xx call;
            break;
        }
        case TOKEN_KEYWORD_AS: {
            eat_next_token(p);
            Ast_Cast *cast = ast_alloc(p, token.location, AST_CAST, sizeof(*cast));
            cast->type = parse_type_definition(p, NULL);
            cast->subexpression = base;

            base = xx cast;
            break;
        }
        default:
            return base;
        }
    }

    UNREACHABLE;
}

Ast_Expression *parse_base_expression(Parser *p)
{
    Token token = peek_next_token(p);
    switch (token.type) {
    case TOKEN_IDENT: {
        eat_next_token(p);

        // Check if we are a type.
        Ast_Type_Definition *literal_type_defn = parse_literal_type(p, token.string_value);
        if (literal_type_defn) return xx literal_type_defn;
            
        return xx make_identifier(p, token);
    }
        
    case TOKEN_NUMBER: {
        eat_next_token(p);
        Ast_Number *number = ast_alloc(p, token.location, AST_NUMBER, sizeof(*number));
        number->flags = token.number_flags;

        if (token.number_flags & NUMBER_FLAGS_FLOAT) {
            number->as.real = token.double_value;
        } else {
            number->as.integer = token.integer_value;
        }

        return xx number;
    }
        
    case TOKEN_STRING: {
        eat_next_token(p);
        Ast_Literal *lit = ast_alloc(p, token.location, AST_LITERAL, sizeof(*lit));
        lit->kind = LITERAL_STRING;
        lit->string_value = token.string_value;
        return xx lit;
    }
        
    case TOKEN_KEYWORD_TRUE: {
        eat_next_token(p);
        Ast_Literal *lit = ast_alloc(p, token.location, AST_LITERAL, sizeof(*lit));
        lit->kind = LITERAL_BOOL;
        lit->bool_value = 1;
        return xx lit;
    }

    case TOKEN_KEYWORD_FALSE: {
        eat_next_token(p);
        Ast_Literal *lit = ast_alloc(p, token.location, AST_LITERAL, sizeof(*lit));
        lit->kind = LITERAL_BOOL;
        lit->bool_value = 0;
        return xx lit;
    }

    case TOKEN_KEYWORD_NULL: {
        eat_next_token(p);
        Ast_Literal *lit = ast_alloc(p, token.location, AST_LITERAL, sizeof(*lit));
        lit->kind = LITERAL_NULL;
        return xx lit;
    }
        
    case TOKEN_KEYWORD_STRUCT:
        return xx parse_struct_desc(p);
        
    case TOKEN_KEYWORD_ENUM:
        return xx parse_enum_defn(p);

    case '(': {
        // nocheckin: remove because parse_declaration_value handles it now.
        token = peek_token(p, 1);
        switch (token.type) {
        case TOKEN_IDENT:
            if (peek_token(p, 2).type != ':') break;
            // fallthrough
        case ')':
        case TOKEN_KEYWORD_USING:
        {
            Ast_Type_Definition *lambda_type = parse_lambda_type(p);
            token = peek_next_token(p);
            if (token.type == '{' || token.type == '#') return xx parse_lambda_definition(p, lambda_type);
            return xx lambda_type;
        }
        }

        eat_next_token(p);
        Ast_Expression *subexpression = parse_expression(p);
        eat_token_type(p, ')', "Missing closing parenthesis around expression.");
        return subexpression;
    }

    case '.':
        // TODO: anonymous member access
        UNIMPLEMENTED;
    }

    if (!p->reported_error) parser_report_error(p, token.location, "Expected a base expression (operand) but got %s.", token_type_to_string(token.type));
    return NULL;
}

Ast_Lambda *parse_lambda_definition(Parser *p, Ast_Type_Definition *lambda_type)
{
    if (!lambda_type) lambda_type = parse_lambda_type(p);

    if (peek_next_token(p).type == '#') {
        eat_next_token(p);
        if (isalpha(peek_character(p))) {
            Token token = eat_token_type(p, TOKEN_IDENT, "Expected an identifier because we parsed an alphabetic character after '#'.");

            if (sv_eq(token.string_value, sv_from_cstr("foreign"))) {
                // TODO: parse an identifier with which library the declaration is from.
                Ast_Lambda *lambda = ast_alloc(p, lambda_type->_expression.location, AST_LAMBDA, sizeof(*lambda));
                lambda->type_definition = lambda_type;
                lambda->my_body_declaration = make_declaration(p, token.location);
                lambda->my_body_declaration->flags = DECLARATION_IS_CONSTANT | DECLARATION_IS_PROCEDURE_BODY | DECLARATION_IS_FOREIGN;
                lambda->my_body_declaration->my_type = lambda_type;
                lambda->is_foreign = true;
                Exit_Block(p, lambda_type->lambda.arguments_block);
                return xx lambda;
            }

            parser_report_error(p, token.location, "Unknown directive '"SV_Fmt"'.", SV_Arg(token.string_value));
        }
    }

    Token token = eat_token_type(p, '{', "Expected opening curly brace after lambda type.");

    Ast_Lambda *lambda = ast_alloc(p, token.location, AST_LAMBDA, sizeof(*lambda));
    lambda->type_definition = lambda_type;
    lambda->my_body_declaration = make_declaration(p, token.location);
    lambda->my_body_declaration->flags = DECLARATION_IS_CONSTANT | DECLARATION_IS_PROCEDURE_BODY;
    lambda->my_body_declaration->my_type = lambda_type;
    lambda->my_body_declaration->my_block = ast_alloc(p, token.location, AST_BLOCK, sizeof(Ast_Block));
    lambda->my_body_declaration->my_block->belongs_to = BLOCK_BELONGS_TO_LAMBDA;
    lambda->my_body_declaration->my_block->belongs_to_data = lambda;

    Ast_Lambda *previous_lambda = p->current_lambda;

    p->current_lambda = lambda;
    parse_into_block(p, lambda->my_body_declaration->my_block);
    Exit_Block(p, lambda_type->lambda.arguments_block);
    p->current_lambda = previous_lambda;

    return lambda;
}

Ast_Declaration *parse_lambda_argument(Parser *p, unsigned index)
{
    uint32_t flags = DECLARATION_IS_LAMBDA_ARGUMENT;

    Token token = eat_next_token(p);

    // Check for using.
    if (token.type == TOKEN_KEYWORD_USING) {
        UNIMPLEMENTED; // TODO: We just need to add a using statement to the block, problem is, that we could just be a type.
        // flags |= DECLARATION_IS_USING;
        token = eat_next_token(p);
    }

    // Check for polymorph.
    if (token.type == '$') {
        flags |= DECLARATION_IS_POLYMORPHIC;
        token = eat_next_token(p);
    }

    if (token.type != TOKEN_IDENT) {
        parser_report_error(p, token.location, "Expected identifier after '%s'.", token_type_to_string(token.type));
    }

    // Parse the parameter declaration.
    Ast_Declaration *decl = make_declaration(p, token.location);
    decl->ident = make_identifier(p, token);
    decl->flags = flags;

    // Add the declaration to the block and create a variable.
    checked_add_to_scope(p, p->current_block, decl);

    Ast_Variable *var = ast_alloc(p, decl->location, AST_VARIABLE, sizeof(*var));
    var->declaration = decl;
    var->lambda_argument_index = index;
    arrput(p->current_block->statements, xx var);

    eat_token_type(p, ':', "Expected ':' after lambda argument name.");

    decl->my_type = parse_type_definition(p, NULL);

    // TODO: this doesn't handle default values for arguments.
    // I think we can just say the expression has to be a constant, and go ahead and set the type definition here.
    return decl;
}

Ast_Type_Definition *parse_struct_desc(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == TOKEN_KEYWORD_STRUCT);

    // Parse the struct's block.
    // Note: This will need to change when we introduce struct parameters.
    token = eat_token_type(p, '{', "Expected '{' after 'struct'.");
    
    Ast_Struct *struct_desc = arena_alloc(p->arena, sizeof(*struct_desc));
    struct_desc->block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*struct_desc->block));
    struct_desc->block->belongs_to = BLOCK_BELONGS_TO_STRUCT;
    struct_desc->block->belongs_to_data = struct_desc;

    parse_into_block(p, struct_desc->block);

    Ast_Type_Definition *defn = make_type_definition(p, token.location, TYPE_DEF_STRUCT);
    defn->struct_desc = struct_desc;
    return defn;
}

Ast_Type_Definition *parse_enum_defn(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == TOKEN_KEYWORD_ENUM);

    Ast_Enum *enum_defn = arena_alloc(p->arena, sizeof(*enum_defn));
    Ast_Type_Definition *defn = make_type_definition(p, token.location, TYPE_DEF_ENUM);
    defn->enum_defn = enum_defn;

    // Parse underlying int type.
    token = peek_next_token(p);
    if (token.type == TOKEN_IDENT) {
        eat_next_token(p);
        enum_defn->underlying_int_type = parse_literal_type(p, token.string_value);
        if (!enum_defn->underlying_int_type) {
            parser_report_error(p, token.location, "Expected an integer literal type name after 'enum' (aliases are not currently implemented).");
            return defn;
        }
    } else {
        enum_defn->underlying_int_type = p->workspace->type_def_u32; // TODO: check this.
    }

    token = eat_token_type(p, '{', "Expected '{' after 'enum'.");

    enum_defn->block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*enum_defn->block));
    enum_defn->block->belongs_to = BLOCK_BELONGS_TO_ENUM;
    enum_defn->block->belongs_to_data = enum_defn;

    while (1) {
        if (peek_next_token(p).type == '}') {
            eat_next_token(p);
            return defn;
        }
        
        token = eat_token_type(p, TOKEN_IDENT, "Expected an identifier to start enum member declaration.");
        eat_token_type(p, ':', "Expected ':' after identifier.");
        if (p->reported_error) return defn;

        // After the first colon, we usually expect a type or ':' or '='.
        // However, enums members can't have different types, so we don't allow a type,
        // and we also don't allow non-constant members because you can't "allocate" an enum.
        Token eq_or_colon = eat_next_token(p);
        if (eq_or_colon.type == '=') {
            // TODO: Should we print the location of the start of the declaration (the identifier) or the location of the '='?
            parser_report_error(p, eq_or_colon.location, "Enum members must be declared as constant (replace '=' with ':').");
            return defn;
        }
        if (eq_or_colon.type != ':') {
            parser_report_error(p, eq_or_colon.location, "Expected ':' after ':'.");   
            return defn;
        }

        Ast_Expression *value = parse_expression(p);
        eat_token_type(p, ';', "Expected semicolon after declaration.");
        Ast_Declaration *member = make_declaration(p, token.location);
        member->ident = make_identifier(p, token);
        member->my_type = defn; // The type of the declaration is the enum type.
        member->root_expression = value;
        member->flags = DECLARATION_IS_CONSTANT | DECLARATION_IS_ENUM_VALUE;
        arrput(enum_defn->block->declarations, member);

        if (p->reported_error) return defn;
    }
    
    return defn;
}

// @Volatile: The scope opened for the arguments is not closed by this function.
// This is so that when parsing a lambda definition, we can have the body's parent be the arguments.
Ast_Type_Definition *parse_lambda_type(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == '(');

    Source_Location location = token.location;

    Ast_Type_Definition *type_definition = make_type_definition(p, location, TYPE_DEF_LAMBDA);

    // Open a scope for the arguments.
    
    type_definition->lambda.arguments_block = ast_alloc(p, token.location, AST_BLOCK, sizeof(Ast_Block));
    type_definition->lambda.arguments_block->belongs_to = BLOCK_IS_LAMBDA_ARGUMENTS;
    Enter_Block(p, type_definition->lambda.arguments_block);

    // Check for closing paren, meaning an empty argument list.

    if (peek_next_token(p).type == ')') {
        eat_next_token(p);
        token = peek_next_token(p); 
        if (token.type == TOKEN_RIGHT_ARROW) {
            eat_next_token(p);
            type_definition->lambda.return_type = parse_type_definition(p, NULL);
        } else {
            type_definition->lambda.return_type = p->workspace->type_def_void;
        }
        return type_definition;
    }

    // Otherwise, parse a list of arguments.

    unsigned count = 0;

    while (1) {
        if (peek_next_token(p).type == TOKEN_DOUBLE_DOT) {
            eat_next_token(p);
            type_definition->lambda.variadic = true;
            eat_token_type(p, ')', "Expected ',' or ')' after lambda argument declaration.");
            token = peek_next_token(p); 
            if (token.type == TOKEN_RIGHT_ARROW) {
                eat_next_token(p);
                type_definition->lambda.return_type = parse_type_definition(p, NULL);
            } else {
                type_definition->lambda.return_type = p->workspace->type_def_void;
            }
            return type_definition;
        }
        
        Ast_Declaration *parameter = parse_lambda_argument(p, count);

        assert(parameter->my_type); // TODO: We want to be able to put "name := value" in procedure type.
        arrput(type_definition->lambda.argument_types, parameter->my_type);
        count += 1;

        if (p->reported_error) return type_definition;
        
        token = eat_next_token(p);
        if (token.type == ',') continue;

        // Check for closing parenthesis to finish lambda type.
        if (token.type == ')') {
            token = peek_next_token(p); 
            if (token.type == TOKEN_RIGHT_ARROW) {
                eat_next_token(p);
                type_definition->lambda.return_type = parse_type_definition(p, NULL);
            } else {
                type_definition->lambda.return_type = p->workspace->type_def_void;
            }
            return type_definition;
        }
        
        parser_report_error(p, token.location, "Expected ',' or ')' after lambda argument declaration.");
    }

    UNREACHABLE;
}

Ast_Type_Definition *parse_literal_type(Parser *p, String_View lit)
{
    switch (lit.count) {
    case 2:
        if (sv_eq(lit, SV("u8"))) return p->workspace->type_def_u8;
        if (sv_eq(lit, SV("s8"))) return p->workspace->type_def_s8;
        break;
    case 3:
        if (sv_eq(lit, SV("int"))) return p->workspace->type_def_int;
        if (sv_eq(lit, SV("u16"))) return p->workspace->type_def_u16;
        if (sv_eq(lit, SV("u32"))) return p->workspace->type_def_u32;
        if (sv_eq(lit, SV("u64"))) return p->workspace->type_def_u64;
        if (sv_eq(lit, SV("s16"))) return p->workspace->type_def_s16;
        if (sv_eq(lit, SV("s32"))) return p->workspace->type_def_s32;
        if (sv_eq(lit, SV("s64"))) return p->workspace->type_def_s64;
        break;
    case 4:
        if (sv_eq(lit, SV("bool"))) return p->workspace->type_def_bool;
        if (sv_eq(lit, SV("void"))) return p->workspace->type_def_void;
        if (sv_eq(lit, SV("Type"))) return p->workspace->type_def_type;
        break;
    case 5:
        if (sv_eq(lit, SV("float"))) return p->workspace->type_def_float;
        break;
    case 6:
        if (sv_eq(lit, SV("string"))) return p->workspace->type_def_string; break;
    case 7:
        if (sv_eq(lit, SV("float64"))) return p->workspace->type_def_float64; break;
        if (sv_eq(lit, SV("float32"))) return p->workspace->type_def_float64; break;
    }
    return NULL;
}

// parse_type_definition() either converts an expression or parses a new one if NULL is passed.
Ast_Type_Definition *parse_type_definition(Parser *parser, Ast_Expression *type_expression)
{
    if (type_expression != NULL) {
        switch (type_expression->kind) {
        // If we're already a type, no conversion needed.
        case AST_TYPE_DEFINITION:
            return (Ast_Type_Definition *)type_expression;

        case AST_IDENT: {
            Ast_Ident *ident = xx type_expression;

            Ast_Type_Definition *literal_type_defn = parse_literal_type(parser, ident->name);
            if (literal_type_defn) return literal_type_defn;

            Ast_Type_Definition *defn = make_type_definition(parser, type_expression->location, TYPE_DEF_IDENT);
            defn->type_name = ident;
            return defn;
        }
            
        case AST_UNARY_OPERATOR: {
            Ast_Unary_Operator *unary = xx type_expression;
            if (unary->operator_type != '*') {
                parser_report_error(parser, type_expression->location, "Here we expected a type, but we got unary operator '%c'.", unary->operator_type);
            }
                
            Ast_Type_Definition *defn = make_type_definition(parser, type_expression->location, TYPE_DEF_POINTER);

            // Unroll unary '*'.
            // while (type_expression->kind == AST_UNARY_OPERATOR) {
            //     Ast_Unary_Operator *unary = xx type_expression;
            //     if (unary->operator_type != '*') {
            //         parser_report_error(parser, type_expression->location, "Here we expected a type, but we got unary operator '%c'.", unary->operator_type);
            //     }
            //     defn->pointer_level += 1;
            //     type_expression = unary->subexpression;
            // }
            defn->pointer_to = parse_type_definition(parser, unary->subexpression);
            return defn;
        }

        case AST_SELECTOR:
            UNIMPLEMENTED;

        case AST_NUMBER:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a number.");
        case AST_LITERAL:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a literal value.");
        case AST_BINARY_OPERATOR:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a binary operation.");
        case AST_CAST:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a cast.");
        case AST_LAMBDA:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a lambda.");
        case AST_TYPE_INSTANTIATION:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got an initializer argument list.");

        case AST_PROCEDURE_CALL: {
            Ast_Type_Definition *defn = make_type_definition(parser, type_expression->location, TYPE_DEF_STRUCT_CALL);
            defn->struct_call = (Ast_Procedure_Call *)type_expression;
            return defn;
        }
        }
    }

    Token token = peek_next_token(parser);   
    switch (token.type) {
    case '*': {
        eat_next_token(parser);
        Ast_Type_Definition *defn = make_type_definition(parser, token.location, TYPE_DEF_POINTER);
        defn->pointer_to = parse_type_definition(parser, NULL);
        return defn;
    }
    case '[': {
        eat_next_token(parser);

        Ast_Type_Definition *defn = make_type_definition(parser, token.location, TYPE_DEF_ARRAY);

        token = peek_next_token(parser);
        switch (token.type) {
        case ']':
            eat_next_token(parser); // ]
            defn->array.length = -1;
            defn->array.element_type = parse_type_definition(parser, NULL);
            return defn;
        case TOKEN_NUMBER:
            eat_next_token(parser); // number
            if (token.number_flags & NUMBER_FLAGS_FLOAT) {
                parser_report_error(parser, token.location, "Array length cannot be a float.");
            }
            eat_token_type(parser, ']', "Missing closing bracket after array length.");
            defn->array.length = token.integer_value;
            defn->array.element_type = parse_type_definition(parser, NULL);
            return defn;
        case TOKEN_DOUBLE_DOT:
            eat_next_token(parser); // ..
            eat_token_type(parser, ']', "Missing closing bracket after array length.");
            defn->array.length = LLONG_MIN;
            defn->array.element_type = parse_type_definition(parser, NULL);
            return defn;
        default:
            parser_report_error(parser, token.location, "Here we expected a type, but we got '%s'.", token_type_to_string(token.type));
        }
    }
    case TOKEN_IDENT: {
        eat_next_token(parser);

        Ast_Type_Definition *literal_type_defn = parse_literal_type(parser, token.string_value);
        if (literal_type_defn) return literal_type_defn;

        Ast_Type_Definition *defn = make_type_definition(parser, token.location, TYPE_DEF_IDENT);
        defn->type_name = make_identifier(parser, token);
        return defn;
    }
    case '(': {
        Ast_Type_Definition *lambda_type = parse_lambda_type(parser);
        Exit_Block(parser, lambda_type->lambda.arguments_block);
        return lambda_type;
    }
    case TOKEN_KEYWORD_STRUCT:
        return parse_struct_desc(parser);
    case TOKEN_KEYWORD_ENUM:
        return parse_enum_defn(parser);
    default:
        parser_report_error(parser, token.location, "Here we expected a type, but we got '%s'.", token_type_to_string(token.type));
    }

    UNREACHABLE;
}

Ast_Statement *parse_if_statement(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == TOKEN_KEYWORD_IF);

    Ast_If *if_stmt = ast_alloc(p, token.location, AST_IF, sizeof(*if_stmt));
    
    if_stmt->condition_expression = parse_expression(p);

    // Check for "then" keyword and consume it.
    token = peek_next_token(p);
    if (token.type == TOKEN_KEYWORD_THEN) {
        eat_next_token(p);
    }
    if_stmt->then_statement = parse_statement(p);

    // Check for "else" keyword and consume it.
    token = peek_next_token(p);
    if (token.type == TOKEN_KEYWORD_ELSE) {
        eat_next_token(p);
        if_stmt->else_statement = parse_statement(p);
    }

    return xx if_stmt;
}

void parse_into_block(Parser *p, Ast_Block *block)
{
    Enter_Block(p, block);
    while (1) {
        Token token = peek_next_token(p);
        if (token.type == '}') {
            eat_next_token(p);
            Exit_Block(p, block);
            return;
        }

        // Catch this before handing it off to parse_statement for a better error message.
        if (token.type == TOKEN_END_OF_INPUT) {
            parser_report_error(p, token.location, "Reached the end of the input before terminating curly brace.");
            // TODO: Print where the block started.
            // TODO: How can we make this type of error message better? We want to be able
            // to track the exact place where we are missing a brace, not just the end.
            Exit_Block(p, block);
            return;
        }

        Ast_Statement *stmt = parse_statement(p);
        if (!stmt) continue; // If we parsed a constant declaration.
        arrput(block->statements, stmt);

        if (statement_requires_semicolon(xx stmt)) {
            eat_semicolon(p, stmt->location);
        } else {
            // Check for a semicolon anyways.
            token = peek_next_token(p);
            if (token.type == ';') eat_next_token(p);
        }

        if (p->reported_error) {
            Exit_Block(p, block);
            return;
        }
    }

    UNREACHABLE;
}

// Wrapper around parse_into_block that allocates, enters, and exits the block for you.
inline Ast_Block *parse_block(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == '{');

    Ast_Block *block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*block));
    parse_into_block(p, block);

    return block;
}

Ast_Statement *parse_assignment(Parser *p, Ast_Expression *pointer_expression)
{
    Token token = eat_next_token(p);

    Ast_Assignment *assign = ast_alloc(p, token.location, AST_ASSIGNMENT, sizeof(*assign));
    assign->pointer = pointer_expression;
    assign->value = parse_expression(p);
    assign->operator_type = token.type;

    return xx assign;
}

Ast_Statement *parse_statement(Parser *p)
{
    Token token = peek_next_token(p);
    switch (token.type) {
    case TOKEN_KEYWORD_RETURN: {
        eat_next_token(p);        

        if (!p->current_lambda) {
            parser_report_error(p, token.location, "Cannot use 'return' outside of a procedure.");
        }
        Ast_Return *ret = ast_alloc(p, token.location, AST_RETURN, sizeof(*ret));
        ret->subexpression = parse_expression(p);
        ret->lambda_i_belong_to = p->current_lambda;
        return xx ret;
    }

    case TOKEN_KEYWORD_IF:
        return parse_if_statement(p);

    case TOKEN_KEYWORD_WHILE: {
        eat_next_token(p);

        Ast_Statement *previous_loop = p->current_loop;
            
        Ast_While *while_stmt = ast_alloc(p, token.location, AST_WHILE, sizeof(*while_stmt));
        while_stmt->condition_expression = parse_expression(p);

        p->current_loop = xx while_stmt;

        // Check for "then" keyword and consume it.
        // TODO: do we want/need this?
        token = peek_next_token(p);
        if (token.type == TOKEN_KEYWORD_THEN) {
            eat_next_token(p);
        }
        while_stmt->then_statement = parse_statement(p);

        p->current_loop = previous_loop;

        return xx while_stmt;
    }

    case TOKEN_KEYWORD_BREAK:
    case TOKEN_KEYWORD_CONTINUE:
    {
        eat_next_token(p);

        if (!p->current_loop) {
            parser_report_error(p, token.location, "Cannot use '%s' outside of a loop.", token_type_to_string(token.type));
        }
                
        Ast_Loop_Control *loop_control = ast_alloc(p, token.location, AST_LOOP_CONTROL, sizeof(*loop_control));
        loop_control->keyword_type = token.type;
        // TODO: We want to parse an expression/identifier here for labeled break/continue statements.
        return xx loop_control;
    }

    case TOKEN_KEYWORD_USING: {
        UNIMPLEMENTED;
        Ast_Using *using = ast_alloc(p, token.location, AST_USING, sizeof(*using));
        using->subexpression = parse_expression(p); // TODO: "using e: Entity;" We need to handle declarations.
        return xx using;
    }

    case '#': {
        eat_next_token(p);

        if (isalpha(peek_character(p))) {
            token = eat_token_type(p, TOKEN_IDENT, "Expected an identifier because we parsed an alphabetic character after '#'.");
            if (sv_eq(token.string_value, sv_from_cstr("import_system_library"))) {
                Source_Location loc = token.location;

                token = eat_token_type(p, TOKEN_STRING, "Expected a string constant with the library path after #import_system_library.");

                Ast_Import *import = ast_alloc(p, loc, AST_IMPORT, sizeof(*import)); 
                import->path_name = token.string_value;
                import->is_system_library = true;
                return xx import;
            }
        }

        parser_report_error(p, token.location, "Expected a statement but got '#'.");
        parser_report_error(p, token.location, "... if this is meant to be a directive then an identifier must be directly after the hash.");
        return NULL;
    }

    case '{':
        return xx parse_block(p);

    case TOKEN_IDENT:
        token = peek_token(p, 1);
        if (token.type == ':') {
            Ast_Declaration *decl = parse_declaration(p);

            // Check for variable declaration.
            if (!(decl->flags & DECLARATION_IS_CONSTANT)) {
                // We are a variable declaration!
                Ast_Variable *var = ast_alloc(p, decl->location, AST_VARIABLE, sizeof(*var));
                var->declaration = decl;
                return xx var;
            }

            // We are a constant declaration.
            if (declaration_requires_semicolon(decl)) {
                eat_semicolon(p, decl->location);
            } else {
                token = peek_next_token(p);
                if (token.type == ';') eat_next_token(p);
            }
                
            return NULL;
        }

        // Parse assignment.
        Ast_Expression *expr = parse_expression(p);
        if (!expr) return NULL;
        token = peek_next_token(p);
        switch (token.type) {
        case '=':
        case TOKEN_PLUSEQUALS:
        case TOKEN_MINUSEQUALS:
        case TOKEN_TIMESEQUALS:
        case TOKEN_DIVEQUALS:
        case TOKEN_MODEQUALS:
            return parse_assignment(p, expr);
        default: {
            Ast_Expression_Statement *stmt = ast_alloc(p, expr->location, AST_EXPRESSION_STATEMENT, sizeof(*stmt));
            stmt->subexpression = expr;
            return xx stmt;
        }
        }
    }

    Ast_Expression *expr = parse_expression(p);
    if (!expr) return NULL;
    Ast_Expression_Statement *stmt = ast_alloc(p, expr->location, AST_EXPRESSION_STATEMENT, sizeof(*stmt));
    stmt->subexpression = expr;
    return xx stmt;
}

void parse_declaration_value(Parser *p, Ast_Declaration *decl)
{
    Token token = eat_next_token(p);

    if (token.type == ':') {
        decl->flags |= DECLARATION_IS_CONSTANT;
        decl->root_expression = parse_expression(p);

        if (decl->root_expression->kind == AST_LAMBDA) {
            decl->flags |= DECLARATION_IS_PROCEDURE_HEADER;
        }

        // If we have a block, we need to set it on the declaration.
        if (decl->root_expression->kind == AST_TYPE_DEFINITION) {
            Ast_Type_Definition *defn = xx decl->root_expression;
            switch (defn->kind) {
            case TYPE_DEF_STRUCT: decl->my_block = defn->struct_desc->block; break;
            case TYPE_DEF_ENUM:   decl->my_block = defn->enum_defn->block; break;
            case TYPE_DEF_LAMBDA: decl->my_block = defn->lambda.arguments_block; break;
            default: break;
            }
        }

        return;
    }
    
    if (token.type == '=') {
        // We parse the expression and create a type instantiation in the current block.
        Ast_Expression *initializer = parse_expression(p);
        
        if (p->current_block->belongs_to == BLOCK_BELONGS_TO_STRUCT) {
            // This means we are a struct field.
            decl->flags |= DECLARATION_IS_STRUCT_FIELD;
            Ast_Struct *struct_desc = p->current_block->belongs_to_data;
            decl->struct_field_index = struct_desc->field_count;
            struct_desc->field_count += 1;
        }

        decl->root_expression = initializer;
        return;
    }

    parser_report_error(p, token.location, "Expected ':' or '=' before the value of the declaration.");
}

// Assumes identifier has been consumed.
Ast_Declaration *parse_declaration(Parser *p)
{
    Token token = eat_token_type(p, TOKEN_IDENT, "Missing identifier for declaration.");
    eat_token_type(p, ':', "Missing ':' after identifier.");

    Ast_Declaration *decl = make_declaration(p, token.location);
    decl->ident = make_identifier(p, token);
    decl->ident->resolved_declaration = decl; // TODO: Does this work for overloads.

    // Default to not being a struct field, this gets set in parse_declaration_value.
    decl->struct_field_index = -1;

    // Flag us depending on the type of block we are in.
    if (p->current_block->belongs_to == BLOCK_BELONGS_TO_ENUM) {
        decl->flags |= DECLARATION_IS_ENUM_VALUE;
    }    

    // Add the declaration to the block.
    checked_add_to_scope(p, p->current_block, decl);

    // After first colon has been parsed, we expect an optional type definition and then ':' or '='.

    token = peek_next_token(p);

    if (token.type == ':' || token.type == '=') {
        parse_declaration_value(p, decl);
        return decl;
    }

    // Otherwise, parse a type.

    // parse_type_definition() either converts an expression or parses a new one if NULL is passed.
    decl->my_type = parse_type_definition(p, NULL);

    token = peek_next_token(p);
    if (token.type == ',' || token.type == ';') {
        if (p->current_block->belongs_to == BLOCK_BELONGS_TO_STRUCT) {
            // This means we are a struct field.
            decl->flags |= DECLARATION_IS_STRUCT_FIELD;
            Ast_Struct *struct_desc = p->current_block->belongs_to_data;
            decl->struct_field_index = struct_desc->field_count;
            struct_desc->field_count += 1;
        }

        // This means decl->root_expression will be null, which is fine because we will set it to the default value later.
        return decl;
    }

    parse_declaration_value(p, decl);
    return decl;
}

Ast_Block *parse_toplevel(Parser *p)
{
    // TODO: what do we set the location to?
    p->current_block = ast_alloc(p, (Source_Location){0}, AST_BLOCK, sizeof(Ast_Block));

    while (1) {
        if (p->reported_error) return p->current_block;

        Token token = peek_next_token(p);
        if (token.type == TOKEN_END_OF_INPUT) {
            eat_next_token(p);
            return p->current_block;
        }

        Ast_Statement *stmt = parse_statement(p);
        if (!stmt) continue; // If we parsed a constant declaration.
        arrput(p->current_block->statements, stmt);

        if (statement_requires_semicolon(stmt)) {
            eat_semicolon(p, stmt->location);
        } else {
            // Check for a semicolon anyways.
            token = peek_next_token(p);
            if (token.type == ';') eat_next_token(p);
        }
    }

    UNREACHABLE;
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

void parser_report_error(Parser *parser, Source_Location loc, const char *format, ...)
{
    va_list args;
    va_start(args, format);

    if (loc.l1 < 0) loc.l1 = loc.l0;
    if (loc.c1 < 0) loc.c1 = loc.c0;

    Source_File file = parser->workspace->files[parser->file_index];

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

    parser->reported_error = true;
    
    va_end(args);
}

inline Parser *parser_init(Workspace *w, int file_index)
{
    Parser *parser = malloc(sizeof(*parser));
    memset(parser, 0, sizeof(*parser));
    parser->file_index = file_index;
    parser->workspace = w;
    parser->arena = context_arena;
    parser->current_input = sv_from_parts(w->files[file_index].data, w->files[file_index].size);
    parser->current_line_number = -1;
    return parser;
}

inline Ast_Expression *parse_expression(Parser *p)
{
    return parse_binary_expression(p, NULL, 1);
}

void checked_add_to_scope(Parser *p, Ast_Block *block, Ast_Declaration *decl)
{
    if (decl->ident) {
        For (block->declarations) {
            if (!block->declarations[it]->ident) continue;
            if (sv_eq(block->declarations[it]->ident->name, decl->ident->name)) {
                parser_report_error(p, decl->ident->_expression.location, "Redeclared identifier '"SV_Fmt"'.", SV_Arg(decl->ident->name));
                parser_report_error(p, block->declarations[it]->ident->_expression.location, "... the first declaration was here.");
                // TODO: Print other declaration location.
            }
        }
    }
    arrput(block->declarations, decl);
}

Ast_Declaration *find_declaration_in_block(const Ast_Block *block, String_View name)
{
    For (block->declarations) {
        if (!block->declarations[it]->ident) continue;

        if (sv_eq(block->declarations[it]->ident->name, name)) {
            return block->declarations[it];
        }
    }
    return NULL;
}

Ast_Declaration *find_declaration_from_identifier(const Ast_Ident *ident)
{
    Ast_Block *block = ident->enclosing_block;
    while (block) {
        Ast_Declaration *decl = find_declaration_in_block(block, ident->name);
        if (decl) return decl;
        block = block->parent;
    }
    return NULL;
}

const char *expr_to_string(Ast_Expression *expr)
{
    Push_Arena(&temporary_arena);
    String_Builder sb = {0};
    print_expr_to_builder(&sb, expr, 0);
    sb_append(&sb, "\0", 1);
    Pop_Arena();
    return sb.data;
}

const char *type_to_string(Ast_Type_Definition *defn)
{
    Push_Arena(&temporary_arena);
    String_Builder sb = {0};
    print_type_to_builder(&sb, defn);
    sb_append(&sb, "\0", 1);
    Pop_Arena();
    return sb.data;
}

const char *stmt_to_string(Ast_Statement *stmt)
{
    Push_Arena(&temporary_arena);
    String_Builder sb = {0};
    print_stmt_to_builder(&sb, stmt, 0);
    sb_append(&sb, "\0", 1);
    Pop_Arena();
    return sb.data;
}

void print_expr_to_builder(String_Builder *sb, const Ast_Expression *expr, size_t depth)
{
    if (!expr) return;
    while (expr->replacement) expr = expr->replacement;
    switch (expr->kind) {
    case AST_NUMBER: {
        const Ast_Number *number = xx expr;
        if (number->flags & NUMBER_FLAGS_FLOAT) {
            sb_print(sb, "%f", number->as.real);
            break;
        }
        sb_print(sb, "%lu", number->as.integer);
        break;
    }
    case AST_LITERAL: {
        const Ast_Literal *literal = xx expr;
        if (literal->kind == LITERAL_BOOL) {
            sb_append_cstr(sb, literal->bool_value ? "true" : "false");
            break;
        }
        if (literal->kind == LITERAL_STRING) {
            sb_append_cstr(sb, "\"");
            sb_append(sb, literal->string_value.data, literal->string_value.count);
            sb_append_cstr(sb, "\"");
            break;
        }
        if (literal->kind == LITERAL_NULL) {
            sb_append_cstr(sb, "null");
            break;
        }
        // Malformed literal.
        sb_append_cstr(sb, "<literal>");
        break;
    }
    case AST_IDENT: {
        const Ast_Ident *ident = xx expr;
        sb_append(sb, ident->name.data, ident->name.count);
        break;
    }
    case AST_UNARY_OPERATOR: {
        const Ast_Unary_Operator *unary = xx expr;
        sb_append_cstr(sb, token_type_to_string(unary->operator_type));
        print_expr_to_builder(sb, unary->subexpression, depth);
        break;
    }
    case AST_BINARY_OPERATOR: {
        const Ast_Binary_Operator *binary = xx expr;
        print_expr_to_builder(sb, binary->left, depth);
        sb_append_cstr(sb, " ");
        sb_append_cstr(sb, token_type_to_string(binary->operator_type));
        sb_append_cstr(sb, " ");
        print_expr_to_builder(sb, binary->right, depth);
        break;
    }
    case AST_LAMBDA: {
        // const Ast_Lambda *lambda = xx expr;
        sb_append_cstr(sb, "<lambda>");
        // print_type_to_builder(sb, lambda->type_definition);
        // sb_append_cstr(sb, " ");
        // print_stmt_to_builder(sb, xx lambda->my_body_declaration->my_block, depth);
        break;
    }
    case AST_PROCEDURE_CALL: {
        const Ast_Procedure_Call *call = xx expr;       
        print_expr_to_builder(sb, call->procedure_expression, depth);
        sb_append_cstr(sb, "(");
        For (call->arguments) {
            if (it > 0) sb_append_cstr(sb, ", ");
            print_expr_to_builder(sb, call->arguments[it], depth);
        }
        sb_append_cstr(sb, ")");
        break;
    }
    case AST_TYPE_DEFINITION:
        print_type_to_builder(sb, xx expr);
        break;
    case AST_CAST: {
        const Ast_Cast *cast = xx expr;
        print_type_to_builder(sb, cast->type);
        sb_append_cstr(sb, " as ");
        print_expr_to_builder(sb, cast->subexpression, depth);
        break;
    }
    case AST_SELECTOR: {
        const Ast_Selector *selector = xx expr;
        print_expr_to_builder(sb, selector->namespace_expression, depth);
        sb_append_cstr(sb, ".");
        if (selector->is_pointer_dereference) sb_append_cstr(sb, "*");
        else  print_expr_to_builder(sb, xx selector->ident, depth);
        break;
    }
    case AST_TYPE_INSTANTIATION: {
        const Ast_Type_Instantiation *inst = xx expr;
        print_type_to_builder(sb, inst->type_definition);
        sb_append_cstr(sb, "{\n");
        depth += 1;
        For (inst->arguments) {
            for (size_t i = 0; i < depth; ++i) sb_append_cstr(sb, "    ");
            print_expr_to_builder(sb, inst->arguments[it], depth);
            sb_append_cstr(sb, ",\n");
        }
        sb_append_cstr(sb, "}");
        break;
    }
    }
}

void print_type_to_builder(String_Builder *sb, const Ast_Type_Definition *defn)
{
    if (!defn) {
        sb_append_cstr(sb, "<null>");
        return;
    }

    switch (defn->kind) {
    case TYPE_DEF_NUMBER:
    case TYPE_DEF_LITERAL:
        sb_append_cstr(sb, defn->name);
        break;
    case TYPE_DEF_STRUCT:
        sb_append_cstr(sb, "struct { ");
        For (defn->struct_desc->block->declarations) {
            if (defn->struct_desc->block->declarations[it]->flags & DECLARATION_IS_CONSTANT) continue;
            if (it > 0) sb_append_cstr(sb, ", ");
            print_decl_to_builder(sb, defn->struct_desc->block->declarations[it], 0);
        }
        sb_append_cstr(sb, " }");
        break;
    case TYPE_DEF_ENUM:
        // TODO: What do we do here?
        sb_append_cstr(sb, "enum");
        break;
    case TYPE_DEF_IDENT:
        sb_append_cstr(sb, "`"); // nocheckin: this is so we can see that it's an identifier.
        sb_append(sb, defn->type_name->name.data, defn->type_name->name.count);
        break;
    case TYPE_DEF_STRUCT_CALL:
    case TYPE_DEF_POINTER:
        sb_append(sb, "*", 1);
        print_type_to_builder(sb, defn->pointer_to);
        break;
    case TYPE_DEF_ARRAY:
        if (defn->array.length < 0) {
            sb_append_cstr(sb, "[] ");
        } else {
            sb_print(sb, "[%lld] ", defn->array.length);
        }
        print_type_to_builder(sb, defn->array.element_type);
        break;
    case TYPE_DEF_LAMBDA:
        sb_append_cstr(sb, "(");
        For (defn->lambda.argument_types) {
            if (it > 0) sb_append_cstr(sb, ", ");
            print_type_to_builder(sb, defn->lambda.argument_types[it]);
        }
        sb_append_cstr(sb, ") -> ");
        print_type_to_builder(sb, defn->lambda.return_type);
        break;
    }
}

void print_stmt_to_builder(String_Builder *sb, const Ast_Statement *stmt, size_t depth)
{
    switch (stmt->kind) {
    case AST_BLOCK: {
        const Ast_Block *block = xx stmt;
        depth += 1;
        sb_append_cstr(sb, "{\n");
        For (block->statements) {
            for (size_t i = 0; i < depth; ++i) sb_append_cstr(sb, "    ");
            print_stmt_to_builder(sb, block->statements[it], depth);
            sb_append_cstr(sb, "\n");
        }
        sb_append_cstr(sb, "}");
        break;
    }
    case AST_WHILE: {
        const Ast_While *while_stmt = xx stmt;
        sb_append_cstr(sb, "while ");
        print_expr_to_builder(sb, while_stmt->condition_expression, depth);
        sb_append_cstr(sb, " ");
        print_stmt_to_builder(sb, while_stmt->then_statement, depth);
        break;
    }
    case AST_IF: {
        const Ast_If *if_stmt = xx stmt;
        sb_append_cstr(sb, "if ");
        print_expr_to_builder(sb, if_stmt->condition_expression, depth);
        sb_append_cstr(sb, " ");
        print_stmt_to_builder(sb, if_stmt->then_statement, depth);
        if (if_stmt->else_statement) {
            sb_append_cstr(sb, " else ");
            print_stmt_to_builder(sb, if_stmt->else_statement, depth);
        }
        break;
    }
    case AST_LOOP_CONTROL: {
        const Ast_Loop_Control *loop_control = xx stmt;
        sb_append_cstr(sb, token_type_to_string(loop_control->keyword_type));
        break;
    }
    case AST_RETURN: {
        const Ast_Return *ret = xx stmt;
        sb_append_cstr(sb, "return ");
        print_expr_to_builder(sb, ret->subexpression, depth);
        break;
    }
    case AST_USING: {
        const Ast_Using *using = xx stmt;
        sb_append_cstr(sb, "using ");
        print_expr_to_builder(sb, using->subexpression, depth);
        break;
    }
    case AST_IMPORT: {
        const Ast_Import *import = xx stmt;
        if (import->is_system_library) sb_append_cstr(sb, "#import_system_library ");
        else                           sb_append_cstr(sb, "#import ");
        sb_append(sb, import->path_name.data, import->path_name.count);
        break;
    }
    case AST_EXPRESSION_STATEMENT: {
        Ast_Expression_Statement *expr = xx stmt;
        print_expr_to_builder(sb, expr->subexpression, depth);
        break;
    }
    case AST_VARIABLE: {
        Ast_Variable *var = xx stmt;
        print_decl_to_builder(sb, var->declaration, depth);
        break;
    }
    case AST_ASSIGNMENT: {
        Ast_Assignment *assign = xx stmt;
        print_expr_to_builder(sb, assign->pointer, depth);
        sb_append_cstr(sb, " = ");
        print_expr_to_builder(sb, assign->value, depth);
        break;
    }
    }
}

void print_decl_to_builder(String_Builder *sb, const Ast_Declaration *decl, size_t depth)
{
    if (decl->flags & DECLARATION_IS_PROCEDURE_HEADER) {
        Ast_Lambda *lambda = xx decl->root_expression; // TODO: handle overload_set
        if (decl->ident) sb_append(sb, decl->ident->name.data, decl->ident->name.count);
        else sb_append_cstr(sb, "<unnamed>");
        sb_append_cstr(sb, " : ");
        print_type_to_builder(sb, lambda->type_definition);
        return;
    }
    
    if (decl->flags & DECLARATION_IS_PROCEDURE_BODY) {
        sb_append_cstr(sb, "<lambda body> ");
        print_stmt_to_builder(sb, xx decl->my_block, depth);
        return;
    }

    if (decl->ident) sb_append(sb, decl->ident->name.data, decl->ident->name.count);
    else sb_append_cstr(sb, "<unnamed>");

    sb_append_cstr(sb, " :");
    if (decl->my_type) {
        sb_append_cstr(sb, " ");
        sb_append_cstr(sb, type_to_string(decl->my_type));
        sb_append_cstr(sb, " ");
    }
    if (decl->root_expression) {
        if (decl->flags & DECLARATION_IS_CONSTANT) sb_append_cstr(sb, ": ");
        else sb_append_cstr(sb, "= ");
        print_expr_to_builder(sb, decl->root_expression, depth);
    }
}

// TODO: Function calls

// TODO: Right now, this parses: `if false N :: 100`, which will add N to the current scope.
// We probably don't want to allow that.
// Although, it might make sense for the compile-time if. 
//     #if OS == .LINUX  Handle :: s32;
