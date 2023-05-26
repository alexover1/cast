#include <assert.h>

#include "common.h"
#include "parser.h"

#define xx (Ast *)

static inline void copy_location_info(Ast *ast, Token token)
{
    ast->l0 = token.line;
    ast->c0 = token.c0;
    ast->c1 = token.c1;
    // TODO: set l1 for blocks, structs, etc...
}

static inline void *ast_alloc(Parser *p, Token token, Ast_Type type, size_t size)
{
    Ast *ast = arena_alloc(p->arena, size);
    ast->type = type;
    copy_location_info(ast, token);
    return ast;
}

static inline Ast_Ident *make_identifier(Parser *p, Token token)
{
    Ast_Ident *ident = arena_alloc(p->arena, sizeof(Ast_Ident));
    copy_location_info(&ident->base, token);
    ident->base.type = AST_IDENT;
    ident->name = arena_sv_to_cstr(context_arena, token.string_value);
    ident->enclosing_block = p->block;
    return ident;
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
    case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT:
    case TOKEN_SHIFT_RIGHT:
    case '&':
        return 5;

    default:
        return 0;
    }
}

Ast *parse_binary_expression(Parser *p, Ast *left, int precedence)
{
    if (left == NULL) left = parse_unary_expression(p);

    Token token;
    Ast_Binary_Operator *bin;

    while (true) {
        token = lexer_peek_token(&p->lexer);
        int token_precedence = operator_precedence_from_token_type(token.type);
        if (token_precedence < precedence) {
            return left;
        }
        lexer_next_token(&p->lexer);
        bin = ast_alloc(p, token, AST_BINARY_OPERATOR, sizeof(Ast_Binary_Operator));
        bin->left = left;
        bin->operator_type = token.type;
        bin->right = parse_binary_expression(p, NULL, token_precedence + 1);
        left = xx bin;
    }

    UNREACHABLE;
}

Ast *parse_unary_expression(Parser *p)
{
    Token token = lexer_peek_token(&p->lexer);
    switch (token.type) {
    case '-':
    case '*':
    case '!':
    case TOKEN_BITWISE_NOT:
    case TOKEN_BITWISE_XOR:
    case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT:
    {
        lexer_next_token(&p->lexer);
        Ast_Unary_Operator *unary = ast_alloc(p, token, AST_UNARY_OPERATOR, sizeof(Ast_Unary_Operator));
        unary->operator_type = token.type;
        unary->subexpression = parse_unary_expression(p);
        return xx unary;
    }
    default:
        return parse_primary_expression(p, NULL);
    }
}

Ast *parse_primary_expression(Parser *p, Ast *base)
{
    if (base == NULL) base = parse_base_expression(p);

    while (true) {
        Token token = lexer_peek_token(&p->lexer);
        switch (token.type) {
        case '.': // TODO: selector
        case '[': // TODO: array subscript
        case '(': // TODO: procedure call
            UNIMPLEMENTED;
        default:
            return base;
        }
    }

    UNREACHABLE;
}

Ast *parse_base_expression(Parser *p)
{
    Token token = lexer_next_token(&p->lexer);
    switch (token.type) {
    case TOKEN_IDENT:
        return xx make_identifier(p, token);
    case TOKEN_NUMBER:
    {
        Ast_Literal *lit = ast_alloc(p, token, AST_LITERAL, sizeof(Ast_Literal));
        if (flag_has(token.number_flags, NUMBER_IS_FLOAT)) {
            lit->kind = LITERAL_FLOAT;
            lit->float_value = token.float_value;
            return xx lit;
        }
        lit->kind = LITERAL_INT;
        lit->int_value = token.int_value;
        return xx lit;
    }
    case TOKEN_STRING:
    {
        Ast_Literal *lit = ast_alloc(p, token, AST_LITERAL, sizeof(Ast_Literal));
        lit->kind = LITERAL_STRING;
        lit->string_value = token.string_value;
        return xx lit;
    }
    case '(':
    {
        // TODO: check for procedure type/defn here.
        // TODO: print the location of the open paren and the place where we expected a closing one.
        Ast *subexpression = parse_expression(p);
        Token next_token = lexer_next_token(&p->lexer);
        if (next_token.type != ')') {
            lexer_report_error(&p->lexer, token, "Missing closing parentheses around expression.");
        }
        return subexpression;
    }
    case '.':
        // TODO: anonymous member access
        UNIMPLEMENTED;
    }

    lexer_report_error(&p->lexer, token, "Expected a base expression (operand) but got %s", token_type_to_string(token.type));
    UNREACHABLE;
}

inline Ast *parse_expression(Parser *p)
{
    return parse_binary_expression(p, NULL, 1);
}
