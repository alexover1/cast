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

static inline void eat_semicolon(Parser *parser)
{
    Token token = eat_next_token(parser);
    if (token.type != ';') {
        parser_report_error(parser, token, "Expected semicolon after statement.");
    }
}

Ast *parse_binary_expression(Parser *p, Ast *left, int precedence)
{
    if (left == NULL) left = parse_unary_expression(p);

    Token token;
    Ast_Binary_Operator *bin;

    while (true) {
        token = peek_next_token(p);
        int token_precedence = operator_precedence_from_token_type(token.type);
        if (token_precedence < precedence) {
            return left;
        }
        eat_next_token(p);
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
    Token token = peek_next_token(p);
    switch (token.type) {
    case '-':
    case '*':
    case '!':
    case TOKEN_BITWISE_NOT:
    case TOKEN_BITWISE_XOR:
    case TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT:
    {
        eat_next_token(p);
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
        Token token = peek_next_token(p);
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
    Token token = eat_next_token(p);
    switch (token.type) {
    case TOKEN_IDENT:
        return xx make_identifier(p, token);
        
    case TOKEN_NUMBER: {
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
        
    case TOKEN_STRING: {
        Ast_Literal *lit = ast_alloc(p, token, AST_LITERAL, sizeof(Ast_Literal));
        lit->kind = LITERAL_STRING;
        lit->string_value = token.string_value;
        return xx lit;
    }
        
    case TOKEN_KEYWORD_TRUE:
    case TOKEN_KEYWORD_FALSE:
    {
        Ast_Literal *lit = ast_alloc(p, token, AST_LITERAL, sizeof(Ast_Literal));
        lit->kind = LITERAL_BOOL;
        lit->bool_value = token.type == TOKEN_KEYWORD_TRUE; // @Hack
        return xx lit;
    }
        
    case TOKEN_KEYWORD_NULL: {
        Ast_Literal *lit = ast_alloc(p, token, AST_LITERAL, sizeof(Ast_Literal));
        lit->kind = LITERAL_NULL;
        return xx lit;
    }
        
    case '(': {
        // TODO: check for procedure type/defn here.
        // TODO: print the location of the open paren and the place where we expected a closing one.
        Ast *subexpression = parse_expression(p);
        Token next_token = eat_next_token(p);
        if (next_token.type != ')') {
            parser_report_error(p, token, "Missing closing parentheses around expression.");
        }
        return subexpression;
    }

    case '.':
        // TODO: anonymous member access
        UNIMPLEMENTED;
    }

    parser_report_error(p, token, "Expected a base expression (operand) but got %s", token_type_to_string(token.type));
    UNREACHABLE;
}

Ast *parse_if_statement(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == TOKEN_KEYWORD_IF);

    Ast_If *if_stmt = ast_alloc(p, token, AST_IF, sizeof(Ast_If));
    
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

Ast_Block *parse_block(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == '{');

    Ast_Block *block = ast_alloc(p, token, AST_BLOCK, sizeof(Ast_Block));

    while (true) {
        token = peek_next_token(p);
        if (token.type == '}') {
            eat_next_token(p);
            return block;
        }

        Ast *stmt = parse_statement(p);
        arrput(block->statements, stmt);
        if (stmt->type != AST_BLOCK) eat_semicolon(p); // TODO: This doesn't handle all cases of curly braces.
    }

    UNREACHABLE;
}

Ast *parse_statement(Parser *p)
{
    Token token = peek_next_token(p);
    switch (token.type) {
    case TOKEN_KEYWORD_RETURN: {
        eat_next_token(p);        
        Ast_Return *ret = ast_alloc(p, token, AST_RETURN, sizeof(Ast_Return));
        ret->subexpression = parse_expression(p);
        return xx ret;
    }

    case TOKEN_KEYWORD_IF:
        return parse_if_statement(p);

    case TOKEN_KEYWORD_WHILE: {
        eat_next_token(p);
        Ast_While *while_stmt = ast_alloc(p, token, AST_WHILE, sizeof(Ast_While));

        while_stmt->condition_expression = parse_expression(p);

        // Check for "then" keyword and consume it.
        // TODO: do we want/need this?
        token = peek_next_token(p);
        if (token.type == TOKEN_KEYWORD_THEN) {
            eat_next_token(p);
        }
        while_stmt->then_statement = parse_statement(p);

        return xx while_stmt;
    }

    case TOKEN_KEYWORD_BREAK:
    case TOKEN_KEYWORD_CONTINUE:
    {
        eat_next_token(p);
        Ast_Loop_Control *loop_control = ast_alloc(p, token, AST_LOOP_CONTROL, sizeof(Ast_Loop_Control));
        loop_control->keyword_type = token.type;
        // TODO: We want to parse an expression/identifier here for labeled break/continue statements.
        return xx loop_control;
    }

    case TOKEN_KEYWORD_USING: {
        UNIMPLEMENTED;
        Ast_Using *using = ast_alloc(p, token, AST_USING, sizeof(Ast_Using));
        using->subexpression = parse_expression(p); // TODO: "using e: Entity;" We need to handle declarations.
        return xx using;
    }

    case '{':
        return xx parse_block(p);
    }

    // TODO: check if expression is an identifier and check for declaration
    Ast *result = parse_expression(p);
    return result;
}

inline Ast *parse_expression(Parser *p)
{
    return parse_binary_expression(p, NULL, 1);
}

inline void parser_init(Parser *parser, String_View input, String_View file_name, String_View path_name)
{
    assert(input.data != NULL);
    
    parser->file_name = file_name;
    parser->path_name = path_name;
    parser->lines = NULL;

    parser->current_input = input;
    parser->current_line = SV_NULL;
    parser->current_line_start = NULL;
    parser->current_line_number = -1;

    // l->peek_token can stay uninitialized as we should never read from it directly
    parser->peek_full = false;

    parser->block = NULL;
}

// TODO: Function calls
// TODO: Declarations
// TODO: Type instantiations
// TODO: Type definitions
