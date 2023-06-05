#include <assert.h>

#include "common.h"
#include "parser.h"

#define xx (void*)

static inline void *ast_alloc(Parser *p, Source_Location loc, Ast_Type type, size_t size)
{
    Ast *ast = arena_alloc(p->arena, size);
    memset(ast, 0, size);
    ast->type = type;
    ast->location = loc;
    return ast;
}

static inline Ast_Ident *make_identifier(Parser *p, Token token)
{
    Ast_Ident *ident = ast_alloc(p, token.location, AST_IDENT, sizeof(*ident));
    ident->name = arena_sv_copy(context_arena, token.string_value);
    ident->enclosing_block = p->block;
    ident->source_file_name = arena_sv_copy(context_arena, p->file_name); // TODO: Do we actually need to copy this?
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
        printf("Error token type is: %s\n", token_type_to_string(token.type));
        parser_report_error(parser, token.location, "Expected semicolon after statement.");
    }
}

static inline void eat_token_type(Parser *parser, Token_Type type, const char *error_message)
{
    Token token = eat_next_token(parser);
    if (token.type != (int)type) {
        parser_report_error(parser, token.location, error_message);
    }
}

static bool statement_requires_semicolon(Ast *stmt)
{
    switch (stmt->type) {
    case AST_BLOCK: return false;
    case AST_STRUCT: return false;
    case AST_ENUM: return false;
    case AST_LAMBDA: return false;
    case AST_TYPE_DEFINITION: {
        Ast_Type_Definition *defn = xx stmt;
        if (defn->struct_desc) return false;
        if (defn->enum_defn) return false;
        if (defn->type_name) return true;
        if (defn->array_element_type) return true;
        if (defn->pointer_to) return true;
        if (defn->lambda_return_type) return true;
        assert(0);
    }
    // TODO: AST_TYPE_INSTANTIATION?
    case AST_IF: {
        Ast_If *if_stmt = xx stmt;
        if (if_stmt->else_statement) return statement_requires_semicolon(if_stmt->else_statement);
        return statement_requires_semicolon(if_stmt->then_statement);
    }
    case AST_WHILE: {
        Ast_While *while_stmt = xx stmt;
        return statement_requires_semicolon(while_stmt->then_statement);
    }
    case AST_DECLARATION: {
        Ast_Declaration *decl = xx stmt;
        return statement_requires_semicolon(decl->expression);
    }
    default: return true;
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
        bin = ast_alloc(p, token.location, AST_BINARY_OPERATOR, sizeof(*bin));
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
        Ast_Unary_Operator *unary = ast_alloc(p, token.location, AST_UNARY_OPERATOR, sizeof(*unary));
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
    Token token = peek_next_token(p);
    switch (token.type) {
    case TOKEN_IDENT:
        eat_next_token(p);
        return xx make_identifier(p, token);
        
    case TOKEN_NUMBER: {
        eat_next_token(p);
        Ast_Literal *lit = ast_alloc(p, token.location, AST_LITERAL, sizeof(*lit));
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
        eat_next_token(p);
        Ast_Literal *lit = ast_alloc(p, token.location, AST_LITERAL, sizeof(*lit));
        lit->kind = LITERAL_STRING;
        lit->string_value = token.string_value;
        return xx lit;
    }
        
    // @CopyPasta
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
        eat_next_token(p);
        // TODO: check for procedure type/defn here.
        // TODO: print the location of the open paren and the place where we expected a closing one.
        Ast *subexpression = parse_expression(p);
        Token next_token = eat_next_token(p);
        if (next_token.type != ')') {
            parser_report_error(p, token.location, "Missing closing parentheses around expression.");
        }
        return subexpression;
    }

    case '.':
        // TODO: anonymous member access
        UNIMPLEMENTED;
    }

    parser_report_error(p, token.location, "Expected a base expression (operand) but got %s.", token_type_to_string(token.type));
    UNREACHABLE;
}

Ast_Type_Definition *parse_struct_desc(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == TOKEN_KEYWORD_STRUCT);

    Ast_Struct *struct_desc = ast_alloc(p, token.location, AST_STRUCT, sizeof(*struct_desc));
    struct_desc->block = parse_block(p);
    struct_desc->block->belongs_to_struct = struct_desc;

    Ast_Type_Definition *defn = ast_alloc(p, token.location, AST_TYPE_DEFINITION, sizeof(*defn));
    defn->struct_desc = struct_desc;
    return defn;
}

Ast_Type_Definition *parse_enum_defn(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == TOKEN_KEYWORD_ENUM);

    // TODO: parse underling integer type like "enum u8 {...}"
    
    Ast_Enum *enum_defn = ast_alloc(p, token.location, AST_ENUM, sizeof(*enum_defn));
    enum_defn->block = parse_block(p);
    enum_defn->block->belongs_to_enum = enum_defn;

    Ast_Type_Definition *defn = ast_alloc(p, token.location, AST_TYPE_DEFINITION, sizeof(*defn));
    defn->enum_defn = enum_defn;
    return defn;
}

// parse_type_definition() either converts an expression or parses a new one if NULL is passed.
Ast_Type_Definition *parse_type_definition(Parser *parser, Ast *type_expression)
{
    if (type_expression != NULL) {
        switch (type_expression->type) {
        case AST_IDENT: {
            Ast_Type_Definition *defn = ast_alloc(parser, type_expression->location, AST_TYPE_DEFINITION, sizeof(*defn));
            defn->type_name = (Ast_Ident *)type_expression;
            return defn;
        }
            
        case AST_UNARY_OPERATOR: {
            Ast_Type_Definition *defn = ast_alloc(parser, type_expression->location, AST_TYPE_DEFINITION, sizeof(*defn));
            // Unroll unary '*'.
            while (type_expression->type == AST_UNARY_OPERATOR) {
                Ast_Unary_Operator *unary = xx type_expression;
                if (unary->operator_type != '*') {
                    parser_report_error(parser, type_expression->location, "Here we expected a type, but we got unary operator '%c'.", unary->operator_type);
                }
                defn->pointer_level += 1;
                type_expression = unary->subexpression;
            }
            defn->pointer_to = parse_type_definition(parser, type_expression);
            return defn;
        }

        case AST_LITERAL:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a literal value '%s'.", ast_to_string(xx type_expression));
        case AST_BINARY_OPERATOR:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a binary operation.");
        case AST_TYPE_INSTANTIATION:
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got an initializer.");

        case AST_PROCEDURE_CALL: {
            Ast_Type_Definition *defn = ast_alloc(parser, type_expression->location, AST_TYPE_DEFINITION, sizeof(*defn));
            defn->struct_call = (Ast_Procedure_Call *)type_expression;
            return defn;
        }

        case AST_TYPE_DEFINITION:
            return (Ast_Type_Definition *)type_expression;

        case AST_LAMBDA_BODY:
        case AST_IF:
        case AST_WHILE:
        case AST_LOOP_CONTROL:
        case AST_RETURN:
        case AST_BLOCK:
        case AST_USING:
        case AST_DECLARATION:
            assert(0);

        case AST_STRUCT:
        case AST_ENUM:
        case AST_LAMBDA:
            // These should be wrapped in a type definition already...
            assert(0);

        // It's nice to be able to debug this case separately.
        case AST_UNINITIALIZED:
            assert(0);
        }
    }

    Token token = peek_next_token(parser);
    Ast_Type_Definition *defn = ast_alloc(parser, token.location, AST_TYPE_DEFINITION, sizeof(*defn));
    
    switch (token.type) {
    case '*': {
        eat_next_token(parser);
        defn->pointer_level = 1;
        for (token = peek_next_token(parser); token.type == '*'; defn->pointer_level++) {
            eat_next_token(parser);
            token = peek_next_token(parser);
        }
        defn->pointer_to = parse_type_definition(parser, NULL);
        return defn;
    }
    case '[': {
        eat_next_token(parser);

        token = peek_next_token(parser);
        switch (token.type) {
        case ']':
            eat_next_token(parser); // ]
            defn->array_length = -1;
            defn->array_element_type = parse_type_definition(parser, NULL);
            return defn;
        case TOKEN_NUMBER:
            eat_next_token(parser); // number
            if (flag_has(token.number_flags, NUMBER_IS_FLOAT)) {
                parser_report_error(parser, token.location, "Array length cannot be a float.");
            }
            eat_token_type(parser, ']', "Missing closing bracket after array length.");
            defn->array_length = token.int_value;
            defn->array_element_type = parse_type_definition(parser, NULL);
            return defn;
        case TOKEN_DOUBLE_DOT:
            eat_next_token(parser); // ..
            eat_token_type(parser, ']', "Missing closing bracket after array length.");
            defn->array_length = -1; // TODO: How do we separate dynamic and slice?
            defn->array_element_type = parse_type_definition(parser, NULL);
            return defn;
        default:
            parser_report_error(parser, token.location, "Here we expected a type, but we got '%s'.", token_type_to_string(token.type));
        }
    }
    case TOKEN_IDENT:
        eat_next_token(parser);
        defn->type_name = make_identifier(parser, token);
        return defn;
    case TOKEN_KEYWORD_STRUCT:
        return parse_struct_desc(parser);
    case TOKEN_KEYWORD_ENUM:
        return parse_enum_defn(parser);
    default:
        parser_report_error(parser, token.location, "Here we expected a type, but we got '%s'.", token_type_to_string(token.type));
    }

    UNREACHABLE;
}

Ast *parse_if_statement(Parser *p)
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

Ast_Block *parse_block(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == '{');

    Ast_Block *block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*block));
    block->parent = p->block;
    p->block = block;

    while (true) {
        token = peek_next_token(p);
        if (token.type == '}') {
            eat_next_token(p);
            p->block = p->block->parent;
            return block;
        }

        Ast *stmt = parse_statement(p);
        arrput(block->statements, stmt);
        if (statement_requires_semicolon(stmt)) eat_semicolon(p);
    }

    UNREACHABLE;
}

Ast *parse_statement(Parser *p)
{
    Token token = peek_next_token(p);
    switch (token.type) {
    case TOKEN_KEYWORD_RETURN: {
        eat_next_token(p);        
        Ast_Return *ret = ast_alloc(p, token.location, AST_RETURN, sizeof(*ret));
        ret->subexpression = parse_expression(p);
        return xx ret;
    }

    case TOKEN_KEYWORD_IF:
        return parse_if_statement(p);

    case TOKEN_KEYWORD_WHILE: {
        eat_next_token(p);
        Ast_While *while_stmt = ast_alloc(p, token.location, AST_WHILE, sizeof(*while_stmt));

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

    case '{':
        return xx parse_block(p);
    }

    // TODO: check if expression is an identifier and check for declaration
    Ast *result = parse_expression(p);

    if (result->type == AST_IDENT) {
        Token token = peek_next_token(p);
        if (token.type == ':') {
            return xx parse_declaration(p, xx result);
        }
    }
    
    return result;
}

// Assumes identifier has been consumed.
Ast_Declaration *parse_declaration(Parser *p, Ast_Ident *ident)
{
    // TODO: DECLARATION_IS_STRUCT_FIELD
    // TODO: DECLARATION_IS_PROCEDURE_HEADER

    Token token = eat_next_token(p);
    assert(token.type == ':');

    Ast_Declaration *decl = ast_alloc(p, token.location, AST_DECLARATION, sizeof(*decl));
    decl->ident = ident;

    token = peek_next_token(p);
    switch (token.type) {
    case ':':
        eat_next_token(p);
        decl->flags |= DECLARATION_IS_COMPTIME;
        decl->expression = parse_expression(p);
        return decl;
    case '=':
        eat_next_token(p);
        decl->expression = parse_expression(p);
        return decl;
    default: {
        // Explicit type annotation.
        Ast *type_expression = parse_expression(p);

        Ast_Type_Instantiation *inst = ast_alloc(p, token.location, AST_TYPE_INSTANTIATION, sizeof(*inst));
        // parse_type_definition() either converts an expression or parses a new one if NULL is passed.
        inst->type_definition = parse_type_definition(p, type_expression);
        inst->flags |= INSTANTIATION_IS_IMPLICIT;

        decl->expression = xx inst;

        // Parse the value.
        token = peek_next_token(p);
        switch (token.type) {
        case ':':
            decl->flags |= DECLARATION_IS_COMPTIME;
            // fallthrough
        case '=':
            eat_next_token(p);
            Ast *value_expression = parse_expression(p);
            arrput(inst->argument_list, value_expression);
            return decl;
        case ';':
            return decl;
        default:
            parser_report_error(p, token.location, "Expected ':' or '=' after type annotation of declaration.");
            return decl;
        }
    }
    }
}

Ast_Block *parse_toplevel(Parser *p)
{
    // TODO: what do we set the location to?
    p->block = ast_alloc(p, (Source_Location){0}, AST_BLOCK, sizeof(*p->block));

    while (true) {
        Token token = peek_next_token(p);
        if (token.type == TOKEN_END_OF_INPUT) {
            eat_next_token(p);
            return p->block;
        }

        Ast *stmt = parse_statement(p);
        arrput(p->block->statements, stmt);
        if (statement_requires_semicolon(stmt)) {
            eat_semicolon(p);
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

void sb_print_bytes(String_View s)
{
    for (size_t i = 0; i < s.count; ++i) {
        printf("%2x ", s.data[i]);
    }
    printf("\n");
}

void parser_report_error(Parser *parser, Source_Location loc, const char *format, ...)
{
    va_list args;
    va_start(args, format);
    const char *msg = vtprint(format, args);
    assert(msg);

    if (loc.l1 < 0) loc.l1 = loc.l0;
    if (loc.c1 < 0) loc.c1 = loc.c0;

    // Setup the allocator and init string builder.

    Arena *previous_arena = context_arena;
    context_arena = &temporary_arena;

    String_Builder sb = {0};

    // Display the error message.
    sb_print(&sb, Loc_Fmt": Error: %s\n", SV_Arg(parser->path_name), Loc_Arg(loc), msg);

    sb_append(&sb, "\n", 1);

    // TODO: I want to "normalize" the indentation.
    // When we add lines to the parser, we can add it
    // after it has been trimmed to remove leading spaces.
    // However, when printing the previous line, if they differ
    // in indentation I want to show that somehow.
    // Basically, if we are like 10 scopes deep in a function,
    // I want to only print indentation 1 level deeper or shallower
    // than the current line, so that when printing diagnostics we
    // don't end up printing like 50 spaces.

    int ln = loc.l0 - 1;

    // Display the previous line if it exists.
    
    if (ln > 0 && parser->lines[ln-1].count) {
        sb_print(&sb, TAB CYN SV_Fmt "\n" RESET, SV_Arg(parser->lines[ln-1]));
    } else {
        // TODO: I kinda want to print the next line.
    }

    // Highlight the token in red.

    loc.c0 -= 1;
    loc.c1 -= 1;

    String_View line = parser->lines[ln];

    sb_print(&sb, TAB CYN SV_Fmt, loc.c0, line.data);
    sb_print(&sb,     RED SV_Fmt, loc.c1 - loc.c0, line.data + loc.c0);
    sb_print(&sb,     CYN SV_Fmt, (int)line.count - loc.c1, line.data + loc.c1);

    sb_append_cstr(&sb, "\n\n" RESET);

    // Finally, print the string builder to stderr and exit.

    context_arena = previous_arena;

    fprintf(stderr, SV_Fmt, SV_Arg(sb));
    exit(1);

    va_end(args);
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
    parser->current_line_number = 0;

    // l->peek_token can stay uninitialized as we should never read from it directly
    parser->peek_full = false;

    parser->block = NULL;
}

inline Ast *parse_expression(Parser *p)
{
    return parse_binary_expression(p, NULL, 1);
}

// TODO: Function calls
// TODO: Declarations
// TODO: Type instantiations
// TODO: Type definitions
