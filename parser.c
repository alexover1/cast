#include <assert.h>
#include <errno.h>
#include <string.h> // strerror
#include <stdio.h> // os_read_entire_file

#include "common.h"
#include "parser.h"
#include "workspace.h"

#define Enter_Block(parser, the_block) do { \
    (the_block)->parent = (parser)->block;  \
    (parser)->block = (the_block);          \
} while (0);

// Note: We don't use the_block_to_exit but I like to pass it so I can see what block I'm exiting at the callsite.
#define Exit_Block(parser, the_block_to_exit) do { \
    (parser)->block = (parser)->block->parent; \
} while (0);

static inline Ast_Ident *make_identifier(Parser *p, Token token)
{
    Ast_Ident *ident = ast_alloc(p, token.location, AST_IDENT, sizeof(*ident));
    ident->name = arena_sv_copy(context_arena, token.string_value);
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
        parser_report_error(parser, token.location, "Expected semicolon after statement.");
    }
}

static inline Token eat_token_type(Parser *parser, Token_Type type, const char *error_message)
{
    Token token = eat_next_token(parser);
    if (token.type != (int)type) {
        parser_report_error(parser, token.location, error_message);
    }
    return token;
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
        if (defn->literal_name) return true;
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

// Currently, this always uses malloc because we call free() on the data,
// and we don't have a system in C for storing the allocator that allocated the data.
Source_File os_read_entire_file(const char *path_as_cstr)
{
    Source_File file;

    // TODO: We should probably copy these as well.
    file.name = path_get_file_name(path_as_cstr);
    file.path = sv_from_cstr(path_as_cstr);

    // Read the file.
    FILE *handle = fopen(path_as_cstr, "rb");
    if (handle == NULL) {
        goto error;
    }

    if (fseek(handle, 0, SEEK_END) < 0) {
        goto error;
    }

    long m = ftell(handle);
    if (m < 0) {
        goto error;
    }

    char *buffer = malloc(m);
    if (buffer == NULL) {
        goto error;
    }

    if (fseek(handle, 0, SEEK_SET) < 0) {
        goto error;
    }

    size_t n = fread(buffer, 1, (size_t) m, handle);
    if (ferror(handle)) {
        goto error;
    }

    file.data = buffer;
    file.size = n;

    return file;

error:
    if (handle) fclose(handle);
    fprintf(stderr, "Error: Could not read source file '%s': %s\n", path_as_cstr, strerror(errno));
    exit(1);
}

inline void *ast_alloc(Parser *p, Source_Location loc, Ast_Type type, size_t size)
{
    Ast *ast = arena_alloc(p->arena, size);
    ast->type = type;
    ast->location = loc;
    ast->file_name = p->file_name;
    memset(ast + sizeof(Ast), 0, size - sizeof(Ast));
    return ast;
}

Ast *parse_binary_expression(Parser *p, Ast *left, int precedence)
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

Ast *parse_unary_expression(Parser *p)
{
    Token token = peek_next_token(p);
    switch (token.type) {
    case '-':
    case '*':
    case '!':
    case TOKEN_BITWISE_NOT:
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

    while (1) {
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
        lit->number_flags = token.number_flags;
        if (flag_has(token.number_flags, NUMBER_FLAGS_FLOAT)) {
            assert(!flag_has(token.number_flags, NUMBER_FLAGS_DOUBLE));
            lit->type = p->workspace->type_def_float;
            lit->double_value = token.double_value;
        } else {
            lit->type = p->workspace->type_def_int; // TODO: What about smaller int types?
            lit->integer_value = token.integer_value;
        }
        lit->base.inferred_type = lit->type;
        return xx lit;
    }
        
    case TOKEN_STRING: {
        eat_next_token(p);
        Ast_Literal *lit = ast_alloc(p, token.location, AST_LITERAL, sizeof(*lit));
        lit->type = p->workspace->type_def_string;
        lit->base.inferred_type = lit->type;
        lit->string_value = token.string_value;
        return xx lit;
    }
        
    // @CopyPasta
    case TOKEN_KEYWORD_TRUE:
        eat_next_token(p);
        return xx p->workspace->literal_true;

    case TOKEN_KEYWORD_FALSE:
        eat_next_token(p);
        return xx p->workspace->literal_false;
        
    case TOKEN_KEYWORD_NULL:
        eat_next_token(p);
        return xx p->workspace->literal_null;
        
    case TOKEN_KEYWORD_STRUCT:
        return xx parse_struct_desc(p);
        
    case TOKEN_KEYWORD_ENUM:
        return xx parse_enum_defn(p);

    case '(': {
        token = peek_token(p, 1);
        switch (token.type) {
        case TOKEN_IDENT:
            if (peek_token(p, 2).type != ':') break;
            // fallthrough
        case ')':
        case TOKEN_KEYWORD_USING:
            return parse_lambda_type_or_definition(p);
        }

        eat_next_token(p);
        Ast *subexpression = parse_expression(p);
        eat_token_type(p, ')', "Missing closing parenthesis around expression.");
        return subexpression;
    }

    case '.':
        // TODO: anonymous member access
        UNIMPLEMENTED;
    }

    parser_report_error(p, token.location, "Expected a base expression (operand) but got %s.", token_type_to_string(token.type));
    UNREACHABLE;
}

Ast *parse_lambda_type_or_definition(Parser *p)
{
    Token token = peek_next_token(p); // Just for location.
    Ast_Block *arguments_block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*arguments_block));
    Enter_Block(p, arguments_block);

    Ast_Type_Definition *type_definition = parse_lambda_type(p);
    token = peek_next_token(p);
    if (token.type == '{') {
        eat_next_token(p);
        Ast_Lambda *lambda = ast_alloc(p, type_definition->base.location, AST_LAMBDA, sizeof(*lambda));
        lambda->type_definition = type_definition;
        lambda->block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*lambda->block));
        lambda->block->belongs_to_lambda = lambda;
        parse_into_block(p, lambda->block);
        Exit_Block(p, arguments_block);
        return xx lambda;
    }
    return xx type_definition;
}

Ast_Declaration *parse_lambda_argument(Parser *p)
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
        parser_report_error(p, token.location, "Expected identifier after %s.", token_type_to_string(token.type));
    }

    // Parse the parameter declaration.
    Ast_Declaration *decl = ast_alloc(p, token.location, AST_DECLARATION, sizeof(*decl));
    decl->ident = make_identifier(p, token);
    decl->flags = flags;

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

    Ast_Struct *struct_desc = ast_alloc(p, token.location, AST_STRUCT, sizeof(*struct_desc));

    // Parse the struct's block.
    // Note: This will need to change when we introduce struct parameters.
    token = eat_token_type(p, '{', "Expected '{' after 'struct'.");
    
    struct_desc->block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*struct_desc->block));
    struct_desc->block->belongs_to_struct = struct_desc;

    parse_into_block(p, struct_desc->block);

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
    enum_defn->block = ast_alloc(p, token.location, AST_BLOCK, sizeof(*enum_defn->block));
    enum_defn->block->belongs_to_enum = enum_defn;

    parse_into_block(p, enum_defn->block);

    Ast_Type_Definition *defn = ast_alloc(p, token.location, AST_TYPE_DEFINITION, sizeof(*defn));
    defn->enum_defn = enum_defn;
    return defn;
}

Ast_Type_Definition *parse_lambda_type(Parser *p)
{
    Token token = eat_next_token(p);
    assert(token.type == '(');

    Source_Location location = token.location;

    Ast_Type_Definition *type_definition = ast_alloc(p, location, AST_TYPE_DEFINITION, sizeof(*type_definition));

    while (1) {
        Ast_Declaration *parameter = parse_lambda_argument(p);
        checked_add_to_scope(p, p->block, parameter);

        assert(parameter->my_type); // TODO: We want to be able to put "name := value" in procedure type.
        arrput(type_definition->lambda_argument_types, parameter->my_type);

        if (p->reported_error) return type_definition;
        
        token = eat_next_token(p);
        if (token.type == ',') continue;

        // Check for closing parenthesis to finish lambda type.
        if (token.type == ')') {
            token = peek_next_token(p); 
            if (token.type == TOKEN_RIGHT_ARROW) {
                eat_next_token(p);
                type_definition->lambda_return_type = parse_type_definition(p, NULL);
            } else {
                type_definition->lambda_return_type = p->workspace->type_def_void;
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
            parser_report_error(parser, type_expression->location, "Here we expected a type, but we got a literal value.");
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

        case AST_IF:
        case AST_WHILE:
        case AST_LOOP_CONTROL:
        case AST_RETURN:
        case AST_BLOCK:
        case AST_USING:
        case AST_LAMBDA:
        case AST_DECLARATION:
        case AST_CAST:
            assert(0);

        case AST_STRUCT:
        case AST_ENUM:
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
            if (flag_has(token.number_flags, NUMBER_FLAGS_FLOAT)) {
                parser_report_error(parser, token.location, "Array length cannot be a float.");
            }
            eat_token_type(parser, ']', "Missing closing bracket after array length.");
            defn->array_length = token.integer_value;
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
    case TOKEN_IDENT: {
        eat_next_token(parser);

        Ast_Type_Definition *literal_type_defn = parse_literal_type(parser, token.string_value);
        if (literal_type_defn) return literal_type_defn;

        defn->type_name = make_identifier(parser, token);
        return defn;
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
        }

        Ast *stmt = parse_statement(p);
        arrput(block->statements, stmt);
        if (statement_requires_semicolon(stmt)) eat_semicolon(p);

        if (p->reported_error) return;
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

Ast *parse_statement(Parser *p)
{
    Token token = peek_next_token(p);
    switch (token.type) {
    case TOKEN_KEYWORD_RETURN: {
        eat_next_token(p);        

        if (!p->block->belongs_to_lambda) {
            parser_report_error(p, token.location, "Cannot use 'return' outside of a procedure.");
        }
        Ast_Return *ret = ast_alloc(p, token.location, AST_RETURN, sizeof(*ret));
        ret->subexpression = parse_expression(p);
        ret->lambda_i_belong_to = p->block->belongs_to_lambda;
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

    case TOKEN_IDENT:
        token = peek_token(p, 1);
        if (token.type == ':') {
            return xx parse_declaration(p);
        }
        break; // fallthrough to end
    }

    return parse_expression(p);
}

// Assumes identifier has been consumed.
Ast_Declaration *parse_declaration(Parser *p)
{
    Token token = eat_token_type(p, TOKEN_IDENT, "Missing identifier for declaration.");
    eat_token_type(p, ':', "Missing ':' after identifier.");

    Ast_Declaration *decl = ast_alloc(p, token.location, AST_DECLARATION, sizeof(*decl));
    decl->ident = make_identifier(p, token);

    if (p->block->belongs_to_struct) {
        decl->flags |= DECLARATION_IS_STRUCT_MEMBER;
    }

    // Add the declaration to the block.
    arrput(p->block->declarations, decl);

    // After first colon has been parsed, we expect an optional type definition and then ':' or '='.

    token = peek_next_token(p);
    switch (token.type) {
    case ':':
        eat_next_token(p);

        decl->flags |= DECLARATION_IS_CONSTANT;
        decl->my_type = NULL;
        decl->expression = parse_expression(p);

        // This makes it super easy to check if we are an enum value later.
        if (p->block->belongs_to_enum) {
            decl->flags |= DECLARATION_IS_ENUM_VALUE;
        }

        return decl;
    case '=':
        eat_next_token(p);
        decl->my_type = NULL;
        decl->expression = parse_expression(p);
        return decl;
    default: {
        // parse_type_definition() either converts an expression or parses a new one if NULL is passed.
        decl->my_type = parse_type_definition(p, NULL);

        // Parse the value.
        token = peek_next_token(p);
        switch (token.type) {
        case ':':
            decl->flags |= DECLARATION_IS_CONSTANT;
            // fallthrough
        case '=':
            eat_next_token(p);
            decl->expression = parse_expression(p);
            return decl;
        case ';':
        case ',':
            return decl;
        default:
            parser_report_error(p, token.location, "Expected ':' or '=' after type definition of declaration.");
            return decl;
        }
    }
    }
}

Ast_Block *parse_toplevel(Parser *p)
{
    // TODO: what do we set the location to?
    p->block = ast_alloc(p, (Source_Location){0}, AST_BLOCK, sizeof(*p->block));

    while (1) {
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

        if (p->reported_error) return p->block;
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
    sb_print(&sb, Loc_Fmt": Error: %s\n", SV_Arg(parser->current_file.path), Loc_Arg(loc), msg);

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

    int ln = loc.l0;

    // Display the previous line if it exists.
    
    if (ln > 0 && parser->lines[ln-1].count) {
        sb_print(&sb, TAB CYN SV_Fmt "\n" RESET, SV_Arg(parser->lines[ln-1]));
    } else {
        // TODO: I kinda want to print the next line.
    }

    // Highlight the token in red.

    String_View line = parser->lines[ln];

    sb_print(&sb, TAB CYN SV_Fmt, loc.c0, line.data);
    sb_print(&sb,     RED SV_Fmt, loc.c1 - loc.c0, line.data + loc.c0);
    sb_print(&sb,     CYN SV_Fmt, (int)line.count - loc.c1, line.data + loc.c1);

    sb_append_cstr(&sb, "\n\n" RESET);

    // Finally, print the string builder to stderr and exit.

    context_arena = previous_arena;

    fprintf(stderr, SV_Fmt, SV_Arg(sb));
    // exit(1);

    va_end(args);
}

inline Parser *parser_init(Source_File file)
{
    Parser *parser = malloc(sizeof(*parser));
    memset(parser, 0, sizeof(*parser));
    parser->arena = context_arena;
    parser->file_name = arena_sv_copy(context_arena, file.name);
    parser->current_file = file;
    parser->current_input = sv_from_parts(file.data, file.size);
    parser->current_line_number = -1;
    return parser;
}

inline Ast *parse_expression(Parser *p)
{
    return parse_binary_expression(p, NULL, 1);
}

void checked_add_to_scope(Parser *p, Ast_Block *block, Ast_Declaration *decl)
{
    if (decl->ident) {
        For (block->declarations) {
            if (sv_eq(block->declarations[it]->ident->name, decl->ident->name)) {
                parser_report_error(p, decl->base.location, "Redeclared identifier '%s'.", decl->ident->name);
                // TODO: Print other declaration location.
            }
        }
    }
    arrput(block->declarations, decl);
}

const char *ast_type_to_string(Ast_Type type)
{
    switch (type) {
    case AST_UNINITIALIZED: return "uninitialized";
    case AST_BLOCK: return "block";
    case AST_LITERAL: return "literal";
    case AST_IDENT: return "ident";
    case AST_UNARY_OPERATOR: return "unary_operator";
    case AST_BINARY_OPERATOR: return "binary_operator";
    case AST_LAMBDA: return "lambda";
    case AST_PROCEDURE_CALL: return "procedure_call";
    case AST_WHILE: return "while";
    case AST_IF: return "if";
    case AST_LOOP_CONTROL: return "loop_control";
    case AST_RETURN: return "return";
    case AST_TYPE_DEFINITION: return "type_definition";
    case AST_TYPE_INSTANTIATION: return "type_instantiation";
    case AST_ENUM: return "enum";
    case AST_STRUCT: return "struct";
    case AST_DECLARATION: return "declaration";
    case AST_USING: return "using";
    case AST_CAST: return "cast";
    }
}

const char *type_to_string(Ast_Type_Definition *defn)
{
    if (defn == NULL) return "**NULL**";
    if (defn->struct_desc) assert(0);
    if (defn->enum_defn) assert(0);
    if (defn->type_name) assert(0);
    if (defn->struct_call) assert(0);
    if (defn->pointer_to) {
        assert(defn->pointer_level < 10);
        return tprint("%.*s%s", (int)defn->pointer_level, "**********", type_to_string(defn->pointer_to));
    }
    if (defn->array_element_type) {
        if (defn->array_length < 0) return tprint("[] %s", type_to_string(defn->array_element_type));
        return tprint("[%lld] %s", defn->array_length, type_to_string(defn->array_element_type));
    }
    if (defn->literal_name) return defn->literal_name;
    if (defn->lambda_return_type) {
        Push_Arena(&temporary_arena);
        String_Builder sb = {0};

        sb_append_cstr(&sb, "(");

        For (defn->lambda_argument_types) {
            if (it > 0) sb_append_cstr(&sb, ", ");
            sb_append_cstr(&sb, type_to_string(defn->lambda_argument_types[it]));
        }

        sb_print(&sb, ") -> %s\0", type_to_string(defn->lambda_return_type));
        Pop_Arena();
        return sb.data;
    }
    UNREACHABLE;
}

// TODO: Function calls
// TODO: Declarations
// TODO: Type instantiations
// TODO: Type definitions
