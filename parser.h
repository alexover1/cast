#pragma once

#include "token.h"
#include "ast.h"
#include "vendor/arena.h"

typedef struct {
    Arena *arena;

    // Stuff for lexing:

    String_View file_name;
    String_View path_name;
    String_View *lines; // @malloced with stb_ds

    String_View current_input;
    String_View current_line;
    const char *current_line_start;
    int current_line_number;

    Token peek_token;
    bool peek_full;
    bool reported_error;

    // Stuff for parsing:

    Ast_Block *block;
} Parser;

void *ast_alloc(Parser *p, Source_Location loc, Ast_Type type, size_t size);

// Lexing:

void parser_init(Parser *parser, String_View input, String_View file_name, String_View path_name);
Token peek_next_token(Parser *parser);
Token eat_next_token(Parser *parser);

void parser_report_error(Parser *parser, Source_Location loc, const char *format, ...);

// Parsing:

Ast *parse_binary_expression(Parser *p, Ast *left, int precedence);
Ast *parse_unary_expression(Parser *p);
Ast *parse_primary_expression(Parser *p, Ast *base);
Ast *parse_base_expression(Parser *p);
Ast *parse_expression(Parser *p);

Ast_Type_Definition *parse_struct_desc(Parser *p);
Ast_Type_Definition *parse_enum_defn(Parser *p);
Ast_Type_Definition *parse_type_definition(Parser *p, Ast *type_expression);

Ast_Block *parse_block(Parser *p);
Ast_Block *parse_toplevel(Parser *p);
Ast *parse_if_statement(Parser *p);
Ast *parse_statement(Parser *p);
Ast_Declaration *parse_declaration(Parser *p, Ast_Ident *ident);

// File and path-related functions:

String_View path_get_file_name(const char *begin);
bool path_file_exist(const char *file_path);
