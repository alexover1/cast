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

    // Stuff for parsing:

    Ast_Block *block;
} Parser;

// Lexing:

void parser_init(Parser *parser, String_View input, String_View file_name, String_View path_name);
Token peek_next_token(Parser *p);
Token eat_next_token(Parser *p);

#ifndef _WIN32
#define Loc_Fmt SV_Fmt":%d:%d"
#else
#define Loc_Fmt SV_Fmt"%s:%d,%d"
#endif

void parser_report_error(Parser *p, Token token, const char *format, ...);

// Parsing:

Ast *parse_binary_expression(Parser *p, Ast *left, int precedence);
Ast *parse_unary_expression(Parser *p);
Ast *parse_primary_expression(Parser *p, Ast *base);
Ast *parse_base_expression(Parser *p);
Ast *parse_expression(Parser *p);

Ast_Block *parse_block(Parser *p);
Ast *parse_if_statement(Parser *p);
Ast *parse_statement(Parser *p);

// File and path-related functions:

String_View path_get_file_name(const char *begin);
bool path_file_exist(const char *file_path);
