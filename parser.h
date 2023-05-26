#pragma once

#include "lexer.h"
#include "ast.h"
#include "vendor/arena.h"

typedef struct {
    Lexer lexer;
    Arena *arena;
    Ast_Block *block;
} Parser;

Ast *parse_binary_expression(Parser *p, Ast *left, int precedence);
Ast *parse_unary_expression(Parser *p);
Ast *parse_primary_expression(Parser *p, Ast *base);
Ast *parse_base_expression(Parser *p);
Ast *parse_expression(Parser *p);
