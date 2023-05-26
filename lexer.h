#pragma once

#include <stdbool.h>
#include <stdint.h>
#include "vendor/sv.h"

typedef enum {
    TOKEN_BITWISE_AND = '&',
    TOKEN_BITWISE_OR = '|',
    TOKEN_BITWISE_XOR = '^',

    // The rest of the ascii values are invisible.
    
    TOKEN_IDENT = 256,
    TOKEN_NUMBER = 257,
    TOKEN_STRING = 258,

    TOKEN_PLUSEQUALS = 259,
    TOKEN_MINUSEQUALS = 260,
    TOKEN_TIMESEQUALS = 261,
    TOKEN_DIVEQUALS = 262,
    TOKEN_MODEQUALS = 263,
    TOKEN_ISEQUAL = 264,
    TOKEN_ISNOTEQUAL = 265,
    TOKEN_LOGICAL_AND = 266,
    TOKEN_LOGICAL_OR = 267,
    TOKEN_LESSEQUALS = 268,
    TOKEN_GREATEREQUALS = 269,

    TOKEN_RIGHT_ARROW = 270,
    TOKEN_DOUBLE_DOT = 271,
    // 272 is unused

    TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT = 273,
    TOKEN_SHIFT_RIGHT = 274,
    // 275 is unused
    TOKEN_BITWISE_AND_EQUALS = 276,
    TOKEN_BITWISE_OR_EQUALS = 277,
    TOKEN_BITWISE_XOR_EQUALS = 278,
    
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_THEN,
    TOKEN_KEYWORD_ELSE,
    // TOKEN_KEYWORD_CASE,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_STRUCT,
    TOKEN_KEYWORD_WHILE,
    TOKEN_KEYWORD_BREAK,
    TOKEN_KEYWORD_CONTINUE,
    TOKEN_KEYWORD_USING,

    TOKEN_KEYWORD_DEFER,
    TOKEN_KEYWORD_SIZE_OF,
    TOKEN_KEYWORD_TYPE_OF,
    TOKEN_KEYWORD_INITIALIZER_OF,
    TOKEN_KEYWORD_TYPE_INFO,
    TOKEN_KEYWORD_NULL,

    TOKEN_KEYWORD_ENUM,
    TOKEN_KEYWORD_TRUE,
    TOKEN_KEYWORD_FALSE,
    TOKEN_KEYWORD_UNION,

    TOKEN_NOTE,
    TOKEN_END_OF_INPUT,

    TOKEN_ERROR,
} Token_Type;

typedef struct {
    int type;
    int l0, c0, c1;
    
    union {
        uint64_t int_value;
        float float_value;
        String_View string_value; // Must be copied to save in AST.
    };

    uint32_t number_flags; // If a number.
} Token;

enum {
    NUMBER_IS_BINARY_LITERAL = 0x1,
    NUMBER_IS_HEX_LITERAL = 0x2,
    // NUMBER_IS_OCTAL_LITERAL = 0x4,
    NUMBER_HAS_FRACTIONAL_PART = 0x8, // TODO: Not sure about how we want int/float to work...
};

typedef struct {
    String_View file_name;
    String_View path_name;
    String_View *lines; // @malloced with stb_ds

    String_View current_input;
    String_View current_line;
    const char *current_line_start;
    int current_line_number;

    Token peek_token;
    bool peek_full;
    // Token eof_token;
} Lexer;

void lexer_init(Lexer *l, String_View contents);

void lexer_next_line(Lexer *lexer);
Token lexer_peek_token(Lexer *lexer);
Token lexer_next_token(Lexer *lexer);

typedef struct {
    int l0, c0, c1;
} Lexer_Position;

#ifndef _WIN32
#define Loc_Fmt SV_Fmt":%d:%d"
#else
#define Loc_Fmt SV_Fmt"%s:%d,%d"
#endif

Lexer_Position position_from_lexer(const Lexer *lexer);
Lexer_Position position_from_token(Token token);

void lexer_report_error(const Lexer *lexer, Lexer_Position pos, const char *fmt, ...);

// File and path-related functions:

String_View path_get_file_name(const char *begin);
bool path_file_exist(const char *file_path);
