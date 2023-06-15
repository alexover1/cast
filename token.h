#pragma once

#include <stdbool.h>
#include <stdint.h>
#include "vendor/sv.h"

typedef enum {
    TOKEN_BITWISE_AND = '&',
    TOKEN_BITWISE_OR = '|',
    TOKEN_BITWISE_XOR = '^',
    TOKEN_BITWISE_NOT = '~',

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
    // TODO: Should we parse left arrow even though we don't use it?
    TOKEN_DOUBLE_DOT = 271,
    // 272 is unused

    TOKEN_POINTER_DEREFERENCE_OR_SHIFT_LEFT = 273,
    TOKEN_SHIFT_RIGHT = 274,
    // 275 is unused
    TOKEN_BITWISE_AND_EQUALS = 276,
    TOKEN_BITWISE_OR_EQUALS = 277,
    TOKEN_BITWISE_XOR_EQUALS = 278,

    TOKEN_ARRAY_SUBSCRIPT = 279, // Not a token, but needed to work as a binary node in the parse tree.
    
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_THEN,
    TOKEN_KEYWORD_ELSE,
    TOKEN_KEYWORD_FOR,
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

const char *token_type_to_string(int type);

// l0 is the index of the starting line. l1 is the index of the ending line.
// c0 is the index of the starting character (in bytes). c1 is the index of
// the ending character.
// @Volatile: These are all 0-based, because we're true programmers.
typedef struct {
    int l0, l1; // line range
    int c0, c1; // character range
    int fid; // file index
} Source_Location;

#ifndef _WIN32
#define Loc_Fmt SV_Fmt":%d:%d"
#else
#define Loc_Fmt SV_Fmt"%s:%d,%d"
#endif

#define Loc_Arg(loc) (loc).l0+1, (loc).c0+1

typedef struct {
    int type;
    Source_Location location;
    
    union {
        unsigned long integer_value;
        double double_value;
        String_View string_value; // Must be copied to save in AST.
    };

    unsigned int number_flags; // If a number.
} Token;

enum {
    NUMBER_FLAGS_BINARY = 0x1,
    NUMBER_FLAGS_HEX = 0x2,
    NUMBER_FLAGS_FLOAT = 0x4,
    NUMBER_FLAGS_FLOAT64 = 0x8,
    NUMBER_FLAGS_SIGNED = 0x10, // Explicit + or -
};

typedef struct {
    String_View name, path;
    char *data; // @Copy @Owned
    size_t size;
    String_View *lines; // Points into this->data
} Source_File;

Source_File os_read_entire_file(const char *path_as_cstr);
