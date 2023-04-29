#pragma once

#include "type_info.h"

//////////////

typedef enum {
    AST_INTEGER, // 5, 4, 100,
    AST_FLOAT, // 3.1415, 59.5
    AST_STRING, // "Hello"
    AST_IDENTIFIER, // maximum

    AST_BINARY, // x<<(8*2)

    AST_PROCEDURE_CALL, // cos(2*PI)

    AST_DECLARATION,
} Ast_Node_Kind;

typedef struct Ast_Node Ast_Node;

typedef enum {
    TOKEN_DOUBLE_EQUAL = 256,
    TOKEN_NOT_EQUAL,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS_EQUAL,
} Token_Kind;

///    +
///   / \
///  5   4
typedef struct {
    Ast_Node *lhs;
    Ast_Node *rhs;
    Token_Kind op;
} Ast_Binary;

/// cos(3.14)
typedef struct {
    Ast_Node *procedure; // A.B.C.Math.max(1, 2, 3)
    Ast_Node *arguments;
    size_t arguments_count;
} Ast_Procedure_Call;

// cos :: (theta: float) -> float {...}
typedef struct {
    const char *name;
    Ast_Node *root_expression;
} Ast_Declaration;

/// my_identifier
typedef struct {
    Ast_Declaration *declaration;
    const char *text;
} Ast_Identifier;

struct Ast_Node {
    Ast_Node_Kind kind;

    Type inferred_type;

    union {
        uint64_t integer;
        double floating;
        const char *string;
        Ast_Binary binary;
        Ast_Procedure_Call procedure_call;
        Ast_Identifier identifier;
        Ast_Declaration declaration;
    };
};