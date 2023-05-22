#pragma once

#include <stddef.h>
#include <stdint.h>

typedef enum {
    AST_UNINITIALIZED = 0,

    AST_BLOCK = 1,
    AST_LITERAL = 2,
    AST_IDENT = 3,
    AST_UNARY_OPERATOR = 4,
    AST_BINARY_OPERATOR = 5,
    AST_LAMBDA_BODY = 6,
    AST_PROCEDURE_CALL = 7,
    AST_WHILE = 8,
    AST_IF = 9,
    AST_LOOP_CONTROL = 10,
    AST_RETURN = 11,
    AST_TYPE_DEFINITION = 12,
    AST_TYPE_INSTANTIATION = 13,
    AST_ENUM = 14,
    AST_LAMBDA = 15,
    AST_STRUCT = 16,

    AST_DECLARATION = 17,
} Ast_Type;

typedef struct {
    Ast_Type type;
    uint32_t flags;
    
    // l0 is the index of the starting line. l1 is the index of the ending line.
    // c0 is the index of the starting character (in bytes). c1 is the index of
    // the ending character.
    int l0, l1, c0, c1;

    const char *filename;
} Ast;

typedef struct Ast_Block Ast_Block;
typedef struct Ast_Lambda Ast_Lambda;
typedef struct Ast_Struct Ast_Struct;
typedef struct Ast_Enum Ast_Enum;

struct Ast_Block {
    Ast base;

    Ast_Block *parent;

    Ast_Lambda *belongs_to_lambda;
    Ast_Struct *belongs_to_struct;
    Ast_Enum *belongs_to_enum;

    Ast **statements; // @malloced with stb_ds
};

typedef enum {
    LITERAL_INT    = 0,
    LITERAL_FLOAT  = 1,
    LITERAL_STRING = 2,
} Literal_Kind;

typedef struct {
    Ast base;
    
    Literal_Kind kind;
    union {
        uint64_t int_value;
        float float_value;
        struct { const char *data; uint64_t count; } string_value;
    };
} Ast_Literal;

typedef struct {
    Ast base;

    const char *name;
    Ast_Block *enclosing_block;
} Ast_Ident;

enum {
    OPERATOR_DOUBLE_EQUALS = 256,
};

typedef struct {
    Ast base;

    int operator_type;
    Ast *subexpression;
} Ast_Unary_Operator;

typedef struct {
    Ast base;

    int operator_type;
    Ast *left;
    Ast *right;
} Ast_Binary_Operator;

typedef struct {
    Ast base;

    Ast_Block arguments_block;
    Ast_Block block; // block->parent == arguments_block
} Ast_Lambda_Body;

typedef struct {
    Ast base;

    Ast *procedure_expression;
    Ast **arguments; // @malloced with stb_ds
} Ast_Procedure_Call;

typedef struct {
    Ast base;
} Ast_While;

typedef struct {
    Ast base;
} Ast_If;

typedef struct {
    Ast base;
} Ast_Loop_Control;

typedef struct {
    Ast base;
} Ast_Return;

typedef struct Ast_Type_Definition Ast_Type_Definition;

// It's useful to have all the different kind of type definitions stored in one structure so that we can find them.
struct Ast_Type_Definition {
    Ast base;

    Ast_Struct *struct_desc;
    Ast_Enum *enum_defn;
    const char *literal_name;

    // If an array
    Ast_Type_Definition *array_element_type;

    // If a pointer
    size_t pointer_level;
    Ast_Type_Definition *pointer_to;

    // If a lambda type
    Ast_Type_Definition *lambda_return_type;
    Ast_Type_Definition **lambda_argument_types;
};

typedef struct {
    Ast base;

    Ast_Type_Definition *type_definition;
    Ast **argument_list; // @malloced with stb_ds
} Ast_Type_Instantiation;

struct Ast_Enum {
    Ast base;

    Ast_Type_Definition *underlying_int_type;
    Ast_Block scope;
};

/// `(x: int) -> int { return x * x; }`
struct Ast_Lambda {
    Ast base;

    Ast_Type_Definition *type_definition;
    Ast_Lambda_Body body;
};

struct Ast_Struct {
    Ast base;
    
    Ast_Block scope;
};

enum {
    DECLARATION_IS_COMPTIME = 0x1,
    DECLARATION_IS_PROCEDURE_HEADER = 0x2,
    DECLARATION_IS_STRUCT_FIELD = 0x4, // NOTE: this is not set for all struct members, only fields
};

typedef struct {
    Ast base;

    Ast_Ident ident;
    Ast *expression;

    uint32_t flags;
} Ast_Declaration;

Ast_Declaration *find_declaration_or_null(const Ast_Block *block, const char *name);

const char *ast_to_string(const Ast *ast);
