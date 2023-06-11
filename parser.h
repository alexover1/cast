#pragma once

#include <stddef.h>
#include <stdint.h>

#include "common.h"
#include "token.h"
#include "vendor/arena.h"

typedef struct Ast Ast;
typedef struct Workspace Workspace;

typedef struct Ast_Block Ast_Block;
typedef struct Ast_Lambda Ast_Lambda;
typedef struct Ast_Struct Ast_Struct;
typedef struct Ast_Enum Ast_Enum;
typedef struct Ast_Type_Definition Ast_Type_Definition;
typedef struct Ast_Declaration Ast_Declaration;

typedef enum {
    AST_UNINITIALIZED = 0,
    AST_BLOCK = 1,
    AST_LITERAL = 2,
    AST_IDENT = 3,
    AST_UNARY_OPERATOR = 4,
    AST_BINARY_OPERATOR = 5,
    AST_LAMBDA = 6,
    AST_PROCEDURE_CALL = 7,
    AST_WHILE = 8,
    AST_IF = 9,
    AST_LOOP_CONTROL = 10,
    AST_RETURN = 11,
    AST_TYPE_DEFINITION = 12,
    AST_TYPE_INSTANTIATION = 13,
    AST_ENUM = 14,
    AST_STRUCT = 15,
    AST_USING = 16,
    AST_DECLARATION = 17,
    AST_CAST = 18,
} Ast_Type;

struct Ast {
    Ast_Type type;
    unsigned int flags;
    String_View file_name;
    Source_Location location;
    Ast *replacement;
    Ast_Type_Definition *inferred_type;
};

struct Ast_Block {
    Ast base;

    Ast_Block *parent;

    Ast_Lambda *belongs_to_lambda;
    Ast_Struct *belongs_to_struct;
    Ast_Enum *belongs_to_enum;
    bool belongs_to_loop; // for or while

    Ast **statements; // @malloced with stb_ds
    Ast_Declaration **declarations;
};

typedef struct {
    Ast base;
    
    Ast_Type_Definition *type;
    unsigned long number_flags;
    union {
        unsigned long integer_value;
        double double_value;
        String_View string_value;
    };
} Ast_Literal;

typedef struct {
    Ast base;

    String_View name;
    Ast_Block *enclosing_block;
    String_View source_file_name;
} Ast_Ident;

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

    Ast *procedure_expression;
    Ast **arguments; // @malloced with stb_ds
} Ast_Procedure_Call;

typedef struct {
    Ast base;

    bool directive;
    Ast *condition_expression;
    Ast *then_statement;
} Ast_While;

typedef struct {
    Ast base;

    bool directive;
    Ast *condition_expression;
    Ast *then_statement;
    Ast *else_statement;
} Ast_If;

typedef struct {
    Ast base;

    int keyword_type; // BREAK or CONTINUE
} Ast_Loop_Control;

typedef struct {
    Ast base;

    Ast *subexpression;
    Ast_Lambda *lambda_i_belong_to;
} Ast_Return;

struct Ast_Type_Definition {
    Ast base;

    Ast_Struct *struct_desc;
    Ast_Enum *enum_defn;
    Ast_Ident *type_name;
    Ast_Procedure_Call *struct_call;

    // If a pointer
    size_t pointer_level;
    Ast_Type_Definition *pointer_to;

    // If a numeric type.
    const char *literal_name;
    unsigned long number_flags;
    Ast_Literal *number_literal_low;
    Ast_Literal *number_literal_high;

    // If an array
    long long array_length;
    Ast_Type_Definition *array_element_type;

    // If a lambda.
    Ast_Type_Definition *lambda_return_type;
    Ast_Type_Definition **lambda_argument_types; // Pointers to the lambda's argument declarations, not copies.

    int size; // Size in bytes of storage for this type.
};

typedef struct {
    Ast base;

    Ast_Type_Definition *type_definition;
    Ast **argument_list;
    // Ast *initializer_expression;
} Ast_Type_Instantiation;

struct Ast_Struct {
    Ast base;
    
    Ast_Block *block;
};

struct Ast_Enum {
    Ast base;

    Ast_Type_Definition *underlying_int_type;
    Ast_Block *block;
};

/// `(x: int) -> int { return x * x; }`
struct Ast_Lambda {
    Ast base;

    Ast_Type_Definition *type_definition;
    Ast_Block *block; // block->parent == arguments_block
};

// Note: For "using declarations" such as `using e: Entity` inside of lambda types, a flag is set instead on the declaration.
typedef struct {
    Ast base;

    Ast *subexpression;
} Ast_Using;

enum {
    DECLARATION_IS_CONSTANT = 0x1,
    DECLARATION_IS_PROCEDURE_HEADER = 0x2,
    DECLARATION_IS_PROCEDURE_BODY = 0x4,
    DECLARATION_IS_STRUCT_MEMBER = 0x8,
    DECLARATION_IS_ENUM_VALUE = 0x10,
    DECLARATION_IS_LAMBDA_ARGUMENT = 0x20,
    DECLARATION_IS_POLYMORPHIC = 0x40,
};

struct Ast_Declaration {
    Ast base;

    Ast_Ident *ident;
    Ast_Type_Definition *my_type;
    Ast *expression;
    Ast_Block *block; // If this declaration owns a block.

    unsigned int flags;
};

typedef struct {
    Ast base;

    Ast_Type_Definition *type;
    Ast *expression;
} Ast_Cast;

// BEGIN PARSER
// ^ this is so I can search to jump here

typedef struct {
    String_View name, path;
    char *data; // @Copy @Owned
    size_t size;
} Source_File;

Source_File os_read_entire_file(const char *path_as_cstr);

#define PARSER_PEEK_CAPACITY 4

typedef struct {
    Arena *arena;
    bool reported_error;

    // Stuff for lexing:

    Source_File current_file;
    String_View *lines; // Points into current_file.data
    String_View file_name;

    String_View current_input;
    String_View current_line;
    const char *current_line_start;
    int current_line_number;

    Token peek_buffer[PARSER_PEEK_CAPACITY];
    size_t peek_begin;
    size_t peek_count;

    // Stuff for parsing:

    Workspace *workspace;
    Ast_Block *block;
} Parser;

void *ast_alloc(Parser *p, Source_Location loc, Ast_Type type, size_t size);
const char *ast_type_to_string(Ast_Type type);
const char *ast_to_string(const Ast *ast);
const char *type_to_string(Ast_Type_Definition *defn);

// Lexing:

Parser *parser_init(Source_File file);
Token parser_fill_peek_buffer(Parser *parser);
Token peek_token(Parser *parser, size_t user_index);
Token peek_next_token(Parser *parser);
Token eat_next_token(Parser *parser);

void parser_report_error(Parser *parser, Source_Location loc, const char *format, ...);

// Parsing:

Ast *parse_binary_expression(Parser *p, Ast *left, int precedence);
Ast *parse_unary_expression(Parser *p);
Ast *parse_primary_expression(Parser *p, Ast *base);
Ast *parse_base_expression(Parser *p);
Ast *parse_expression(Parser *p);

Ast *parse_lambda_type_or_definition(Parser *p);
Ast_Declaration *parse_lambda_argument(Parser *p);

Ast_Type_Definition *parse_struct_desc(Parser *p);
Ast_Type_Definition *parse_enum_defn(Parser *p);
Ast_Type_Definition *parse_lambda_type(Parser *p);
Ast_Type_Definition *parse_literal_type(Parser *p, String_View lit);
Ast_Type_Definition *parse_type_definition(Parser *p, Ast *type_expression);

Ast_Declaration *parse_declaration(Parser *p);

Ast_Block *parse_block(Parser *p);
Ast_Block *parse_toplevel(Parser *p);
void parse_into_block(Parser *p, Ast_Block *block);
Ast *parse_if_statement(Parser *p);
Ast *parse_statement(Parser *p);

Ast_Declaration *find_declaration_if_exists(const Ast_Ident *ident);
void checked_add_to_scope(Parser *p, Ast_Block *block, Ast_Declaration *decl);

// File and path-related functions:

String_View path_get_file_name(const char *begin);
bool path_file_exist(const char *file_path);
