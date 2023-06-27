#pragma once

#include <stddef.h>
#include <stdint.h>

#include <llvm-c/Core.h>

#include "common.h"
#include "token.h"
#include "vendor/arena.h"

// enum Ast_Type {
//     AST_UNINITIALIZED = 0,
//     AST_BLOCK = 1,
//     AST_LITERAL = 2,
//     AST_IDENT = 3,
//     AST_UNARY_OPERATOR = 4,
//     AST_BINARY_OPERATOR = 5,
//     AST_LAMBDA = 6,
//     AST_PROCEDURE_CALL = 7,
//     AST_WHILE = 8,
//     AST_IF = 9,
//     AST_LOOP_CONTROL = 10,
//     AST_RETURN = 11,
//     AST_TYPE_DEFINITION = 12,
//     AST_TYPE_INSTANTIATION = 13,
//     AST_ENUM = 14,
//     AST_STRUCT = 15,
//     AST_USING = 16,
//     AST_DECLARATION = 17,
//     AST_CAST = 18,
// };

typedef struct Ast_Expression Ast_Expression;
typedef struct Ast_Statement Ast_Statement;
typedef struct Ast_Node Ast_Node;
typedef struct Workspace Workspace;

typedef struct Ast_Block Ast_Block;
typedef struct Ast_Lambda Ast_Lambda;
typedef struct Ast_Struct Ast_Struct;
typedef struct Ast_Enum Ast_Enum;
typedef struct Ast_Type_Definition Ast_Type_Definition;
typedef struct Ast_Declaration Ast_Declaration;

typedef enum {
    AST_NUMBER = 1,
    AST_LITERAL = 2,
    AST_IDENT = 3,
    AST_UNARY_OPERATOR = 4,
    AST_BINARY_OPERATOR = 5,
    AST_LAMBDA = 6,
    AST_PROCEDURE_CALL = 7,
    AST_TYPE_DEFINITION = 8,
    AST_CAST = 9,
    AST_SELECTOR = 10,
    AST_TYPE_INSTANTIATION = 11,
} Ast_Expression_Kind;

struct Ast_Expression {
    // @Volatile: These first 3 fields must be the same as Ast_Statement because of ast_alloc().
    Ast_Expression_Kind kind;
    Source_Location location;

    // @Volatile: Both of these are set during type-checking.
    Ast_Type_Definition *inferred_type;
    Ast_Expression *replacement;
};

typedef enum {
    AST_BLOCK = 1,
    AST_WHILE = 2,
    AST_IF = 3,
    AST_FOR = 4,
    AST_LOOP_CONTROL = 5,
    AST_RETURN = 6,
    AST_USING = 7,
    AST_IMPORT = 8,
    AST_EXPRESSION_STATEMENT = 9, // When an expression is used as a statement...
    AST_VARIABLE = 10,
    AST_ASSIGNMENT = 11,
} Ast_Statement_Kind;

struct Ast_Statement {
    // @Volatile: These first 3 fields must be the same as Ast_Expression because of ast_alloc().
    Ast_Statement_Kind kind;
    Source_Location location;
    bool typechecked; // Because we don't have an inferred_type to check against.

    // Ast_Expression *expression;
    // Ast_Block *block;
};

typedef enum {
    BLOCK_BELONGS_TO_FILE = 1,
    BLOCK_BELONGS_TO_LAMBDA = 2,
    BLOCK_BELONGS_TO_STRUCT = 3,
    BLOCK_BELONGS_TO_ENUM = 4,
    BLOCK_IS_LAMBDA_ARGUMENTS = 5,
} Ast_Block_Kind;

struct Ast_Block {
    Ast_Statement _statement;

    Ast_Block *parent;

    Ast_Block_Kind belongs_to;
    void *belongs_to_data;

    Ast_Statement **statements; // @malloced with stb_ds
    Ast_Declaration **declarations; // @malloced with stb_ds
};

// BEGIN EXPRESSIONS

// This is technically a literal, but it's so common that we want to check if a literal is a number,
// that it's more useful if we separate it.
typedef struct {
    Ast_Expression _expression;

    unsigned int flags; // Stores information about the value, for example if it is signed or has a fractional part.
    union {
        unsigned long integer;
        double real;
    } as;
    bool inferred_type_is_final;
} Ast_Number;

typedef enum {
    LITERAL_BOOL = 1,
    LITERAL_STRING = 2,
    LITERAL_NULL = 3,
} Literal_Kind;

typedef struct {
    Ast_Expression _expression;
    
    // Ast_Type_Definition *default_type; // If we cannot be inferred, this is our type.
    Literal_Kind kind;
    union {
        bool bool_value;
        String_View string_value;
    };
} Ast_Literal;

typedef struct {
    Ast_Expression _expression;

    String_View name;
    Ast_Block *enclosing_block;

    Ast_Declaration *resolved_declaration; // @Volatile: Set during typechecking.
} Ast_Ident;

typedef struct {
    Ast_Expression _expression;

    int operator_type;
    Ast_Expression *subexpression;
} Ast_Unary_Operator;

typedef struct {
    Ast_Expression _expression;

    int operator_type;
    Ast_Expression *left;
    Ast_Expression *right;
} Ast_Binary_Operator;

/// `(x: int) -> int { return x * x; }`
struct Ast_Lambda {
    Ast_Expression _expression;

    Ast_Type_Definition *type_definition;
    Ast_Declaration *my_body_declaration;
    bool is_foreign;

    // String_View name;
    // Ast_Block *block; // block->parent == arguments_block
};

typedef struct {
    Ast_Expression _expression;

    Ast_Expression *procedure_expression;
    Ast_Expression **arguments; // @malloced with stb_ds
} Ast_Procedure_Call;

struct Ast_Struct {
    Ast_Block *block;
    int field_count;
    Ast_Type_Definition **field_types; // @Volatile: Set after this struct has been typechecked.
};

struct Ast_Enum {
    Ast_Type_Definition *underlying_int_type;
    Ast_Block *block;
};

typedef enum {
    TYPE_DEF_NUMBER = 1,
    TYPE_DEF_LITERAL = 2,
    TYPE_DEF_STRUCT = 3,
    TYPE_DEF_ENUM = 4,
    TYPE_DEF_IDENT = 5,
    TYPE_DEF_STRUCT_CALL = 6,
    TYPE_DEF_POINTER = 7,
    TYPE_DEF_ARRAY = 8,
    TYPE_DEF_LAMBDA = 9,
} Type_Def_Kind;

typedef enum {
    ARRAY_KIND_FIXED = 0,
    ARRAY_KIND_SLICE = 1,
    ARRAY_KIND_DYNAMIC = 2,
} Type_Def_Array_Kind;

struct Ast_Type_Definition {
    Ast_Expression _expression;

    Type_Def_Kind kind;
    const char *name; // May be NULL. For literals this is the name of the type like "int" or "float".

    union {
        Ast_Struct *struct_desc;
        Ast_Enum *enum_defn;
        Ast_Ident *type_name;
        Ast_Procedure_Call *struct_call;
        Ast_Type_Definition *pointer_to;
        Literal_Kind literal;
        struct {
            unsigned long flags;
            Ast_Number *literal_low;
            Ast_Number *literal_high;
        } number;
        struct {
            long long length;
            Ast_Type_Definition *element_type;
            Type_Def_Array_Kind kind;
        } array;
        struct {
            Ast_Block *arguments_block;
            Ast_Type_Definition *return_type;
            Ast_Type_Definition **argument_types; // Pointers to the lambda's argument declarations, not copies.
            bool variadic;
        } lambda;
    };

    // bool any; // TODO: Document what this means.
    int size; // Size in bytes of storage for this type.
};

typedef struct {
    Ast_Expression _expression;

    Ast_Type_Definition *type;
    Ast_Expression *subexpression;
} Ast_Cast;

typedef struct {
    Ast_Expression _expression;

    Ast_Expression *namespace_expression;
    Ast_Ident *ident; // The identifier we are looking up.
    // Ast_Declaration *resolved_declaration;
    int struct_field_index;
} Ast_Selector;

typedef struct {
    Ast_Expression _expression;

    Ast_Type_Definition *type_definition;
    Ast_Expression **arguments;
} Ast_Type_Instantiation;

// BEGIN STATEMENTS

typedef struct {
    Ast_Statement _statement;

    Ast_Expression *condition_expression;
    Ast_Statement *then_statement;
} Ast_While;

typedef struct {
    Ast_Statement _statement;

    bool directive;
    Ast_Expression *condition_expression;
    Ast_Statement *then_statement;
    Ast_Statement *else_statement;
} Ast_If;

typedef struct {
    Ast_Statement _statement;

    Ast_Expression *range_expression;
    Ast_Statement *then_statement;
    Ast_Declaration *iterator_declaration;
} Ast_For;

typedef struct {
    Ast_Statement _statement;

    int keyword_type; // BREAK or CONTINUE
} Ast_Loop_Control;

typedef struct {
    Ast_Statement _statement;

    Ast_Expression *subexpression;
    Ast_Lambda *lambda_i_belong_to;
} Ast_Return;

typedef struct {
    Ast_Statement _statement;

    Ast_Expression *subexpression;
} Ast_Using;

// enum {
//     INSTANTIATION_INITIALIZER_VALUE_IS_DEFAULT_FROM_TYPE = 0x100,
// };

// typedef struct {
//     Ast_Statement _statement;

//     const char *name;
//     unsigned int flags;
//     Ast_Type_Definition *type_definition;
//     Ast_Expression *initializer_expression;

//     LLVMValueRef llvm_value; // @Cleanup
// } Ast_Type_Instantiation;

typedef struct {
    Ast_Statement _statement;

    Ast_Expression *subexpression;
} Ast_Expression_Statement;

typedef struct {
    Ast_Statement _statement;

    Ast_Declaration *declaration;
    int lambda_argument_index; // -1 when not a lambda argument.
} Ast_Variable;

typedef struct {
    Ast_Statement _statement;
    
    Ast_Expression *pointer; // a.b.c
    Ast_Expression *value;
} Ast_Assignment;

typedef struct {
    Ast_Statement _statement;

    String_View path_name;
    bool is_system_library;
} Ast_Import;

enum {
    DECLARATION_IS_CONSTANT = 0x1,
    DECLARATION_IS_PROCEDURE_HEADER = 0x2,
    DECLARATION_IS_PROCEDURE_BODY = 0x4,
    DECLARATION_IS_STRUCT_FIELD = 0x8,
    DECLARATION_IS_ENUM_VALUE = 0x10,
    DECLARATION_IS_LAMBDA_ARGUMENT = 0x20,
    DECLARATION_IS_POLYMORPHIC = 0x40,
    DECLARATION_IS_FOR_LOOP_ITERATOR = 0x800,
    // These are set during typechecking.
    DECLARATION_TYPE_WAS_INFERRED_FROM_EXPRESSION = 0x80,
    DECLARATION_VALUE_WAS_INFERRED_FROM_TYPE = 0x100, // Default value (zero) was added.
    DECLARATION_HAS_BEEN_TYPECHECKED = 0x200,
    DECLARATION_IS_FOREIGN = 0x400,
};

// This is so we can store a flattened list of nodes for typechecking.
struct Ast_Node {
    Ast_Expression *expression;
    Ast_Statement *statement;
};

struct Ast_Declaration {
    Source_Location location;
    size_t serial;

    Ast_Ident *ident;
    Ast_Type_Definition *my_type;
    Ast_Expression *root_expression;

    int struct_field_index; // If a struct member.

    Ast_Block *my_block; // If this declaration owns a block.

    Ast_Node *flattened;
    size_t typechecking_position;

    LLVMValueRef llvm_value; // @Cleanup

    unsigned int flags;
};

// BEGIN PARSER
// ^ this is so I can search to jump here

#define PARSER_PEEK_CAPACITY 4

typedef struct {
    Arena *arena;
    bool reported_error;

    // Stuff for lexing:

    int file_index;

    String_View current_input;
    String_View current_line;
    const char *current_line_start;
    int current_line_number;

    Token peek_buffer[PARSER_PEEK_CAPACITY];
    size_t peek_begin;
    size_t peek_count;

    // Stuff for parsing:

    Workspace *workspace;
    Ast_Block *current_block;
    Ast_Lambda *current_lambda;
    Ast_Statement *current_loop; // Points at either Ast_While or Ast_For.
    size_t serial;
} Parser;

void *ast_alloc(Parser *p, Source_Location loc, unsigned int type, size_t size);

// Debugging:

const char *expr_to_string(Ast_Expression *expr);
const char *type_to_string(Ast_Type_Definition *defn);
const char *stmt_to_string(Ast_Statement *stmt);
void print_expr_to_builder(String_Builder *sb, const Ast_Expression *expr, size_t depth);
void print_type_to_builder(String_Builder *sb, const Ast_Type_Definition *defn);
void print_stmt_to_builder(String_Builder *sb, const Ast_Statement *stmt, size_t depth);
void print_decl_to_builder(String_Builder *sb, const Ast_Declaration *decl, size_t depth);

// Lexing:

Parser *parser_init(Workspace *w, int file_index);
Token parser_fill_peek_buffer(Parser *parser);
Token peek_token(Parser *parser, size_t user_index);
Token peek_next_token(Parser *parser);
Token eat_next_token(Parser *parser);
Source_Location parser_current_location(Parser *parser);

int peek_character(Parser *parser);

void parser_report_error(Parser *parser, Source_Location loc, const char *format, ...);

// Parsing:

Ast_Expression *parse_binary_expression(Parser *p, Ast_Expression *left, int precedence);
Ast_Expression *parse_unary_expression(Parser *p);
Ast_Expression *parse_primary_expression(Parser *p, Ast_Expression *base);
Ast_Expression *parse_base_expression(Parser *p);
Ast_Expression *parse_expression(Parser *p);

Ast_Type_Definition *parse_lambda_type(Parser *p);
Ast_Declaration *parse_lambda_argument(Parser *p, unsigned index);
Ast_Lambda *parse_lambda_definition(Parser *p, Ast_Type_Definition *lambda_type);

Ast_Type_Definition *parse_struct_desc(Parser *p);
Ast_Type_Definition *parse_enum_defn(Parser *p);
Ast_Type_Definition *parse_literal_type(Parser *p, String_View lit);
Ast_Type_Definition *parse_type_definition(Parser *p, Ast_Expression *type_expression);

void parse_declaration_value(Parser *p, Ast_Declaration *decl);
Ast_Declaration *parse_declaration(Parser *p);

Ast_Block *parse_block(Parser *p);
Ast_Block *parse_toplevel(Parser *p);
void parse_into_block(Parser *p, Ast_Block *block);
Ast_Statement *parse_while_statement(Parser *p);
Ast_Statement *parse_if_statement(Parser *p);
Ast_Statement *parse_for_statement(Parser *p);
Ast_Statement *parse_assignment(Parser *p, Ast_Expression *pointer_expression);
Ast_Statement *parse_statement(Parser *p);

Ast_Declaration *find_declaration_in_block(const Ast_Block *block, String_View name);
Ast_Declaration *find_declaration_from_identifier(const Ast_Ident *ident);
void checked_add_to_scope(Parser *p, Ast_Block *block, Ast_Declaration *decl);

// File and path-related functions:

String_View path_get_file_name(const char *begin);
bool path_file_exist(const char *file_path);
