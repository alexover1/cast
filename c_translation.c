#include "ir_builder.h"
#include "context_alloc.h"
#include "string_builder.h"

// typedef enum {
//     CLANG_LITERAL,
//     CLANG_OPERATOR_EXPRESSION,
//     CLANG_PROCEDURE_CALL,
//     CLANG_STATEMENT,
//     CLANG_DECLARATION,
// } Clang_Tag;

// typedef struct {
//     Clang_Tag tag;
//     union {
//         uint64_t integer_literal;
//         const char *string_literal;
//         struct { Clang_Expr *left; Clang_Expr *right; } binary;
//     };
// } Clang_Expr;

// typedef struct {
//     Clang_Tag tag;
//     Clang_Expr *operand;
// } Clang_Stmt;

// typedef struct {
//     Clang_Tag tag;
    
//     const char *name;
//     Clang_Expr *expr;
// } Clang_Decl;

// typedef struct {
//     Clang_Tag tag;

//     union {
//         Clang_Expr expr;
//         Clang_Stmt stmt;
//         Clang_Decl decl;
//     };
// } Clang_Node;

const char *c_translate_value(const Ir_Builder *builder, Ir_Index index)
{
    Ir_Value value = builder->values[index];
    switch (value.kind) {
    case IR_CONSTANT_INTEGER:
        return tprint("#define %s %lld\n", value.name, value.integer_value);

    case IR_ADD:
        return tprint("int %s = %s + %s;\n", value.name,
            builder->values[value.binary.left].name,
            builder->values[value.binary.right].name);

    case IR_MUL:
        return tprint("int %s = %s * %s;\n", value.name,
            builder->values[value.binary.left].name,
            builder->values[value.binary.right].name);
        
    case IR_ALLOCA:
        return tprint("int *%s = alloca(sizeof(int));\n    *%s = %s;\n", value.name, value.name, builder->values[value.operand].name);

    case IR_LOAD:
        return tprint("int %s = *%s;\n", value.name, builder->values[value.operand].name);

    case IR_STORE:
        return tprint("*%s = %s;\n", builder->values[value.binary.left].name, builder->values[value.binary.right].name);

    case IR_RETURN:
        return tprint("return %s;\n", builder->values[value.operand].name);

    default:
        assert(0 && "unreachable");
    }
    // TODO: different types besides int
}

const char *c_translate_builder(const Ir_Builder *builder)
{
    // TODO: horrendous memory leak
    String_Builder sb = {0};

    sb_append_cstr(&sb, "#include <stdio.h>\n");
    sb_append_cstr(&sb, "#include <alloca.h>\n");

    sb_append_cstr(&sb, "\nint main(int argc, char **argv) {\n");

    for (Ir_Index i = 0; i < arrlen(builder->values); ++i) {
        sb_print(&sb, "    %s", c_translate_value(builder, i));
    }

    sb_append_cstr(&sb, "}\n");
    
    return sb.data;
}
