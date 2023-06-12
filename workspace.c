#include <errno.h>
#include <string.h> // strerror...
#include <limits.h>
#include <float.h>

#include "workspace.h"

static Ast_Type_Definition *make_type_leaf(const char *literal_name, int size_in_bytes)
{
    Ast_Type_Definition *leaf = context_alloc(sizeof(*leaf));
    leaf->base.type = AST_TYPE_DEFINITION;
    leaf->number_flags = 0; // Not a number.
    leaf->literal_name = literal_name;
    leaf->size = size_in_bytes;
    return leaf;
}

static Ast_Type_Definition *make_integer_type_leaf(const char *literal_name, int size_in_bytes, unsigned long low, unsigned long high)
{
    Ast_Type_Definition *leaf = context_alloc(sizeof(*leaf));
    leaf->base.type = AST_TYPE_DEFINITION;
    leaf->literal_name = literal_name;
    leaf->size = size_in_bytes;
    leaf->number_flags = NUMBER_FLAGS_NUMBER;
    leaf->number_literal_low = make_integer_literal(leaf, low);
    leaf->number_literal_high = make_integer_literal(leaf, high);
    return leaf;
}

static Ast_Type_Definition *make_pointer_type_definition(size_t pointer_level, Ast_Type_Definition *pointer_to)
{
    Ast_Type_Definition *defn = context_alloc(sizeof(*defn));
    defn->base.type = AST_TYPE_DEFINITION;
    defn->pointer_level = pointer_level;
    defn->pointer_to = pointer_to;
    defn->size = 8;
    return defn;
}

Ast_Literal *make_integer_literal(Ast_Type_Definition *type_def, unsigned long value)
{
    Ast_Literal *literal = context_alloc(sizeof(*literal));
    literal->base.type = AST_LITERAL;
    literal->base.inferred_type = type_def;
    literal->type = type_def;
    literal->number_flags = NUMBER_FLAGS_NUMBER;
    literal->integer_value = value;
    return literal;
}

Ast_Literal *make_float_literal(Ast_Type_Definition *type_def, double value)
{
    Ast_Literal *literal = context_alloc(sizeof(*literal));
    literal->base.type = AST_LITERAL;
    literal->base.inferred_type = type_def;
    literal->type = type_def;
    literal->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT;
    literal->double_value = value;
    return literal;
}

Ast_Literal *make_string_literal(Workspace *w, String_View value)
{
    Ast_Literal *literal = context_alloc(sizeof(*literal));
    literal->base.type = AST_LITERAL;
    literal->base.inferred_type = w->type_def_string;
    literal->type = w->type_def_string;
    literal->string_value = value;
    return literal;
}

void workspace_typecheck(Workspace *w)
{
    while (arrlenu(w->typecheck_queue)) {
        size_t i = 0;
        while (i < arrlenu(w->typecheck_queue)) {
            Ast *ast = w->typecheck_queue[i];
            typecheck_ast(w, ast);
            if (ast->inferred_type) {
                arrdelswap(w->typecheck_queue, i);
            } else {
                i += 1;
            }
        }
    }
}

void workspace_llvm(Workspace *w)
{
    For_ (w->declarations) {
        Ast_Declaration *it = w->declarations[it_index];

        if (it->llvm_value) continue;

        printf("---------------------------------\n");
        dump_ast(xx it);
        printf("---------------------------------\n");

        assert(it->expression);

        if (it->flags & DECLARATION_IS_CONSTANT) {
            llvm_build_value(w, NULL, it->expression);
            // printf("##################################\n");
            // LLVMDumpValue(value);
            // printf("\n");
            // printf("##################################\n");
        }
    }
}

Workspace create_workspace(const char *name)
{
    Workspace workspace;
    workspace.name = name;
    workspace.llvm = (Llvm){0};
    workspace.declarations = NULL;
    workspace.typecheck_queue = NULL;

    // Create type definitions for built-in types.
    workspace.type_def_int = make_integer_type_leaf("int", 8, LLONG_MIN, LLONG_MAX);
    workspace.type_def_u8  = make_integer_type_leaf("u8",  1, 0,         UCHAR_MAX);
    workspace.type_def_u16 = make_integer_type_leaf("u16", 2, 0,         USHRT_MAX);
    workspace.type_def_u32 = make_integer_type_leaf("u32", 4, 0,         UINT_MAX);
    workspace.type_def_u64 = make_integer_type_leaf("u64", 8, 0,         ULLONG_MAX);
    workspace.type_def_s8  = make_integer_type_leaf("s8",  1, SCHAR_MIN, SCHAR_MAX);
    workspace.type_def_s16 = make_integer_type_leaf("s16", 2, SHRT_MIN,  SHRT_MAX);
    workspace.type_def_s32 = make_integer_type_leaf("s32", 4, INT_MIN,   INT_MAX);
    workspace.type_def_s64 = make_integer_type_leaf("s64", 8, LLONG_MIN, LLONG_MAX);

    workspace.type_def_s8->number_flags |= NUMBER_FLAGS_SIGNED;
    workspace.type_def_s16->number_flags |= NUMBER_FLAGS_SIGNED;
    workspace.type_def_s32->number_flags |= NUMBER_FLAGS_SIGNED;
    workspace.type_def_s64->number_flags |= NUMBER_FLAGS_SIGNED;
    
    workspace.type_def_float = make_type_leaf("float", 4);
    workspace.type_def_float->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT;
    workspace.type_def_float->number_literal_low = make_float_literal(workspace.type_def_float, FLT_MIN);
    workspace.type_def_float->number_literal_high = make_float_literal(workspace.type_def_float, FLT_MAX);

    workspace.type_def_float32 = make_type_leaf("float32", 4);
    workspace.type_def_float32->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT;
    workspace.type_def_float32->number_literal_low = make_float_literal(workspace.type_def_float32, FLT_MIN);
    workspace.type_def_float32->number_literal_high = make_float_literal(workspace.type_def_float32, FLT_MAX);
    
    workspace.type_def_float64 = make_type_leaf("float64", 8);
    workspace.type_def_float64->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT | NUMBER_FLAGS_DOUBLE;
    workspace.type_def_float64->number_literal_low = make_float_literal(workspace.type_def_float64, DBL_MIN);
    workspace.type_def_float64->number_literal_low->number_flags |= NUMBER_FLAGS_DOUBLE;
    workspace.type_def_float64->number_literal_high = make_float_literal(workspace.type_def_float64, DBL_MAX);
    workspace.type_def_float64->number_literal_high->number_flags |= NUMBER_FLAGS_DOUBLE;
    
    workspace.type_def_bool = make_type_leaf("bool", 1);
    workspace.type_def_void = make_type_leaf("void", 0);
    workspace.type_def_void_pointer = make_pointer_type_definition(1, workspace.type_def_void);
    workspace.type_def_string = make_type_leaf("string", 16);

    workspace.type_def_type = make_type_leaf("Type", 8);
   
    // Create syntax tree nodes for common literals.
    workspace.literal_true = make_integer_literal(workspace.type_def_bool, 1);
    workspace.literal_false = make_integer_literal(workspace.type_def_bool, 0);
    workspace.literal_null = make_integer_literal(workspace.type_def_void_pointer, 0);

    return workspace;
}

inline void workspace_add_file(Workspace *w, const char *path_as_cstr)
{
    Source_File file = os_read_entire_file(path_as_cstr);
    Parser *parser = parser_init(file);
    parser->workspace = w;
    Ast_Block *block = parse_toplevel(parser);
    if (parser->reported_error) exit(1);
    flatten_for_typechecking(w, xx block);
    free(parser);
    free(file.data);
}

inline void workspace_add_string(Workspace *w, String_View input)
{
    Source_File file;
    file.name = (String_View)SV_STATIC("(string)");
    file.path = (String_View)SV_STATIC("(added from a string)");
    file.data = malloc(input.count);
    memcpy(file.data, input.data, input.count);
    file.size = input.count;
    Parser *parser = parser_init(file);
    parser->workspace = w;
    Ast_Block *block = parse_toplevel(parser);
    if (parser->reported_error) exit(1);
    flatten_for_typechecking(w, xx block);
    free(parser);
}
