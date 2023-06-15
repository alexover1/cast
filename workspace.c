#include <errno.h>
#include <string.h> // strerror...
#include <limits.h>
#include <float.h>

#include "workspace.h"

void workspace_parse_entire_file(Workspace *w, Source_File file);

static Ast_Type_Definition *make_type_leaf(Workspace *w, const char *literal_name, int size_in_bytes)
{
    Ast_Type_Definition *leaf = context_alloc(sizeof(*leaf)); // context_alloc zero-inits.
    leaf->_expression.kind = AST_TYPE_DEFINITION;
    leaf->_expression.inferred_type = w->type_def_type;
    leaf->literal_name = literal_name;
    leaf->size = size_in_bytes;
    leaf->is_leaf = true;
    return leaf;
}

static Ast_Type_Definition *make_integer_type_leaf(Workspace *w, const char *literal_name, int size_in_bytes, unsigned long low, unsigned long high)
{
    Ast_Type_Definition *leaf = make_type_leaf(w, literal_name, size_in_bytes);
    leaf->number_flags = NUMBER_FLAGS_NUMBER;
    leaf->number_literal_low = make_integer_literal(leaf, low);
    leaf->number_literal_high = make_integer_literal(leaf, high);
    return leaf;
}

static Ast_Type_Definition *make_pointer_type_definition(Workspace *w, size_t pointer_level, Ast_Type_Definition *pointer_to)
{
    Ast_Type_Definition *defn = context_alloc(sizeof(*defn));
    defn->_expression.kind = AST_TYPE_DEFINITION;
    defn->_expression.inferred_type = w->type_def_type;
    defn->pointer_level = pointer_level;
    defn->pointer_to = pointer_to;
    defn->size = 8;
    return defn;
}

inline Ast_Literal *make_literal(Ast_Type_Definition *default_type, unsigned long value)
{   
    Ast_Literal *literal = context_alloc(sizeof(*literal));
    literal->_expression.kind = AST_LITERAL;
    literal->default_type = default_type;
    literal->integer_value = value;
    return literal;
}

inline Ast_Literal *make_integer_literal(Ast_Type_Definition *int_type, unsigned long value)
{
    Ast_Literal *literal = make_literal(int_type, value);
    literal->number_flags = NUMBER_FLAGS_NUMBER;
    return literal;
}

inline Ast_Literal *make_float_literal(Ast_Type_Definition *float_type, double value)
{
    Ast_Literal *literal = make_literal(float_type, *((unsigned long *)&value));
    literal->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT;
    return literal;
}

inline Ast_Literal *make_string_literal(Ast_Type_Definition *string_type, String_View value)
{
    Ast_Literal *literal = context_alloc(sizeof(*literal));
    literal->_expression.kind = AST_LITERAL;
    literal->default_type = string_type;
    literal->string_value = value;
    return literal;
}

// Currently, this always uses malloc because we call free() on the data,
// and we don't have a system in C for storing the allocator that allocated the data.
Source_File os_read_entire_file(const char *path_as_cstr)
{
    Source_File file;
    file.lines = NULL;

    // TODO: We should probably copy these as well.
    file.name = path_get_file_name(path_as_cstr);
    file.path = sv_from_cstr(path_as_cstr);

    // Read the file.
    FILE *handle = fopen(path_as_cstr, "rb");
    if (handle == NULL) {
        goto error;
    }

    if (fseek(handle, 0, SEEK_END) < 0) {
        goto error;
    }

    long m = ftell(handle);
    if (m < 0) {
        goto error;
    }

    char *buffer = malloc(m);
    if (buffer == NULL) {
        goto error;
    }

    if (fseek(handle, 0, SEEK_SET) < 0) {
        goto error;
    }

    size_t n = fread(buffer, 1, (size_t) m, handle);
    if (ferror(handle)) {
        goto error;
    }

    file.data = buffer;
    file.size = n;

    return file;

error:
    if (handle) fclose(handle);
    fprintf(stderr, "Error: Could not read source file '%s': %s\n", path_as_cstr, strerror(errno));
    exit(1);
}

void workspace_typecheck(Workspace *w)
{
    Ast_Declaration **queue = NULL;
    
    For (w->declarations) {
        Ast_Declaration *decl = w->declarations[it];

        if (!(decl->flags & DECLARATION_IS_CONSTANT)) continue; // Only constant declarations get async processing.

        flatten_decl_for_typechecking(decl);
        arrput(queue, decl);
    }
    
    while (arrlenu(queue)) {
        size_t i = 0;
        while (i < arrlenu(queue)) {
            typecheck_declaration(w, queue[i]);
            if (queue[i]->flags & DECLARATION_HAS_BEEN_TYPECHECKED) {
                arrdelswap(queue, i);
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
        (void)it;
    }
}

void workspace_init(Workspace *w, const char *name)
{
    w->name = name;
    w->llvm = (Llvm){0};
    w->declarations = NULL;
    w->typecheck_queue = NULL;

    // Create type definitions for built-in types.
    w->type_def_type = make_type_leaf(w, "Type", 8);
    w->type_def_type->_expression.inferred_type = w->type_def_type; // And on and on and on...

    w->type_def_int = make_integer_type_leaf(w, "int", 8, LLONG_MIN, LLONG_MAX);
    w->type_def_u8  = make_integer_type_leaf(w, "u8",  1, 0,         UCHAR_MAX);
    w->type_def_u16 = make_integer_type_leaf(w, "u16", 2, 0,         USHRT_MAX);
    w->type_def_u32 = make_integer_type_leaf(w, "u32", 4, 0,         UINT_MAX);
    w->type_def_u64 = make_integer_type_leaf(w, "u64", 8, 0,         ULLONG_MAX);
    w->type_def_s8  = make_integer_type_leaf(w, "s8",  1, SCHAR_MIN, SCHAR_MAX);
    w->type_def_s16 = make_integer_type_leaf(w, "s16", 2, SHRT_MIN,  SHRT_MAX);
    w->type_def_s32 = make_integer_type_leaf(w, "s32", 4, INT_MIN,   INT_MAX);
    w->type_def_s64 = make_integer_type_leaf(w, "s64", 8, LLONG_MIN, LLONG_MAX);

    w->type_def_s8->number_flags |= NUMBER_FLAGS_SIGNED;
    w->type_def_s16->number_flags |= NUMBER_FLAGS_SIGNED;
    w->type_def_s32->number_flags |= NUMBER_FLAGS_SIGNED;
    w->type_def_s64->number_flags |= NUMBER_FLAGS_SIGNED;
    
    w->type_def_float = make_type_leaf(w, "float", 4);
    w->type_def_float->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT;
    w->type_def_float->number_literal_low = make_float_literal(w->type_def_float, FLT_MIN);
    w->type_def_float->number_literal_high = make_float_literal(w->type_def_float, FLT_MAX);

    w->type_def_float32 = make_type_leaf(w, "float32", 4);
    w->type_def_float32->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT;
    w->type_def_float32->number_literal_low = make_float_literal(w->type_def_float32, FLT_MIN);
    w->type_def_float32->number_literal_high = make_float_literal(w->type_def_float32, FLT_MAX);
    
    w->type_def_float64 = make_type_leaf(w, "float64", 8);
    w->type_def_float64->number_flags = NUMBER_FLAGS_NUMBER | NUMBER_FLAGS_FLOAT | NUMBER_FLAGS_DOUBLE;
    w->type_def_float64->number_literal_low = make_float_literal(w->type_def_float64, DBL_MIN);
    w->type_def_float64->number_literal_low->number_flags |= NUMBER_FLAGS_DOUBLE;
    w->type_def_float64->number_literal_high = make_float_literal(w->type_def_float64, DBL_MAX);
    w->type_def_float64->number_literal_high->number_flags |= NUMBER_FLAGS_DOUBLE;
    
    w->type_def_bool = make_type_leaf(w, "bool", 1);
    w->type_def_void = make_type_leaf(w, "void", 0);
    w->type_def_void_pointer = make_pointer_type_definition(w, 1, w->type_def_void);
    w->type_def_string = make_type_leaf(w, "string", 16);
   
    // Create syntax tree nodes for common literals.
    w->literal_true = make_literal(w->type_def_bool, 1);
    w->literal_false = make_literal(w->type_def_bool, 0);
    w->literal_null = make_literal(w->type_def_void_pointer, 0);
}

inline void workspace_parse_entire_file(Workspace *w, Source_File file)
{
    // Add the file to the workspace.
    int fid = arrlen(w->files);
    arrput(w->files, file);

    // Create a parser and do some parsing!
    Parser *parser = parser_init(w, fid);

    parse_toplevel(parser);
    if (parser->reported_error) exit(1);
    // flatten_stmt_for_typechecking(w, xx block);

    free(parser);
}

inline void workspace_add_file(Workspace *w, const char *path_as_cstr)
{
    Source_File file = os_read_entire_file(path_as_cstr);
    workspace_parse_entire_file(w, file);
}

inline void workspace_add_string(Workspace *w, String_View input)
{
    Source_File file;
    file.name = (String_View)SV_STATIC("(string)");
    file.path = (String_View)SV_STATIC("(added from a string)");
    file.data = malloc(input.count);
    file.size = input.count;
    memcpy(file.data, input.data, input.count);
    file.lines = NULL;

    workspace_parse_entire_file(w, file);
}
