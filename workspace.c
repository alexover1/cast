#include <errno.h>
#include <string.h> // strerror...
#include <limits.h>
#include <float.h>

#include "workspace.h"

void workspace_parse_entire_file(Workspace *w, Source_File file);

static Ast_Type_Definition *make_type_leaf(Workspace *w, const char *literal_name, int size_in_bytes)
{
    Ast_Type_Definition *defn = context_alloc(sizeof(*defn)); // context_alloc zero-inits.
    defn->_expression.kind = AST_TYPE_DEFINITION;
    defn->_expression.inferred_type = w->type_def_type;
    defn->literal_name = literal_name;
    defn->size = size_in_bytes;
    defn->flags = TYPE_IS_LEAF | TYPE_IS_LITERAL | TYPE_HAS_STORAGE;
    return defn;
}

static Ast_Type_Definition *make_integer_type_leaf(Workspace *w, const char *literal_name, int size_in_bytes, bool is_signed, unsigned long low, unsigned long high)
{
    Ast_Type_Definition *defn = make_type_leaf(w, literal_name, size_in_bytes);
    defn->flags |= TYPE_IS_NUMERIC;
    defn->number_literal_low = make_number(low);
    defn->number_literal_high = make_number(high);
    if (is_signed) {
        defn->number_flags = NUMBER_FLAGS_SIGNED;
        defn->number_literal_low->flags |= NUMBER_FLAGS_SIGNED;
        defn->number_literal_high->flags |= NUMBER_FLAGS_SIGNED;
    }
    return defn;
}

static Ast_Type_Definition *make_float_type_leaf(Workspace *w, const char *literal_name, int size_in_bytes, bool requires_float64, double low, double high)
{
    Ast_Type_Definition *defn = make_type_leaf(w, literal_name, size_in_bytes);
    defn->flags |= TYPE_IS_NUMERIC;
    defn->number_flags = NUMBER_FLAGS_FLOAT;
    defn->number_literal_low = make_number_float(low);
    defn->number_literal_high = make_number_float(high);
    if (requires_float64) {
        defn->number_flags |= NUMBER_FLAGS_FLOAT64;
        defn->number_literal_low->flags |= NUMBER_FLAGS_FLOAT64;
        defn->number_literal_high->flags |= NUMBER_FLAGS_FLOAT64;
    }
    return defn;
}

inline Ast_Number *make_number(unsigned long value)
{
    Ast_Number *number = context_alloc(sizeof(*number));
    number->_expression.kind = AST_NUMBER;
    number->as.integer = value;
    return number;
}

inline Ast_Number *make_number_float(double value)
{
    Ast_Number *number = context_alloc(sizeof(*number));
    number->_expression.kind = AST_NUMBER;
    number->flags = NUMBER_FLAGS_FLOAT;
    number->as.real = value;
    return number;
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
#if 0
        String_Builder sb = {0};
        print_decl_to_builder(&sb, decl, 0);
        sb_append_cstr(&sb, "\n");
        For (decl->flattened) {
            Ast_Node node = decl->flattened[it];
            sb_append_cstr(&sb, ">>> ");
            if (node.expression) {
                sb_print(&sb, "[%d] ", node.expression->kind);
                print_expr_to_builder(&sb, node.expression, 0);
            }
            if (node.statement)  print_stmt_to_builder(&sb, node.statement, 0);
            sb_append_cstr(&sb, "\n");
        }
        printf(SV_Fmt, SV_Arg(sb));
#endif
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
    // Add all function declarations in a pre-pass.
    For (w->declarations) {
        Ast_Declaration *decl = w->declarations[it];
        if (decl->flags & DECLARATION_IS_PROCEDURE_BODY) {
            LLVMTypeRef type = llvm_get_type(w, decl->my_type); assert(type);
            LLVMValueRef function = LLVMAddFunction(w->llvm.module, "", type);
            decl->llvm_value = function;
        }
    }
    
    // Now build the LLVM IR.
    For (w->declarations) {
        Ast_Declaration *decl = w->declarations[it];

        if (!(decl->flags & DECLARATION_IS_CONSTANT)) continue;

        llvm_build_declaration(w, decl);
    }
}

void workspace_save(Workspace *w, const char *ir_path, const char *asm_path)
{
    char *error_message = NULL;

    LLVMPrintModuleToFile(w->llvm.module, ir_path, &error_message);
    if (error_message) {
        fprintf(stderr, "Error: Could not output LLVM module to file '%s': %s.\n", ir_path, error_message);
        LLVMDisposeMessage(error_message);
    }

    if (LLVMTargetMachineEmitToFile(w->llvm.target_machine, w->llvm.module, asm_path, LLVMObjectFile, &error_message) != 0) {
        fprintf(stderr, "Error: Could not output object file '%s': %s.\n", asm_path, error_message);
        LLVMDisposeMessage(error_message);
    }
}

void workspace_init(Workspace *w, const char *name)
{
    w->name = name;
    w->llvm = (Llvm){0};
    w->declarations = NULL;
    w->files = NULL;

    // Create type definitions for built-in types.
    w->type_def_type = make_type_leaf(w, "Type", 8);
    w->type_def_type->_expression.inferred_type = w->type_def_type; // And on and on and on...

    w->type_def_int = make_integer_type_leaf(w, "int", 8, true,  LLONG_MIN, LLONG_MAX);
    w->type_def_u8  = make_integer_type_leaf(w, "u8",  1, false, 0,         UCHAR_MAX);
    w->type_def_u16 = make_integer_type_leaf(w, "u16", 2, false, 0,         USHRT_MAX);
    w->type_def_u32 = make_integer_type_leaf(w, "u32", 4, false, 0,         UINT_MAX);
    w->type_def_u64 = make_integer_type_leaf(w, "u64", 8, false, 0,         ULLONG_MAX);
    w->type_def_s8  = make_integer_type_leaf(w, "s8",  1, true,  SCHAR_MIN, SCHAR_MAX);
    w->type_def_s16 = make_integer_type_leaf(w, "s16", 2, true,  SHRT_MIN,  SHRT_MAX);
    w->type_def_s32 = make_integer_type_leaf(w, "s32", 4, true,  INT_MIN,   INT_MAX);
    w->type_def_s64 = make_integer_type_leaf(w, "s64", 8, true,  LLONG_MIN, LLONG_MAX);

    w->type_def_float   = make_float_type_leaf(w, "float",   4, false, FLT_MIN, FLT_MAX);
    w->type_def_float32 = make_float_type_leaf(w, "float32", 4, false, FLT_MIN, FLT_MAX);   
    w->type_def_float64 = make_float_type_leaf(w, "float64", 8, true,  DBL_MIN, DBL_MAX);
    
    w->type_def_bool = make_type_leaf(w, "bool", 1);
    w->type_def_bool->literal_kind = LITERAL_BOOL;

    w->type_def_string = make_type_leaf(w, "string", 16);
    w->type_def_string->literal_kind = LITERAL_STRING;

    w->type_def_void = make_type_leaf(w, "void", 0);
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
