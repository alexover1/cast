#include <errno.h>
#include <string.h> // strerror...
#include <limits.h>
#include <float.h>

#include "workspace.h"

void workspace_parse_entire_file(Workspace *w, Source_File file);

static Ast_Type_Definition *make_type_definition(Workspace *w, const char *name, Type_Def_Kind kind, int size_bytes)
{
    Ast_Type_Definition *defn = context_alloc(sizeof(*defn));
    defn->_expression.kind = AST_TYPE_DEFINITION;
    defn->_expression.inferred_type = w->type_def_type;
    defn->name = name;
    defn->kind = kind;
    defn->size = size_bytes;
    return defn;
}

static Ast_Type_Definition *make_type_definition_literal(Workspace *w, Literal_Kind literal_kind, const char *literal_name, int size_bytes)
{
    Ast_Type_Definition *defn = make_type_definition(w, literal_name, TYPE_DEF_LITERAL, size_bytes);
    defn->literal = literal_kind;
    return defn;
}

static Ast_Type_Definition *make_type_definition_integer(Workspace *w, const char *literal_name, int size_bytes, bool is_signed, unsigned long low, unsigned long high)
{
    Ast_Type_Definition *defn = make_type_definition(w, literal_name, TYPE_DEF_NUMBER, size_bytes);
    defn->number.literal_low = make_number(low);
    defn->number.literal_high = make_number(high);
    if (is_signed) {
        defn->number.flags = NUMBER_FLAGS_SIGNED;
        defn->number.literal_low->flags |= NUMBER_FLAGS_SIGNED;
        defn->number.literal_high->flags |= NUMBER_FLAGS_SIGNED;
    }
    return defn;
}

static Ast_Type_Definition *make_type_definition_float(Workspace *w, const char *literal_name, int size_bytes, bool requires_float64, double low, double high)
{
    Ast_Type_Definition *defn = make_type_definition(w, literal_name, TYPE_DEF_NUMBER, size_bytes);
    defn->number.flags = NUMBER_FLAGS_FLOAT;
    defn->number.literal_low = make_number_float(low);
    defn->number.literal_high = make_number_float(high);
    if (requires_float64) {
        defn->number.flags |= NUMBER_FLAGS_FLOAT64;
        defn->number.literal_low->flags |= NUMBER_FLAGS_FLOAT64;
        defn->number.literal_high->flags |= NUMBER_FLAGS_FLOAT64;
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

        if (!(decl->flags & DECLARATION_IS_CONSTANT) && !(decl->flags & DECLARATION_IS_GLOBAL_VARIABLE)) continue; // Only constant declarations get async processing.

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
                sb_print(&sb, "[%d] ", (*node.expression)->kind);
                print_expr_to_builder(&sb, *node.expression, 0);
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
    // Predeclare all globals (functions and variables). TODO: We should have a "Module" system and then we call llvm_build_module which handles this.
    For (w->declarations) {
        Ast_Declaration *decl = w->declarations[it];

        if (decl->flags & DECLARATION_IS_PROCEDURE) {
            Ast_Procedure *proc = xx decl->root_expression;

            LLVMTypeRef function_type = llvm_get_type(w, proc->lambda_type);
            assert(function_type);

            const char *name = arena_sv_to_cstr(context_arena, decl->ident->name);
            LLVMValueRef function = LLVMAddFunction(w->llvm.module, name, function_type);
            LLVMSetFunctionCallConv(function, LLVMCCallConv); // Not sure if we need this, but...

            proc->llvm_value = function;
            decl->llvm_value = function;
            continue;
        }

        if (decl->flags & DECLARATION_IS_GLOBAL_VARIABLE) {
            assert(!(decl->flags & DECLARATION_IS_CONSTANT));

            const char *name = arena_sv_to_cstr(context_arena, decl->ident->name);

            LLVMTypeRef type = llvm_get_type(w, decl->my_type); assert(type);
            LLVMValueRef global = LLVMAddGlobal(w->llvm.module, type, name);
            LLVMSetLinkage(global, LLVMExternalLinkage);
            LLVMSetInitializer(global, llvm_build_expression(w, decl->root_expression));

            decl->llvm_value = global;
        }
    }
    
    // Now build the LLVM IR.
    For (w->declarations) {
        Ast_Declaration *decl = w->declarations[it];

        if (decl->flags & DECLARATION_IS_PROCEDURE) {           
            Ast_Procedure *proc = xx decl->root_expression;

            if (!proc->body_block) continue; // Has no body (most likely #foreign or a bug).

            LLVMValueRef function = proc->llvm_value;
            assert(function); // Should've been added in the pre-pass.

            LLVMBasicBlockRef entry = LLVMAppendBasicBlock(function, "entry");
            LLVMPositionBuilderAtEnd(w->llvm.builder, entry);
            llvm_build_statement(w, function, xx proc->body_block->parent); // Arguments.
            llvm_build_statement(w, function, xx proc->body_block);

            // TODO: typechecker doesn't detect missing returns of non-void functions.

            if (proc->lambda_type->lambda.return_type == w->type_def_void) {
                if (LLVMGetBasicBlockTerminator(LLVMGetLastBasicBlock(function)) == NULL) {
                    LLVMBuildRetVoid(w->llvm.builder);
                }
            }

            if (LLVMVerifyFunction(function, LLVMPrintMessageAction)) {
                printf("===============================\n");
                LLVMDumpValue(function);
                printf("===============================\n");
                exit(1);
            }
        }
    }
}

String_View path_trim_ext(String_View path)
{
    size_t i = 0;
    while (i < path.count && path.data[path.count-i-1] != '.') {
        i += 1;
    }
    if (path.data[path.count-i-1] == '.') i += 1;
    return sv_from_parts(path.data, path.count - i);
}

void workspace_save(Workspace *w)
{
    assert(arrlenu(w->files) > 0);

    String_View path = path_trim_ext(w->files[0].path);
    
    char *llvm_path = tprint(SV_Fmt".llvm", SV_Arg(path));
    char *obj_path = tprint(SV_Fmt".o", SV_Arg(path));
    char *asm_path = tprint(SV_Fmt".asm", SV_Arg(path));

    char *error_message = NULL;
    LLVMPrintModuleToFile(w->llvm.module, llvm_path, &error_message);
    if (error_message) {
        fprintf(stderr, "Error: Could not output LLVM module to file '%s': %s.\n", llvm_path, error_message);
        LLVMDisposeMessage(error_message);
    }

    if (LLVMTargetMachineEmitToFile(w->llvm.target_machine, w->llvm.module, asm_path, LLVMAssemblyFile, &error_message) != 0) {
        fprintf(stderr, "Error: Could not output assembly file '%s': %s.\n", asm_path, error_message);
        LLVMDisposeMessage(error_message);
    }

    if (LLVMTargetMachineEmitToFile(w->llvm.target_machine, w->llvm.module, obj_path, LLVMObjectFile, &error_message) != 0) {
        fprintf(stderr, "Error: Could not output object file '%s': %s.\n", obj_path, error_message);
        LLVMDisposeMessage(error_message);
    }
}

void workspace_init(Workspace *w, const char *name)
{
    w->name = name;
    w->llvm = (Llvm){0};
    w->global_block = context_alloc(sizeof(Ast_Block));
    w->declarations = NULL;
    w->files = NULL;

    // Create type definitions for built-in types.
    w->type_def_type = make_type_definition(w, "Type", TYPE_DEF_LITERAL, 8);
    w->type_def_type->_expression.inferred_type = w->type_def_type; // And on and on and on...

    w->type_def_int = make_type_definition_integer(w, "int", 8, true,  LLONG_MIN, LLONG_MAX);
    w->type_def_u8  = make_type_definition_integer(w, "u8",  1, false, 0,         UCHAR_MAX);
    w->type_def_u16 = make_type_definition_integer(w, "u16", 2, false, 0,         USHRT_MAX);
    w->type_def_u32 = make_type_definition_integer(w, "u32", 4, false, 0,         UINT_MAX);
    w->type_def_u64 = make_type_definition_integer(w, "u64", 8, false, 0,         ULLONG_MAX);
    w->type_def_s8  = make_type_definition_integer(w, "s8",  1, true,  SCHAR_MIN, SCHAR_MAX);
    w->type_def_s16 = make_type_definition_integer(w, "s16", 2, true,  SHRT_MIN,  SHRT_MAX);
    w->type_def_s32 = make_type_definition_integer(w, "s32", 4, true,  INT_MIN,   INT_MAX);
    w->type_def_s64 = make_type_definition_integer(w, "s64", 8, true,  LLONG_MIN, LLONG_MAX);

    w->type_def_float   = make_type_definition_float(w, "float",   4, false, FLT_MIN, FLT_MAX);
    w->type_def_float32 = make_type_definition_float(w, "float32", 4, false, FLT_MIN, FLT_MAX);   
    w->type_def_float64 = make_type_definition_float(w, "float64", 8, true,  DBL_MIN, DBL_MAX);
    
    w->type_def_bool = make_type_definition_literal(w, LITERAL_BOOL, "bool", 1);

    w->type_def_string = make_type_definition_literal(w, LITERAL_STRING, "string", 16);

    w->type_def_void = make_type_definition(w, "void", TYPE_DEF_LITERAL, 0);
}

inline void workspace_parse_entire_file(Workspace *w, Source_File file)
{
    // Add the file to the workspace.
    int fid = arrlen(w->files);
    arrput(w->files, file);

    // Create a parser and do some parsing!
    Parser *parser = parser_init(w, fid);
    parser->current_block = w->global_block;

    parse_toplevel(parser);
    if (parser->reported_error) exit(1);

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
