#include <string.h>

#include "common.h"
#include "interp.h"
#include "object.h"

#define INTERP_DO_TRACING

#define xx (void*)

static void infer_object(Interp *interp, Object *object)
{
    assert(!object->inferred_type);

    const Ast_Declaration *decl = object->key;
    const Ast *ast = decl->expression;

    // Type definitions must add a type into the type table.

    if (ast->type == AST_TYPE_DEFINITION) {
        const Ast_Type_Definition *defn = xx ast;

        object->type_value = interp_get_type(interp, defn);

        if (object->type_value != NULL) {
            // We only want to set the inferred type if we successfully created our type.
            object->inferred_type = interp->type_table.TYPE;

            #if defined(INTERP_DO_TRACING)
                printf("[defn] "SV_Fmt" :: %s\n", SV_Arg(decl->ident->name), type_to_string(&interp->type_table, object->type_value));
            #endif
        } 

        return;
    }

    object->inferred_type = typecheck_expression(interp, ast);

    // Replace comptime types for non-comptime declarations.

    if (object->inferred_type != NULL && (decl->flags & DECLARATION_IS_COMPTIME) == 0) {
        if (object->inferred_type == interp->type_table.comptime_int)         object->inferred_type = interp->type_table.INT;
        else if (object->inferred_type == interp->type_table.comptime_float)  object->inferred_type = interp->type_table.FLOAT;
        else if (object->inferred_type == interp->type_table.comptime_string) object->inferred_type = interp->type_table.STRING;
    }
}

#define BYTE 8

static LLVMTypeRef llvm_build_type(Interp *interp, Type inferred_type)
{
    // TODO: handle type_table.null
    assert(inferred_type != interp->type_table.null);
    
    switch (inferred_type->tag) {
    case TYPE_INTEGER:
        return LLVMIntTypeInContext(interp->llvm_context, inferred_type->runtime_size * BYTE);
    case TYPE_FLOAT:
        if (inferred_type == interp->type_table.FLOAT) {
            return LLVMFloatTypeInContext(interp->llvm_context);
        }
        // We only have 2 floating-point types.
        assert(inferred_type == interp->type_table.float64);
        return LLVMDoubleTypeInContext(interp->llvm_context);
    case TYPE_BOOL:
        return LLVMIntTypeInContext(interp->llvm_context, 1 * BYTE);
    case TYPE_STRING: {
        LLVMTypeRef elems[] = {
            LLVMPointerType(LLVMInt8TypeInContext(interp->llvm_context), 0), // *u8
            LLVMInt64TypeInContext(interp->llvm_context), // u64
        };
        return LLVMStructTypeInContext(interp->llvm_context, elems, sizeof(elems)/sizeof(elems[0]), 1); // 1 means packed
    }
    case TYPE_VOID:
        return LLVMVoidTypeInContext(interp->llvm_context);
    case TYPE_PROCEDURE: {
        const Type_Info_Procedure *info = xx inferred_type;
        assert(info->parameter_count <= 128);
        LLVMTypeRef param_types[128];
        for (size_t i = 0; i < info->parameter_count; ++i) {
            param_types[i] = llvm_build_type(interp, info->parameters[i]);
        }
        LLVMTypeRef return_type = llvm_build_type(interp, info->return_type);
        return LLVMFunctionType(return_type, param_types, info->parameter_count, 0); // 0 means no varargs
    }
    case TYPE_STRUCT: {
        const Type_Info_Struct *info = xx inferred_type;
        assert(info->field_count <= 128);
        LLVMTypeRef field_types[128];
        for (size_t i = 0; i < info->field_count; ++i) {
            field_types[i] = llvm_build_type(interp, info->field_data[i].type);
        }
        return LLVMStructTypeInContext(interp->llvm_context, field_types, info->field_count, 1); // 1 means packed
    }
    case TYPE_POINTER: {
        // We store a type like '***int' by storing the base element and the number of pointers.
        // But, LLVM wants pointer(pointer(pointer(int))) so we have to unroll it.
        const Type_Info_Pointer *info = xx inferred_type;
        LLVMTypeRef res = llvm_build_type(interp, info->element_type);
        for (size_t i = 0; i < info->pointer_level; ++i) {
            res = LLVMPointerType(res, 0);
        }
        return res;
    }
    case TYPE_ARRAY: {
        const Type_Info_Array *info = xx inferred_type;
        return LLVMArrayType(llvm_build_type(interp, info->element_type), (unsigned) info->element_count);
    }
    case TYPE_TYPE:
    case TYPE_CODE:
        return LLVMPointerTypeInContext(interp->llvm_context, 0);
    }
}

static inline LLVMValueRef llvm_create_stack_variable(Interp *interp, LLVMTypeRef type, LLVMValueRef initializer)
{
    // Step 1: Allocate memory on the stack for the variable
    LLVMValueRef alloca_inst = LLVMBuildAlloca(interp->llvm_builder, type, "");

    // Step 2: Store the constant initializer into the allocated memory
    LLVMBuildStore(interp->llvm_builder, initializer, alloca_inst);

    return alloca_inst;
}

static LLVMValueRef llvm_build_value_from_ast(Interp *interp, const Ast *ast, LLVMTypeRef type)
{
    assert(ast);   
    switch (ast->type) {
    case AST_BLOCK: {
        const Ast_Block *block = xx ast;
        LLVMBasicBlockRef basic_block = LLVMCreateBasicBlockInContext(interp->llvm_context, NULL);
        LLVMPositionBuilderAtEnd(interp->llvm_builder, basic_block);
        For (block->statements) {
            llvm_build_value_from_ast(interp, block->statements[it], NULL);
        }
        return LLVMBasicBlockAsValue(basic_block);
    }

    case AST_LITERAL: {
        const Ast_Literal *lit = xx ast;
        assert(type);
        switch (lit->kind) {
        case LITERAL_INT:
            return LLVMConstInt(type, lit->int_value, 1);
        case LITERAL_BOOL:
            return LLVMConstInt(type, lit->bool_value, 0);
        case LITERAL_NULL:
            return LLVMConstNull(type);
        case LITERAL_FLOAT:
            return LLVMConstReal(type, (double) lit->float_value);
        case LITERAL_STRING: {
            const char *data = lit->string_value.data;
            size_t count = lit->string_value.count;

            // Create the array and *u8 types.
            LLVMTypeRef u8_type = LLVMInt8TypeInContext(interp->llvm_context);
            LLVMTypeRef array_type = LLVMArrayType(u8_type, count);
            LLVMTypeRef u8_pointer_type = LLVMPointerType(u8_type, 0);

            // Create global variable with the array of characters.
            LLVMValueRef global_string = LLVMAddGlobal(interp->llvm_module, array_type, "");
            LLVMSetLinkage(global_string, LLVMPrivateLinkage);
            LLVMSetInitializer(global_string, LLVMConstStringInContext(interp->llvm_context, data, count, 1));

            // Create constant structure value.
            LLVMValueRef struct_fields[] = {
                LLVMConstBitCast(global_string, u8_pointer_type),
                LLVMConstInt(LLVMInt64TypeInContext(interp->llvm_context), count, 0),
            };
                    
            return LLVMConstStructInContext(interp->llvm_context,
                struct_fields, sizeof(struct_fields)/sizeof(struct_fields[0]), 1);
        }
        }
    }

    case AST_IDENT:
        UNIMPLEMENTED;

    case AST_UNARY_OPERATOR: {
        const Ast_Unary_Operator *unary = xx ast;
        (void)unary;
        UNIMPLEMENTED;
    }

    case AST_DECLARATION:
        assert(0);

    default:
        // Unhandled
        assert(0);
    }
}

Type interp_get_type(Interp *interp, const Ast_Type_Definition *defn)
{
    {
        const Type_Info *existing_type = hmget(interp->type_definitions, defn);
        if (existing_type) return existing_type;
    }
    
    if (defn->struct_desc) {
        const Ast_Block *block = defn->struct_desc->block;

        ptrdiff_t *children = NULL;

        For (block->statements) {
            if (block->statements[it]->type != AST_DECLARATION) continue;

            const Ast_Declaration *decl = xx block->statements[it]; 

            if (decl->flags & DECLARATION_IS_COMPTIME) continue; // Skip comptime.
            
            ptrdiff_t child = hmgeti(interp->objects, decl);
            assert(child >= 0);

            if (interp->objects[child].inferred_type == NULL) {
                // Child is not finished, so we can't make our type yet.
                return NULL;
            }

            arrput(children, child);
        }

        size_t n = arrlenu(children);

        Type_Info_Struct struct_type;
        struct_type.info.tag = TYPE_STRUCT;
        struct_type.info.runtime_size = -1;
        struct_type.field_data = arena_alloc(&interp->type_table.arena, n * sizeof(Type_Info_Struct_Field));
        struct_type.field_count = n;

        For (children) {
            Object *object = &interp->objects[children[it]];
            Type_Info_Struct_Field *field = struct_type.field_data + it;
            field->type = object->inferred_type;
            field->name = arena_sv_to_cstr(&interp->type_table.arena, object->key->ident->name);
            field->offset = -1;
        }

        Type type = type_table_append(&interp->type_table, &struct_type, sizeof(struct_type));
        hmput(interp->type_definitions, defn, type);
        return type;
    }

    if (defn->enum_defn) {
        UNIMPLEMENTED;
    }

    if (defn->type_name) {
        Type type = parse_literal_type(&interp->type_table, defn->type_name->name);
        if (type != NULL) {
            hmput(interp->type_definitions, defn, type);
            return type;
        }
        
        const Ast_Declaration *decl = find_declaration_or_null(defn->type_name);
        if (!decl) {
            parser_report_error(&interp->parser, decl->base.location, "Undeclared identifier '"SV_Fmt"'.", SV_Arg(defn->type_name->name));
            return NULL;
        }
        Object *object = hmgetp_null(interp->objects, decl);
        assert(object);
        if (object->inferred_type == NULL) return NULL; // We must wait on this.
        if (object->inferred_type != interp->type_table.TYPE) {
            parser_report_error(&interp->parser, decl->base.location, "Type mismatch: '"SV_Fmt"' is not a type (actual type is %s).",
                SV_Arg(defn->type_name->name), type_to_string(&interp->type_table, object->inferred_type));
            return NULL;
        }
        hmput(interp->type_definitions, defn, object->type_value);
        return object->type_value;
    }

    if (defn->array_element_type) {
        UNIMPLEMENTED;
    }

    if (defn->pointer_to) {
        UNIMPLEMENTED;       
    }

    if (defn->struct_call) {
        UNIMPLEMENTED;
    }

    if (defn->lambda_argument_types && defn->lambda_return_type) {
        UNIMPLEMENTED;
    }

    UNREACHABLE;
}

void interp_add_scope(Interp *interp, const Ast_Block *block)
{
    For (block->statements) {
        // Adding objects in a depth-first order.
        
        if (block->statements[it]->type == AST_BLOCK) {
            interp_add_scope(interp, xx block->statements[it]);
            continue;
        }

        // We only care about declarations.
        
        if (block->statements[it]->type != AST_DECLARATION) continue;

        const Ast_Declaration *decl = xx block->statements[it];

        // Add inner scopes.

        if (decl->expression->type == AST_TYPE_DEFINITION) {
            const Ast_Type_Definition *defn = xx decl->expression;
            if (defn->struct_desc) interp_add_scope(interp, defn->struct_desc->block);
            if (defn->enum_defn)   interp_add_scope(interp, defn->enum_defn->block);
        } else if (decl->expression->type == AST_LAMBDA) {
            const Ast_Lambda *lambda = xx decl->expression;
            interp_add_scope(interp, &lambda->body.block);
        }
        
        Object object = init_object(decl);
        ptrdiff_t i = hmlen(interp->objects);
        hmputs(interp->objects, object);
        arrput(interp->queue, i);
    }
}

void interp_run_main_loop(Interp *interp)
{
    while (arrlenu(interp->queue)) {
        size_t i = 0;
        while (i < arrlenu(interp->queue)) {
            Object *object = &interp->objects[interp->queue[i]];
            infer_object(interp, object);

            if (object->inferred_type) {
                arrdelswap(interp->queue, i);
            } else {
                i += 1;
            }
        }
    }

    {
        LLVMTypeRef llvm_function_type = LLVMFunctionType(LLVMVoidTypeInContext(interp->llvm_context), NULL, 0, 0);
        LLVMValueRef llvm_function = LLVMAddFunction(interp->llvm_module, "main", llvm_function_type);
        LLVMBasicBlockRef llvm_block = LLVMAppendBasicBlockInContext(interp->llvm_context, llvm_function, "");
        LLVMPositionBuilderAtEnd(interp->llvm_builder, llvm_block);
    }

    for (int i = 0; i < hmlen(interp->objects); ++i) {
        const Ast_Declaration *decl = interp->objects[i].key;
        Type type = interp->objects[i].inferred_type;
        assert(type);

        LLVMTypeRef llvm_type = llvm_build_type(interp, type);
        LLVMValueRef llvm_value = llvm_build_value_from_ast(interp, decl->expression, llvm_type);

        if ((decl->flags&DECLARATION_IS_COMPTIME) == 0) {
            // Wrap the value in a stack variable.
            llvm_value = llvm_create_stack_variable(interp, llvm_type, llvm_value);
        }

        interp->objects[i].llvm_value = llvm_value;
        interp->objects[i].llvm_type = llvm_type;
    }

    printf("--- Generated LLVM module. ---\n");
    LLVMDumpModule(interp->llvm_module);
}

void interp_prepare_llvm_context(Interp *interp)
{
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmParsers();
    LLVMInitializeAllAsmPrinters();

    interp->llvm_context = LLVMContextCreate();
    LLVMContextSetOpaquePointers(interp->llvm_context, 1);
    LLVMContextSetDiscardValueNames(interp->llvm_context, 1);

    interp->llvm_module = LLVMModuleCreateWithNameInContext(interp->workspace_name, interp->llvm_context);
    LLVMSetSourceFileName(interp->llvm_module, interp->parser.path_name.data, interp->parser.path_name.count);

    interp->llvm_builder = LLVMCreateBuilderInContext(interp->llvm_context);
}

Interp init_interp(const char *workspace_name)
{
    Interp interp;
    interp.workspace_name = workspace_name;
    interp.arena = (Arena){0};

    interp.objects = NULL;
    interp.type_definitions = NULL;
    hmdefault(interp.type_definitions, (Type)NULL);
    interp.queue = NULL;
    interp.type_table = type_table_init();

    interp.llvm_context = NULL;
    interp.llvm_module = NULL;
    interp.llvm_builder = NULL;
    return interp;
}

Object init_object(const Ast_Declaration *declaration)
{
    Object object;
    object.key = declaration;
    object.inferred_type = NULL;
    object.llvm_type = NULL;
    object.type_value = NULL;
    return object;
}
