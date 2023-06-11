#include <llvm-c/Core.h>
#include <llvm-c/Target.h>

#include "common.h"
#include "workspace.h"

#define xx (void*)

void workspace_setup_llvm(Workspace *w)
{
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmParsers();
    LLVMInitializeAllAsmPrinters();

    w->llvm.context = LLVMContextCreate();
    LLVMContextSetOpaquePointers(w->llvm.context, 1);
    LLVMContextSetDiscardValueNames(w->llvm.context, 1);

    w->llvm.module = LLVMModuleCreateWithNameInContext(w->name, w->llvm.context);
    w->llvm.builder = LLVMCreateBuilderInContext(w->llvm.context);
}

LLVMTypeRef llvm_create_type(Llvm llvm, Ast_Type_Definition *type_def)
{
    assert(type_info);
    switch (type_info->tag) {
    case TYPE_INTEGER:
        return LLVMIntTypeInContext(llvm.context, type_info->runtime_size * 8);
    case TYPE_FLOAT:
        switch (type_info->runtime_size) {
        case 1: assert(0);
        case 2: return LLVMHalfTypeInContext(llvm.context);
        case 4: return LLVMFloatTypeInContext(llvm.context);
        case 8: return LLVMDoubleTypeInContext(llvm.context);
        default: assert(0);
        }
    case TYPE_BOOL:
        return LLVMInt8TypeInContext(llvm.context);
    case TYPE_STRING: {
        LLVMTypeRef elems[] = {
            LLVMPointerType(LLVMInt8TypeInContext(llvm.context), 0), // *u8
            LLVMInt64TypeInContext(llvm.context),                    // s64
        };
        return LLVMStructTypeInContext(llvm.context, elems, sizeof(elems)/sizeof(elems[0]), 1); // 1 means packed
    }
    case TYPE_VOID:
        return LLVMVoidTypeInContext(llvm.context);
    case TYPE_PROCEDURE: {
        const Type_Info_Procedure *proc_info = xx type_info;
        LLVMTypeRef *param_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * proc_info->parameter_count);
        for (size_t i = 0; i < proc_info->parameter_count; ++i) {
            param_types[i] = llvm_create_type(llvm, proc_info->parameters[i]);
        }
        LLVMTypeRef return_type = llvm_create_type(llvm, proc_info->return_type);
        return LLVMFunctionType(return_type, param_types, proc_info->parameter_count, 0); // 0 means not variadic
    }
    case TYPE_STRUCT: {
        const Type_Info_Struct *struct_info = xx type_info;
        LLVMTypeRef *field_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * struct_info->field_count);
        for (size_t i = 0; i < struct_info->field_count; ++i) {
            field_types[i] = llvm_create_type(llvm, struct_info->field_data[i].type);
        }
        return LLVMStructTypeInContext(llvm.context, field_types, struct_info->field_count, 1); // 1 means packed
    }
    case TYPE_POINTER: {
        // We store a type like '***int' by storing the base element and the number of pointers.
        // But, LLVM wants pointer(pointer(pointer(int))) so we have to unroll it.
        const Type_Info_Pointer *pointer_info = xx type_info;
        LLVMTypeRef res = llvm_create_type(llvm, pointer_info->element_type);
        for (size_t i = 0; i < pointer_info->pointer_level; ++i) {
            res = LLVMPointerType(res, 0);
        }
        return res;
    }
    case TYPE_ARRAY: {
        const Type_Info_Array *array_info = xx type_info;
        return LLVMArrayType(llvm_create_type(llvm, array_info->element_type), (unsigned) array_info->element_count);
    }
    case TYPE_TYPE:
    case TYPE_CODE:
        return LLVMPointerTypeInContext(llvm.context, 0);
    }
}

LLVMValueRef llvm_const_string(Llvm llvm, const char *data, size_t count)
{
    // Create the array and *u8 types.
    LLVMTypeRef u8_type = LLVMInt8TypeInContext(llvm.context);
    LLVMTypeRef array_type = LLVMArrayType(u8_type, count);
    LLVMTypeRef u8_pointer_type = LLVMPointerType(u8_type, 0);

    // Create global variable with the array of characters.
    LLVMValueRef global_string = LLVMAddGlobal(llvm.module, array_type, "");
    LLVMSetLinkage(global_string, LLVMPrivateLinkage);
    LLVMSetInitializer(global_string, LLVMConstStringInContext(llvm.context, data, count, 1));

    // Create constant structure value.
    LLVMValueRef struct_fields[] = {
        LLVMConstBitCast(global_string, u8_pointer_type),
        LLVMConstInt(LLVMInt64TypeInContext(llvm.context), count, 0),
    };
            
    return LLVMConstStructInContext(llvm.context,
        struct_fields, sizeof(struct_fields)/sizeof(struct_fields[0]), 1);
}

LLVMValueRef llvm_create_value(Llvm llvm, LLVMTypeRef type, const Ast *ast)
{
    assert(ast);
    switch (ast->type) {
    case AST_BLOCK: {
        const Ast_Block *block = xx ast;
        LLVMBasicBlockRef basic_block = LLVMCreateBasicBlockInContext(llvm.context, NULL);
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block);
        For (block->statements) {
            llvm_create_value(llvm, NULL, block->statements[it]);
        }
        return LLVMBasicBlockAsValue(basic_block);
    }

    case AST_LITERAL: {
        assert(type);
        const Ast_Literal *lit = xx ast;
        (void)lit;
        UNIMPLEMENTED;
        return NULL;
        // switch (lit->kind) {
        // case LITERAL_INT:   return LLVMConstInt(type, lit->int_value, 1);
        // case LITERAL_BOOL:  return LLVMConstInt(type, lit->bool_value, 0);
        // case LITERAL_NULL:  return LLVMConstNull(type);
        // case LITERAL_FLOAT: return LLVMConstReal(type, (double) lit->float_value);
        // case LITERAL_STRING: return llvm_const_string(llvm, lit->string_value.data, lit->string_value.count);
        // }
    }

    default:
        UNIMPLEMENTED;
    }
}
