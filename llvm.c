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
    // LLVMContextSetDiscardValueNames(w->llvm.context, 1);

    w->llvm.module = LLVMModuleCreateWithNameInContext(w->name, w->llvm.context);
    w->llvm.builder = LLVMCreateBuilderInContext(w->llvm.context);
}

void workspace_dispose_llvm(Workspace *w)
{
    LLVMDisposeBuilder(w->llvm.builder);
    LLVMDisposeModule(w->llvm.module);
    LLVMContextDispose(w->llvm.context); // @Bad: Why is this not consistently named?
}

LLVMTypeRef llvm_create_type(Workspace *w, const Ast_Type_Definition *type_def)
{
    Llvm llvm = w->llvm;
    assert(type_def);

    if (type_def->struct_desc) {
        // Allocate at least enough for every single declaration in the struct.
        LLVMTypeRef *field_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * arrlenu(type_def->struct_desc->block->declarations));

        size_t count = 0;
        For (type_def->struct_desc->block->declarations) {
            Ast_Declaration *member = type_def->struct_desc->block->declarations[it];
            if (member->expression && member->expression->type == AST_TYPE_INSTANTIATION) {
                field_types[count] = llvm_create_type(w, member->base.inferred_type);
                count += 1;
            }
        }
       
        return LLVMStructTypeInContext(llvm.context, field_types, count, 1); // 1 means packed
    }

    else if (type_def->enum_defn) {
        UNIMPLEMENTED;
    }

    else if (type_def->type_name) {       
        UNIMPLEMENTED;
    }

    else if (type_def->struct_call) {
        UNIMPLEMENTED;
    }

    else if (type_def->pointer_to) {
        // TODO: Handle `***void`
        if (type_def->pointer_to == w->type_def_void) {
            return LLVMPointerTypeInContext(llvm.context, 0); // opaque
        }

        LLVMTypeRef res = llvm_create_type(w, type_def->pointer_to);
        for (size_t i = 0; i < type_def->pointer_level; ++i) {
            res = LLVMPointerType(res, 0);
        }
        return res;
    }

    else if (type_def->literal_name) {
        if (type_def->number_flags & NUMBER_FLAGS_NUMBER) {
            if (type_def->number_flags & NUMBER_FLAGS_FLOAT) {
                // Floating-point type.
                if (type_def->number_flags & NUMBER_FLAGS_DOUBLE) {
                    return LLVMDoubleTypeInContext(llvm.context);
                }
                return LLVMFloatTypeInContext(llvm.context);
            } else {
                // Integer type.
                return LLVMIntTypeInContext(llvm.context, type_def->size * 8);
            }
        }

        else if (type_def == w->type_def_bool) return LLVMInt8TypeInContext(llvm.context);
        else if (type_def == w->type_def_void) return LLVMVoidTypeInContext(llvm.context);
        
        else if (type_def == w->type_def_string) {
            LLVMTypeRef elems[] = {
                LLVMPointerType(LLVMInt8TypeInContext(llvm.context), 0), // *u8
                LLVMInt64TypeInContext(llvm.context),                    // s64
            };
            return LLVMStructTypeInContext(llvm.context, elems, sizeof(elems)/sizeof(elems[0]), 1); // 1 means packed
        }

        else if (type_def == w->type_def_type) {
            return LLVMPointerTypeInContext(llvm.context, 0);
        }

        else UNREACHABLE;
    }

    else if (type_def->array_element_type) {
        assert(type_def->array_length > 0); // TODO: handle dynamic array
        LLVMTypeRef element_type = llvm_create_type(w, type_def->array_element_type);
        return LLVMArrayType(element_type, (unsigned) type_def->array_length);
    }

    else if (type_def->lambda_return_type) {
        size_t arg_count = arrlenu(type_def->lambda_argument_types);
        LLVMTypeRef *param_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * arg_count);
        For (type_def->lambda_argument_types) {
            param_types[it] = llvm_create_type(w, type_def->lambda_argument_types[it]);
        }
        LLVMTypeRef return_type = llvm_create_type(w, type_def->lambda_return_type);
        return LLVMFunctionType(return_type, param_types, arg_count, 0); // 0 means not variadic
    }

    else UNREACHABLE;
}

LLVMValueRef llvm_get_named_value(LLVMValueRef function, const char *name)
{
    LLVMBasicBlockRef block = LLVMGetLastBasicBlock(function);
    while (block != NULL) {
        LLVMValueRef inst = LLVMGetFirstInstruction(block);
        while (inst != NULL) {
            const char *inst_name = LLVMGetValueName(inst);
            if (inst_name != NULL && strcmp(name, inst_name) == 0) {
                return inst;
            }
            inst = LLVMGetNextInstruction(inst);
        }
        block = LLVMGetPreviousBasicBlock(block);
    }
    return NULL;
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

LLVMValueRef llvm_build_value(Workspace *w, LLVMValueRef function, Ast *ast)
{
    Llvm llvm = w->llvm;

    assert(ast);

    switch (ast->type) {
    case AST_BLOCK: {
        const Ast_Block *block = xx ast;
        LLVMBasicBlockRef basic_block = LLVMAppendBasicBlock(function, "");
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block);
        For (block->statements) {
            llvm_build_value(w, function, block->statements[it]);
        }
        return LLVMBasicBlockAsValue(basic_block);
    }

    case AST_LITERAL: {
        const Ast_Literal *lit = xx ast;
        const Ast_Type_Definition *type_def = lit->type;
        LLVMTypeRef type = llvm_create_type(w, type_def);
        assert(type);

        if (type_def->number_flags & NUMBER_FLAGS_NUMBER) {
            if (type_def->number_flags & NUMBER_FLAGS_FLOAT) {
                // Floating-point type.
                return LLVMConstReal(type, lit->double_value);
            } else {
                // Integer type.
                return LLVMConstInt(type, lit->integer_value, 1);
            }
        }
        else if (type_def == w->type_def_bool) {
            return LLVMConstInt(type, lit->integer_value, 0);
        }
        else if (type_def == w->type_def_void) {
            assert(0);
        }
        else if (type_def == w->type_def_string) {
            return llvm_const_string(llvm, lit->string_value.data, lit->string_value.count);
        }
        else if (type_def == w->type_def_type) {
            UNIMPLEMENTED;
        }
        else UNREACHABLE;
    }

    case AST_IDENT: {
        const Ast_Ident *ident = xx ast;
        Ast_Declaration *resolved_decl = find_declaration_if_exists(ident);
        assert(resolved_decl);

        if (resolved_decl->flags & DECLARATION_IS_CONSTANT) {
            if (!resolved_decl->llvm_value) resolved_decl->llvm_value = llvm_build_value(w, function, resolved_decl->expression);
            return resolved_decl->llvm_value; // Direct pointer to the value.
        }

        // So we are not a constant, which means we point at a type instantiation.
        assert(resolved_decl->expression->type == AST_TYPE_INSTANTIATION);

        const Ast_Type_Instantiation *inst = xx resolved_decl->expression;

        if (!inst->llvm_value) {
            report_error(xx &ident->base, "Use of uninitialized value is not allowed."); // TODO: error message
        }

        LLVMTypeRef my_type = llvm_create_type(w, inst->base.inferred_type);
        return LLVMBuildLoad2(llvm.builder, my_type, inst->llvm_value, "");
    }

    case AST_UNARY_OPERATOR:
        dump_ast(ast);
        assert(0);

    case AST_BINARY_OPERATOR: {
        const Ast_Binary_Operator *binary = xx ast;
        const Ast_Type_Definition *defn = ast->inferred_type;
        LLVMValueRef left = llvm_build_value(w, function, binary->left);
        LLVMValueRef right = llvm_build_value(w, function, binary->right);
        switch (binary->operator_type) {
        case '+': return LLVMBuildAdd(llvm.builder, left, right, "");
        case '-': return LLVMBuildSub(llvm.builder, left, right, "");
        case '*': return LLVMBuildMul(llvm.builder, left, right, "");
        case '/':
            assert(defn->number_flags & NUMBER_FLAGS_NUMBER);
            if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
                return LLVMBuildFDiv(llvm.builder, left, right, "");
            }
            if (defn->number_flags & NUMBER_FLAGS_SIGNED) {
                return LLVMBuildSDiv(llvm.builder, left, right, "");
            }
            return LLVMBuildUDiv(llvm.builder, left, right, "");
        // case '%': return LLVMBuildRem(llvm.builder, left, right, "");
        case TOKEN_BITWISE_AND:
        case TOKEN_LOGICAL_AND: return LLVMBuildAnd(llvm.builder, left, right, "");
        case TOKEN_BITWISE_OR:
        case TOKEN_LOGICAL_OR: return LLVMBuildOr(llvm.builder, left, right, "");
        case TOKEN_BITWISE_XOR: return LLVMBuildXor(llvm.builder, left, right, "");
        case TOKEN_ISEQUAL:
            if (defn->number_flags & NUMBER_FLAGS_NUMBER) {
                if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
                    return LLVMBuildFCmp(llvm.builder, LLVMRealOEQ, left, right, "");
                }
                return LLVMBuildICmp(llvm.builder, LLVMIntEQ, left, right, "");
            }
            assert(0);
        case TOKEN_ISNOTEQUAL:
            if (defn->number_flags & NUMBER_FLAGS_NUMBER) {
                if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
                    return LLVMBuildFCmp(llvm.builder, LLVMRealONE, left, right, "");
                }
                return LLVMBuildICmp(llvm.builder, LLVMIntNE, left, right, "");
            }
            assert(0);
        // @Cleanup: Copypasta.
        case '>':
            if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
                return LLVMBuildFCmp(llvm.builder, LLVMRealOGT, left, right, "");
            }
            if (defn->number_flags & NUMBER_FLAGS_SIGNED) {
                return LLVMBuildICmp(llvm.builder, LLVMIntSGT, left, right, "");
            }
            // Pointer or unsigned integer.
            return LLVMBuildICmp(llvm.builder, LLVMIntUGT, left, right, "");
        case TOKEN_GREATEREQUALS:
            if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
                return LLVMBuildFCmp(llvm.builder, LLVMRealOGE, left, right, "");
            }
            if (defn->number_flags & NUMBER_FLAGS_SIGNED) {
                return LLVMBuildICmp(llvm.builder, LLVMIntSGE, left, right, "");
            }
            // Pointer or unsigned integer.
            return LLVMBuildICmp(llvm.builder, LLVMIntUGE, left, right, "");
        case '<':
            if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
                return LLVMBuildFCmp(llvm.builder, LLVMRealOLT, left, right, "");
            }
            if (defn->number_flags & NUMBER_FLAGS_SIGNED) {
                return LLVMBuildICmp(llvm.builder, LLVMIntSLT, left, right, "");
            }
            // Pointer or unsigned integer.
            return LLVMBuildICmp(llvm.builder, LLVMIntULT, left, right, "");
        case TOKEN_LESSEQUALS:
            if (defn->number_flags & NUMBER_FLAGS_FLOAT) {
                return LLVMBuildFCmp(llvm.builder, LLVMRealOLE, left, right, "");
            }
            if (defn->number_flags & NUMBER_FLAGS_SIGNED) {
                return LLVMBuildICmp(llvm.builder, LLVMIntSLE, left, right, "");
            }
            // Pointer or unsigned integer.
            return LLVMBuildICmp(llvm.builder, LLVMIntULE, left, right, "");
        default:
            printf("Unhandled binary operator type: %s\n", token_type_to_string(binary->operator_type));
            assert(0);
        }
    }

    case AST_LAMBDA: {
        const Ast_Lambda *lambda = xx ast;
        LLVMTypeRef type = llvm_create_type(w, lambda->type_definition); assert(type);
        const char *name = arena_sv_to_cstr(&temporary_arena, lambda->name);
        LLVMValueRef function = LLVMAddFunction(llvm.module, name, type);
        llvm_build_value(w, function, xx lambda->block);
        return function;
    }

    case AST_TYPE_INSTANTIATION: {
        Ast_Type_Instantiation *inst = xx ast;
        const char *name = inst->name;
        if (!name) name = ""; // For some reason, LLVM won't accept NULL.
        LLVMTypeRef type = llvm_create_type(w, inst->type_definition); assert(type);
        LLVMValueRef alloca = LLVMBuildAlloca(llvm.builder, type, name);
        // LLVMSetValueName2(alloca, "catdog", 6);
        LLVMValueRef initializer = llvm_build_value(w, function, inst->initializer_expression);
        LLVMBuildStore(llvm.builder, initializer, alloca);
        inst->llvm_value = alloca;
        return alloca;
    }

    case AST_IF: {
        const Ast_If *if_stmt = xx ast;
        LLVMValueRef condition = llvm_build_value(w, function, if_stmt->condition_expression);

        LLVMBasicBlockRef basic_block_then = LLVMAppendBasicBlock(function, "then");
        LLVMBasicBlockRef basic_block_else;
        LLVMBasicBlockRef basic_block_merge = LLVMAppendBasicBlock(function, "merge");

        // Emit the conditional break.
        if (if_stmt->else_statement) {
            basic_block_else = LLVMAppendBasicBlock(function, "else");
            LLVMBuildCondBr(llvm.builder, condition, basic_block_then, basic_block_else);
        } else {
            LLVMBuildCondBr(llvm.builder, condition, basic_block_then, basic_block_merge);
        }
        
        // Emit the "then" statement.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_then);
        llvm_build_value(w, function, if_stmt->then_statement);
        LLVMBuildBr(llvm.builder, basic_block_merge);

        // Emit the "else" statement.
        if (if_stmt->else_statement) {
            // Emit the "then" statement.
            LLVMPositionBuilderAtEnd(llvm.builder, basic_block_else);
            llvm_build_value(w, function, if_stmt->else_statement);
            LLVMBuildBr(llvm.builder, basic_block_merge);
        }

        // Emit code for the merge block.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_merge);

        return LLVMBasicBlockAsValue(basic_block_merge);
    }

    case AST_RETURN: {
        const Ast_Return *ret = xx ast;
        LLVMValueRef value = llvm_build_value(w, function, ret->subexpression);
        return LLVMBuildRet(llvm.builder, value);
    }

    case AST_DECLARATION:
        assert(0);

    default:
        dump_ast(xx ast);
        assert(0);
    }
}
