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

LLVMTypeRef llvm_get_type(Workspace *w, const Ast_Type_Definition *type_def)
{
    Llvm llvm = w->llvm;
    assert(type_def);

    if (type_def->struct_desc) {
        // Allocate at least enough for every single declaration in the struct.
        LLVMTypeRef *field_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * arrlenu(type_def->struct_desc->block->declarations));

        size_t count = 0;
        For (type_def->struct_desc->block->declarations) {
            Ast_Declaration *member = type_def->struct_desc->block->declarations[it];
            if (member->flags & DECLARATION_IS_CONSTANT) continue; // Skip constants.

            field_types[count] = llvm_get_type(w, member->my_type);
            count += 1;
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

        LLVMTypeRef res = llvm_get_type(w, type_def->pointer_to);
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
        LLVMTypeRef element_type = llvm_get_type(w, type_def->array_element_type);
        return LLVMArrayType(element_type, (unsigned) type_def->array_length);
    }

    else if (type_def->lambda_return_type) {
        size_t arg_count = arrlenu(type_def->lambda_argument_types);
        LLVMTypeRef *param_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * arg_count);
        For (type_def->lambda_argument_types) {
            param_types[it] = llvm_get_type(w, type_def->lambda_argument_types[it]);
        }
        LLVMTypeRef return_type = llvm_get_type(w, type_def->lambda_return_type);
        return LLVMFunctionType(return_type, param_types, arg_count, 0); // 0 means not variadic
    }

    else UNREACHABLE;
}

#define LLVM_COMPARISON_PREDICATE(flt, sgn, uns) do { \
    if (use_float) { \
        *real_predicate = (flt); \
        return LLVMFCmp; \
    } \
    *int_predicate = (defn->number_flags & NUMBER_FLAGS_SIGNED) ? (sgn) : (uns); \
    return LLVMICmp; \
} while (0)

LLVMOpcode llvm_get_opcode(int operator_type, Ast_Type_Definition *defn, LLVMIntPredicate *int_predicate, LLVMRealPredicate *real_predicate)
{
    bool use_float = defn->number_flags & NUMBER_FLAGS_FLOAT;
    switch (operator_type) {
    case '+':
        if (use_float) return LLVMFAdd;
        return LLVMAdd;
    case '-':
        if (use_float) return LLVMFSub;
        return LLVMSub;
    case '*':
        if (use_float) return LLVMFMul;
        return LLVMMul;
    case '/':
        if (use_float) return LLVMFDiv;
        if (defn->number_flags & NUMBER_FLAGS_SIGNED) return LLVMSDiv;
        return LLVMUDiv;
    case TOKEN_BITWISE_AND:
    case TOKEN_LOGICAL_AND:
        return LLVMAnd;
    case TOKEN_BITWISE_OR:
    case TOKEN_LOGICAL_OR:
        return LLVMOr;
    case TOKEN_ISEQUAL:
        if (defn->number_flags & NUMBER_FLAGS_NUMBER) {
            if (use_float) {
                *real_predicate = LLVMRealOEQ;
                return LLVMFCmp;
            }
            *int_predicate = LLVMIntEQ;
            return LLVMICmp;
        }
        // If we're a structure we need to compare all the fields.
        assert(0);
    case TOKEN_ISNOTEQUAL:
        if (defn->number_flags & NUMBER_FLAGS_NUMBER) {
            if (use_float) {
                *real_predicate = LLVMRealONE;
                return LLVMFCmp;
            }
            *int_predicate = LLVMIntNE;
            return LLVMICmp;
        }
        // If we're a structure we need to compare all the fields.
        assert(0);
    case '>':
        LLVM_COMPARISON_PREDICATE(LLVMRealOGT, LLVMIntSGT, LLVMIntUGT);
    case '<':
        LLVM_COMPARISON_PREDICATE(LLVMRealOLT, LLVMIntSLT, LLVMIntULT);
    case TOKEN_GREATEREQUALS:
        LLVM_COMPARISON_PREDICATE(LLVMRealOGE, LLVMIntSGE, LLVMIntUGE);
    case TOKEN_LESSEQUALS:
        LLVM_COMPARISON_PREDICATE(LLVMRealOLE, LLVMIntSLE, LLVMIntULE);
    default:
        printf("Unhandled binary operator type: %s\n", token_type_to_string(operator_type));
        assert(0);
    }
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

LLVMValueRef llvm_build_expression(Workspace *w, Ast_Expression *expr)
{
    Llvm llvm = w->llvm;
    switch (expr->kind) {
    case AST_LITERAL: {
        const Ast_Literal *lit = xx expr;
        LLVMTypeRef type = llvm_get_type(w, expr->inferred_type);
        assert(type);

        if (expr->inferred_type->number_flags & NUMBER_FLAGS_NUMBER) {
            if (expr->inferred_type->number_flags & NUMBER_FLAGS_FLOAT) {
                // Floating-point type.
                return LLVMConstReal(type, lit->double_value);
            } else {
                // Integer type.
                return LLVMConstInt(type, lit->integer_value, 1);
            }
        }

        if (expr->inferred_type == w->type_def_bool) {
            return LLVMConstInt(type, lit->integer_value, 0);
        }

        if (expr->inferred_type == w->type_def_string) {
            return llvm_const_string(llvm, lit->string_value.data, lit->string_value.count);
        }

        if (expr->inferred_type == w->type_def_type) {
            UNIMPLEMENTED;
        }

        UNREACHABLE;
    }
    case AST_IDENT: {
        const Ast_Ident *ident = xx expr;
        assert(ident->resolved_declaration);

        UNIMPLEMENTED;

        // if (resolved_decl->instantiation) {
        //     if (!resolved_decl->instantiation->llvm_value) {
        //         // TODO: We should be able to catch this error WAY sooner.
        //         report_error(ident->_expression.location, "Use of uninitialized value is not allowed."); // TODO: error message
        //     }
        //     return LLVMBuildLoad2(
        //         llvm.builder,
        //         llvm_get_type(w, ident->_expression.inferred_type),
        //         resolved_decl->instantiation->llvm_value,
        //         "");
        // }

        // assert(resolved_decl->root_expression);
        // return llvm_build_expression(w, resolved_decl->root_expression);
    }
    case AST_UNARY_OPERATOR:
        UNIMPLEMENTED;
    case AST_BINARY_OPERATOR: {
        const Ast_Binary_Operator *binary = xx expr;
        LLVMValueRef LHS = llvm_build_expression(w, binary->left);
        LLVMValueRef RHS = llvm_build_expression(w, binary->right);
        LLVMIntPredicate int_predicate;
        LLVMRealPredicate real_predicate;
        LLVMOpcode opcode = llvm_get_opcode(binary->operator_type, expr->inferred_type, &int_predicate, &real_predicate);
        if (opcode == LLVMICmp) {
            return LLVMBuildICmp(llvm.builder, int_predicate, LHS, RHS, "");
        }
        if (opcode == LLVMFCmp) {
            return LLVMBuildFCmp(llvm.builder, real_predicate, LHS, RHS, "");
        }
        return LLVMBuildBinOp(llvm.builder, opcode, LHS, RHS, "");
    }
    case AST_LAMBDA: {
            UNIMPLEMENTED;
        // const Ast_Lambda *lambda = xx expr;
        // LLVMTypeRef type = llvm_get_type(w, lambda->type_definition); assert(type);
        // LLVMValueRef function = LLVMAddFunction(llvm.module, lambda->name.data, type);
        // llvm_build_statement(w, function, xx lambda->block);
        // return function;
    }
    case AST_PROCEDURE_CALL:
        UNIMPLEMENTED;
    case AST_TYPE_DEFINITION:
        report_error(w, expr->location, "Types cannot be used as values in our LLVM implementation yet.");
    case AST_CAST: {
        const Ast_Cast *cast = xx expr;
        LLVMTypeRef dest_type = llvm_get_type(w, cast->type);
        return LLVMBuildBitCast(llvm.builder, llvm_build_expression(w, cast->subexpression), dest_type, "");
    }
    }
}

void llvm_build_statement(Workspace *w, LLVMValueRef function, Ast_Statement *stmt)
{
    Llvm llvm = w->llvm;
    switch (stmt->kind) {
    case AST_BLOCK: {
        const Ast_Block *block = xx stmt;
        LLVMBasicBlockRef basic_block = LLVMAppendBasicBlock(function, "");
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block);
        For (block->statements) {
            llvm_build_statement(w, function, block->statements[it]);
        }
        break;
    }
    case AST_VARIABLE: {
        Ast_Variable *var = xx stmt;
        const char *name = var->declaration->ident->name.data;
        LLVMTypeRef type = llvm_get_type(w, var->declaration->my_type); assert(type);
        LLVMValueRef alloca = LLVMBuildAlloca(llvm.builder, type, name);
        LLVMValueRef initializer = llvm_build_expression(w, var->declaration->root_expression);
        LLVMBuildStore(llvm.builder, initializer, alloca);
        var->declaration->llvm_value = alloca;
        break;
    }
    case AST_IF: {
        const Ast_If *if_stmt = xx stmt;
        LLVMValueRef condition = llvm_build_expression(w, if_stmt->condition_expression);

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
        llvm_build_statement(w, function, if_stmt->then_statement);
        LLVMBuildBr(llvm.builder, basic_block_merge);

        // Emit the "else" statement.
        if (if_stmt->else_statement) {
            // Emit the "then" statement.
            LLVMPositionBuilderAtEnd(llvm.builder, basic_block_else);
            llvm_build_statement(w, function, if_stmt->else_statement);
            LLVMBuildBr(llvm.builder, basic_block_merge);
        }

        // Emit code for the merge block.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_merge);

        break;
    }

    case AST_RETURN: {
        const Ast_Return *ret = xx stmt;
        LLVMValueRef value = llvm_build_expression(w, ret->subexpression);
        LLVMBuildRet(llvm.builder, value);
        break;
    }

    default:
        printf("%s\n", stmt_to_string(stmt));
        assert(0);
    }
}