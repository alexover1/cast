#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <limits.h> // LLONG_MIN

#include "common.h"
#include "workspace.h"

// For now, who cares if we zero terminate? I don't see any problems. And it lets you call c functions easier.
#define DONT_ZERO_TERMINATE 0

void workspace_setup_llvm(Workspace *w)
{
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmParsers();
    LLVMInitializeAllAsmPrinters();

    // Initialize the LLVM target.

    char *triple = LLVMGetDefaultTargetTriple();

    char *error_message = NULL;
    LLVMTargetRef target = NULL;

    if (LLVMGetTargetFromTriple(triple, &target, &error_message) != 0) {
        fprintf(stderr, "Error: Could not create LLVM target: %s\n", error_message);
        LLVMDisposeMessage(error_message);
        LLVMDisposeMessage(triple);
        return;
    }

    printf("%s\n", LLVMGetTargetDescription(target));

    LLVMTargetMachineRef target_machine = LLVMCreateTargetMachine(
        target,                  // T
        triple,                  // Triple
        "",                      // Cpu
        "",                      // Features
        LLVMCodeGenLevelDefault, // Level
        LLVMRelocPIC,            // Reloc
        LLVMCodeModelDefault     // CodeModel
    );
    if (target_machine == NULL) {
        fprintf(stderr, "Error: Could not create LLVM target machine\n");
        LLVMDisposeMessage(triple);
        return;
    }

    w->llvm.target_machine = target_machine;

    // Create the LLVM context.
    
    w->llvm.context = LLVMContextCreate();
    LLVMContextSetOpaquePointers(w->llvm.context, 1);
    // LLVMContextSetDiscardValueNames(w->llvm.context, 1);

    // Create the LLVM module.

    w->llvm.module = LLVMModuleCreateWithNameInContext(w->name, w->llvm.context);
    w->llvm.builder = LLVMCreateBuilderInContext(w->llvm.context);

    LLVMSetTarget(w->llvm.module, triple);
    LLVMSetModuleDataLayout(w->llvm.module, LLVMCreateTargetDataLayout(target_machine));

    LLVMDisposeMessage(triple);

    // Now create the types for some of our built-ins.

    LLVMTypeRef elems[3];

    elems[0] = LLVMPointerType(LLVMInt8TypeInContext(w->llvm.context), 0), // *u8
    elems[1] = LLVMInt64TypeInContext(w->llvm.context),                    // s64
    w->llvm.string_type = LLVMStructTypeInContext(w->llvm.context, elems, 2, 1); // 1 means packed

    elems[0] = LLVMPointerTypeInContext(w->llvm.context, 0),
    elems[1] = LLVMInt64TypeInContext(w->llvm.context),
    w->llvm.slice_type = LLVMStructTypeInContext(w->llvm.context, elems, 2, 1); // 1 means packed

    elems[0] = LLVMPointerTypeInContext(w->llvm.context, 0), // data: ptr
    elems[1] = LLVMInt64TypeInContext(w->llvm.context),      // count: s64
    elems[2] = LLVMInt64TypeInContext(w->llvm.context),      // capacity: s64
    w->llvm.dynamic_array_type = LLVMStructTypeInContext(w->llvm.context, elems, 3, 1); // 1 means packed
}

void workspace_dispose_llvm(Workspace *w)
{
    LLVMDisposeBuilder(w->llvm.builder);
    LLVMDisposeModule(w->llvm.module);
    LLVMContextDispose(w->llvm.context); // @Bad: Why is this not consistently named?
}

LLVMTypeRef llvm_get_type(Workspace *w, const Ast_Type_Definition *defn)
{
    Llvm llvm = w->llvm;
    assert(defn);
    while (defn->_expression.replacement) defn = xx defn->_expression.replacement; // TODO: We should store ** in the typechecker so this goes away.
    switch (defn->kind) {
    case TYPE_DEF_ANY:
        assert(0);
    case TYPE_DEF_NUMBER:
        if (defn->number.flags & NUMBER_FLAGS_FLOAT) {
            // Floating-point type.
            if (defn->number.flags & NUMBER_FLAGS_FLOAT64) return LLVMDoubleTypeInContext(llvm.context);
            return LLVMFloatTypeInContext(llvm.context);
        }
        // Integer type.
        return LLVMIntTypeInContext(llvm.context, defn->size * 8);
    case TYPE_DEF_LITERAL:
        if (defn == w->type_def_void) return LLVMVoidTypeInContext(llvm.context);
        if (defn == w->type_def_type) report_error(w, defn->_expression.location, "Runtime types are not implemented in LLVM.");
        switch (defn->literal) {
        case LITERAL_BOOL:   return LLVMInt1TypeInContext(llvm.context);
        case LITERAL_STRING: return llvm.string_type;
        case LITERAL_NULL:   return LLVMPointerTypeInContext(llvm.context, 0);
        }
    case TYPE_DEF_STRUCT: {
        // Allocate at least enough for every single declaration in the struct.
        // Not all of the declarations are actually fields, but this is a fast allocator in the temporary buffer so who cares.
        LLVMTypeRef *field_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * arrlenu(defn->struct_desc->block->declarations));

        size_t count = 0;
        For (defn->struct_desc->block->declarations) {
            Ast_Declaration *member = defn->struct_desc->block->declarations[it];

            if (!(member->flags & DECLARATION_IS_STRUCT_FIELD)) continue;

            field_types[count] = llvm_get_type(w, member->my_type);
            count += 1;
        }
       
        return LLVMStructTypeInContext(llvm.context, field_types, count, 1); // 1 means packed
    }
    case TYPE_DEF_ENUM:
        return LLVMInt32TypeInContext(llvm.context);
    case TYPE_DEF_POINTER:
        return LLVMPointerType(llvm_get_type(w, defn->pointer_to), 0);
    case TYPE_DEF_ARRAY: {
        if (defn->array.length == -1) return llvm.slice_type;
        if (defn->array.length == LLONG_MIN) return llvm.dynamic_array_type;
        return LLVMArrayType(llvm_get_type(w, defn->array.element_type), defn->array.length);
    }
    case TYPE_DEF_STRUCT_CALL:
        UNIMPLEMENTED;
    case TYPE_DEF_IDENT:
        UNREACHABLE;
    case TYPE_DEF_LAMBDA: {
        size_t arg_count = arrlenu(defn->lambda.argument_types);
        LLVMTypeRef *param_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * arg_count);
        For (defn->lambda.argument_types) {
            param_types[it] = llvm_get_type(w, defn->lambda.argument_types[it]);
        }
        LLVMTypeRef return_type = llvm_get_type(w, defn->lambda.return_type);
        return LLVMFunctionType(return_type, param_types, arg_count, 0); // 0 means not variadic
    }       
    }
}

#define LLVM_COMPARISON_PREDICATE(flt, sgn, uns) do { \
    if (use_float) { \
        *real_predicate = (flt); \
        return LLVMFCmp; \
    } \
    *int_predicate = (defn->number.flags & NUMBER_FLAGS_SIGNED) ? (sgn) : (uns); \
    return LLVMICmp; \
} while (0)

LLVMOpcode llvm_get_opcode(int operator_type, Ast_Type_Definition *defn, LLVMIntPredicate *int_predicate, LLVMRealPredicate *real_predicate)
{
    bool use_float = defn->kind == TYPE_DEF_NUMBER ? defn->number.flags & NUMBER_FLAGS_FLOAT : false;
    switch (operator_type) {
    case '+':
    case TOKEN_PLUSEQUALS:
        if (use_float) return LLVMFAdd;
        return LLVMAdd;
    case '-':
    case TOKEN_MINUSEQUALS:
        if (use_float) return LLVMFSub;
        return LLVMSub;
    case '*':
    case TOKEN_TIMESEQUALS:
        if (use_float) return LLVMFMul;
        return LLVMMul;
    case '/':
    case TOKEN_DIVEQUALS:
        if (use_float) return LLVMFDiv;
        if (defn->number.flags & NUMBER_FLAGS_SIGNED) return LLVMSDiv;
        return LLVMUDiv;
    case '%':
    case TOKEN_MODEQUALS:
        if (use_float) return LLVMFRem;
        if (defn->number.flags & NUMBER_FLAGS_SIGNED) return LLVMSRem;
        return LLVMURem;
    case TOKEN_BITWISE_AND:
    case TOKEN_LOGICAL_AND:
        return LLVMAnd;
    case TOKEN_BITWISE_OR:
    case TOKEN_LOGICAL_OR:
        return LLVMOr;
    case TOKEN_ISEQUAL:
        if (defn->kind == TYPE_DEF_NUMBER) {
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
        if (defn->kind == TYPE_DEF_NUMBER) {
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
    LLVMSetInitializer(global_string, LLVMConstStringInContext(llvm.context, data, count, DONT_ZERO_TERMINATE));

    // Create constant structure value.
    LLVMValueRef struct_fields[] = {
        LLVMConstBitCast(global_string, u8_pointer_type),
        LLVMConstInt(LLVMInt64TypeInContext(llvm.context), count, 0),
    };
            
    return LLVMConstStructInContext(llvm.context,
        struct_fields, sizeof(struct_fields)/sizeof(struct_fields[0]), 1);
}

LLVMValueRef llvm_build_lvalue(Workspace *w, Ast_Expression *expr)
{
    Llvm llvm = w->llvm;
    while (expr->replacement) expr = expr->replacement;
    switch (expr->kind) {
    case AST_IDENT: {
        const Ast_Ident *ident = xx expr;          
        assert(ident->resolved_declaration);
        assert(!(ident->resolved_declaration->flags & DECLARATION_IS_CONSTANT)); // It should have been substituted.
        assert(ident->resolved_declaration->llvm_value); // Must have been initialized.
        return ident->resolved_declaration->llvm_value;
    }
    case AST_SELECTOR: {
        const Ast_Selector *selector = xx expr;
        LLVMTypeRef type = llvm_get_type(w, selector->namespace_expression->inferred_type);
        LLVMValueRef pointer = llvm_build_lvalue(w, selector->namespace_expression);
        if (selector->is_pointer_dereference) {
            return LLVMBuildLoad2(llvm.builder, type, pointer, "");
        }
        assert(selector->struct_field_index >= 0);
        return LLVMBuildStructGEP2(
            llvm.builder,
            llvm_get_type(w, selector->_expression.inferred_type),
            pointer,
            selector->struct_field_index,
            "");
    }
    default:
        fprintf(stderr, "Internal Error: '%s' is not an lvalue.\n", expr_to_string(expr));
        exit(1);
    }
}

LLVMValueRef llvm_build_expression(Workspace *w, Ast_Expression *expr)
{
    Llvm llvm = w->llvm;
    while (expr->replacement) expr = expr->replacement;
    switch (expr->kind) {
    case AST_NUMBER: {
        const Ast_Number *number = xx expr;
        LLVMTypeRef type = llvm_get_type(w, expr->inferred_type);
        assert(expr->inferred_type->kind == TYPE_DEF_NUMBER);
        if (expr->inferred_type->number.flags & NUMBER_FLAGS_FLOAT) return LLVMConstReal(type, number->as.real);
        return LLVMConstInt(type, number->as.integer, 1);
    }
    case AST_LITERAL: {
        const Ast_Literal *lit = xx expr;           
        LLVMTypeRef type = llvm_get_type(w, expr->inferred_type);
        switch (lit->kind) {
        case LITERAL_BOOL:   return LLVMConstInt(LLVMInt1TypeInContext(llvm.context), lit->bool_value, 0);
        case LITERAL_STRING: return llvm_const_string(llvm, lit->string_value.data, lit->string_value.count);
        case LITERAL_NULL:   return LLVMConstNull(type);
        }
    }
    case AST_IDENT: {
        const Ast_Ident *ident = xx expr;
        assert(ident->resolved_declaration);

        assert(!(ident->resolved_declaration->flags & DECLARATION_IS_CONSTANT)); // It should have been substituted.

        // Because during typechecking we assured that the variable's initialization came before us, this is safe.
        assert(ident->resolved_declaration->llvm_value);

        if (ident->resolved_declaration->flags & DECLARATION_IS_LAMBDA_ARGUMENT) {
            return ident->resolved_declaration->llvm_value;
        }
            
        return LLVMBuildLoad2(
            llvm.builder,
            llvm_get_type(w, ident->_expression.inferred_type),
            ident->resolved_declaration->llvm_value,
            "");
    }
    case AST_UNARY_OPERATOR: {
        const Ast_Unary_Operator *unary = xx expr;

        if (unary->operator_type == '*') {
            return llvm_build_lvalue(w, unary->subexpression);
        }       

        UNIMPLEMENTED;
    }
    case AST_BINARY_OPERATOR: {
        const Ast_Binary_Operator *binary = xx expr;
        LLVMValueRef LHS = llvm_build_expression(w, binary->left);
        LLVMValueRef RHS = llvm_build_expression(w, binary->right);
        LLVMIntPredicate int_predicate;
        LLVMRealPredicate real_predicate;
        LLVMOpcode opcode = llvm_get_opcode(binary->operator_type, binary->left->inferred_type, &int_predicate, &real_predicate);
        if (opcode == LLVMICmp) {
            return LLVMBuildICmp(llvm.builder, int_predicate, LHS, RHS, "");
        }
        if (opcode == LLVMFCmp) {
            return LLVMBuildFCmp(llvm.builder, real_predicate, LHS, RHS, "");
        }
        return LLVMBuildBinOp(llvm.builder, opcode, LHS, RHS, "");
    }
    case AST_LAMBDA: {
        const Ast_Lambda *lambda = xx expr;
        LLVMValueRef procedure = lambda->my_body_declaration->llvm_value;
        assert(procedure); // These get created in a pre-pass.
        return procedure;
    }
    case AST_PROCEDURE_CALL: {
        const Ast_Procedure_Call *call = xx expr;
        LLVMValueRef procedure = llvm_build_expression(w, call->procedure_expression);
        LLVMTypeRef procedure_type = llvm_get_type(w, call->procedure_expression->inferred_type);
        size_t args_count = arrlenu(call->arguments);
        LLVMValueRef *args = arena_alloc(&temporary_arena, sizeof(LLVMValueRef) * args_count);
        For (call->arguments) {
            args[it] = llvm_build_expression(w, call->arguments[it]);
        }
        return LLVMBuildCall2(llvm.builder, procedure_type, procedure, args, args_count, "");
    }
    case AST_TYPE_DEFINITION:
        report_error(w, expr->location, "Types cannot be used as values in our LLVM implementation yet.");
    case AST_CAST: {
        const Ast_Cast *cast = xx expr;
        LLVMTypeRef dest_type = llvm_get_type(w, cast->type);
        return LLVMBuildBitCast(llvm.builder, llvm_build_expression(w, cast->subexpression), dest_type, "");
    }
    case AST_SELECTOR: {
        const Ast_Selector *selector = xx expr;
            
        if (selector->struct_field_index >= 0) {
            LLVMTypeRef field_type = llvm_get_type(w, selector->_expression.inferred_type);
            LLVMTypeRef struct_type = llvm_get_type(w, selector->namespace_expression->inferred_type);
#if 1

            LLVMValueRef struct_pointer = llvm_build_lvalue(w, selector->namespace_expression);
            LLVMValueRef field_pointer = LLVMBuildStructGEP2(
                llvm.builder,
                struct_type,
                struct_pointer,
                selector->struct_field_index,
                "");

            return LLVMBuildLoad2(llvm.builder, field_type, field_pointer, "");
#else
            LLVMValueRef field_pointer = llvm_build_lvalue(w, selector);
            return LLVMBuildLoad2(llvm.builder, field_type, field_pointer, "");
#endif
        }

        assert(0);
    }
    case AST_TYPE_INSTANTIATION: {
        const Ast_Type_Instantiation *inst = xx expr;

        assert(!(inst->type_definition->kind == TYPE_DEF_LITERAL)); // Typechecking should replace this.

        size_t n = arrlenu(inst->arguments);

        LLVMValueRef *values = arena_alloc(&temporary_arena, n * sizeof(LLVMValueRef));
        For (inst->arguments) {
            values[it] = llvm_build_expression(w, inst->arguments[it]);
        }
        return LLVMConstStructInContext(llvm.context, values, n, 1);

        // LLVMTypeRef struct_type = llvm_get_type(w, inst->type_definition);
        // LLVMValueRef struct_pointer = lvalue;

        // For (inst->arguments) {
        //     LLVMValueRef field_pointer = LLVMBuildStructGEP2(
        //         llvm.builder,
        //         struct_type,
        //         struct_pointer,
        //         it,
        //         "");
        //     LLVMValueRef field_value = llvm_build_rvalue(w, field_pointer, inst->arguments[it]);
        //     if (field_value) LLVMBuildStore(llvm.builder, field_value, field_pointer);
        // }
        // return NULL;
    }
    }
}

void llvm_build_statement(Workspace *w, LLVMValueRef function, Ast_Statement *stmt)
{
    Llvm llvm = w->llvm;
    switch (stmt->kind) {
    case AST_BLOCK: {
        const Ast_Block *block = xx stmt;
        // LLVMBasicBlockRef basic_block = LLVMAppendBasicBlock(function, "");
        // LLVMPositionBuilderAtEnd(llvm.builder, basic_block);
        For (block->statements) {
            llvm_build_statement(w, function, block->statements[it]);
        }
        break;
    }
    case AST_WHILE: {
        const Ast_While *while_stmt = xx stmt;
        LLVMValueRef condition = llvm_build_expression(w, while_stmt->condition_expression);
            
        LLVMBasicBlockRef basic_block_then = LLVMAppendBasicBlock(function, "then");
        LLVMBasicBlockRef basic_block_merge = LLVMAppendBasicBlock(function, "merge");

        LLVMBuildCondBr(llvm.builder, condition, basic_block_then, basic_block_merge);

        // Emit the "then" statement.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_then);
        llvm_build_statement(w, function, while_stmt->then_statement);
        LLVMBuildCondBr(llvm.builder, condition, basic_block_then, basic_block_merge);
            
        // Emit code for the merge block.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_merge);
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
        if (LLVMGetBasicBlockTerminator(basic_block_then) == NULL) {
            LLVMBuildBr(llvm.builder, basic_block_merge);
        }

        // Emit the "else" statement.
        if (if_stmt->else_statement) {
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
    case AST_VARIABLE: {
        Ast_Variable *var = xx stmt;
        const char *name = var->declaration->ident->name.data;

        if (var->declaration->flags & DECLARATION_IS_LAMBDA_ARGUMENT) {
            var->declaration->llvm_value = LLVMGetParam(function, var->lambda_argument_index);
            LLVMSetValueName2(var->declaration->llvm_value, name, var->declaration->ident->name.count);
            break;
        }

        LLVMTypeRef type = llvm_get_type(w, var->declaration->my_type);

        // @Cleanup
        if (var->declaration->my_type->kind == TYPE_DEF_LAMBDA) {
            type = LLVMPointerType(type, 0);
        }

        LLVMValueRef alloca = LLVMBuildAlloca(llvm.builder, type, name);
        LLVMValueRef initializer = llvm_build_expression(w, var->declaration->root_expression);
        LLVMBuildStore(llvm.builder, initializer, alloca);
        var->declaration->llvm_value = alloca;
        break;
    }
    case AST_ASSIGNMENT: {
        Ast_Assignment *assign = xx stmt;

        if (assign->pointer->kind == AST_IDENT) {
            Ast_Ident *ident = xx assign->pointer;
            Ast_Type_Definition *defn = assign->pointer->inferred_type;

            LLVMValueRef pointer = ident->resolved_declaration->llvm_value;
            LLVMValueRef value = llvm_build_expression(w, assign->value);

            if (assign->operator_type == '=') {
                LLVMBuildStore(llvm.builder, value, pointer);
                break;
            }

            // Otherwise, build the arithmetic.
            LLVMIntPredicate int_predicate;
            LLVMRealPredicate real_predicate;
            LLVMOpcode opcode = llvm_get_opcode(assign->operator_type, defn, &int_predicate, &real_predicate);

            LLVMValueRef current = LLVMBuildLoad2(llvm.builder, llvm_get_type(w, defn), pointer, "");
            LLVMValueRef new_value = LLVMBuildBinOp(llvm.builder, opcode, current, value, "");
            LLVMBuildStore(llvm.builder, new_value, pointer);
            break;
        }

        UNIMPLEMENTED;
    }
    case AST_EXPRESSION_STATEMENT: {
        Ast_Expression_Statement *x = xx stmt;
        llvm_build_expression(w, x->subexpression);
        break;
    }
    default:
        printf("%s\n", stmt_to_string(stmt));
        assert(0);
    }
}


void llvm_build_declaration(Workspace *w, Ast_Declaration *decl)
{
    if (decl->flags & DECLARATION_IS_PROCEDURE_HEADER) {
        Ast_Lambda *lambda = xx decl->root_expression;
        LLVMSetValueName2(lambda->my_body_declaration->llvm_value, decl->ident->name.data, decl->ident->name.count);
    }

    if (decl->flags & DECLARATION_IS_PROCEDURE_BODY) {
        if (decl->flags & DECLARATION_IS_FOREIGN) return; // Nothing to build.

        assert(decl->llvm_value); // Should've been added in the pre-pass.
        LLVMValueRef function = decl->llvm_value;
        LLVMBasicBlockRef entry = LLVMAppendBasicBlock(function, "entry");
        LLVMPositionBuilderAtEnd(w->llvm.builder, entry);
        llvm_build_statement(w, function, xx decl->my_block->parent); // Arguments.
        llvm_build_statement(w, function, xx decl->my_block);

        if (decl->my_type->lambda.return_type == w->type_def_void) {
            if (LLVMGetBasicBlockTerminator(LLVMGetLastBasicBlock(function)) == NULL) {
                LLVMBuildRetVoid(w->llvm.builder);
            }
        }

        if (LLVMVerifyFunction(function, LLVMPrintMessageAction)) {
            String_View name;
            name.data = LLVMGetValueName2(function, &name.count);
            printf("===============================\n");
            LLVMDumpValue(function);
            printf("===============================\n");
            exit(1);
        }
    }
}
