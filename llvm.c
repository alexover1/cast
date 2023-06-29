#include "common.h"
#include "workspace.h"

#define Replace_Type(type) do { \
    while (type->_expression.replacement) type = xx type->_expression.replacement; \
} while(0)

// For now, who cares if we zero terminate? I don't see any problems. And it lets you call c functions easier.
#define DONT_ZERO_TERMINATE 0
#define USE_STRUCT_PACKING 0

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

void workspace_execute_llvm(Workspace *w)
{
    Ast_Declaration *main_decl = find_declaration_in_block(w->global_block, sv_from_cstr("main"));
    if (!main_decl) {
        fprintf(stderr, "Error: Cannot run a program with no 'main' entry point.");
        workspace_dispose_llvm(w);
        exit(1);
    }
    if (main_decl->root_expression->kind != AST_LAMBDA) {
        report_error(w, main_decl->location, "'main' must be a procedure.");
        workspace_dispose_llvm(w);
        exit(1);
    }

    Ast_Lambda *lambda = xx main_decl->root_expression;

    if (arrlen(lambda->type_definition->lambda.argument_types) != 0) {
        report_error(w, main_decl->location, "'main' entry point must not take any arguments.");
    }

    // Verify and optimize the module
    char* error = NULL;
    LLVMVerifyModule(w->llvm.module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    LLVMPassManagerRef passManager = LLVMCreatePassManager();
    LLVMAddPromoteMemoryToRegisterPass(passManager);
    LLVMAddInstructionCombiningPass(passManager);
    LLVMAddReassociatePass(passManager);
    LLVMAddGVNPass(passManager);
    LLVMAddCFGSimplificationPass(passManager);
    LLVMRunPassManager(passManager, w->llvm.module);
    LLVMDisposePassManager(passManager);

    // Create execution engine
    error = NULL;
    LLVMCreateExecutionEngineForModule(&w->llvm.execution_engine, w->llvm.module, &error);
    if (error != NULL) {
        fprintf(stderr, "Error: Failed to create execution engine: %s\n", error);
        LLVMDisposeMessage(error);
        workspace_dispose_llvm(w);
        exit(1);
    }

    // Execute the function
    LLVMRunFunction(w->llvm.execution_engine, lambda->my_body_declaration->llvm_value, 0, NULL);
}

void workspace_dispose_llvm(Workspace *w)
{
    LLVMDisposeBuilder(w->llvm.builder);

    if (w->llvm.execution_engine) {
        LLVMDisposeExecutionEngine(w->llvm.execution_engine);
    } else {
        LLVMDisposeModule(w->llvm.module);
    }
    
    LLVMContextDispose(w->llvm.context); // @Bad: Why is this not consistently named?
}

LLVMTypeRef llvm_get_type(Workspace *w, const Ast_Type_Definition *defn)
{
    Llvm llvm = w->llvm;
    assert(defn);
    // while (defn->_expression.replacement) defn = xx defn->_expression.replacement; // TODO: We should store ** in the typechecker so this goes away.
    switch (defn->kind) {
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
       
        return LLVMStructTypeInContext(llvm.context, field_types, count, USE_STRUCT_PACKING);
    }
    case TYPE_DEF_ENUM:
        return LLVMInt32TypeInContext(llvm.context);
    case TYPE_DEF_POINTER:
        return LLVMPointerType(llvm_get_type(w, defn->pointer_to), 0);
    case TYPE_DEF_ARRAY: {
        switch (defn->array.kind) {
        case ARRAY_KIND_FIXED:
            // return LLVMPointerTypeInContext(llvm.context, 0);
            return LLVMArrayType(llvm_get_type(w, defn->array.element_type), defn->array.length);
        case ARRAY_KIND_SLICE:
            return llvm.slice_type;
        case ARRAY_KIND_DYNAMIC:
            return llvm.dynamic_array_type;
        }
    }
    case TYPE_DEF_STRUCT_CALL:
        UNIMPLEMENTED;
    case TYPE_DEF_IDENT:
        report_error(w, defn->_expression.location, "This identifier was not replaced by the type-checker (this is an internal error).");
        return NULL; // Note: This would cause a segfault if report_error didn't call exit();
    case TYPE_DEF_LAMBDA: {
        size_t arg_count = arrlenu(defn->lambda.argument_types);
        LLVMTypeRef *param_types = arena_alloc(&temporary_arena, sizeof(LLVMTypeRef) * arg_count);
        For (defn->lambda.argument_types) {
            // Replace_Type(defn->lambda.argument_types[it]);
            // if (defn->lambda.argument_types[it]->kind == TYPE_DEF_STRUCT) {
            //     assert(defn->size >= 0);
            //     const int num_i64s = (defn->size + 7) / 8;

            //     param_types[it] = LLVMArrayType(LLVMInt64TypeInContext(llvm.context), num_i64s);
                   
            //     // We need to pass it by pointer but with the byval attribute.
            //     // param_types[it] = LLVMPointerTypeInContext(llvm.context, 0);
            // } else {
                param_types[it] = llvm_get_type(w, defn->lambda.argument_types[it]);
            // }
        }
        LLVMTypeRef return_type = llvm_get_type(w, defn->lambda.return_type);
        return LLVMFunctionType(return_type, param_types, arg_count, defn->lambda.variadic);
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
    case TOKEN_BITWISE_AND_EQUALS:
        return LLVMAnd;
    case TOKEN_BITWISE_OR:
    case TOKEN_LOGICAL_OR:
    case TOKEN_BITWISE_OR_EQUALS:
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
    case TOKEN_SHIFT_LEFT:
        return LLVMShl;
    case TOKEN_SHIFT_RIGHT:
        if (defn->number.flags & NUMBER_FLAGS_SIGNED) return LLVMAShr;
        return LLVMLShr;
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
    LLVMTypeRef array_type = LLVMArrayType(u8_type, count+1); // +1 for zero termination
    LLVMTypeRef pointer_type = LLVMPointerTypeInContext(llvm.context, 0);

    // Create global variable with the array of characters.
    LLVMValueRef global_string = LLVMAddGlobal(llvm.module, array_type, "");
    LLVMSetLinkage(global_string, LLVMPrivateLinkage);
    LLVMSetInitializer(global_string, LLVMConstStringInContext(llvm.context, data, count, 0));

    return LLVMConstBitCast(global_string, pointer_type);
}

LLVMValueRef llvm_build_pointer(Workspace *w, Ast_Expression *expr)
{
    Llvm llvm = w->llvm;
    // while (expr->replacement) expr = expr->replacement;
    switch (expr->kind) {
    case AST_IDENT: {
        const Ast_Ident *ident = xx expr;          
        assert(ident->resolved_declaration);
        assert(!(ident->resolved_declaration->flags & DECLARATION_IS_CONSTANT)); // It should have been substituted.
        assert(!(ident->resolved_declaration->flags & DECLARATION_IS_FOR_LOOP_ITERATOR)); // Should have thrown an error that you can't assign to this.
        assert(ident->resolved_declaration->llvm_value); // Must have been initialized.
        return ident->resolved_declaration->llvm_value;
    }
    case AST_SELECTOR: {
        const Ast_Selector *selector = xx expr;
        LLVMTypeRef struct_type = llvm_get_type(w, selector->namespace_expression->inferred_type);
        LLVMValueRef struct_pointer = llvm_build_pointer(w, selector->namespace_expression);

        assert(selector->struct_field_index >= 0);

        // @Speed: This is all just for debugging.
        String_View struct_name;
        struct_name.data = LLVMGetValueName2(struct_pointer, &struct_name.count);
        char *name = tprint(SV_Fmt".%d", SV_Arg(struct_name), selector->struct_field_index);
        
        return LLVMBuildStructGEP2(llvm.builder, struct_type, struct_pointer, selector->struct_field_index, name);
    }
    case AST_UNARY_OPERATOR: {           
        Ast_Unary_Operator *unary = xx expr;

        if (unary->operator_type == TOKEN_POINTER_DEREFERENCE) {
            LLVMTypeRef type = llvm_get_type(w, unary->subexpression->inferred_type);
            LLVMValueRef pointer = llvm_build_pointer(w, unary->subexpression);
                
            // @Speed: This is all just for debugging.
            String_View pointer_name;
            pointer_name.data = LLVMGetValueName2(pointer, &pointer_name.count);
            char *name = tprint(SV_Fmt".deref", SV_Arg(pointer_name));
                
            return LLVMBuildLoad2(llvm.builder, type, pointer, name);
        }
    }
    case AST_BINARY_OPERATOR: {
        Ast_Binary_Operator *binary = xx expr;

        if (binary->operator_type == TOKEN_ARRAY_SUBSCRIPT) {
            LLVMValueRef array_pointer = llvm_build_pointer(w, binary->left);
            LLVMValueRef index = llvm_build_expression(w, binary->right);

            if (binary->left->inferred_type->array.kind != ARRAY_KIND_FIXED) {
                // @Speed: This is all just for debugging.
                String_View array_name;
                array_name.data = LLVMGetValueName2(array_pointer, &array_name.count);
                char *name = tprint(SV_Fmt".deref", SV_Arg(array_name));

                array_pointer = LLVMBuildLoad2(llvm.builder, LLVMTypeOf(array_pointer), array_pointer, name);
            }
                
            // @Speed: This is all just for debugging.
            String_View array_name;
            array_name.data = LLVMGetValueName2(array_pointer, &array_name.count);
            char *name = tprint(SV_Fmt".elem", SV_Arg(array_name));
                
            return LLVMBuildGEP2(llvm.builder, LLVMTypeOf(array_pointer), array_pointer, &index, 1, name);
        }

        // Must be pointer arithmetics.
        assert(binary->left->inferred_type->kind == TYPE_DEF_POINTER);

        UNIMPLEMENTED;

        // LLVMValueRef lhs = llvm_build_pointer(w, binary->left);

        // if (binary->right->inferred_type->kind == TYPE_DEF_NUMBER) {
        //     lhs = LLVMBuildPtrToInt(llvm.builder, lhs, LLVMTypeOf(RHS), "");
        //     LLVMValueRef result = LLVMBuildBinOp(llvm.builder, opcode, LHS, RHS, "");
        //     return LLVMBuildIntToPtr(llvm.builder, result, llvm_get_type(w, binary->left->inferred_type), "");
        // }
            
        // LLVMValueRef result = LLVMBuildBinOp(llvm.builder, opcode, LHS, RHS, "");
        // return LLVMBuildIntToPtr(llvm.builder, result, llvm_get_type(w, binary->left->inferred_type), "");
    }
    default:
        break;
    }
    if (expr->inferred_type->kind != TYPE_DEF_POINTER) {
        report_error(w, expr->location, "We encountered something that we tried to use as a pointer that wasn't a pointer (this is an internal errro).");
    }
    return llvm_build_expression(w, expr);
}

// Accepts a pointer to a struct and packs it into an allocated i64 array.
LLVMValueRef llvm_pack_struct_into_i64_array(LLVMBuilderRef builder, LLVMValueRef function, LLVMTargetDataRef target_data, LLVMValueRef struct_value)
{
    LLVMBasicBlockRef entry_block = LLVMGetLastBasicBlock(function);
    
    LLVMTypeRef struct_type = LLVMTypeOf(struct_value);
    size_t struct_size = LLVMABISizeOfType(target_data, struct_type);
    size_t num_i64s = (struct_size + 7) / 8;

    // Allocate memory for the array of i64s.
    LLVMValueRef packed_array = LLVMBuildArrayAlloca(builder, LLVMInt64Type(), LLVMConstInt(LLVMInt32Type(), num_i64s, 0), "packed_array");

    // Cast the input struct pointer into a byte pointer.
    LLVMTypeRef byte_ptr_type = LLVMPointerType(LLVMInt8Type(), 0);
    LLVMValueRef byte_ptr = LLVMBuildBitCast(builder, struct_value, byte_ptr_type, "byte_ptr");

    // Create the loop.
    LLVMBasicBlockRef loop_block = LLVMAppendBasicBlock(function, "pack_loop");
    LLVMBasicBlockRef end_block = LLVMAppendBasicBlock(function, "pack_end");
    LLVMBuildBr(builder, loop_block); // Enter the loop.
    LLVMPositionBuilderAtEnd(builder, loop_block);

    LLVMValueRef offset_phi = LLVMBuildPhi(builder, LLVMInt64Type(), "offset_phi");
    LLVMValueRef byte_ptr_phi = LLVMBuildPhi(builder, byte_ptr_type, "byte_ptr_phi");
    LLVMValueRef packed_array_phi = LLVMBuildPhi(builder, LLVMPointerType(LLVMInt64Type(), 0), "packed_array_phi");

    // Load a byte from the struct and OR it with the current value in the packed array.
    LLVMValueRef byte = LLVMBuildLoad2(builder, LLVMInt8Type(), byte_ptr_phi, "byte");
    // LLVMValueRef index = LLVMBuildUDiv(builder, offset_phi, LLVMConstInt(LLVMInt64Type(), 8, 0), "index");
    LLVMValueRef current = LLVMBuildLoad2(builder, LLVMInt64Type(), packed_array_phi, "current");
    LLVMValueRef shift = LLVMBuildShl(builder, LLVMBuildZExt(builder, byte, LLVMInt64Type(), "zext"), offset_phi, "shift");
    LLVMValueRef packed_value = LLVMBuildOr(builder, current, shift, "packed_value");
    LLVMBuildStore(builder, packed_value, packed_array_phi);

    // Increment the byte pointer and offset.
    LLVMValueRef indices = LLVMConstInt(LLVMInt32Type(), 1, 0);
    LLVMValueRef byte_ptr_inc = LLVMBuildGEP2(builder, LLVMInt8Type(), byte_ptr_phi, &indices, 1, "byte_ptr_inc");
    LLVMValueRef offset_inc = LLVMBuildAdd(builder, offset_phi, LLVMConstInt(LLVMInt64Type(), 8, 0), "offset_inc");

    // Check if we are done.
    LLVMValueRef offset_cmp = LLVMBuildICmp(builder, LLVMIntEQ, offset_inc, LLVMConstInt(LLVMInt64Type(), struct_size, 0), "offset_cmp");
    LLVMBuildCondBr(builder, offset_cmp, end_block, loop_block);

    // Add the loop variables to the phi nodes.
    LLVMAddIncoming(offset_phi, &offset_inc, &loop_block, 1);
    LLVMAddIncoming(byte_ptr_phi, &byte_ptr_inc, &loop_block, 1);
    LLVMAddIncoming(packed_array_phi, &packed_array, &loop_block, 1);

    LLVMValueRef offset_init = LLVMConstInt(LLVMInt64Type(), 0, 0);
    LLVMAddIncoming(offset_phi, &offset_init, &entry_block, 1);
    LLVMAddIncoming(byte_ptr_phi, &byte_ptr, &entry_block, 1);
    LLVMAddIncoming(packed_array_phi, &packed_array, &entry_block, 1);

    LLVMPositionBuilderAtEnd(builder, end_block);

    return packed_array;
}

LLVMValueRef llvm_build_expression(Workspace *w, Ast_Expression *expr)
{
    Llvm llvm = w->llvm;
    // while (expr->replacement) expr = expr->replacement;
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
        case LITERAL_BOOL: return LLVMConstInt(LLVMInt1TypeInContext(llvm.context), lit->bool_value, 0);
        case LITERAL_NULL: return LLVMConstNull(type);

        case LITERAL_STRING: {
            LLVMValueRef string_data_pointer = llvm_const_string(llvm, lit->string_value.data, lit->string_value.count);
                    
            if (expr->inferred_type == w->type_def_string) {
                // Create constant structure value.
                LLVMValueRef struct_fields[] = {
                    string_data_pointer,
                    LLVMConstInt(LLVMInt64TypeInContext(llvm.context), lit->string_value.count, 0),
                };
            
                return LLVMConstStructInContext(llvm.context,
                    struct_fields, sizeof(struct_fields)/sizeof(struct_fields[0]), USE_STRUCT_PACKING);
            }

            // If we are not inferred to be string, we must be *u8.
            return string_data_pointer;
        }
        }
    }
    case AST_IDENT: {
        const Ast_Ident *ident = xx expr;
        assert(ident->resolved_declaration);

        assert(!(ident->resolved_declaration->flags & DECLARATION_IS_CONSTANT)); // It should have been substituted.

        // Because during typechecking we assured that the variable's initialization came before us, this is safe.
        assert(ident->resolved_declaration->llvm_value);

        if (ident->resolved_declaration->flags & DECLARATION_IS_FOR_LOOP_ITERATOR) {
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

        switch (unary->operator_type) {
        case '!': {
            LLVMValueRef value = llvm_build_expression(w, unary->subexpression);
            if (!value) return NULL;
            return LLVMBuildNot(llvm.builder, value, "");
        }
        case '*':
            return llvm_build_pointer(w, unary->subexpression);
        case TOKEN_POINTER_DEREFERENCE: {
            LLVMTypeRef type = llvm_get_type(w, expr->inferred_type);
            LLVMValueRef pointer = llvm_build_expression(w, unary->subexpression);
            if (!pointer) return NULL;

            // @Speed: This is all just for debugging.
            String_View pointer_name;
            pointer_name.data = LLVMGetValueName2(pointer, &pointer_name.count);
            char *name = tprint(SV_Fmt".deref", SV_Arg(pointer_name));
                
            return LLVMBuildLoad2(llvm.builder, type, pointer, name);
        }
        default:
            UNIMPLEMENTED;
        }
    }
    case AST_BINARY_OPERATOR: {
        const Ast_Binary_Operator *binary = xx expr;

        if (binary->operator_type == TOKEN_ARRAY_SUBSCRIPT) {
            return LLVMBuildLoad2(llvm.builder, llvm_get_type(w, expr->inferred_type), llvm_build_pointer(w, expr), "");
        }

        LLVMValueRef LHS = llvm_build_expression(w, binary->left);
        LLVMValueRef RHS = llvm_build_expression(w, binary->right);

        LLVMIntPredicate int_predicate;
        LLVMRealPredicate real_predicate;
        LLVMOpcode opcode = llvm_get_opcode(binary->operator_type, binary->left->inferred_type, &int_predicate, &real_predicate);

        // Comparison operators.
        if (opcode == LLVMICmp) return LLVMBuildICmp(llvm.builder, int_predicate, LHS, RHS, "");
        if (opcode == LLVMFCmp) return LLVMBuildFCmp(llvm.builder, real_predicate, LHS, RHS, "");

        // @Hack for pointer arithmetic in LLVM.
        if (binary->left->inferred_type->kind == TYPE_DEF_POINTER && binary->right->inferred_type->kind == TYPE_DEF_NUMBER) {
            LHS = LLVMBuildPtrToInt(llvm.builder, LHS, LLVMTypeOf(RHS), "");
            LLVMValueRef result = LLVMBuildBinOp(llvm.builder, opcode, LHS, RHS, "");
            return LLVMBuildIntToPtr(llvm.builder, result, llvm_get_type(w, binary->left->inferred_type), "");
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
        assert(cast->type->size >= 0);
        assert(cast->subexpression->inferred_type->size >= 0);

        LLVMTypeRef dest_type = llvm_get_type(w, cast->type);
        LLVMValueRef value = llvm_build_expression(w, cast->subexpression);

        LLVMOpcode opcode;
        
        if (cast->type->size > cast->subexpression->inferred_type->size) {
            // For now, we always do a zero-extending cast (this seems closest to bitcast).
            // Sign extension cast will probably be a separate instruction or a modifier on the "as" keyword.
            opcode = LLVMZExt;
        } else if (cast->type->size < cast->subexpression->inferred_type->size) {
            opcode = LLVMTrunc;
        } else {
            opcode = LLVMBitCast;
        }
        
        return LLVMBuildCast(llvm.builder, opcode, value, dest_type, "");
    }
    case AST_SELECTOR: {
        const Ast_Selector *selector = xx expr;
            
        if (selector->struct_field_index >= 0) {
            LLVMTypeRef field_type = llvm_get_type(w, selector->_expression.inferred_type);
            LLVMValueRef field_pointer = llvm_build_pointer(w, expr);
            return LLVMBuildLoad2(llvm.builder, field_type, field_pointer, "");
        }

        assert(0);
    }
    case AST_TYPE_INSTANTIATION: {
        const Ast_Type_Instantiation *inst = xx expr;

        assert(inst->type_definition->kind != TYPE_DEF_LITERAL); // Typechecking should replace this.

        size_t n = arrlenu(inst->arguments);

        LLVMValueRef *values = arena_alloc(&temporary_arena, n * sizeof(LLVMValueRef));
        For (inst->arguments) {
            // printf(">> %s\n", expr_to_string(inst->arguments[it]));
            values[it] = llvm_build_expression(w, inst->arguments[it]);
        }
        return LLVMConstStructInContext(llvm.context, values, n, USE_STRUCT_PACKING);
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
            
        LLVMBasicBlockRef basic_block_loop = LLVMAppendBasicBlock(function, "loop");
        LLVMBuildBr(llvm.builder, basic_block_loop); // Implicit break from current block to the loop.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_loop);

        LLVMBasicBlockRef basic_block_then = LLVMAppendBasicBlock(function, "then");
        LLVMBasicBlockRef basic_block_merge = LLVMAppendBasicBlock(function, "merge");

        LLVMValueRef condition = llvm_build_expression(w, while_stmt->condition_expression);
        LLVMBuildCondBr(llvm.builder, condition, basic_block_then, basic_block_merge);

        // Emit the "then" statement.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_then);
        llvm_build_statement(w, function, while_stmt->then_statement);
        LLVMBuildBr(llvm.builder, basic_block_loop);
        // LLVMBuildCondBr(llvm.builder, condition, basic_block_loop, basic_block_merge);
            
        // Emit code for the merge block.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_merge);
        break;
    }
    case AST_IF: {
        const Ast_If *if_stmt = xx stmt;
        LLVMValueRef condition = llvm_build_expression(w, if_stmt->condition_expression);

        LLVMBasicBlockRef basic_block_then = LLVMAppendBasicBlock(function, "then");
        LLVMBasicBlockRef basic_block_else, basic_block_merge;

        // Emit the conditional break.
        if (if_stmt->else_statement) {
            basic_block_else = LLVMAppendBasicBlock(function, "else");
            LLVMBuildCondBr(llvm.builder, condition, basic_block_then, basic_block_else);
            basic_block_merge = LLVMAppendBasicBlock(function, "merge");
        } else {
            basic_block_merge = LLVMAppendBasicBlock(function, "merge");
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
            if (LLVMGetBasicBlockTerminator(basic_block_else) == NULL) {
                LLVMBuildBr(llvm.builder, basic_block_merge);
            }
        }

        // Emit code for the merge block.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_merge);
        break;
    }
    case AST_FOR: {
        Ast_For *for_stmt = xx stmt;
            
        LLVMBasicBlockRef basic_block_current = LLVMGetLastBasicBlock(function);

        // Add the loop block.
        LLVMBasicBlockRef basic_block_loop = LLVMAppendBasicBlock(function, "loop");
        LLVMBuildBr(llvm.builder, basic_block_loop); // Implicit break from current block to the loop.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_loop);

        LLVMTypeRef i64 = LLVMInt64TypeInContext(llvm.context);

        LLVMValueRef zero = LLVMConstInt(i64, 0, 0);
        LLVMValueRef it_phi = LLVMBuildPhi(llvm.builder, i64, "it_phi");

        LLVMAddIncoming(it_phi, &zero, &basic_block_current, 1);

        for_stmt->iterator_declaration->llvm_value = it_phi;

        // Temporary assert until we have iterators over arrays.
        assert(for_stmt->range_expression->kind == AST_BINARY_OPERATOR);
        const Ast_Binary_Operator *binary = xx for_stmt->range_expression;
        assert(binary->operator_type == TOKEN_DOUBLE_DOT);

        // Add the loop body & the exit condition.
        LLVMBasicBlockRef basic_block_then = LLVMAppendBasicBlock(function, "then");
        LLVMBasicBlockRef basic_block_merge = LLVMAppendBasicBlock(function, "merge");       

        LLVMValueRef cond = LLVMBuildICmp(llvm.builder, LLVMIntSLE, it_phi, llvm_build_expression(w, binary->right), "");
        LLVMBuildCondBr(llvm.builder, cond, basic_block_then, basic_block_merge);

        // Emit code for the then block.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_then);
        llvm_build_statement(w, function, for_stmt->then_statement);
        LLVMValueRef it_incr = LLVMBuildAdd(llvm.builder, it_phi, LLVMConstInt(i64, 1, 0), "it_incr");
        LLVMAddIncoming(it_phi, &it_incr, &basic_block_then, 1);
        LLVMBuildBr(llvm.builder, basic_block_loop);

        // Emit code for the merge block.
        LLVMPositionBuilderAtEnd(llvm.builder, basic_block_merge);
        break;
    }
    case AST_LOOP_CONTROL: {
        UNIMPLEMENTED;
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

        LLVMValueRef alloca = LLVMBuildAlloca(llvm.builder, llvm_get_type(w, var->declaration->my_type), name);
        var->declaration->llvm_value = alloca;
       
        if (var->declaration->flags & DECLARATION_IS_LAMBDA_ARGUMENT) {
            LLVMBuildStore(llvm.builder, LLVMGetParam(function, var->lambda_argument_index), alloca);
            break;
        }

        LLVMValueRef initializer = llvm_build_expression(w, var->declaration->root_expression);
        LLVMBuildStore(llvm.builder, initializer, alloca);
        break;
    }
    case AST_ASSIGNMENT: {
        Ast_Assignment *assign = xx stmt;
        LLVMValueRef pointer = llvm_build_pointer(w, assign->pointer);
        LLVMValueRef value = llvm_build_expression(w, assign->value);
        LLVMBuildStore(llvm.builder, value, pointer);
        break;
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
