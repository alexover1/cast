#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

#include <raylib.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>

#define PACKING_BIT_SIZE (sizeof(uint64_t) * 8)

#define ARRAY_LEN(arr) (sizeof((arr)) / sizeof((arr)[0]))

#define BYTE_TO_BINARY_PATTERN "0b%c%c%c%c%c%c%c%c"
#define BYTE_TO_BINARY(byte)  \
  ((byte) & 0x80 ? '1' : '0'), \
  ((byte) & 0x40 ? '1' : '0'), \
  ((byte) & 0x20 ? '1' : '0'), \
  ((byte) & 0x10 ? '1' : '0'), \
  ((byte) & 0x08 ? '1' : '0'), \
  ((byte) & 0x04 ? '1' : '0'), \
  ((byte) & 0x02 ? '1' : '0'), \
  ((byte) & 0x01 ? '1' : '0') 

void pack_struct(const void *input, size_t input_size, uint64_t *output, size_t output_size)
{
    size_t output_index = 0;

    const uint8_t *cursor = input;
    const uint8_t *end = (uint8_t*)input + input_size;

    while (cursor < end) {
        const size_t output_array_index = output_index / PACKING_BIT_SIZE;
        const size_t output_bit_index = output_index % PACKING_BIT_SIZE;
        uint64_t mask = ((uint64_t)*cursor) << output_bit_index;
        output[output_array_index] |= mask;
        output_index += 8;
        cursor += 1;
    }
}

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
    LLVMValueRef index = LLVMBuildUDiv(builder, offset_phi, LLVMConstInt(LLVMInt64Type(), 8, 0), "index");
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

int main2(void)
{
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmParsers();
    LLVMInitializeAllAsmPrinters();

    // Target information.
    char *triple = LLVMGetDefaultTargetTriple();

    char *error_message = NULL;
    LLVMTargetRef target = NULL;

    if (LLVMGetTargetFromTriple(triple, &target, &error_message) != 0) {
        fprintf(stderr, "Error: Could not create LLVM target: %s\n", error_message);
        LLVMDisposeMessage(error_message);
        LLVMDisposeMessage(triple);
        return 1;
    }

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
        return 1;
    }

    LLVMTargetDataRef target_data = LLVMCreateTargetDataLayout(target_machine);

    LLVMModuleRef module = LLVMModuleCreateWithName("My Program");
    LLVMSetTarget(module, triple);
    LLVMSetModuleDataLayout(module, target_data);

    LLVMDisposeMessage(triple);

    // Build some IR.
    LLVMBuilderRef builder = LLVMCreateBuilder();

    LLVMTypeRef my_main_type = LLVMFunctionType(LLVMVoidType(), NULL, 0, false);
    LLVMValueRef my_main = LLVMAddFunction(module, "main", my_main_type);
    LLVMBasicBlockRef entry_block = LLVMAppendBasicBlock(my_main, "entry");
    LLVMPositionBuilderAtEnd(builder, entry_block);

    // Structure type information.
    LLVMTypeRef struct_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), "MyStruct");
    LLVMTypeRef struct_elements[] = { LLVMInt32Type(), LLVMInt8Type(), LLVMDoubleType() };
    LLVMStructSetBody(struct_type, struct_elements, ARRAY_LEN(struct_elements), 0);

    // Constant structure that needs to be packed.
    LLVMValueRef field_values[] = {
        LLVMConstInt(LLVMInt32Type(), 0xcafebabe, 0),
        LLVMConstInt(LLVMInt8Type(), 0xaa, 0),
        LLVMConstReal(LLVMDoubleType(), 3.1415),
    };
    LLVMValueRef const_struct_value = LLVMConstStruct(field_values, ARRAY_LEN(field_values), 0);

    LLVMValueRef struct_ptr = LLVMBuildAlloca(builder, struct_type, "struct_ptr");
    LLVMBuildStore(builder, const_struct_value, struct_ptr);

    llvm_pack_struct_into_i64_array(builder, my_main, target_data, struct_ptr);

    LLVMBuildRetVoid(builder);
    LLVMVerifyFunction(my_main, LLVMAbortProcessAction);

    LLVMDisposeBuilder(builder);

    // Display the result.
    LLVMDumpModule(module);

    LLVMExecutionEngineRef engine;
    if (LLVMCreateExecutionEngineForModule(&engine, module, &error_message) != 0) {
        fprintf(stderr, "Error: Could not create LLVM execution engine\n");
        LLVMDisposeMessage(error_message);
        return 1;
    }

    LLVMRunFunction(engine, my_main, 0, NULL);
    
    // LLVMDisposeExecutionEngine(engine);
    LLVMDisposeTargetMachine(target_machine);
    LLVMDisposeTargetData(target_data);
    LLVMDisposeModule(module);

    return 0;
}

typedef struct {
    uint8_t xs[8];
    uint64_t foo;
    uint8_t bar;
} Foobar;

int main(void)
{
    Foobar foo = {0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0xaa, 0xbb, 0xcafebabecafebabe, 0xab};
    uint64_t packed[8] = {0};

    pack_struct(&foo, sizeof(foo), packed, ARRAY_LEN(packed));

    for (size_t i = 0; i < ARRAY_LEN(packed); ++i) {
        printf("packed[%zu]: %llx\n", i, packed[i]);
    }
}

void raylib_example(void)
{
    InitWindow(800, 600, "My Window");
    SetTargetFPS(60);
    while (!WindowShouldClose()) {
        BeginDrawing();
        ClearBackground(RED);
        EndDrawing();
    }
}

