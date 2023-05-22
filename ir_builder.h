#ifndef IR_BUILDER_H_
#define IR_BUILDER_H_

#include <stddef.h>
#include <stdint.h>

typedef ptrdiff_t Ir_Index;

/// Generate code for `return (x * 2) + 1`
/// $0 = x * 2 
/// $1 = $0 + 1
/// $2 = return $1

typedef enum {
    IR_CONSTANT_INTEGER,
    IR_ADD,
    IR_MUL,

    // Allocates memory on the stack and returns the pointer.
    IR_ALLOCA,

    /// Takes a pointer and returns the value at that address.
    IR_LOAD,
    
    /// Takes a pointer and a value and writes the value into the pointer.
    ///     binary = [pointer, value]
    IR_STORE,

    /// Returns a value from the current function.
    IR_RETURN,
} Ir_Value_Kind;

typedef struct {
    Ir_Index left;
    Ir_Index right;
} Ir_Value_Binary;

typedef struct {
    const char *name;
    Ir_Value_Kind kind;
    size_t use_count;

    union {
        uint64_t integer_value;
        Ir_Value_Binary binary;
        Ir_Index operand;
    };
} Ir_Value;

typedef struct {
    Ir_Value *values;
} Ir_Builder;

Ir_Index ir_build_constant_integer(Ir_Builder *builder, uint64_t value);
Ir_Index ir_build_add(Ir_Builder *builder, Ir_Index left, Ir_Index right);
Ir_Index ir_build_mul(Ir_Builder *builder, Ir_Index left, Ir_Index right);

Ir_Index ir_build_alloca(Ir_Builder *builder, Ir_Index initializer);
Ir_Index ir_build_load(Ir_Builder *builder, Ir_Index pointer);
Ir_Index ir_build_store(Ir_Builder *builder, Ir_Index pointer, Ir_Index value);

Ir_Index ir_build_return(Ir_Builder *builder, Ir_Index return_value);

/// Computes the statements for a program. It will be very obvious if you
/// forget to call this function as you will receive empty functions.
void ir_builder_finalize_values(Ir_Builder *builder);
// TODO: The name doesn't really tell you what it does.

/// Prints a singe value into a string.
const char *ir_value_to_string(const Ir_Builder *builder, Ir_Index index);

/// Prints the entire program.
const char *ir_builder_to_string(const Ir_Builder *builder);

#endif // IR_BUILDER_H_
