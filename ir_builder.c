#include "common.h"
#include "ir_builder.h"

static inline Ir_Index ir_builder_create_value(Ir_Builder *builder, Ir_Value init)
{
    Ir_Index result = arrlen(builder->values);
    if (init.name == NULL) {
        init.name = tprint("x%zu", result);
    }
    arrput(builder->values, init);
    return result;
}

Ir_Index ir_build_constant_integer(Ir_Builder *builder, uint64_t x)
{
    return ir_builder_create_value(builder, (Ir_Value){
        .kind = IR_CONSTANT_INTEGER,
        .integer_value = x,
    });
}

Ir_Index ir_build_add(Ir_Builder *builder, Ir_Index lefti, Ir_Index righti)
{
    Ir_Value *left = &builder->values[lefti];
    Ir_Value *right = &builder->values[righti];
        
    // Constant folding on the expression.
    if (left->kind == IR_CONSTANT_INTEGER && right->kind == IR_CONSTANT_INTEGER) {
        return ir_build_constant_integer(builder, left->integer_value + right->integer_value);
    }

    left->use_count += 1;
    right->use_count += 1;
    
    return ir_builder_create_value(builder, (Ir_Value){
        .kind = IR_ADD,
        .binary = {
            .left = lefti,
            .right = righti,
        },
    });
}

Ir_Index ir_build_mul(Ir_Builder *builder, Ir_Index lefti, Ir_Index righti)
{
    Ir_Value *left = &builder->values[lefti];
    Ir_Value *right = &builder->values[righti];
        
    // Constant folding on the expression.
    if (left->kind == IR_CONSTANT_INTEGER && right->kind == IR_CONSTANT_INTEGER) {
        return ir_build_constant_integer(builder, left->integer_value * right->integer_value);
    }

    left->use_count += 1;
    right->use_count += 1;
    
    return ir_builder_create_value(builder, (Ir_Value){
        .kind = IR_MUL,
        .binary = {
            .left = lefti,
            .right = righti,
        },
    });
}

// TODO: Maybe we would have this function accept the type that we are allocating?
Ir_Index ir_build_alloca(Ir_Builder *builder, Ir_Index initializer)
{
    builder->values[initializer].use_count += 1;
    
    return ir_builder_create_value(builder, (Ir_Value){
        .kind = IR_ALLOCA,
        .operand = initializer,
    });
}

Ir_Index ir_build_load(Ir_Builder *builder, Ir_Index pointer)
{
    builder->values[pointer].use_count += 1;

    return ir_builder_create_value(builder, (Ir_Value){
        .kind = IR_LOAD,
        .operand = pointer,
    });
}

Ir_Index ir_build_store(Ir_Builder *builder, Ir_Index pointer, Ir_Index value)
{
    builder->values[pointer].use_count += 1;
    builder->values[value].use_count += 1;

    return ir_builder_create_value(builder, (Ir_Value){
        .kind = IR_STORE,
        .binary = {
            .left = pointer,
            .right = value,
        },
    });
}

Ir_Index ir_build_return(Ir_Builder *builder, Ir_Index return_value)
{
    builder->values[return_value].use_count += 1;

    return ir_builder_create_value(builder, (Ir_Value){
        .kind = IR_RETURN,
        .operand = return_value,
    });
}

void ir_builder_finalize_values(Ir_Builder *builder)
{
    (void)builder;
}

const char *ir_value_to_string(const Ir_Builder *builder, Ir_Index index)
{
    Ir_Value value = builder->values[index];
    switch (value.kind) {
    case IR_CONSTANT_INTEGER:
        return tprint("$%s = %llu", value.name, value.integer_value);
        
    case IR_ADD:
        return tprint("$%s = add $%s and $%s", value.name,
            builder->values[value.binary.left].name,
            builder->values[value.binary.right].name);
        
    case IR_MUL:
        return tprint("$%s = mul $%s by $%s", value.name,
            builder->values[value.binary.left].name,
            builder->values[value.binary.right].name);
        
    case IR_ALLOCA:
        return tprint("$%s = alloca $%s", value.name,
            builder->values[value.operand].name);

    case IR_LOAD:
        return tprint("$%s = load from $%s", value.name,
            builder->values[value.operand].name);
        
    case IR_STORE:
        return tprint("store $%s in $%s",
            builder->values[value.binary.right].name,
            builder->values[value.binary.left].name);

    case IR_RETURN:
        return tprint("return $%s", builder->values[value.operand].name);

    default:
        return tprint("(ERROR: kind = %d)", value.kind);
    }
}

const char *ir_builder_to_string(const Ir_Builder *builder)
{
    // TODO: We are leaking all of our precious memory!

    String_Builder sb = {0};

    for (Ir_Index i = 0; i < arrlen(builder->values); ++i) {
        sb_print(&sb, "%s\n", ir_value_to_string(builder, i));
    }

    return sb.data;
}
