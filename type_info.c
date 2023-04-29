#include "type_info.h"
#include "ast.h"
#include "vendor/stb_ds.h"

bool types_are_equal(Type_Info a, Type_Info b);
const char *type_to_string(Type type);
extern Type_Table type_table;

void infer_types(Ast_Node *node)
{
    if (node->inferred_type != NULL) return;

    switch (node->kind) {
        case AST_INTEGER:
            node->inferred_type = type_table.s64;
            break;
        case AST_FLOAT:
            node->inferred_type = type_table.f32;
            break;
        case AST_STRING:
            node->inferred_type = type_table.string;
            break;
        case AST_BINARY: {
            Ast_Node *left = node->binary.lhs;
            Ast_Node *right = node->binary.rhs;
            infer_types(left);
            infer_types(right);

            if (left->inferred_type != right->inferred_type && !types_are_equal(*left->inferred_type, *right->inferred_type)) {
                printf("error: The types on either side of the binary node must be the same.\n");
                printf("info: Got types %s and %s\n", type_to_string(left->inferred_type), type_to_string(right->inferred_type));
                exit(1);
            }

            // For now, assume all operators work only on int and float.

            if (left->inferred_type->tag != TYPE_INTEGER && left->inferred_type->tag != TYPE_FLOAT) {
                printf("error: Operator '+' expects type int or float but got type %s\n", type_to_string(left->inferred_type));
                exit(1);
            }

            if (node->binary.op == TOKEN_DOUBLE_EQUAL) {
                node->inferred_type = type_table.bool_type;
            } else {
                // int or float
                node->inferred_type = left->inferred_type;
            }
        } break;
        
        case AST_PROCEDURE_CALL: {
            Ast_Procedure_Call *call = &node->procedure_call;
            Type_Info_Procedure proc = call->procedure->inferred_type->procedure;

            // Set type of procedure we are trying to call.
            infer_types(call->procedure);

            if (call->procedure->inferred_type->tag != TYPE_PROCEDURE) {
                printf("error: '");
                // print_ast(*start->as.procedure_call.procedure);
                printf("' is not a procedure.\n");
                exit(1);
            }
            
            // Set argument types
            for (int i = 0; i < call->arguments_count; ++i) {
                infer_types(&call->arguments[i]);
            }

            if (call->arguments_count != proc.parameters_count) {
                printf("error: Incorrect number of arguments to function 'print'.\n");
                printf("info: Expected %d arguments but got %d\n", proc.parameters_count, call->arguments_count);
                exit(1);
            }

            for (int i = 0; i < proc.parameters_count; ++i) {
                if (!types_are_equal(*call->arguments[i].inferred_type, *proc.parameters[i])) {
                    printf("error: Parameter %d of 'print' was expected to be %s but got %s.\n",
                        i + 1,
                        type_to_string(proc.parameters[i]),
                        type_to_string(call->arguments[i].inferred_type));
                    exit(1);
                }
            }

            node->inferred_type = proc.return_type;
        } break;

        case AST_IDENTIFIER:
            node->inferred_type = node->identifier.declaration->root_expression->inferred_type;
            break;
        
        default:
            assert(0 && "not implemented yet");
    }

    // print_ast(*start);
    // printf(" is of type %s\n", type_as_string(start->inferred_type.tag));
}

bool types_are_equal(Type_Info a, Type_Info b)
{
    if (a.tag != b.tag) return false;

    switch (a.tag) {
        case TYPE_INTEGER:
            return (a.runtime_size == b.runtime_size) && (a.signed_integer == b.signed_integer);
        case TYPE_FLOAT:
            return a.runtime_size == b.runtime_size;

        case TYPE_STRING:
        case TYPE_BOOL:
        case TYPE_VOID:
            return true;
        
        case TYPE_PROCEDURE:
            if (a.procedure.parameters_count != b.procedure.parameters_count) return false;
            if (!types_are_equal(*a.procedure.return_type, *b.procedure.return_type)) {
                return false;
            }
            for (size_t i = 0; i < a.procedure.parameters_count; ++i) {
                if (!types_are_equal(*a.procedure.parameters[i], *b.procedure.parameters[i])) return false;
            }
            return true;

        default: assert(0 && "unreachable");
    }
}

Type add_type_to_table(Type_Table *table, Type_Info new_type)
{
    Type_Info *result = arraddnptr(table->internal_array, 1);
    *result = new_type;
    return result;
}

void add_preloaded_types_to_table(Type_Table *table)
{
    table->s64 = add_type_to_table(table, (Type_Info){
        .tag = TYPE_INTEGER,
        .runtime_size = 8,
        .signed_integer = true,
    });
    
    table->f32 = add_type_to_table(table, (Type_Info){
        .tag = TYPE_FLOAT,
        .runtime_size = 4,
    });

    table->string = add_type_to_table(table, (Type_Info){
        .tag = TYPE_STRING,
        .runtime_size = 16, // TODO: check?
    });
    
    table->bool_type = add_type_to_table(table, (Type_Info){
        .tag = TYPE_BOOL,
        .runtime_size = 1,
    });

    table->void_type = add_type_to_table(table, (Type_Info){
        .tag = TYPE_VOID,
        .runtime_size = 0,
    });
}

