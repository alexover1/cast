#include <string.h>

#include "common.h"
#include "interp.h"
#include "object.h"

#define INTERP_DO_TRACING

static void infer_object(Interp *interp, Object *object)
{
    if (object->inferred_type != NULL) return;

    const Ast_Declaration *decl = object->key;
    const Ast *ast = decl->expression;

    // Type instantiations need to have their types created before being typechecked,
    // unlike type definitions that always result in type 'Type.'

    if (ast->type == AST_TYPE_INSTANTIATION) {
        const Ast_Type_Instantiation *inst = Down(ast);
        assert(inst->type_definition);

        // This still might return NULL.
        object->inferred_type = interp_get_type(interp, inst->type_definition);
        return;
    }
        
    // Type definitions must add a type into the type table.

    if (ast->type == AST_TYPE_DEFINITION) {
        const Ast_Type_Definition *defn = Down(ast);

        object->type_value = interp_get_type(interp, defn);

        if (object->type_value != NULL) {
            // We only want to set the inferred type if we successfully created our type.
            object->inferred_type = interp->type_table.TYPE;

            #if defined(INTERP_DO_TRACING)
                printf("[defn] %s :: %s\n", decl->ident.name, type_to_string(&interp->type_table, object->type_value));
            #endif
        } 

        return;
    }

    object->inferred_type = typecheck_ast(interp, ast);

    // Replace comptime types for non-comptime declarations.

    if (object->inferred_type != NULL && (decl->flags & DECLARATION_IS_COMPTIME) == 0) {
        if (object->inferred_type == interp->type_table.comptime_int)         object->inferred_type = interp->type_table.INT;
        else if (object->inferred_type == interp->type_table.comptime_float)  object->inferred_type = interp->type_table.FLOAT;
        else if (object->inferred_type == interp->type_table.comptime_string) object->inferred_type = interp->type_table.STRING;
    }
}

Type interp_get_type(Interp *interp, const Ast_Type_Definition *defn)
{
    if (defn->struct_desc) {
        const Ast_Block *block = &defn->struct_desc->scope;

        ptrdiff_t *children = NULL;

        For (block->statements) {
            if (block->statements[it]->type != AST_DECLARATION) continue;

            const Ast_Declaration *decl = Down(block->statements[it]);            

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
            field->name = object->key->ident.name;
            field->offset = -1;
        }

        return type_table_append(&interp->type_table, &struct_type, sizeof(struct_type));
    }

    if (defn->enum_defn) {
        UNIMPLEMENTED;
    }

    if (defn->literal_name) {
        return parse_literal_type(&interp->type_table, defn->literal_name, strlen(defn->literal_name)); // TODO: store literal_name sized
    }

    if (defn->type_name) {
        const Ast_Declaration *decl = find_declaration_or_null(defn->type_name);
        if (!decl) {
            fprintf(stderr, "error: Undeclared identifier '%s'\n", defn->type_name->name);
            exit(1);
        }
        Object *object = hmgetp_null(interp->objects, decl);
        assert(object);
        if (object->inferred_type == NULL) return NULL; // We must wait on this.
        if (object->inferred_type != interp->type_table.TYPE) {
            fprintf(stderr, "error: '%s' is not a type\n", ast_to_string(Down(decl->expression)));
            exit(1);
        }
        return object->type_value;
    }

    if (defn->array_element_type) {
        UNIMPLEMENTED;
    }

    if (defn->pointer_to) {
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
        
        if (block->statements[it]->type == AST_BLOCK) interp_add_scope(interp, Down(block->statements[it]));

        // We only care about declarations.
        
        if (block->statements[it]->type != AST_DECLARATION) continue;

        const Ast_Declaration *decl = Down(block->statements[it]);

        // Add inner scopes.

        if (decl->expression->type == AST_TYPE_DEFINITION) {
            const Ast_Type_Definition *defn = Down(decl->expression);
            if (defn->struct_desc) interp_add_scope(interp, &defn->struct_desc->scope);
            if (defn->enum_defn)   interp_add_scope(interp, &defn->enum_defn->scope);
        }
        else if (decl->expression->type == AST_LAMBDA) {
            const Ast_Lambda *lambda = Down(decl->expression);
            interp_add_scope(interp, &lambda->body.block);
        }
        
        Object object = init_object(decl);
        ptrdiff_t i = hmlen(interp->objects);
        hmputs(interp->objects, object);
        arrput(interp->queue, &interp->objects[i]);
    }
}

void interp_run_main_loop(Interp *interp)
{
    while (arrlenu(interp->queue)) {
        size_t i = 0;
        while (i < arrlenu(interp->queue)) {
            infer_object(interp, interp->queue[i]);

            if (interp->queue[i]->inferred_type) {
                arrdelswap(interp->queue, i);
            } else {
                i += 1;
            }
        }
    }
}

Interp init_interp(void)
{
    Interp interp;
    interp.objects = NULL;
    interp.type_table = type_table_init();
    return interp;
}

Object init_object(const Ast_Declaration *declaration)
{
    Object object;
    object.key = declaration;
    object.inferred_type = NULL;
    object.type_value = NULL;
    return object;
}
