#include <string.h>

#include "context_alloc.h"
#include "interp.h"
#include "object.h"
#include "vendor/stb_ds.h"

#define PIPE_DO_TRACING

static void interp_one_object(Interp *interp, Object *object)
{
    const Ast_Declaration *decl = object->key;

#if defined(PIPE_DO_TRACING)
    printf(":  %s\n", ast_to_string(Base(decl)));
#endif

    if (object->inferred_type == NULL) {
        const Ast *ast = decl->expression;
        object->inferred_type = typecheck_ast(ast);

        // Replace comptime types for non-comptime declarations.

        if (object->inferred_type && (decl->flags & DECLARATION_IS_COMPTIME) == 0) {
            if (object->inferred_type == &type_info_comptime_int)         object->inferred_type = &type_info_int;
            else if (object->inferred_type == &type_info_comptime_float)  object->inferred_type = &type_info_float;
            else if (object->inferred_type == &type_info_comptime_string) object->inferred_type = &type_info_string;
        }

        // Type definitions must add a type into the type table.

        if (ast->type == AST_TYPE_DEFINITION) {
            const Ast_Type_Definition *defn = Down(ast);
            Type created_type = interp_create_type(interp, defn);

            if (created_type) {
                hmput(interp->type_table, defn, created_type);
                object->index_within_type_table = hmgeti(interp->type_table, defn);

#if defined(PIPE_DO_TRACING)
                printf("@  %s (%ld)\n", type_to_string(created_type), object->index_within_type_table);
#endif
            }
        }
    }

    // We could use an else here, but I want an object to try and flow as far as it can.
    if (object->inferred_type == NULL) return;

#if defined(PIPE_DO_TRACING)
    printf(">  %s\n", type_to_string(object->inferred_type));
#endif

    // TODO: sizes
}

Type interp_create_type(Interp *interp, const Ast_Type_Definition *defn)
{
    if (defn->struct_desc) {
        const Ast_Block *block = &defn->struct_desc->scope;

        ptrdiff_t *indices = NULL;

        For (block->statements) {
            if (block->statements[it]->type != AST_DECLARATION) continue;

            const Ast_Declaration *decl = Down(block->statements[it]);            

            if (decl->flags & DECLARATION_IS_COMPTIME) continue; // Skip comptime.
            
            ptrdiff_t index = hmgeti(interp->object_table, decl);
            assert(index >= 0);

            if (interp->object_table[index].inferred_type == NULL) {
                // Child is not finished, so we can't make our type yet.
                return NULL;
            }

            arrput(indices, index);
        }

        // If only C had builtin SOA...

        Type_Info *info = context_alloc(sizeof(Type_Info));
        info->tag = TYPE_STRUCT;
        Type_Info_Struct *structure = &info->structure;

        if (arrlenu(indices) > 0) {
            size_t member_size = sizeof(Type) + sizeof(const char *) + sizeof(size_t);
            size_t n = arrlenu(indices);

            structure->types   = (Type *)        context_alloc(n*member_size);
            structure->names   = (const char **) structure->types + n*sizeof(Type);
            structure->offsets = (size_t *)      structure->names + n*sizeof(const char *);
            structure->field_count = n;

            For (indices) {
                Object *object = &interp->object_table[indices[it]];
                structure->types[it] = object->inferred_type;
                structure->names[it] = object->key->ident.name;
            }
        }

        return (Type)info;
    }

    if (defn->enum_defn) {
        UNIMPLEMENTED;
    }

    if (defn->literal_name) {
        return parse_literal_type(defn->literal_name, strlen(defn->literal_name)); // TODO: store literal_name sized
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

    return NULL;
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
        hmputs(interp->object_table, object);
    }
}

void interp_run_main_loop(Interp *interp)
{
    bool halting = false;

    while (!halting) {
        halting = true;

        for (ptrdiff_t i = 0; i < hmlen(interp->object_table); ++i) {
            interp_one_object(interp, &interp->object_table[i]);

            if (interp->object_table[i].inferred_type == NULL) {
                halting = false;
            }
        }
    }
}

Interp init_interp(void)
{
    Interp interp;
    interp.object_table = NULL;
    interp.type_table = NULL;
    hmdefault(interp.type_table, NULL); // Default to NULL Type.
    return interp;
}

Object init_object(const Ast_Declaration *declaration)
{
    Object object;
    object.key = declaration;
    object.inferred_type = NULL;
    object.index_within_type_table = -1;
    object.ir_index = -1;
    return object;
}

