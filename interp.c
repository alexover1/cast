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
        object->inferred_type = interp_create_type(interp, inst->type_definition);
        return;
    }
        
    // Type definitions must add a type into the type table.

    if (ast->type == AST_TYPE_DEFINITION) {
        const Ast_Type_Definition *defn = Down(ast);

        Type created_type = interp_create_type(interp, defn);

        if (created_type) {
            object->index_within_type_table = hmlen(interp->type_table);
            hmput(interp->type_table, defn, created_type);

            // We only want to set the inferred type if we successfully created our type.
            object->inferred_type = &type_info_type;

            #if defined(INTERP_DO_TRACING)
                printf("[defn] %s :: %s (%ld)\n", decl->ident.name, type_to_string(created_type), object->index_within_type_table);
            #endif
        } 

        return;
    }

    object->inferred_type = typecheck_ast(interp, ast);

    // Replace comptime types for non-comptime declarations.

    if (object->inferred_type && (decl->flags & DECLARATION_IS_COMPTIME) == 0) {
        if (object->inferred_type == &type_info_comptime_int)         object->inferred_type = &type_info_int;
        else if (object->inferred_type == &type_info_comptime_float)  object->inferred_type = &type_info_float;
        else if (object->inferred_type == &type_info_comptime_string) object->inferred_type = &type_info_string;
    }
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

    if (defn->type_name) {
        const Ast_Declaration *decl = find_declaration_or_null(defn->type_name);
        if (!decl) {
            fprintf(stderr, "error: Undeclared identifier '%s'\n", defn->type_name->name);
            exit(1);
        }
        Object *object = hmgetp_null(interp->object_table, decl);
        assert(object);
        if (!object->inferred_type) return NULL; // We must wait on this.
        if (object->index_within_type_table < 0) {
            fprintf(stderr, "error: '%s' is not a type\n", ast_to_string(Down(decl->expression)));
            exit(1);
        }
        return interp->type_table[object->index_within_type_table].value;
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
    size_t iterations = 0;
    bool halting = false;

    while (!halting) {
        halting = true;
        iterations += 1;

        for (ptrdiff_t i = 0; i < hmlen(interp->object_table); ++i) {
            infer_object(interp, &interp->object_table[i]);

            if (interp->object_table[i].inferred_type == NULL) {
                halting = false;
            }
        }
    }

    printf("[interp] Ran %zu iterations\n", iterations);
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

