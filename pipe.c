#include <string.h>

#include "context_alloc.h"
#include "pipe.h"
#include "vendor/stb_ds.h"

typedef const Ast_Declaration *Decl;
typedef const Ast_Type_Definition *Defn;
typedef const Ast_Block *Scope;

#define PIPE_DO_TRACING

static Type create_type(Pipe *pipe, Defn defn)
{
    if (defn->struct_desc) {
        Scope scope = &defn->struct_desc->scope;

        ptrdiff_t *indices = NULL;

        For (scope->statements) {
            if (scope->statements[it]->type != AST_DECLARATION) continue;

            Decl decl = Down(scope->statements[it]);            

            if (decl->flags & DECLARATION_IS_COMPTIME) continue; // Skip comptime.
            
            ptrdiff_t index = hmgeti(pipe->objects, decl);
            assert(index >= 0);

            if (!pipe->objects[index].value.inferred_type) {
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
                Object_Entry *entry  = &pipe->objects[indices[it]];
                structure->types[it] = entry->value.inferred_type;
                structure->names[it] = entry->key->ident.name;
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

static void pipe_entry(Pipe *pipe, Object_Entry *entry)
{
    Decl decl = entry->key;
    Object *object = &entry->value;

#if defined(PIPE_DO_TRACING)
    printf(":  %s\n", ast_to_string(Base(decl)));
#endif

    if (object->inferred_type == NULL) {
        const Ast *ast = decl->expression;
        object->inferred_type = infer_ast_type(ast);

        // Replace comptime types for non-comptime declarations.

        if (object->inferred_type && (decl->flags & DECLARATION_IS_COMPTIME) == 0) {
            if (object->inferred_type == &type_info_comptime_int)         object->inferred_type = &type_info_int;
            else if (object->inferred_type == &type_info_comptime_float)  object->inferred_type = &type_info_float;
            else if (object->inferred_type == &type_info_comptime_string) object->inferred_type = &type_info_string;
        }

        // Type definitions must add a type into the type table.

        if (ast->type == AST_TYPE_DEFINITION) {
            Defn defn = Down(ast);
            Type created_type = create_type(pipe, defn);

            if (created_type) {
                hmput(pipe->types, defn, created_type);
                object->index_within_type_table = hmgeti(pipe->types, defn);

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

Pipe init_pipe(void)
{
    Pipe pipe;
    pipe.objects = NULL;
    pipe.types = NULL;
    hmdefault(pipe.types, NULL); // Default to NULL Type.
    return pipe;
}

Object init_object(void)
{
    Object object;
    object.inferred_type = NULL;
    object.index_within_type_table = -1;
    object.ir_index = -1;
    return object;
}

void pipe_add_scope(Pipe *pipe, Scope scope)
{
    For (scope->statements) {
        // Adding objects in a depth-first order.
        
        if (scope->statements[it]->type == AST_BLOCK) pipe_add_scope(pipe, Down(scope->statements[it]));

        // We only care about declarations.
        
        if (scope->statements[it]->type != AST_DECLARATION) continue;

        Decl decl = Down(scope->statements[it]);

        // Add inner scopes.

        if (decl->expression->type == AST_TYPE_DEFINITION) {
            Defn defn = Down(decl->expression);
            if (defn->struct_desc) pipe_add_scope(pipe, &defn->struct_desc->scope);
            if (defn->enum_defn)   pipe_add_scope(pipe, &defn->enum_defn->scope);
        }
        else if (decl->expression->type == AST_LAMBDA) {
            const Ast_Lambda *lambda = Down(decl->expression);
            pipe_add_scope(pipe, &lambda->body.block);
        }
        
        hmput(pipe->objects, decl, init_object());
    }
}

void pipe_forward(Pipe *pipe)
{
    for (ptrdiff_t i = 0; i < hmlen(pipe->objects); ++i) {
        pipe_entry(pipe, &pipe->objects[i]);
    }
}
