#ifndef GAME_LISP_CONTEXT_H
#define GAME_LISP_CONTEXT_H 1

#include "../util.h"

#define PKGTAB_BUCKETS 256
#define SYMTAB_BUCKETS 256

typedef struct context_data* context;
typedef struct package_data* package;
typedef struct symbol_data* symbol;
typedef struct value* value;

context make_context(void);

package context_current_package(context ctx);
void context_set_current_package(context ctx, string name);
package context_def_package(context ctx, string name);

symbol context_gensym(context ctx);
symbol context_intern_symbol(context ctx, string name);
symbol package_intern_symbol(package pkg, string name);
void context_import_symbol(context ctx, symbol sym);
void package_import_symbol(package pkg, symbol sym);
symbol context_intern_static(context ctx, const char* pkg, const char* name);

// Violates the invariant that symbol is a non-null type.
symbol unsafe_package_get_symbol(package pkg, string name);

string package_name(package);
void package_get_exports(package, void (*cb)(symbol, void*), void*);

value context_bool(context ctx, bool);
symbol context_lang(context ctx, const char* name);

bool is_keyword(context, symbol);
bool is_lambda_list_keyword(context, symbol);

#endif
