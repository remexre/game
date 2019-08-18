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
package context_def_package(context ctx, string name);

symbol context_intern_symbol(context ctx, string name);
symbol package_intern_symbol(package pkg, string name);
error_return package_get_symbol(package pkg, string name, symbol* out);

string package_name(package);

value context_bool(context ctx, bool);
symbol context_lang(context ctx, const char* name);

#endif
