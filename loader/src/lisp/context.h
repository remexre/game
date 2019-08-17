#ifndef GAME_LISP_CONTEXT_H
#define GAME_LISP_CONTEXT_H 1

#include "symbol.h"
#include "../util.h"

#define PKGTAB_BUCKETS 256
#define SYMTAB_BUCKETS 256

typedef struct context_data* context;
typedef struct package_data* package;

context make_context(void);

package context_current_package(context ctx);
package context_def_package(context ctx, string name);

symbol context_intern_symbol(context ctx, string name);
symbol package_intern_symbol(package pkg, string name);

string package_name(package);

#endif
