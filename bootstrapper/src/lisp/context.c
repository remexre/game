#include "context.h"
#include "prims.h"
#include "value.h"
#include <gc.h>
#include "../common.h"

static package make_package(string);
static symbol make_symbol(package, string);

struct context_data {
	string current_package;
	struct pkgtab_link* pkgtab[PKGTAB_BUCKETS];
};

struct pkgtab_link {
	package pkg;
	struct pkgtab_link* next;
};

struct package_data {
	string name;
	struct symtab_link* symtab[SYMTAB_BUCKETS];
};

struct symtab_link {
	symbol sym;
	struct symtab_link* next;
};

context make_context(void) {
	// Initialize the context.
	context ctx = GC_malloc(sizeof(struct context_data));
	ctx->current_package = string_from_static_cstr("lang");
	upto(i, PKGTAB_BUCKETS)
		ctx->pkgtab[i] = NULL;

	// Initialize the LANG package.
	package pkg = context_current_package(ctx);

	symbol nil = package_intern_symbol(pkg, string_from_static_cstr("nil"));
	nil->flags |= HAS_GLOBAL;
	nil->global = NIL;

	symbol t = package_intern_symbol(pkg, string_from_static_cstr("t"));
	t->flags |= HAS_GLOBAL;
	t->global = symbol_to_value(t);

#define DEFUN(NAME, FUNC) do { \
	string defun__name = string_from_static_cstr(NAME); \
	symbol defun__sym = package_intern_symbol(pkg, defun__name); \
	defun__sym->flags |= HAS_FUNCTION; \
	defun__sym->function = native_to_value(lisp_##FUNC, defun__name); \
} while(0)

	DEFUN("atom", atom);
	DEFUN("exit", exit);
	DEFUN("funcall", funcall);
	DEFUN("print", print);
	DEFUN("set", set);
	DEFUN("set-class", set_class);
	DEFUN("set-function", set_function);
	DEFUN("set-macro", set_macro);

	return ctx;
}

package context_current_package(context ctx) {
	return context_def_package(ctx, ctx->current_package);
}

#define HASHMAP_GET_OR_INSERT(TABLE, LINK, LINK_FIELD, BUCKETS, MAKE) do { \
	hash h = djb2a(name); \
	struct LINK** entry = &TABLE[h % BUCKETS]; \
	struct LINK* iter = *entry; \
	while(iter) { \
		if(string_cmp(iter->LINK_FIELD->name, name) == 0) \
			return iter->LINK_FIELD; \
		iter = iter->next; \
	} \
	\
	struct LINK* link = GC_malloc(sizeof(struct LINK)); \
	link->LINK_FIELD = MAKE; \
	link->next = *entry; \
	*entry = link; \
	return link->LINK_FIELD; \
} while(0)

package context_def_package(context ctx, string name) {
	HASHMAP_GET_OR_INSERT(ctx->pkgtab, pkgtab_link, pkg, PKGTAB_BUCKETS, make_package(name));
}

static package make_package(string name) {
	package package = GC_malloc(sizeof(*package));
	package->name = name;
	upto(i, SYMTAB_BUCKETS)
		package->symtab[i] = NULL;
	return package;
}

symbol context_intern_symbol(context ctx, string name) {
	return package_intern_symbol(context_current_package(ctx), name);
}

symbol package_intern_symbol(package pkg, string name) {
	HASHMAP_GET_OR_INSERT(pkg->symtab, symtab_link, sym, SYMTAB_BUCKETS, make_symbol(pkg, name));
}

static symbol make_symbol(package pkg, string name) {
	expect(pkg, "make_symbol must be called with a non-null package");

	symbol symbol = GC_malloc(sizeof(*symbol));
	symbol->name = name;
	symbol->name_hash = djb2a(symbol->name);
	symbol->fq_name = string_cat(pkg->name,
		string_cat(string_from_static_cstr("::"), symbol->name));
	symbol->fq_hash = djb2a(symbol->fq_name);
	symbol->package = pkg;
	symbol->flags = 0;
	return symbol;
}

string package_name(package pkg) {
	return pkg->name;
}

value context_bool(context ctx, bool b) {
	if(b) {
		package pkg = context_def_package(ctx, string_from_static_cstr("lang"));
		return symbol_to_value(package_intern_symbol(pkg, string_from_static_cstr("t")));
	} else {
		return NIL;
	}
}

symbol context_lang(context ctx, const char* name) {
	package pkg = context_def_package(ctx, string_from_static_cstr("lang"));
	return package_intern_symbol(pkg, string_from_cstr(name));
}
