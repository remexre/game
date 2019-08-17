#include "context.h"
#include "preds.h"
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
	symbol t = package_intern_symbol(pkg, string_from_static_cstr("t"));
	t->global = symbol_to_value(t);

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
	symbol symbol = GC_malloc(sizeof(*symbol));
	symbol->name = name;
	symbol->name_hash = djb2a(symbol->name);
	symbol->fq_name = string_cat(pkg->name,
		string_cat(string_from_static_cstr("::"), symbol->name));
	symbol->fq_hash = djb2a(symbol->fq_name);
	symbol->class = NIL;
	symbol->function = NIL;
	symbol->global = NIL;
	symbol->macro = NIL;
	symbol->package = pkg;
	return symbol;
}

string package_name(package pkg) {
	return pkg->name;
}

symbol context_quote(context ctx) {
	package pkg = context_def_package(ctx, string_from_static_cstr("lang"));
	return package_intern_symbol(pkg, string_from_static_cstr("quote"));
}

value context_t(context ctx) {
	package pkg = context_def_package(ctx, string_from_static_cstr("lang"));
	return symbol_to_value(package_intern_symbol(pkg, string_from_static_cstr("t")));
}
