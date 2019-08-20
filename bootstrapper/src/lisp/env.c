#include <stdio.h>
#include "env.h"
#include "value.h"
#include <gc.h>
#include "../common.h"

env make_env(context ctx) {
	env env = GC_malloc(sizeof(struct env));
	env->ctx = ctx;
	upto(i, LEXICAL_BUCKETS)
		env->lexical[i] = NULL;
	return env;
}

void env_add(env env, symbol name, value val) {
	struct env_mapping_link** bucket = &env->lexical[name->fq_hash % LEXICAL_BUCKETS];
	struct env_mapping_link* link = GC_malloc(sizeof(struct env_mapping_link));
	link->name = name;
	link->val = val;
	link->next = *bucket;
	*bucket = link;
}

env env_clone(env old) {
	env new = GC_malloc(sizeof(struct env));
	new->ctx = old->ctx;
	upto(i, LEXICAL_BUCKETS)
		new->lexical[i] = old->lexical[i];
	return new;
}

error_return env_get(env env, symbol name, value* out) {
	if(is_keyword(env->ctx, name)) {
		*out = symbol_to_value(name);
		return ok;
	}

	struct env_mapping_link* bucket = env->lexical[name->fq_hash % LEXICAL_BUCKETS];
	while(bucket) {
		if(string_cmp(name->fq_name, bucket->name->fq_name) == 0) {
			*out = bucket->val;
			return ok;
		}
		bucket = bucket->next;
	}

	if(name->flags & HAS_GLOBAL) {
		*out = name->global;
		return ok;
	}
	return make_error(UNBOUND_VAR, name->fq_name);
}
