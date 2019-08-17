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

error env_get(env env, symbol name, value* out) {
	struct env_mapping_link* bucket = env->lexical[name->fq_hash % LEXICAL_BUCKETS];
	while(bucket) {
		// TODO: Actually check
		bucket = bucket->next;
	}

	// TODO: Check globals
	return make_error(UNBOUND_VAR, name->fq_name);
}
