#ifndef GAME_ENV_H
#define GAME_ENV_H 1

#include "context.h"

#define LEXICAL_BUCKETS 64

typedef struct env* env;

struct env_mapping_link {
	symbol name;
	value val;
	struct env_mapping_link* next;
};

struct env {
	context ctx;
	struct env_mapping_link* lexical[LEXICAL_BUCKETS];
};

env make_env(context ctx);
void env_add(env, symbol, value);
env env_clone(env);
error_return env_get(env, symbol, value* out);

#endif
