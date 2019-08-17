#include "env.h"
#include <gc.h>
#include "../common.h"

struct env {
	context ctx;
};

env make_env(context ctx) {
	env env = GC_malloc(sizeof(struct env));
	env->ctx = ctx;
	return env;
}
