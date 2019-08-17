#ifndef GAME_ENV_H
#define GAME_ENV_H 1

#include "context.h"

typedef struct env* env;

env make_env(context ctx);

#endif
