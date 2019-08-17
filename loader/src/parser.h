#ifndef GAME_PARSER_H
#define GAME_PARSER_H 1

#include "lisp/context.h"
#include "lisp/value.h"

error parse_one(string, context, value*);
error parse_all(string, context, value*);

#endif
