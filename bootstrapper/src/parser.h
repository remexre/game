#ifndef GAME_PARSER_H
#define GAME_PARSER_H 1

#include "lisp/context.h"
#include "lisp/value.h"

error_return parse_one(string*, context, value*);
error_return eat_ignored(string*);
error_return symbolish_to_value(string, context, value*);

#endif
