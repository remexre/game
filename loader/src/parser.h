#ifndef GAME_PARSER_H
#define GAME_PARSER_H 1

#include "lisp.h"

error parse_one(string, package, value*);
error parse_all(string, package, value*);

#endif
