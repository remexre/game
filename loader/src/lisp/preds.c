#include "preds.h"
#include "args.h"
#include "../common.h"

native_func(atom) {
	value val;
	try(parse_args(string_from_static_cstr("atom"), args, 1, 0, false, &val));

	if(null(val) || get_tag(val) != TAG_CONS)
		*out = context_t(ctx);
	else
		*out = NIL;

	return ok;
}
