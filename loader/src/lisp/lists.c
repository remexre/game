#include "lists.h"
#include "args.h"
#include "../common.h"

native_func(nreverse_list) {
	value val;
	try(parse_args(string_from_static_cstr("nreverse"), args, 1, 0, false, &val));

	value prev = NIL;
	while(!null(val)) {
		struct cons* cons;
		try(as_cons_ref(val, &cons));
		value next = cons->tl;
		cons->tl = prev;
		prev = val;
		val = next;
	}
	*out = prev;
	return ok;
}
