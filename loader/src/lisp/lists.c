#include "lists.h"
#include "../common.h"

error_return nreverse_list(value val, value* out) {
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
