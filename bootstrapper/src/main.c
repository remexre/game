#include "buffer.h"
#include "io.h"
#include "lisp/args.h"
#include "lisp/env.h"
#include "lisp/eval.h"
#include "../tmp/lang.lisp.h"
#include <linenoise.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <unistd.h>
#include "common.h"

static noreturn void usage(int argc, char **argv);
static native_func(linenoise);

int main(int argc, char **argv) {
	bool repl = false;

	int c;
	while((c = getopt(argc, argv, "hr")) != -1) {
		switch(c) {
		case 'r':
			repl = true;
			break;
		case '?':
		case 'h':
		default:
			usage(argc, argv);
		}
	}

	if(!repl && optind == argc)
		usage(argc, argv);

	context ctx = make_context();
	env e = make_env(ctx);

	symbol linenoise_sym = context_intern_static(ctx, "linenoise", "linenoise");
	linenoise_sym->flags |= HAS_FUNCTION;
	linenoise_sym->function = native_to_value(lisp_linenoise, linenoise_sym);

	string lang_lisp_src = (string) { .len = lang_lisp_len, .data = (char*) lang_lisp };
	expect_ok(eval_string(lang_lisp_src, NULL, e), "Error evaluating lang file");

	buffer cli_src = make_buffer(64);
	for(int i = optind; i < argc; i++) {
		buffer_append_cstr(&cli_src, argv[i]);
		buffer_append_char(&cli_src, ' ');
	}
	expect_ok(eval_string(buffer_to_string(cli_src), NULL, e), "Error evaluating command line");

	if(repl) {
		linenoiseHistorySetMaxLen(1000);
		linenoiseSetMultiLine(1);
		while(1) {
			string prompt = package_name(context_current_package(ctx));
			prompt = string_cat(prompt, string_from_static_cstr("> "));

			char* line_cstr = linenoise(cstr_from_string(prompt));
			if(!line_cstr) break;
			linenoiseHistoryAdd(line_cstr);
			string line = string_from_cstr(line_cstr);
			linenoiseFree(line_cstr);

			value result;
			error err = eval_string(line, &result, e);
			if(err.code != OK) {
				if(isatty(STDERR_FILENO))
					fputs("\x1b[1;31m", stderr);
				string_fputs(err.msg, stderr);
				if(isatty(STDERR_FILENO))
					fputs("\x1b[0m", stderr);
				fputs("\n", stderr);
				continue;
			}

			if(result)
				string_fputs(show_value(result, true), stdout);
		}
	}

	return 0;
}

static noreturn void usage(int argc, char **argv) {
	fprintf(stderr, "Usage: %s [flags] exprs...\n",
		argc ? argv[0] : "game");
	fprintf(stderr, "Flags:\n");
	fprintf(stderr, "  -h  Shows this help message.\n");
	fprintf(stderr, "  -r  Starts a REPL. If no exprs are provided, this must be set.\n");
	exit(1);
}

static native_func(linenoise) {
	UNUSED(ctx);

	value prompt_val;
	try(parse_args(string_from_static_cstr("linenoise"), args, 1, 0, NULL, &prompt_val));

	string prompt;
	try(as_string(prompt_val, &prompt));

	char* line_cstr = linenoise(cstr_from_string(prompt));
	if(!line_cstr) {
		*out = NIL;
		return ok;
	}
	string line = string_from_cstr(line_cstr);
	linenoiseFree(line_cstr);

	*out = string_to_value(line);
	return ok;
}
