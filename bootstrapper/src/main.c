#include "buffer.h"
#include "io.h"
#include "lisp/env.h"
#include "lisp/eval.h"
#include "../tmp/lang.lisp.h"
#include <linenoise.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <unistd.h>
#include "common.h"

static noreturn void usage(int argc, char **argv);

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
	env env = make_env(ctx);

	string lang_lisp_src = (string) { .len = lang_lisp_len, .data = (char*) lang_lisp };
	expect_ok(eval_string(lang_lisp_src, NULL, env), "Error evaluating lang file");

	buffer cli_src = make_buffer(64);
	for(size_t i = optind; i < argc; i++) {
		buffer_append_cstr(&cli_src, argv[i]);
		buffer_append_char(&cli_src, ' ');
	}
	expect_ok(eval_string(buffer_to_string(cli_src), NULL, env), "Error evaluating command line");

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
			error err = eval_string(line, &result, env);
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
