#include "io.h"
#include "lisp/context.h"
#include "lisp/env.h"
#include "lisp/eval.h"
#include "lisp/value.h"
#include "parser.h"
#include <linenoise.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <unistd.h>
#include "common.h"

static void show_error(error);
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

	if(optind + 1 != argc)
		usage(argc, argv);
	string main_file_path = string_from_static_cstr(argv[optind]);

	string main_file_src;
	expect_ok(read_file(main_file_path, &main_file_src),
		"Couldn't read main file");

	context ctx = make_context();

	value main_src;
	expect_ok(parse_all(main_file_src, ctx, &main_src),
		"Couldn't parse main file");

	value result;
	env env = make_env(ctx);
	expect_ok(eval_body(main_src, &result, env),
		"Error in main file");

	if(result) {
		printf("Main file evaluated to: ");
		string_fputs(show_value(result, true), stdout);
	}

	if(repl) {
		linenoiseHistorySetMaxLen(1000);
		linenoiseSetMultiLine(1);
		while(1) {
			char* line = linenoise("> ");
			if(!line) break;
			linenoiseHistoryAdd(line);
			string src_str = string_from_cstr(line);
			linenoiseFree(line);

			value src;
			error err = parse_all(src_str, ctx, &src);
			if(err.code != OK) {
				show_error(err);
				continue;
			}

			err = eval_body(src, &result, env);
			if(err.code != OK) {
				show_error(err);
				continue;
			}

			if(result)
				string_fputs(show_value(result, true), stdout);
		}
	}

	return 0;
}

static void show_error(error err) {
	if(isatty(STDERR_FILENO))
		fputs("\x1b[1;31m", stderr);
	string_fputs(err.msg, stderr);
	if(isatty(STDERR_FILENO))
		fputs("\x1b[0m", stderr);
	fputs("\n", stderr);
}

static noreturn void usage(int argc, char **argv) {
	fprintf(stderr, "Usage: %s [flags] main-file-path\n",
		argc ? argv[0] : "game");
	exit(1);
}
