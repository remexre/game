#include "io.h"
#include "lisp.h"
#include <stdlib.h>
#include <stdnoreturn.h>
#include <unistd.h>
#include "common.h"

noreturn void usage(int argc, char **argv) {
	fprintf(stderr, "Usage: %s [flags] main-file-path\n",
		argc ? argv[0] : "game");
	exit(1);
}

int main(int argc, char **argv) {
	int c;
	while((c = getopt(argc, argv, "h")) != -1) {
		switch(c) {
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

	string_fputs(main_file_src, stdout);

	return 0;
}
