#include "io.h"
#include "lisp.h"
#include <stdnoreturn.h>
#include <unistd.h>
#include "common.h"

noreturn void usage(int argc, char **argv);

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
	str main_file_path = str_from_static_cstr(argv[optind]);

	str main_file_src;
	expect(read_file(main_file_path, &main_file_src) != 0,
		"Couldn't read main file.");

	for(int i = optind; i < argc; i++)
		printf("arg = %s\n", argv[i]);

	return 0;
}

noreturn void usage(int argc, char **argv) {
	fprintf(stderr, "Usage: %s [flags] main-file-path\n",
		argc ? argv[0] : "game");
	exit(1);
}
