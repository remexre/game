#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static void usage(const char* name);

int main(int argc, char** argv) {
	int opt;
	while ((opt = getopt(argc, argv, "h")) != -1) {
		switch(opt) {
		case 'h':
			usage(argv[0]);
			exit(0);
			break;
		default:
			usage(argv[0]);
			exit(1);
			break;
		}
	}

	float buf[256];
	size_t i = 0;
	do {
		i = 0;
		while(i < 256 && (scanf("%f", &buf[i]) == 1))
			i++;
		fwrite(buf, 4, i, stdout);
	} while(i == 256);

	return 0;
}

static void usage(const char* name) {
	fprintf(stderr, "USAGE: %s [FLAGS] < GEFS-MODEL > MODEL\n\n", name);
	fprintf(stderr, "DESCRIPTION:\n");
	fprintf(stderr, "  Converts a GEFS model (whitespace separated floats as strings) to a flat\n");
	fprintf(stderr, "  float array (as raw bytes).\n\n");
	fprintf(stderr, "FLAGS:\n");
	fprintf(stderr, "  -h    Displays this help message.\n");
}
