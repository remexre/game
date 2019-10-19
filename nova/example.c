#include <stdio.h>
#include <stdlib.h>

typedef struct Nova Nova;
char* nova_init(const char* app_name, const char* vert_path, const char* frag_path,
                    Nova** out_nova);
char* nova_free(Nova* nova);
void nova_free_error(char* error);
char* nova_loop(Nova* nova, int* out_should_close);

static void check_error(const char* action, char* error) {
	if(!error)
		return;
	fprintf(stderr, "failed to %s: %s", action, error);
	nova_free_error(error);
	exit(1);
}

int main(void) {
	Nova* nova = NULL;
	check_error("initialize nova",
		nova_init("Vulkan Example", "assets/shaders/tutorial.vert.spv",
			"assets/shaders/tutorial.frag.spv", &nova));

	int should_close = 0;
	while(!should_close) {
		check_error("draw", nova_loop(nova, &should_close));
	}

	check_error("free nova", nova_free(nova));
	return 0;
}
