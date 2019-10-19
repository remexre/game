#include <stdio.h>
#include <stdlib.h>

typedef struct Renderer Renderer;
char* renderer_init(const char* app_name, const char* vert_path, const char* frag_path,
                    Renderer** out_renderer);
char* renderer_free(Renderer* renderer);

void renderer_free_error(char* error);

static void check_error(const char* action, char* error);

int main(void) {
	Renderer* renderer = NULL;
	check_error("initialize renderer",
		renderer_init("Vulkan Example", "./vert.spv", "./frag.spv", &renderer));

	check_error("free renderer", renderer_free(renderer));
	return 0;
}

void check_error(const char* action, char* error) {
	if(!error)
		return;
	fprintf(stderr, "failed to %s: %s", action, error);
	renderer_free_error(error);
	exit(1);
}
