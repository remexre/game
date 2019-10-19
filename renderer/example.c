#include <stdio.h>
#include <stdlib.h>

typedef struct Renderer Renderer;
char* renderer_init(const char* app_name, const char* vert_path, const char* frag_path,
                    Renderer** out_renderer);
char* renderer_free(Renderer* renderer);
void renderer_free_error(char* error);
char* renderer_loop(Renderer* renderer, int* out_should_close);

static void check_error(const char* action, char* error) {
	if(!error)
		return;
	fprintf(stderr, "failed to %s: %s", action, error);
	renderer_free_error(error);
	exit(1);
}

int main(void) {
	Renderer* renderer = NULL;
	check_error("initialize renderer",
		renderer_init("Vulkan Example", "assets/shaders/tutorial.vert.spv",
			"assets/shaders/tutorial.frag.spv", &renderer));

	int should_close = 0;
	while(!should_close) {
		check_error("draw", renderer_loop(renderer, &should_close));
	}

	check_error("free renderer", renderer_free(renderer));
	return 0;
}
