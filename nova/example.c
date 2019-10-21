#include <stdio.h>
#include <stdlib.h>

#include "nova.h"

static void check_error(const char* action, char* error) {
	if(!error)
		return;
	fprintf(stderr, "failed to %s: %s\n", action, error);
	nova_free_error(error);
	exit(1);
}

static void on_event(const char* event, void* ignored) {
	(void) ignored;
	puts(event);
}

int main(void) {
	NovaRenderer* renderer = NULL;
	check_error("initialize renderer",
		nova_init("Vulkan Example", 0, "assets/shaders/tutorial.vert.spv",
			"assets/shaders/tutorial.frag.spv", &renderer));

	const NovaVertex vertices[3] = {
		{{  0.0, -0.5, 0.0 }, { 0.0, 0.0 }, { 1.0, 0.0, 0.0 }},
		{{  0.5,  0.5, 0.0 }, { 0.0, 0.0 }, { 0.0, 1.0, 0.0 }},
		{{ -0.5,  0.5, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0, 1.0 }},
	};

	const NovaUniforms uniforms = {
		.model = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1},
		.view = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1},
		.proj = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1},

		.ambient = {0.2, 0.0, 0.0},
		.diffuse = {0.0, 0.0, 0.0},
		.specular = 1.0,
	};

	NovaVBO* vbo;
	check_error("create vbo", nova_new_vbo(renderer, vertices, 3, &vbo));

	int should_close = 0;
	while(!should_close) {
		check_error("flip", nova_flip(renderer, on_event, NULL, &should_close));
		check_error("draw", nova_draw_vbo(renderer, vbo, &uniforms));
	}

	check_error("free vbo", nova_free_vbo(renderer, vbo));
	check_error("free renderer", nova_free(renderer));
	return 0;
}
