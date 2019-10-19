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

struct DrawContext {
	VBO* triangle;

	float specularity;
};

static void on_draw(NovaDraw* draw, struct DrawContext* ctx) {
	check_error("draw vbo", nova_draw_vbo(draw, ctx->triangle, NULL, NULL, NULL,
		ctx->specularity));
}

int main(void) {
	Nova* nova = NULL;
	check_error("initialize nova",
		nova_init("Vulkan Example", "assets/shaders/tutorial.vert.spv",
			"assets/shaders/tutorial.frag.spv", &nova));

	const struct Vertex vertices[3] = {
		{{  0.0, -0.5, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0, 0.0 }},
		{{  0.5,  0.5, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0, 0.0 }},
		{{ -0.5,  0.5, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0, 0.0 }},
	};

	struct VBO* triangle;
	check_error("alloc vbo", nova_alloc_vbo(nova, vertices, 3, &triangle));

	struct DrawContext ctx = {
		.triangle = triangle,
		.specularity = 1.0
	};
	check_error("set up callbacks", nova_on_draw(nova, (NovaOnDrawFunc) on_draw, &ctx));

	int should_close = 0;
	while(!should_close)
		check_error("draw", nova_loop(nova, &should_close));

	check_error("free vbo", nova_free_vbo(nova, ctx.triangle));

	check_error("free nova", nova_free(nova));
	return 0;
}
