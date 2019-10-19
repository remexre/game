#include <stdio.h>
#include <stdlib.h>

#define WARN_UNUSED_RESULT __attribute__((warn_unused_result))

typedef struct Nova Nova;
typedef struct NovaDraw NovaDraw;
// TODO: VBO's members shouldn't be exposed.
typedef struct VBO { unsigned int n; } VBO;
typedef void (*NovaOnDrawFunc)(NovaDraw*, void*);
char* nova_init(const char* app_name, const char* vert_path, const char* frag_path,
                    Nova** out_nova) WARN_UNUSED_RESULT;
char* nova_free(Nova* nova) WARN_UNUSED_RESULT;
void nova_free_error(char* error);
char* nova_on_draw(Nova* nova, NovaOnDrawFunc func, void*) WARN_UNUSED_RESULT;
char* nova_loop(Nova* nova, int* out_should_close) WARN_UNUSED_RESULT;

char* nova_draw_vbo(NovaDraw* draw, VBO* vbo, float model[16], float view[16], float proj[16],
	float specularity) WARN_UNUSED_RESULT;

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

	struct VBO triangle = { 3 };
	struct DrawContext ctx = {
		.triangle = &triangle,
		.specularity = 1.0
	};
	check_error("set up callbacks", nova_on_draw(nova, (NovaOnDrawFunc) on_draw, &ctx));

	int should_close = 0;
	while(!should_close)
		check_error("draw", nova_loop(nova, &should_close));

	check_error("free nova", nova_free(nova));
	return 0;
}
