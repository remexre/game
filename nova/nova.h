#ifndef NOVA_H
#define NOVA_H 1

#ifndef NOVA__WARN_UNUSED_RESULT
#define NOVA__WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#endif // NOVA__WARN_UNUSED_RESULT

typedef struct NovaUniforms {
	float model[16];
	float view[16];
	float proj[16];
	float ambient[3];
	float diffuse[3];
	float specular;
} NovaUniforms;

typedef struct NovaVertex {
	float position[3];
	float texcoords[2];
	float normal[3];
} NovaVertex;

typedef void (*NovaOnEventFunc)(const char* event, void* cli);

typedef struct NovaRenderer NovaRenderer;
typedef struct NovaVBO NovaVBO;

void nova_free_error(char* error);

char* nova_init(const char* app_name, int debug, const char* vert_path, const char* frag_path,
                NovaRenderer** out_renderer) NOVA__WARN_UNUSED_RESULT;
char* nova_free(NovaRenderer* renderer) NOVA__WARN_UNUSED_RESULT;

char* nova_flip(NovaRenderer* renderer, NovaOnEventFunc on_event, void* on_event_clo,
                int* out_should_close) NOVA__WARN_UNUSED_RESULT;

char* nova_new_vbo(NovaRenderer* renderer, const NovaVertex* vertices, unsigned int vertex_count,
                   NovaVBO** out_vbo) NOVA__WARN_UNUSED_RESULT;
char* nova_free_vbo(NovaRenderer* renderer, NovaVBO* vbo) NOVA__WARN_UNUSED_RESULT;

char* nova_set_title(NovaRenderer* renderer, const char* title) NOVA__WARN_UNUSED_RESULT;

char* nova_draw_vbo(NovaRenderer* renderer, NovaVBO* vbo,
                    const NovaUniforms* uniforms) NOVA__WARN_UNUSED_RESULT;

#endif // NOVA_H
