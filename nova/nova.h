#ifndef NOVA_H
#define NOVA_H 1

#define NOVA__WARN_UNUSED_RESULT __attribute__((warn_unused_result))

void nova_free_error(char* error);

typedef struct Nova Nova;
typedef struct NovaDraw NovaDraw;
typedef void (*NovaOnDrawFunc)(NovaDraw*, void*);
char* nova_init(const char* app_name, const char* vert_path, const char* frag_path,
                Nova** out_nova) NOVA__WARN_UNUSED_RESULT;
char* nova_free(Nova* nova) NOVA__WARN_UNUSED_RESULT;
char* nova_set_title(Nova* nova, const char* title) NOVA__WARN_UNUSED_RESULT;
char* nova_on_draw(Nova* nova, NovaOnDrawFunc func, void*) NOVA__WARN_UNUSED_RESULT;
char* nova_loop(Nova* nova, int* out_should_close) NOVA__WARN_UNUSED_RESULT;

typedef struct VBO VBO;
typedef struct Vertex {
	float position[3];
	float texcoords[2];
	float normal[3];
} Vertex;
char* nova_alloc_vbo(Nova* nova, const Vertex* vertices,
	             unsigned int num_vertices, VBO** out_vbo) NOVA__WARN_UNUSED_RESULT;
char* nova_free_vbo(Nova* nova, VBO* vbo) NOVA__WARN_UNUSED_RESULT;

char* nova_draw_vbo(NovaDraw* draw, VBO* vbo, float model[16], float view[16], float proj[16],
	            float specularity) NOVA__WARN_UNUSED_RESULT;

#endif // NOVA_H
