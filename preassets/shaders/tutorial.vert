#version 450
#pragma shader_stage(vertex)

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texcoords;
layout(location = 2) in vec3 normal;

layout(location = 0) out vec3 fragColor;

layout(binding = 0) uniform UBO {
	mat4 model;
	mat4 view;
	mat4 proj;
	vec3 ambient;
	vec3 diffuse;
	float specular;
} ubo;

void main() {
	gl_Position = vec4(position, 1.0);
	fragColor = normal;
}
