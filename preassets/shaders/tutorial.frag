#version 450 core
#pragma shader_stage(fragment)

layout(location = 0) in vec3 fragColor;

layout(location = 0) out vec4 color;

layout(binding = 0) uniform UBO {
	mat4 model;
	mat4 view;
	mat4 proj;
	vec3 ambient;
	vec3 diffuse;
	float specular;
} ubo;

void main(void) {
	color = vec4(fragColor, 1.0);
}
