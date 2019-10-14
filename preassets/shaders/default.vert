#version 450
#pragma shader_stage(vertex)

layout(location = 0) in vec3 pos;
layout(location = 1) in vec2 texcoords;
layout(location = 2) in vec3 normal;

layout(binding = 0) uniform UBO {
	mat4 proj;
	mat4 view;
	mat4 model;
	vec3 diffuse;
	vec3 ambient;
	vec3 wsLightPos;
} ubo;

layout(location = 0) out vec2 msTexcoords;
layout(location = 1) out vec3 wsNormal;

void main(void) {
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(pos, 1);

	msTexcoords = texcoords;
	wsNormal = (ubo.model * vec4(normal, 0)).xyz;
}
