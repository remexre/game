#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 pos;
layout(location = 1) in vec2 texcoords;
layout(location = 2) in vec3 normal;

layout(location = 0) uniform mat4 proj;
layout(location = 1) uniform mat4 view;
layout(location = 2) uniform mat4 model;

out vec2 msTexcoords;
out vec3 wsNormal;
out vec3 wsPos;

void main(void) {
	vec4 pos = model * vec4(pos, 1);
	wsPos = pos.xyz;
	gl_Position = proj * view * pos;

	msTexcoords = texcoords;
	wsNormal = (model * vec4(normal, 0)).xyz;
}
