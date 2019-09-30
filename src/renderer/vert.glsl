#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 pos;
layout(location = 2) in vec2 texcoords;
layout(location = 5) in vec3 normal;

layout(location = 0) uniform mat4 proj;
layout(location = 1) uniform mat4 view;
layout(location = 2) uniform mat4 model;

out vec2 vsTexcoords;
out vec3 vsNormal;

void main(void) {
	gl_Position = proj * view * model * vec4(pos, 1);
	vsTexcoords = texcoords;
	vsNormal = normal;
}
