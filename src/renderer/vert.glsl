#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 pos;

layout(location = 0) uniform mat4 proj;
layout(location = 1) uniform mat4 view;
layout(location = 2) uniform mat4 model;

void main(void) {
	gl_Position = proj * view * model * vec4(pos, 1);
}
