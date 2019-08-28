#version 330 core
#extension GL_ARB_explicit_uniform_location : require

layout(location = 0) in vec3 pos;

layout(location = 2) uniform mat4 model;
layout(location = 1) uniform mat4 view;
layout(location = 0) uniform mat4 proj;

void main(void) {
	// gl_Position = model * view * proj * vec4(pos, 0);
	gl_Position = vec4(pos, 1);
}
