#version 330 core
#extension GL_ARB_explicit_uniform_location : require

layout(location = 0) in vec3 pos;

layout(location = 3) uniform mat4 model;
layout(location = 2) uniform mat4 view;
layout(location = 1) uniform mat4 proj;

layout(location = 0) uniform float t;

void main(void) {
	mat4 modifier = mat4(
		vec4(cos(t), sin(t), 0, 0),
		vec4(-sin(t), cos(t), 0, 0),
		vec4(0, 0, 1, 0),
		vec4(0, 0, 0, 1));
	gl_Position = proj * view * model * modifier * vec4(pos, 1);
}
