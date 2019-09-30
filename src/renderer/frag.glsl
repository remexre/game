#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec3 color;

layout(location = 3) uniform vec4 diffuse;

void main(void) {
	color = vec3(1, 0, 1);
}
