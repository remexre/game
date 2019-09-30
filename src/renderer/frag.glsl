#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

in vec2 vsTexcoords;
in vec3 vsNormal;

layout(location = 3) uniform vec4 diffuse;

layout(location = 0) out vec3 color;

void main(void) {
	color = vec3(1, 0, 1);
}
