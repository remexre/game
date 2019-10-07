#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

in vec3 lightDirection;
in vec2 vsTexcoords;
in vec3 vsNormal;

layout(location = 3) uniform vec3 diffuse;
layout(location = 4) uniform vec3 ambient;

layout(location = 0) out vec3 color;

void main(void) {
	vec3 adjDiffuse = diffuse * max(dot(lightDirection, vsNormal), 0.0);
	color = adjDiffuse + ambient;
}
