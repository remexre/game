#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

in vec2 msTexcoords;
in vec3 wsNormal;
in vec3 wsPos;

layout(location = 1) uniform mat4 view;
layout(location = 3) uniform vec3 diffuse;
layout(location = 4) uniform vec3 ambient;
layout(location = 5) uniform vec3 wsLightPos;

layout(location = 0) out vec3 color;

void main(void) {
	vec3 wsLightDir = normalize(wsLightPos - wsPos);
	vec3 adjDiffuse = clamp(dot(wsLightDir, wsNormal), 0, 1) * diffuse;
	color = adjDiffuse + ambient;
}
