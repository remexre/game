#version 330 core
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 pos;
layout(location = 1) in vec2 texcoords;
layout(location = 2) in vec3 normal;

layout(location = 0) uniform mat4 proj;
layout(location = 1) uniform mat4 view;
layout(location = 2) uniform mat4 model;
layout(location = 5) uniform vec3 lightPos;

out vec3 lightDirection;
out vec2 vsTexcoords;
out vec3 vsNormal;

void main(void) {
	gl_Position = proj * view * model * vec4(pos, 1);

	lightDirection = (view * vec4(normalize(pos - lightPos), 0.0)).xyz;

	vsTexcoords = texcoords;
	vsNormal = normal;
}
