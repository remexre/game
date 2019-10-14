#version 450 core
#pragma shader_stage(fragment)

layout(location = 0) in vec2 msTexcoords;
layout(location = 1) in vec3 wsNormal;

layout(binding = 0) uniform UBO {
	mat4 proj;
	mat4 view;
	mat4 model;
	vec3 diffuse;
	vec3 ambient;
	vec3 wsLightPos;
} ubo;

layout(location = 0) out vec3 color;

void main(void) {
	vec3 wsPos = (inverse(ubo.view) * inverse(ubo.proj) * gl_FragCoord).xyz;
	vec3 wsLightDir = normalize(ubo.wsLightPos - wsPos);
	float diffuseFactor = clamp(dot(wsLightDir, wsNormal), 0, 1);
	color = (ubo.ambient) + (ubo.diffuse * diffuseFactor);
}
