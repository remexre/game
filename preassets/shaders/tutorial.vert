#version 450
#pragma shader_stage(vertex)

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 texcoords;
layout(location = 2) in vec3 normal;

layout(location = 0) out vec3 fragColor;

vec3 colors[3] = vec3[](
	vec3(1.0, 0.0, 0.0),
	vec3(0.0, 1.0, 0.0),
	vec3(0.0, 0.0, 1.0)
);

void main() {
	fragColor = colors[gl_VertexIndex % 3];
	gl_Position = vec4(fragColor, 1.0);
}
