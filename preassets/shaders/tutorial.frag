#version 450 core
#pragma shader_stage(fragment)

layout(location = 0) in vec3 fragColor;

layout(location = 0) out vec4 color;

void main(void) {
	color = vec4(fragColor, 1.0);
}
