#version 330 core

layout(location = 0) in vec3 worldPos;

void main(void) {
	gl_Position = vec4(worldPos, 1);
}
