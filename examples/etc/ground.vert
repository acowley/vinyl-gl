#version 150
in vec3 vertexPos;
uniform mat4 proj;
out vec2 texCoord;

void main() {
  texCoord = vertexPos.xz * 0.25f;
  gl_Position = proj * vec4(vertexPos, 1.0);
}
