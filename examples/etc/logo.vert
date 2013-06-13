#version 150
in vec2 vertexCoord;
out vec2 texCoord;

void main() {
  texCoord = vec2(vertexCoord.x > 0.0 ? 1.0 : 0.0,
                  vertexCoord.y > 0.0 ? 0.0 : 1.0);
  gl_Position = vec4(vertexCoord*2.0-1.0,1,1);
}
