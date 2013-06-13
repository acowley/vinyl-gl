#version 150
in vec3 vertexPos;
in vec3 vertexNormal;
in vec3 vertexColor;
uniform mat4 cam;
uniform mat4 proj;
out vec3 pointNormal;
out vec3 diffuse;
out vec4 eye;

void main() {
  gl_Position = proj * vec4(vertexPos,1);
  eye = -(cam * vec4(vertexPos,1));
  pointNormal = vertexNormal;
  diffuse = vertexColor;
}
