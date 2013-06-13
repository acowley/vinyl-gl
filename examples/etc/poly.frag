#version 150
in vec3 pointNormal;
in vec4 eye;           // Direction to the viewer
in vec3 diffuse;       // Material color
uniform vec3 lightDir; // Direction to the light
out vec4 fragColor;

const vec4 specular = vec4(1); // Light specular color
const float shininess = 150;   // Material property

void main() {
  vec3 e = normalize(eye.xyz);
  float intensity = dot(pointNormal, lightDir);
  vec4 spec = vec4(0.0);
  if(intensity > 0.0) {
    float specR = max(0.0, dot(reflect(-lightDir, pointNormal), e));
    spec = specular * pow(specR, shininess);
  }
  fragColor = vec4(max(0.65,intensity)*diffuse,1)+spec;
}
