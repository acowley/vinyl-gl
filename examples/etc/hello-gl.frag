#version 150

uniform float fade_factor;
uniform sampler2D textures[2];
in vec2 texcoord;
out vec4 fragColor;

void main() {
    fragColor = mix(texture(textures[0], texcoord),
                    texture(textures[1], texcoord),
                    fade_factor);
}
