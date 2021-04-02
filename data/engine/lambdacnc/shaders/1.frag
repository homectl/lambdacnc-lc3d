#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform sampler2D s0;
smooth in vec2 vo1;
out vec4 f0;
void main() {
    f0 = texture2D (s0,vo1);
}