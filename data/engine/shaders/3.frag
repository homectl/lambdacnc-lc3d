#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
smooth in vec4 vo1;
smooth in vec4 vo2;
smooth in vec3 vo3;
smooth in vec4 vo4;
out vec4 f0;
void main() {
    f0 = vec4 (0.0,0.0,0.0,0.5);
}