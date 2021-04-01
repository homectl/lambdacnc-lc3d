#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
smooth in float vo1;
out vec4 f0;
void main() {
    f0 = vec4 (vo1,vo1,vo1,1.0);
}