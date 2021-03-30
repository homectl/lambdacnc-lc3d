#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
smooth in vec4 vo1;
smooth in vec4 vo2;
out vec4 f0;
vec4 black;
void main() {
    black = vec4 (0.0,0.0,0.0,1.0);
    f0 = black;
}