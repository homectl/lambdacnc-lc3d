#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
smooth in vec3 vo1;
out vec4 f0;
vec4 white;
void main() {
    white = vec4 (1.0,1.0,1.0,1.0);
    f0 = white;
}