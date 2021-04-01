#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform int index;
smooth in vec3 vo1;
out vec4 f0;
void main() {
    f0 = (index) < (1) ? vec4 (0.7,0.7,0.2,1.0) : vec4 (1.0,0.5,1.0,1.0);
}