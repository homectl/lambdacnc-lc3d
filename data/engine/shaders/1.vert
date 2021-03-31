#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
in vec2 vi1;
in vec2 vi2;
smooth out vec2 vo1;
void main() {
    gl_Position = vec4 ((vi1).x,(vi1).y,-1.0,1.0);
    vo1 = vi2;
}