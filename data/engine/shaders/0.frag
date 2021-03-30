#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
smooth in vec4 vo1;
smooth in vec3 vo2;
out vec4 f0;
vec3 diffuseLight_4_VecSFloat3(vec4 z0,vec3 z1,vec4 z2,vec3 z3) {
    return (z3) * (max (dot (normalize (z1),(normalize ((z2) - (z0))).xyz),0.0));
}
vec4 lightPos_Float;
void main() {
    lightPos_Float = vec4 (10000.0,10000.0,14000.0,1.0);
    f0 = vec4 (((diffuseLight_4_VecSFloat3 (vo1
                                           ,vo2
                                           ,lightPos_Float
                                           ,vec3 (2.0,2.0,2.0))) * (vec3 (0.8,0.8,0.8))).x
              ,((diffuseLight_4_VecSFloat3 (vo1
                                           ,vo2
                                           ,lightPos_Float
                                           ,vec3 (2.0,2.0,2.0))) * (vec3 (0.8,0.8,0.8))).y
              ,((diffuseLight_4_VecSFloat3 (vo1
                                           ,vo2
                                           ,lightPos_Float
                                           ,vec3 (2.0,2.0,2.0))) * (vec3 (0.8,0.8,0.8))).z
              ,1.0);
}