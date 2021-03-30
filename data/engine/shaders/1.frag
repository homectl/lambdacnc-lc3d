#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform sampler2D s0;
uniform sampler2D s1;
uniform sampler2D s2;
smooth in vec4 vo1;
smooth in vec3 vo2;
smooth in vec4 vo3;
out vec4 f0;
vec3 diffuseLight_4_VecSFloat3(vec4 z0,vec4 z1,vec4 z2,vec3 z3) {
    return (z3) * (max (dot (z1,normalize ((z2) - (z0))),0.0));
}
vec4 lightPos_Float;
void main() {
    lightPos_Float = vec4 (10000.0,10000.0,14000.0,1.0);
    f0 = vec4 (((diffuseLight_4_VecSFloat3 (vo1
                                           ,vo3
                                           ,lightPos_Float
                                           ,vec3 (2.0,2.0,2.0))) * ((texture2D (s0,((vo2).xy) * (5.0e-5))).xyz)).x
              ,((diffuseLight_4_VecSFloat3 (vo1
                                           ,vo3
                                           ,lightPos_Float
                                           ,vec3 (2.0,2.0,2.0))) * ((texture2D (s1,((vo2).xy) * (5.0e-5))).xyz)).y
              ,((diffuseLight_4_VecSFloat3 (vo1
                                           ,vo3
                                           ,lightPos_Float
                                           ,vec3 (2.0,2.0,2.0))) * ((texture2D (s2,((vo2).xy) * (5.0e-5))).xyz)).z
              ,1.0);
}