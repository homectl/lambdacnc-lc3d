#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform sampler2D diffuseTexture;
uniform sampler2D s0;
uniform sampler2D s1;
uniform float time;
smooth in vec3 vo1;
smooth in vec4 vo2;
smooth in vec4 vo3;
smooth in vec4 vo4;
smooth in vec4 vo5;
out vec4 f0;
mat4 rotMatrixZ(float z0) {
    return mat4 (vec4 (cos (z0),sin (z0),0.0,0.0)
                ,vec4 ((0.0) - (sin (z0)),cos (z0),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec4 diffuseLight_4_VecScalar4Float(vec4 z0,vec4 z1,vec4 z2,vec4 z3) {
    return (z3) * (max (dot (z1,normalize ((z2) - (z0))),0.0));
}
vec4 getLightPos1(float z0) {
    return (rotMatrixZ ((z0) * (5.0))) * (vec4 (-60000.0,-60000.0,30000.0,1.0));
}
vec4 getLightPos2(float z0) {
    return (rotMatrixZ ((z0) * (5.0))) * (vec4 (60000.0,60000.0,30000.0,1.0));
}
vec2 texelSize;
void main() {
    texelSize = vec2 (6.25e-4,6.25e-4);
    f0 = (((((vec4 (0.0,0.0,0.0,0.0)) + ((diffuseLight_4_VecScalar4Float (vo3
                                                                         ,vo2
                                                                         ,getLightPos1 ((time) / (10.0))
                                                                         ,vec4 (0.7
                                                                               ,0.3
                                                                               ,0.3
                                                                               ,1.0))) * (((0.0) + ((((((((vo4).xyz) / ((vo4).w)) * (0.5)) + (0.5)).z) - (max
        ((5.0e-4) * ((1.0) - (dot (vo2
                                  ,normalize ((getLightPos1 ((time) / (10.0))) - (vo3)))))
        ,5.0e-5))) > ((texture2D (s0
                                 ,((((((vo4).xyz) / ((vo4).w)) * (0.5)) + (0.5)).xy) + ((vec2 (0.0
                                                                                              ,0.0)) * (texelSize)))).x) ? 0.3 : 1.0)) / (1.0)))) + ((diffuseLight_4_VecScalar4Float
        (vo3
        ,vo2
        ,getLightPos2 ((time) / (10.0))
        ,vec4 (0.3
              ,0.3
              ,0.7
              ,1.0))) * (((0.0) + ((((((((vo5).xyz) / ((vo5).w)) * (0.5)) + (0.5)).z) - (max
        ((5.0e-4) * ((1.0) - (dot (vo2
                                  ,normalize ((getLightPos2 ((time) / (10.0))) - (vo3)))))
        ,5.0e-5))) > ((texture2D (s1
                                 ,((((((vo5).xyz) / ((vo5).w)) * (0.5)) + (0.5)).xy) + ((vec2 (0.0
                                                                                              ,0.0)) * (texelSize)))).x) ? 0.3 : 1.0)) / (1.0)))) / (1.2)) * (texture2D
        (diffuseTexture,((vo1).xy) * (2.5e-5)))) * (1.0);
}