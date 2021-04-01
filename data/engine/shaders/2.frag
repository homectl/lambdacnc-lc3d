#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform sampler2D diffuseTexture;
uniform sampler2D s0;
uniform sampler2D s1;
uniform sampler2D s2;
uniform float time;
smooth in vec4 vo1;
smooth in vec4 vo2;
smooth in vec3 vo3;
smooth in vec4 vo4;
out vec4 f0;
float getBias_4(vec4 z0,vec4 z1) {
    return max ((5.0e-4) * ((1.0) - (dot (z0,z1))),5.0e-5);
}
mat4 rotMatrixZ(float z0) {
    return mat4 (vec4 (cos (z0),sin (z0),0.0,0.0)
                ,vec4 ((0.0) - (sin (z0)),cos (z0),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec3 diffuseLight_4_VecSFloat3(vec4 z0,vec4 z1,vec4 z2,vec3 z3) {
    return (z3) * (max (dot (z1,normalize ((z2) - (z0))),0.0));
}
vec4 getLightPos_Float(float z0) {
    return (rotMatrixZ (5.890486225480862)) * (vec4 (80000.0,10000.0,40000.0,1.0));
}
vec4 mkLightDir_Float(float z0,vec4 z1) {
    return normalize ((getLightPos_Float (z0)) - (z1));
}
float pcf_4(float z0,float z1,vec4 z2,vec4 z3) {
    return ((z0) - (getBias_4 (z2,z3))) > (z1) ? 0.3 : 1.0;
}
vec3 perspectiveDivide_4(vec4 z0) {
    return ((((z0).xyz) / ((z0).w)) * (0.5)) + (0.5);
}
vec2 texelSize;
void main() {
    texelSize = vec2 (1.25e-3,2.5e-3);
    f0 = vec4 ((((diffuseLight_4_VecSFloat3 (vo1
                                            ,vo4
                                            ,getLightPos_Float ((time) / (10.0))
                                            ,vec3 (1.0,1.0,1.0))) * ((texture2D (diffuseTexture
                                                                                ,((vo3).xy) * (2.5e-5))).xyz)) * ((pcf_4 ((perspectiveDivide_4 (vo2)).z
                                                                                                                         ,(texture2D (s0
                                                                                                                                     ,((perspectiveDivide_4 (vo2)).xy) + ((vec2 (0.0
                                                                                                                                                                                ,0.0)) * (texelSize)))).x
                                                                                                                         ,vo4
                                                                                                                         ,mkLightDir_Float ((time) / (10.0),vo1))) / (1.0))).x
              ,(((diffuseLight_4_VecSFloat3 (vo1
                                            ,vo4
                                            ,getLightPos_Float ((time) / (10.0))
                                            ,vec3 (1.0,1.0,1.0))) * ((texture2D (diffuseTexture
                                                                                ,((vo3).xy) * (2.5e-5))).xyz)) * ((pcf_4 ((perspectiveDivide_4 (vo2)).z
                                                                                                                         ,(texture2D (s1
                                                                                                                                     ,((perspectiveDivide_4 (vo2)).xy) + ((vec2 (0.0
                                                                                                                                                                                ,0.0)) * (texelSize)))).x
                                                                                                                         ,vo4
                                                                                                                         ,mkLightDir_Float ((time) / (10.0),vo1))) / (1.0))).y
              ,(((diffuseLight_4_VecSFloat3 (vo1
                                            ,vo4
                                            ,getLightPos_Float ((time) / (10.0))
                                            ,vec3 (1.0,1.0,1.0))) * ((texture2D (diffuseTexture
                                                                                ,((vo3).xy) * (2.5e-5))).xyz)) * ((pcf_4 ((perspectiveDivide_4 (vo2)).z
                                                                                                                         ,(texture2D (s2
                                                                                                                                     ,((perspectiveDivide_4 (vo2)).xy) + ((vec2 (0.0
                                                                                                                                                                                ,0.0)) * (texelSize)))).x
                                                                                                                         ,vo4
                                                                                                                         ,mkLightDir_Float ((time) / (10.0),vo1))) / (1.0))).z
              ,1.0);
}