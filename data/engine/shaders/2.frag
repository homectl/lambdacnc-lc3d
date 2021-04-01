#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform sampler2D diffuseTexture;
uniform sampler2D s0;
uniform float time;
smooth in vec4 vo1;
smooth in vec4 vo2;
smooth in vec3 vo3;
smooth in vec4 vo4;
out vec4 f0;
mat4 rotMatrixZ(float z0) {
    return mat4 (vec4 (cos (z0),sin (z0),0.0,0.0)
                ,vec4 ((0.0) - (sin (z0)),cos (z0),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec4 diffuseLight_4_VecSFloat4(vec4 z0,vec4 z1,vec4 z2,vec4 z3) {
    return (z3) * (max (dot (z1,normalize ((z2) - (z0))),0.0));
}
vec4 lightPos_Float(float z0) {
    return (rotMatrixZ (5.890486225480862)) * (vec4 (80000.0,10000.0,40000.0,1.0));
}
vec2 texelSize;
void main() {
    texelSize = vec2 (1.25e-3,2.5e-3);
    f0 = ((diffuseLight_4_VecSFloat4 (vo1
                                     ,vo4
                                     ,lightPos_Float ((time) / (10.0))
                                     ,vec4 (1.0,1.0,1.0,1.0))) * (texture2D (diffuseTexture
                                                                            ,((vo3).xy) * (2.5e-5)))) * (((0.0) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
        ((5.0e-4) * ((1.0) - (dot (vo4
                                  ,normalize ((lightPos_Float ((time) / (10.0))) - (vo1)))))
        ,5.0e-5))) > ((texture2D (s0
                                 ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + ((vec2 (0.0
                                                                                              ,0.0)) * (texelSize)))).x) ? 0.3 : 1.0)) / (1.0));
}