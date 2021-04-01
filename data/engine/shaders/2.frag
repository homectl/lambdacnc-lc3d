#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform sampler2D diffuseTexture;
uniform sampler2D s0;
uniform sampler2D s1;
uniform sampler2D s10;
uniform sampler2D s11;
uniform sampler2D s12;
uniform sampler2D s13;
uniform sampler2D s14;
uniform sampler2D s15;
uniform sampler2D s16;
uniform sampler2D s17;
uniform sampler2D s2;
uniform sampler2D s3;
uniform sampler2D s4;
uniform sampler2D s5;
uniform sampler2D s6;
uniform sampler2D s7;
uniform sampler2D s8;
uniform sampler2D s9;
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
vec3 diffuseLight_4_VecSFloat3(vec4 z0,vec4 z1,vec4 z2,vec3 z3) {
    return (z3) * (max (dot (z1,normalize ((z2) - (z0))),0.0));
}
vec4 lightPos(float z0) {
    return (rotMatrixZ ((z0) * (8.0))) * (vec4 (80000.0,10000.0,40000.0,1.0));
}
void main() {
    f0 = vec4 ((((diffuseLight_4_VecSFloat3 (vo1
                                            ,vo4
                                            ,lightPos ((time) / (10.0))
                                            ,vec3 (1.0,1.0,1.0))) * ((texture2D (diffuseTexture
                                                                                ,((vo3).xy) * (2.5e-5))).xyz)) * ((((((((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s0
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,-1.0)))).x) ? 0.0 : 1.0) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s1
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,0.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s2
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,1.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s3
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (0.0
                                                                                                   ,-1.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s4
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,0.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s5
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,1.0)))).x) ? 0.0 : 1.0)) / (4.0))).x
              ,(((diffuseLight_4_VecSFloat3 (vo1
                                            ,vo4
                                            ,lightPos ((time) / (10.0))
                                            ,vec3 (1.0,1.0,1.0))) * ((texture2D (diffuseTexture
                                                                                ,((vo3).xy) * (2.5e-5))).xyz)) * ((((((((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s6
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,-1.0)))).x) ? 0.0 : 1.0) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s7
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,0.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s8
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,1.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s9
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (0.0
                                                                                                   ,-1.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s10
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,0.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s11
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,1.0)))).x) ? 0.0 : 1.0)) / (4.0))).y
              ,(((diffuseLight_4_VecSFloat3 (vo1
                                            ,vo4
                                            ,lightPos ((time) / (10.0))
                                            ,vec3 (1.0,1.0,1.0))) * ((texture2D (diffuseTexture
                                                                                ,((vo3).xy) * (2.5e-5))).xyz)) * ((((((((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s12
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,-1.0)))).x) ? 0.0 : 1.0) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s13
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,0.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s14
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,1.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s15
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (0.0
                                                                                                   ,-1.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s16
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,0.0)))).x) ? 0.0 : 1.0)) + ((((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).z) - (max
              ((5.0e-4) * ((1.0) - (dot (vo4
                                        ,normalize ((lightPos ((time) / (10.0))) - (vo1)))))
              ,5.0e-5))) > ((texture2D (s17
                                       ,((((((vo2).xyz) / ((vo2).w)) * (0.5)) + (0.5)).xy) + (vec2 (-1.0
                                                                                                   ,1.0)))).x) ? 0.0 : 1.0)) / (4.0))).z
              ,1.0);
}