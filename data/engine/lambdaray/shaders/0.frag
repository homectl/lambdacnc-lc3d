#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform vec2 screenSize;
smooth in vec2 vo1;
out vec4 f0;
vec3 cameraPos;
vec3 lookAt_Float;
vec3 upVec;
float aspectRatio_Float_2_1(vec2 z0) {
    return ((z0).y) / ((z0).x);
}
float sqrnorm_3(vec3 z0) {
    return pow (length (z0),2.0);
}
mat3 viewMatrix;
void main() {
    cameraPos = vec3 (0.0,1.0,-20.0);
    lookAt_Float = vec3 (0.0,0.0,0.0);
    upVec = vec3 (0.2,1.0,0.0);
    viewMatrix = mat3 (normalize (cross (upVec
                                        ,normalize ((lookAt_Float) - (cameraPos))))
                      ,cross (normalize ((lookAt_Float) - (cameraPos))
                             ,normalize (cross (upVec,normalize ((lookAt_Float) - (cameraPos)))))
                      ,normalize ((lookAt_Float) - (cameraPos)));
    f0 = (vec4 (0.0,0.0,0.0,1.0)) + ((mod (atan ((((vec3 (0.0
                                                         ,1.0
                                                         ,-20.0)) + ((normalize ((viewMatrix) * (vec3 ((1.5) * (((vo1).x) - (0.5))
                                                                                                      ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                                                                      ,1.0)))) * (0.16))) + ((normalize ((viewMatrix) * (vec3
                                                ((1.5) * (((vo1).x) - (0.5))
                                                ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                ,1.0)))) * ((0.0) - ((((vec3 (0.0,1.0,-20.0)) + ((normalize
                                                ((viewMatrix) * (vec3 ((1.5) * (((vo1).x) - (0.5))
                                                                      ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                                      ,1.0)))) * (0.16))).y) / ((normalize ((viewMatrix) * (vec3
                                                ((1.5) * (((vo1).x) - (0.5))
                                                ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                ,1.0)))).y))))).x
                                                ,((vec3 (0.0,1.0,-20.0)) + ((normalize ((viewMatrix) * (vec3
                                                ((1.5) * (((vo1).x) - (0.5))
                                                ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                ,1.0)))) * (0.16))).z)
                                          ,0.52359)) < (0.261799) ? vec4 (1.0
                                                                         ,1.0
                                                                         ,1.0
                                                                         ,((true) ^ ((((vec3 (0.0,1.0,-20.0)) + ((normalize ((viewMatrix) * (vec3
                                                                         ((1.5) * (((vo1).x) - (0.5))
                                                                         ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                                         ,1.0)))) * (0.16))).y) > (0.0))) && (((sqrnorm_3 ((vec3 (0.0
                                                                                                                                 ,1.0
                                                                                                                                 ,-20.0)) + ((normalize ((viewMatrix) * (vec3
                                                                         ((1.5) * (((vo1).x) - (0.5))
                                                                         ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                                         ,1.0)))) * (0.16)))) < (pow (14.0,2.0))) && ((sqrnorm_3 ((vec3 (0.0
                                                                                                                                        ,1.0
                                                                                                                                        ,-20.0)) + ((normalize ((viewMatrix) * (vec3
                                                                         ((1.5) * (((vo1).x) - (0.5))
                                                                         ,((1.5) * (aspectRatio_Float_2_1 (screenSize))) * (((vo1).y) - (0.5))
                                                                         ,1.0)))) * (0.16)))) > (pow (2.6,2.0)))) ? 1.0 : 0.0) : vec4 (0.0
                                                                                                                                      ,0.0
                                                                                                                                      ,1.0
                                                                                                                                      ,((true) ^ ((((vec3 (0.0,1.0,-20.0)) + ((normalize
                                                                                                                                      ((viewMatrix) * (vec3 ((1.5) * (((vo1).x) - (0.5))
                                                                                                                                                            ,((1.5) * (aspectRatio_Float_2_1
                                                                                                                                                            (screenSize))) * (((vo1).y) - (0.5))
                                                                                                                                                            ,1.0)))) * (0.16))).y) > (0.0))) && (((sqrnorm_3
                                                                                                                                      ((vec3 (0.0,1.0,-20.0)) + ((normalize ((viewMatrix) * (vec3
                                                                                                                                      ((1.5) * (((vo1).x) - (0.5))
                                                                                                                                      ,((1.5) * (aspectRatio_Float_2_1
                                                                                                                                      (screenSize))) * (((vo1).y) - (0.5))
                                                                                                                                      ,1.0)))) * (0.16)))) < (pow (14.0,2.0))) && ((sqrnorm_3 ((vec3
                                                                                                                                      (0.0,1.0,-20.0)) + ((normalize ((viewMatrix) * (vec3
                                                                                                                                      ((1.5) * (((vo1).x) - (0.5))
                                                                                                                                      ,((1.5) * (aspectRatio_Float_2_1
                                                                                                                                      (screenSize))) * (((vo1).y) - (0.5))
                                                                                                                                      ,1.0)))) * (0.16)))) > (pow (2.6,2.0)))) ? 1.0 : 0.0));
}