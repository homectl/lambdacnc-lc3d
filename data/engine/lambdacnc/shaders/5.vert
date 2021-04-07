#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform vec3 position;
uniform vec2 screenSize;
uniform float time;
in vec3 vi1;
in vec3 vi2;
smooth out vec3 vo1;
smooth out vec4 vo2;
smooth out vec4 vo3;
smooth out vec4 vo4;
smooth out vec4 vo5;
vec4 ext0_Float_3(vec3 z0) {
    return vec4 ((z0).x,(z0).y,(z0).z,0.0);
}
vec3 neg_VecSFloat3(vec3 z0) {
    return - (z0);
}
mat4 translateBefore4(vec3 z0) {
    return mat4 (vec4 (1.0,0.0,0.0,0.0)
                ,vec4 (0.0,1.0,0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 ((z0).x,(z0).y,(z0).z,1.0));
}
float aspectRatio_Float_2_1(vec2 z0) {
    return ((z0).x) / ((z0).y);
}
vec4 cameraPos_Float(float z0) {
    return (vec4 (90000.0,40000.0,20000.0,0.0)) * (1.4);
}
mat4 lookat(vec3 z0,vec3 z1,vec3 z2) {
    return (transpose (mat4 (ext0_Float_3 (normalize (cross (z2
                                                            ,normalize ((z0) - (z1)))))
                            ,ext0_Float_3 (cross (normalize ((z0) - (z1))
                                                 ,normalize (cross (z2,normalize ((z0) - (z1))))))
                            ,ext0_Float_3 (normalize ((z0) - (z1)))
                            ,vec4 (0.0,0.0,0.0,1.0)))) * (translateBefore4 (neg_VecSFloat3 (z0)));
}
mat4 orthographic(float z0,float z1,float z2,float z3) {
    return mat4 (vec4 ((2.0) / (((z3) * (z2)) - ((0.0) - ((z3) * (z2))))
                      ,0.0
                      ,0.0
                      ,0.0)
                ,vec4 (0.0,(2.0) / ((z2) - ((0.0) - (z2))),0.0,0.0)
                ,vec4 (0.0,0.0,(0.0) - ((2.0) / ((z1) - (z0))),0.0)
                ,vec4
                ((0.0) - ((((z3) * (z2)) + ((0.0) - ((z3) * (z2)))) / (((z3) * (z2)) - ((0.0) - ((z3) * (z2)))))
                ,(0.0) - (((z2) + ((0.0) - (z2))) / ((z2) - ((0.0) - (z2))))
                ,(0.0) - (((z1) + (z0)) / ((z1) - (z0)))
                ,1.0));
}
mat4 perspective(float z0,float z1,float z2,float z3) {
    return mat4 (vec4 (((2.0) * (z0)) / (((z3) * ((z0) * (tan
                      ((z2) / (2.0))))) - ((0.0) - ((z3) * ((z0) * (tan ((z2) / (2.0)))))))
                      ,0.0
                      ,0.0
                      ,0.0)
                ,vec4 (0.0
                      ,((2.0) * (z0)) / (((z0) * (tan ((z2) / (2.0)))) - ((0.0) - ((z0) * (tan
                      ((z2) / (2.0))))))
                      ,0.0
                      ,0.0)
                ,vec4 ((((z3) * ((z0) * (tan ((z2) / (2.0))))) + ((0.0) - ((z3) * ((z0) * (tan
                      ((z2) / (2.0))))))) / (((z3) * ((z0) * (tan
                      ((z2) / (2.0))))) - ((0.0) - ((z3) * ((z0) * (tan ((z2) / (2.0)))))))
                      ,(((z0) * (tan ((z2) / (2.0)))) + ((0.0) - ((z0) * (tan
                      ((z2) / (2.0)))))) / (((z0) * (tan ((z2) / (2.0)))) - ((0.0) - ((z0) * (tan
                      ((z2) / (2.0))))))
                      ,(0.0) - (((z1) + (z0)) / ((z1) - (z0)))
                      ,-1.0)
                ,vec4 (0.0,0.0,(0.0) - ((((2.0) * (z1)) * (z0)) / ((z1) - (z0))),0.0));
}
mat4 cameraMat_2_Float(vec2 z0,float z1) {
    return (perspective (3000.0
                        ,350000.0
                        ,0.5235987755982988
                        ,aspectRatio_Float_2_1 (z0))) * (lookat ((cameraPos_Float (z1)).xyz
                                                                ,vec3 (0.0,0.0,10000.0)
                                                                ,vec3 (0.0,0.0,1.0)));
}
mat4 lightMat_4(vec4 z0) {
    return (orthographic (3000.0,350000.0,50000.0,1.0)) * (lookat ((z0).xyz
                                                                  ,vec3 (0.0,0.0,0.0)
                                                                  ,vec3 (0.0,0.0,1.0)));
}
mat4 modelMat_Float(float z0) {
    return mat4 (vec4 (cos (1.5707963267948966),sin (1.5707963267948966),0.0,0.0)
                ,vec4 ((0.0) - (sin (1.5707963267948966)),cos (1.5707963267948966),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec4 positionObject_Float_3_3_Float(float z0,vec3 z1,vec3 z2) {
    return (vec4 ((z2).x,(z2).y,(z2).z,1.0)) + (vec4 ((z1).x,(z1).y,(z1).z,0.0));
}
mat4 rotMatrixZ(float z0) {
    return mat4 (vec4 (cos (z0),sin (z0),0.0,0.0)
                ,vec4 ((0.0) - (sin (z0)),cos (z0),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
void main() {
    gl_Position = (cameraMat_2_Float (screenSize
                                     ,(time) / (10.0))) * ((modelMat_Float
        ((time) / (10.0))) * (positionObject_Float_3_3_Float ((time) / (10.0)
                                                             ,position
                                                             ,vi1)));
    vo1 = (vi1) + (vec3 (24000.0,0.0,0.0));
    vo2 = normalize ((modelMat_Float ((time) / (10.0))) * (vec4 ((vi2).x
                                                                ,(vi2).y
                                                                ,(vi2).z
                                                                ,0.0)));
    vo3 = (modelMat_Float ((time) / (10.0))) * (positionObject_Float_3_3_Float
        ((time) / (10.0),position,vi1));
    vo4 = (lightMat_4 ((rotMatrixZ (((time) / (10.0)) * (5.0))) * (vec4 (-60000.0
                                                                        ,-60000.0
                                                                        ,30000.0
                                                                        ,1.0)))) * ((modelMat_Float ((time) / (10.0))) * (positionObject_Float_3_3_Float
        ((time) / (10.0),position,vi1)));
    vo5 = (lightMat_4 ((rotMatrixZ (((time) / (10.0)) * (5.0))) * (vec4 (60000.0
                                                                        ,60000.0
                                                                        ,30000.0
                                                                        ,1.0)))) * ((modelMat_Float ((time) / (10.0))) * (positionObject_Float_3_3_Float
        ((time) / (10.0),position,vi1)));
}