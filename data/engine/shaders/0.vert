#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform vec3 position;
uniform float time;
in vec3 vi1;
in vec3 vi2;
smooth out vec4 vo1;
smooth out vec3 vo2;
smooth out vec4 vo3;
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
mat4 lookat(vec3 z0,vec3 z1,vec3 z2) {
    return (transpose (mat4 (ext0_Float_3 (normalize (cross (z2
                                                            ,normalize ((z0) - (z1)))))
                            ,ext0_Float_3 (cross (normalize ((z0) - (z1))
                                                 ,normalize (cross (z2,normalize ((z0) - (z1))))))
                            ,ext0_Float_3 (normalize ((z0) - (z1)))
                            ,vec4 (0.0,0.0,0.0,1.0)))) * (translateBefore4 (neg_VecSFloat3 (z0)));
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
mat4 modelMat_Float(float z0) {
    return mat4 (vec4 (cos (2.356194490192345),sin (2.356194490192345),0.0,0.0)
                ,vec4 ((0.0) - (sin (2.356194490192345)),cos (2.356194490192345),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec4 positionObject_Float_3_3_Float(float z0,vec3 z1,vec3 z2) {
    return (vec4 ((z2).x,(z2).y,(z2).z,1.0)) + (vec4_3_Float (z1,0.0));
}
mat4 projmat;
vec4 vec4_3_Float(vec3 z0,float z1) {
    return vec4 ((z0).x,(z0).y,(z0).z,z1);
}
void main() {
    projmat = (perspective (10000.0,300000.0,45.0,1.75)) * (lookat (vec3 (0.0
                                                                         ,80000.0
                                                                         ,44000.0)
                                                                   ,vec3 (0.0,0.0,0.0)
                                                                   ,vec3 (0.0,0.0,1.0)));
    gl_Position = (projmat) * ((modelMat_Float
        ((time) / (10.0))) * (positionObject_Float_3_3_Float ((time) / (10.0)
                                                             ,position
                                                             ,vi1)));
    vo1 = (modelMat_Float ((time) / (10.0))) * (positionObject_Float_3_3_Float
        ((time) / (10.0),position,vi1));
    vo2 = vi1;
    vo3 = normalize ((modelMat_Float ((time) / (10.0))) * (vec4_3_Float (vi2,0.0)));
}