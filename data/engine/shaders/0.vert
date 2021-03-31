#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform vec3 position;
uniform float time;
in vec3 vi1;
in vec3 vi2;
smooth out float vo1;
vec4 ext0_Float_3(vec3 z0) {
    return vec4 ((z0).x,(z0).y,(z0).z,0.0);
}
vec3 neg_VecSFloat3(vec3 z0) {
    return - (z0);
}
mat4 rotMatrixZ(float z0) {
    return mat4 (vec4 (cos (z0),sin (z0),0.0,0.0)
                ,vec4 ((0.0) - (sin (z0)),cos (z0),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
mat4 translateBefore4(vec3 z0) {
    return mat4 (vec4 (1.0,0.0,0.0,0.0)
                ,vec4 (0.0,1.0,0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 ((z0).x,(z0).y,(z0).z,1.0));
}
vec4 lightPos_Float(float z0) {
    return (rotMatrixZ (2.356194490192345)) * (vec4 (60000.0,10000.0,50000.0,1.0));
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
mat4 lightMat_2_Float(vec2 z0,float z1) {
    return (orthographic (10000.0,300000.0,70000.0,((z0).y) / ((z0).x))) * (lookat
        ((lightPos_Float (z1)).xyz,vec3 (0.0,0.0,0.0),vec3 (0.0,0.0,1.0)));
}
mat4 modelMat_Float(float z0) {
    return mat4 (vec4 (cos (2.356194490192345),sin (2.356194490192345),0.0,0.0)
                ,vec4 ((0.0) - (sin (2.356194490192345)),cos (2.356194490192345),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec4 positionObject_Float_3_3_Float(float z0,vec3 z1,vec3 z2) {
    return (vec4 ((z2).x,(z2).y,(z2).z,1.0)) + (vec4 ((z1).x,(z1).y,(z1).z,0.0));
}
void main() {
    gl_Position = (lightMat_2_Float (vec2 (750.0,750.0)
                                    ,(time) / (10.0))) * ((modelMat_Float
        ((time) / (10.0))) * (positionObject_Float_3_3_Float ((time) / (10.0)
                                                             ,position
                                                             ,vi1)));
    vo1 = ((((lightMat_2_Float (vec2 (750.0,750.0)
                               ,(time) / (10.0))) * ((modelMat_Float
        ((time) / (10.0))) * (positionObject_Float_3_3_Float ((time) / (10.0)
                                                             ,position
                                                             ,vi1)))).z) * (0.5)) + (0.5);
}