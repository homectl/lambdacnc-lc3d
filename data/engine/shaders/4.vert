#version 330 core
vec4 texture2D(sampler2D s,vec2 uv) {
    return texture(s,uv);
}
uniform vec2 screenSize;
uniform float time;
in vec3 vi1;
in vec3 vi2;
smooth out vec3 vo1;
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
mat4 rotMatrixZ(float z0) {
    return mat4 (vec4 (cos (z0),sin (z0),0.0,0.0)
                ,vec4 ((0.0) - (sin (z0)),cos (z0),0.0,0.0)
                ,vec4 (0.0,0.0,1.0,0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec4 bulbOffset_Float;
mat4 cameraMat_2_Float(vec2 z0,float z1) {
    return (perspective (3000.0
                        ,350000.0
                        ,0.5235987755982988
                        ,aspectRatio_Float_2_1 (z0))) * (lookat (vec3 (0.0,180000.0,60000.0)
                                                                ,vec3 (0.0,0.0,10000.0)
                                                                ,vec3 (0.0,0.0,1.0)));
}
vec4 lightPos(float z0) {
    return (rotMatrixZ ((z0) * (8.0))) * (vec4 (80000.0,10000.0,40000.0,1.0));
}
mat4 rotMatrixX(float z0) {
    return mat4 (vec4 (1.0,0.0,0.0,0.0)
                ,vec4 (0.0,cos (z0),sin (z0),0.0)
                ,vec4 (0.0,(0.0) - (sin (z0)),cos (z0),0.0)
                ,vec4 (0.0,0.0,0.0,1.0));
}
vec4 scale(float z0,vec4 z1) {
    return (z1) * (vec4 (z0,z0,z0,1.0));
}
void main() {
    bulbOffset_Float = vec4 (0.0,0.0,4200.0,0.0);
    gl_Position = (cameraMat_2_Float (screenSize,(time) / (10.0))) * ((((rotMatrixX
        (-1.5707963267948966)) * (scale (200.0
                                        ,vec4 ((vi1).x,(vi1).y,(vi1).z,0.0)))) + (lightPos
        ((time) / (10.0)))) + (bulbOffset_Float));
    vo1 = vi2;
}