/* From https://github.com/nglevin/linmath.h */

#ifndef LINMATH_H
#define LINMATH_H

#include <math.h>

typedef float vec3[3];
typedef float vec4[4];
typedef vec4 mat4x4[4];
typedef float quat[4];
void vec3_add(vec3 r, vec3 a, vec3 b);
void vec3_sub(vec3 r, vec3 a, vec3 b);
void vec3_scale(vec3 r, vec3 v, float s);
float vec3_mul_inner(vec3 a, vec3 b);
void vec3_mul_cross(vec3 r, vec3 a, vec3 b);
float vec3_len(vec3 v);
void vec3_norm(vec3 r, vec3 v);
void vec3_reflect(vec3 r, vec3 v, vec3 n);
void vec4_add(vec4 r, vec4 a, vec4 b);
void vec4_sub(vec4 r, vec4 a, vec4 b);
void vec4_scale(vec4 r, vec4 v, float s);
float vec4_mul_inner(vec4 a, vec4 b);
void vec4_mul_cross(vec4 r, vec4 a, vec4 b);
float vec4_len(vec4 v);
void vec4_norm(vec4 r, vec4 v);
void vec4_reflect(vec4 r, vec4 v, vec4 n);
void vec3_to_vec4(vec4 r, vec3 a, float end);
void mat4x4_identity(mat4x4 M);
void mat4x4_dup(mat4x4 M, mat4x4 N);
void mat4x4_row(vec4 r, mat4x4 M, int i);
void mat4x4_col(vec4 r, mat4x4 M, int i);
void mat4x4_transpose(mat4x4 M, mat4x4 N);
void mat4x4_add(mat4x4 M, mat4x4 a, mat4x4 b);
void mat4x4_sub(mat4x4 M, mat4x4 a, mat4x4 b);
void mat4x4_scale(mat4x4 M, mat4x4 a, float k);
void mat4x4_scale_aniso(mat4x4 M, mat4x4 a, float x, float y, float z);
void mat4x4_mul(mat4x4 M, mat4x4 a, mat4x4 b);
void mat4x4_mul_vec4(vec4 r, mat4x4 M, vec4 v);
void mat4x4_translate(mat4x4 T, float x, float y, float z);
void mat4x4_translate_in_place(mat4x4 M, float x, float y, float z);
void mat4x4_from_vec3_mul_outer(mat4x4 M, vec3 a, vec3 b);
void mat4x4_rotate(mat4x4 R, mat4x4 M, float x, float y, float z, float angle);
void mat4x4_rotate_X(mat4x4 Q, mat4x4 M, float angle);
void mat4x4_rotate_Y(mat4x4 Q, mat4x4 M, float angle);
void mat4x4_rotate_Z(mat4x4 Q, mat4x4 M, float angle);
void mat4x4_invert(mat4x4 T, mat4x4 M);
void mat4x4_frustum(mat4x4 M, float l, float r, float b, float t, float n, float f);
void mat4x4_ortho(mat4x4 M, float l, float r, float b, float t, float n, float f);
void mat4x4_perspective(mat4x4 m, float y_fov, float aspect, float n, float f);
void mat4x4_look_at(mat4x4 m, vec3 eye, vec3 center, vec3 up);
void mat4x4_lerp(mat4x4 M, float fract, mat4x4 a, mat4x4 b);
void quat_identity(quat q);
void quat_add(quat r, quat a, quat b);
void quat_sub(quat r, quat a, quat b);
void quat_mul(quat r, quat p, quat q);
void quat_scale(quat r, quat v, float s);
float quat_inner_product(quat a, quat b);
void quat_conj(quat r, quat q);
#define quat_norm vec4_norm
void quat_mul_vec3(vec3 r, quat q, vec3 v);
void mat4x4_from_quat(mat4x4 M, quat q);
void mat4x4_mul_quat(mat4x4 R, mat4x4 M, quat q);
void quat_from_mat4x4(quat q, mat4x4 M);

#endif
