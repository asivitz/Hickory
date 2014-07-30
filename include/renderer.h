//
//  renderer.h
//  flip
//
//  Created by Axis Sivitz on 9/25/13.
//  Copyright (c) 2013 Axis Sivitz. All rights reserved.
//

#ifndef flip_renderer_h
#define flip_renderer_h

#include "ShaderProgram.h"

typedef enum command_type
{
   C_SQUARE,
   C_TC_SQUARE,
   C_POINTS,
   C_LINE_RECT,
   C_VAO
} command_type;

typedef struct tc_payload
{
   float tc[4];
} tc_payload;

typedef struct points_payload
{
   unsigned int num_points;
   float * points;
   float size;
   int psize; // x,y,z = 3 x,y = 2
} points_payload;

typedef struct line_rect_payload
{
   float width, height, thickness;
} line_rect_payload;

typedef struct vao_payload
{
   int vao;
   unsigned int num_indices;
   int type;
} vao_payload;

typedef struct _float_uniform float_uniform;
struct _float_uniform
{
    float data[4];
    unsigned int num;
    GLint loc;
    float_uniform * next;
};

typedef struct _draw_command draw_command;
struct _draw_command
{
   draw_command * next;
   
   float mat[16];
   float color[4];
   float color2[4];
   int texid;
   unsigned int shader;
   float depth;
   int label;
   command_type type;
   void * payload;
   float_uniform * fluniform;
};

ShaderProgram * getShader(unsigned int num);
GLuint getMainVAO();

void prune_label(int label);
void add_command_to_end(draw_command * cmd, int shader);
void add_blended_command(draw_command * cmd);
void reset_renderer();
void init_renderer();
void render_commands(float view_mat[16], int label);
draw_command * add_draw_command(float * mat, float * color, float * color2, int texid, unsigned int shader, int label, float depth, int blend);
void set_tc_command(draw_command * cmd, float * tc);
void set_points_command(draw_command * cmd, int num_points, float * points, float size, int psize);
void set_line_rect_command(draw_command * cmd, float width, float height, float thickness);
vao_payload * set_vao_command(draw_command * cmd, int vao, unsigned int num_indices, GLuint type);

GLuint make_vao();
GLuint attach_index_array();
void buffer_vertices_num(int vbo, float * data, int num);
unsigned int buffer_square_indices(unsigned int vbo, int num_squares);
void add_uniform_to_vao_payload(vao_payload * vaopayload, int uniform_loc, float uniform_val);

void add_float_uniform(draw_command * cmd, GLint loc, unsigned int num, float * data);

#endif
