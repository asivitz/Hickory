//
//  renderer.c
//  flip
//
//  Created by Axis Sivitz on 9/25/13.
//  Copyright (c) 2013 Axis Sivitz. All rights reserved.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "renderer.h"
#include "Drawing.h"
#include "defs.h"
#include "shaderprogram.h"
#include "platform.h"

#define RENDERER_DEBUG 1

#define MAX_SHADERS 20
#define MAX_COMMANDS 1024

typedef struct
{
    ShaderProgram * shader;
    draw_command * first_in_bucket;
    draw_command * last_in_bucket;
} ShaderInfo;

unsigned int num_shaders = 0;

ShaderInfo * shaderInfos[MAX_SHADERS];

unsigned int num_commands = 0;
draw_command commands[MAX_COMMANDS];
draw_command * blended_pile;

#define MEM_POOL_SIZE (1 << 15)
short mem_pool[MEM_POOL_SIZE];
unsigned int pool_counter = 0;

GLuint vao1;
GLuint alt_tcbuf;

GLuint getMainVAO()
{
   return vao1;
}

ShaderProgram * buildShaderProgramWithFilePaths(const char * inVertShaderPath, const char * inFragShaderPath)
{
    ShaderProgram * shader = buildShaderProgram(inVertShaderPath, inFragShaderPath);
    return shader;
}

int addShader(ShaderProgram * program)
{
    if (num_shaders < MAX_SHADERS)
    {
        int idnum = num_shaders;
        ShaderInfo * info = malloc(sizeof(ShaderInfo));
        info->first_in_bucket = NULL;
        info->last_in_bucket = NULL;
        info->shader = program;
        shaderInfos[idnum] = info;
        num_shaders++;
        return idnum;
    }
    else
        dlog("Error: Can't add shader beyond MAX_SHADERS");
    return -1;
}

int load_shader(const char * inVertShaderPath, const char * inFragShaderPath)
{
   ShaderProgram * program = buildShaderProgramWithFilePaths(inVertShaderPath, inFragShaderPath);
   if (program == NULL)
       return -1;

   int idnum = addShader(program);
   setupSquareDrawingForProgram(program);
   return idnum;
}

void init_renderer()
{
   glGenVertexArrays(1, &vao1);
   glBindVertexArray(vao1);
   
   glGenBuffers(1, &alt_tcbuf);
   
   setupSquareDrawing();
   
   for (int i = 0; i < num_shaders; i++)
   {
       ShaderInfo * info = shaderInfos[i];
       setupSquareDrawingForProgram(info->shader);
   }

   
   glDisable(GL_DITHER);
   glDisable(GL_BLEND);
   glDisable(GL_STENCIL_TEST);
//   glDisable(GL_TEXTURE_2D);
   glEnable(GL_DEPTH_TEST);
   // Disable other state variables as appropriate.

   reset_renderer();
}

void * pool_malloc(unsigned int num_bytes)
{
   void * loc = (mem_pool + pool_counter);
   pool_counter += num_bytes;
   
#if RENDERER_DEBUG
   if (pool_counter > MEM_POOL_SIZE)
      dlog("MEM POOL OVERFLOW: num bytes %d", pool_counter);
#endif
   return loc;
}

void reset_renderer()
{
   num_commands = 0;
   
   for (int i = 0; i < num_shaders; i++)
   {
       ShaderInfo * info = shaderInfos[i];
       info->first_in_bucket = info->last_in_bucket = NULL;
   }
   
   blended_pile = NULL;
   
   pool_counter = 0;
}

draw_command * list_tail(draw_command * head)
{
   while (head != NULL && head->next != NULL)
      head = head->next;
   return head;
}

draw_command * prune_label_from_list(draw_command * head, int label)
{
   if (head == NULL)
      return NULL;
   else if (head->label == label)
   {
      return prune_label_from_list(head->next, label);
   }
   else
   {
      head->next = prune_label_from_list(head->next, label);
      return head;
   }
}

void prune_label(int label)
{
   blended_pile = prune_label_from_list(blended_pile, label);
   for (int i = 0; i < num_shaders; i++)
   {
       ShaderInfo * info = shaderInfos[i];
       info->first_in_bucket = prune_label_from_list(info->first_in_bucket, label);
       info->last_in_bucket = list_tail(info->first_in_bucket);
   }
}

draw_command * add_draw_command(float * mat, float * color, float * color2, int texid, unsigned int shader, int label, float depth, int blend)
{
#if 0
    dlog("mat: %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f", mat[0], mat[1], mat[2], mat[3], mat[4], mat[5], mat[6], mat[7], mat[8], mat[9], mat[10], mat[11], mat[12], mat[13], mat[14], mat[15]);
    dlog("texid: %d", texid);
#endif
   draw_command * newone = (commands + num_commands);
   memcpy(newone->mat, mat, 16 * sizeof(float));
   memcpy(newone->color, color, 4 * sizeof(float));
   memcpy(newone->color2, color2, 4 * sizeof(float));
   newone->texid = texid;
   newone->shader = shader;
   newone->depth = depth;
   newone->label = label;
   newone->type = C_SQUARE;
   newone->fluniform = NULL;

   newone->next = NULL;
   num_commands++;
   
#if RENDERER_DEBUG
   if (num_commands > MAX_COMMANDS)
      dlog("DRAW COMMAND OVERFLOW: num commands %d", num_commands);
#endif
//   NSLog(@"num commands:%d", num_commands);
   
   if (blend)
   {
      add_blended_command(newone);
   }
   else
   {
      add_command_to_end(newone, shader);
   }
   
   return newone;
}

void add_float_uniform(draw_command * cmd, GLint loc, unsigned int num, float * data)
{
    float_uniform * uni = pool_malloc(sizeof(float_uniform));
    uni->num = num;
    uni->loc = loc;
    for (int i = 0; i < num; i++)
        uni->data[i] = data[i];
    uni->next = NULL;

    if (cmd->fluniform == NULL)
        cmd->fluniform = uni;
    else
    {
        float_uniform * lst = cmd->fluniform;
        while (lst->next != NULL)
            lst = lst->next;
        lst->next = uni;
    }
}

void set_tc_command(draw_command * cmd, float * tc)
{
   cmd->type = C_TC_SQUARE;
   
   tc_payload * payload = pool_malloc(sizeof(tc_payload));
   memcpy(payload->tc, tc, 4 * sizeof(float));
   
   cmd->payload = payload;
}

vao_payload * set_vao_command(draw_command * cmd, int vao, unsigned int num_indices, GLuint type)
{
   cmd->type = C_VAO;
   
   vao_payload * payload = pool_malloc(sizeof(vao_payload));
   payload->vao = vao;
   payload->num_indices = num_indices;
   payload->type = type;
   
   cmd->payload = payload;
   return payload;
}

void add_blended_command(draw_command * cmd)
{
   if (blended_pile == NULL || blended_pile->depth >= cmd->depth)
   {
      cmd->next = blended_pile;
      blended_pile = cmd;
   }
   else
   {
      draw_command * index = blended_pile;
      draw_command * after = blended_pile->next;
      while (after != NULL && after->depth < cmd->depth)
      {
         index = after;
         after = after->next;
      }
      index->next = cmd;
      cmd->next = after;
   }
}

void add_command_to_end(draw_command * cmd, int shader)
{
    ShaderInfo * info = shaderInfos[shader];
    draw_command * last = info->last_in_bucket;
    if (last == NULL)
    {
        info->last_in_bucket = info->first_in_bucket = cmd;
    }
    else
    {
        last->next = cmd;
        info->last_in_bucket = cmd;
    }
}

void exec_dc(ShaderProgram * program, draw_command * cmd)
{
   if (cmd->texid > -1)
      glBindTexture(GL_TEXTURE_2D, cmd->texid);
   glUniform4fv(shaderProgramUniformLoc(program, SP_UNIFORM_COLOR), 1, cmd->color);
   glUniform4fv(shaderProgramUniformLoc(program, SP_UNIFORM_COLOR2), 1, cmd->color2);
   glUniformMatrix4fv(shaderProgramUniformLoc(program, SP_UNIFORM_MODEL_MAT), 1, GL_FALSE, cmd->mat);

   float_uniform * lst = cmd->fluniform;
   while (lst != NULL)
   {
       glUniform1fv(lst->loc, lst->num, lst->data);
       lst = lst->next;
   }

   if (cmd->type == C_TC_SQUARE)
   {
      tc_payload * payload = (tc_payload *)cmd->payload;
      float x = payload->tc[0];
      float y = payload->tc[1];
      float width = payload->tc[2];
      float height = payload->tc[3];
      
      GLfloat tCoords[] = {
         x, y,
         x, y + height,
         x + width, y + height,
         x + width, y};
      
      glBindBuffer(GL_ARRAY_BUFFER, alt_tcbuf);
      glBufferData(GL_ARRAY_BUFFER, 8 * sizeof(float), NULL, GL_STREAM_DRAW);
      glVertexAttribPointer(shaderProgramAttrLoc(program, SP_ATTR_TEX_COORDS), 2, GL_FLOAT, 0, 0, 0);
      
      void * mapped_data = glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
      memcpy(mapped_data, tCoords, 8 * sizeof(float));
      
      glUnmapBuffer(GL_ARRAY_BUFFER);

      glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_SHORT, 0);
      
      rebindTC(program);
   }
   else if (cmd->type == C_VAO)
   {
      vao_payload * payload = (vao_payload *)cmd->payload;

      glBindVertexArray(payload->vao);
      
      glDrawElements(payload->type, payload->num_indices, GL_UNSIGNED_SHORT, 0);
      
      glBindVertexArray(getMainVAO());
   }
   else
   {
      glDrawElements(GL_TRIANGLE_FAN, 4, GL_UNSIGNED_SHORT, 0);
   }
}

void engage_shader(ShaderProgram * program, float * view_mat)
{
   setShaderProgramActive(program);
   glUniform1i(shaderProgramUniformLoc(program, SP_UNIFORM_TEXID), 0);
   setupSquareDrawingForProgram(program);
   glUniformMatrix4fv(shaderProgramUniformLoc(program, SP_UNIFORM_VIEW_MAT), 1, GL_FALSE, view_mat);
}

void render_commands(float view_mat[16], int label)
{
//   glEnable(GL_DEPTH_TEST);
   
   //We're not blending, so sort by shader and go nuts
   
   for (int i = 0; i < num_shaders; i++)
   {
      ShaderProgram * program = NULL;
      ShaderInfo * info = shaderInfos[i];
      draw_command * cmd = info->first_in_bucket;
      
      while (cmd != NULL)
      {
         if (cmd->label == label)
         {
            if (program == NULL)
            {
               program = info->shader;
               engage_shader(program, view_mat);
            }
            
            exec_dc(program, cmd);
         }
         cmd = cmd->next;
      }
   }
   
   //We're blending, so take the depth sorted commands and just check to see if we don't have to change the shader
   glEnable(GL_BLEND);
   glDepthMask(GL_FALSE);
//   glDisable(GL_DEPTH_TEST);

   draw_command * cmd = blended_pile;
   int current_shader = -1;
   ShaderProgram * current_program;
   
   while (cmd != NULL)
   {
      if (cmd->label == label)
      {
         if (cmd->shader != current_shader)
         {
            current_shader = cmd->shader;
            current_program = getShader(current_shader);
            engage_shader(current_program, view_mat);
         }
      
         exec_dc(current_program, cmd);
      }
      cmd = cmd->next;
   }
      
   glDisable(GL_BLEND);
//   glEnable(GL_DEPTH_TEST);
   glDepthMask(GL_TRUE);

/*checkGLError();*/
}

ShaderProgram * getShader(unsigned int num)
{
    if (num >= num_shaders)
        dlog("Error: Invalid Shader Id Num");
    ShaderInfo * info = shaderInfos[num];
    return info->shader;
}

void attach_vertex_array(unsigned int shader, int attrId, int len, int stride, int offset)
{
    ShaderProgram * program = getShader(shader);
    int attrLoc = shaderProgramAttrLoc(program, attrId);

    glEnableVertexAttribArray(attrLoc);
    glVertexAttribPointer(attrLoc, len, GL_FLOAT, 0, stride * sizeof(GL_FLOAT), (const GLvoid *)(offset * sizeof(GL_FLOAT)));
}

int grab_uniform_loc(unsigned int shader, int uniformCode)
{
    ShaderProgram * program = getShader(shader);
    int loc = shaderProgramUniformLoc(program, uniformCode);
    return loc;
}
