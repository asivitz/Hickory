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

#define MAX_SHADERS 20

unsigned int num_shaders = 0;

ShaderProgram * shaders[MAX_SHADERS];

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
        shaders[idnum] = program;
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
   return idnum;
}

ShaderProgram * getShader(unsigned int num)
{
    if (num >= num_shaders)
        dlog("Error: Invalid Shader Id Num");
    return shaders[num];
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

void use_shader(unsigned int shader)
{
    ShaderProgram * program = getShader(shader);
    setShaderProgramActive(program);
}
