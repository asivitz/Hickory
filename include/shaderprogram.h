//
//  shaderprogram.h
//
//  Created by Axis Sivitz on 4/22/14.
//  Copyright (c) 2014 Axis Sivitz. All rights reserved.
//

#ifndef __ENGINE_SHADER_PROGRAM_H_
#define __ENGINE_SHADER_PROGRAM_H_

#if TARGET_OS_IPHONE
#include <OpenGLES/ES2/gl.h>
#include <OpenGLES/ES2/glext.h>
#else
#include <OpenGL/OpenGL.h>
#include <OpenGL/gl3.h>
#endif

#include <stdbool.h>

#define NO_UNIFORM_SET -6

typedef enum
{
   SP_ATTR_POSITION,
   SP_ATTR_TEX_COORDS,
   SP_ATTR_COLOR,
   SP_ATTR_COLOR2,
   SP_ATTR_NORMALS,
   SP_ATTR_MAXNUM
} ShaderAttributeCode;

typedef enum
{
   SP_UNIFORM_TEXID,
   SP_UNIFORM_COLOR,
   SP_UNIFORM_COLOR2,
   SP_UNIFORM_MODEL_MAT,
   SP_UNIFORM_VIEW_MAT,
   SP_UNIFORM_SIZE,
   SP_UNIFORM_MAXNUM
} ShaderUniformCode;

typedef struct
{
   GLuint programId;
   GLuint vertShaderId;
   GLuint fragShaderId;
   GLint attributeLocations[SP_ATTR_MAXNUM];
   GLint uniformLocations[SP_UNIFORM_MAXNUM];
} ShaderProgram;

ShaderProgram * buildShaderProgram(const char * vertShaderPath, const char * fragShaderPath);
void setShaderProgramActive(ShaderProgram * program);
GLint shaderProgramAttrLoc(ShaderProgram * program, ShaderAttributeCode code);
GLint shaderProgramUniformLoc(ShaderProgram * program, ShaderUniformCode code);

#endif
