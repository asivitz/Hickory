//
//  shaderprogram.c
//  flip
//
//  Created by Axis Sivitz on 4/22/14.
//  Copyright (c) 2014 Axis Sivitz. All rights reserved.
//

#include "shaderprogram.h"
#include "platform.h"
#include "Drawing.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>

GLint shaderProgramAttrLoc(ShaderProgram * program, ShaderAttributeCode code)
{
   return program->attributeLocations[code];
}

GLint shaderProgramUniformLoc(ShaderProgram * program, ShaderUniformCode code)
{
   return program->uniformLocations[code];
}

void setShaderProgramActive(ShaderProgram * program)
{
   glUseProgram(program->programId);
}

GLint compileShader(const char * sourceCode, GLenum type)
{

    GLboolean compileSupported;
    glGetBooleanv(GL_SHADER_COMPILER, &compileSupported);
    if (!compileSupported)
    {
        dlog("ERROR: Shader compilation not supported.");
        return -1;
    }

   GLint shaderId = glCreateShader(type);

   glShaderSource(shaderId, 1, &sourceCode, NULL);
   glCompileShader(shaderId);

   GLint compiled;
   glGetShaderiv(shaderId, GL_COMPILE_STATUS, &compiled);
   
   if(!compiled)
   {
      GLint infoLen = 0;
      glGetShaderiv(shaderId, GL_INFO_LOG_LENGTH, &infoLen);
      
      if(infoLen > 1)
      {
         char* infoLog = (char *)malloc(infoLen * sizeof(char));
         glGetShaderInfoLog(shaderId, infoLen, NULL, infoLog);
         dlog("*** ERROR: Can't compile shader\n\t%s", infoLog);
         free(infoLog);
      }
      glDeleteShader(shaderId);
      return -1;
   }
   
   return shaderId;
}

char * contentsOfFile(const char * inPath)
{
   int fd;
   
   if ((fd=open(inPath,O_RDONLY))==-1)
   {
      dlog("*** ERROR: Can't open file %s\n", inPath);
      return NULL;
   }
   
   struct stat buf;
   fstat(fd, &buf);
   off_t size = buf.st_size;
   
   char * source = (char *)malloc(size + 1);
   source[size] = '\0';
   
   read(fd, source, size);
   close(fd);
//   printf("Size: %lld Data from file:\n%s",size, source);

   return source;
}

char * sourceStringForFileName(const char * inFileName)
{
   char * source = contentsOfFile(inFileName);

   if (source == NULL)
      return source;
   
   #if TARGET_OS_IPHONE
      return source;
   #else
   char * final = NULL;
   asprintf(&final, "#version 150\n%s", source);
   free(source);
   return final;
   #endif
}

bool linkProgram(GLint programId, GLint vertShader, GLint fragShader)
{
   glAttachShader(programId, vertShader);
   glAttachShader(programId, fragShader);
   glLinkProgram(programId);
   
   GLint linked;
   glGetProgramiv(programId, GL_LINK_STATUS, &linked);
   if(!linked)
   {
      GLint infoLen = 0;
      glGetProgramiv(programId, GL_INFO_LOG_LENGTH, &infoLen);
      if(infoLen > 1)
      {
         char* infoLog = (char *)malloc(infoLen * sizeof(char));
         glGetProgramInfoLog(programId, infoLen, NULL, infoLog);
         printf("*** ERROR: Can't link shader program:\n\t%s", infoLog);
         free(infoLog);
         return false;
      }
   }
   
   return true;
}

ShaderProgram * buildShaderProgram(const char * vertShaderPath, const char * fragShaderPath)
{
   char * vertSource = sourceStringForFileName(vertShaderPath);
   char * fragSource = sourceStringForFileName(fragShaderPath);

   if (vertSource == NULL)
   {
      dlog("Can't find Vertex Shader: %s\n", vertShaderPath);
      return NULL;
   }

   if (fragSource == NULL)
   {
      dlog("Can't find Fragment Shader: %s\n", fragShaderPath);
      return NULL;
   }

   GLint vertShader = -1, fragShader = -1;
   
   vertShader = compileShader(vertSource, GL_VERTEX_SHADER);
   if (vertShader < 0)
   {
      printf("Error compiling Vertex Shader: %s\n", vertShaderPath);
      free(vertSource);
      free(fragSource);
      return NULL;
   }
   
   fragShader = compileShader(fragSource, GL_FRAGMENT_SHADER);
   if (fragShader < 0)
   {
      printf("Error compiling Fragment Shader: %s\n", fragShaderPath);
      free(vertSource);
      free(fragSource);
      glDeleteShader(vertShader);
      return NULL;
   }
   
   GLint programId = glCreateProgram();
   if (!linkProgram(programId, vertShader, fragShader))
   {
      printf("Error linking Shader Program: %s-%s\n", vertShaderPath, fragShaderPath);
      free(vertSource);
      free(fragSource);
      glDeleteShader(vertShader);
      glDeleteShader(fragShader);
      return NULL;
   }
   
   free(vertSource);
   free(fragSource);
   
   ShaderProgram * program = malloc(sizeof(ShaderProgram));
   program->programId = programId;
   program->vertShaderId = vertShader;
   program->fragShaderId = fragShader;
   
   program->attributeLocations[SP_ATTR_POSITION] = glGetAttribLocation(programId, "position");
   program->attributeLocations[SP_ATTR_TEX_COORDS] = glGetAttribLocation(programId, "texCoords");
   program->attributeLocations[SP_ATTR_COLOR] = glGetAttribLocation(programId, "color");
   program->attributeLocations[SP_ATTR_COLOR2] = glGetAttribLocation(programId, "color2");
   
   program->uniformLocations[SP_UNIFORM_TEXID] = glGetUniformLocation(programId, "tex");
   program->uniformLocations[SP_UNIFORM_COLOR] = glGetUniformLocation(programId, "color");
   program->uniformLocations[SP_UNIFORM_COLOR2] = glGetUniformLocation(programId, "color2");
   program->uniformLocations[SP_UNIFORM_MODEL_MAT] = glGetUniformLocation(programId, "modelMat");
   program->uniformLocations[SP_UNIFORM_VIEW_MAT] = glGetUniformLocation(programId, "viewMat");
   program->uniformLocations[SP_UNIFORM_SIZE] = glGetUniformLocation(programId, "size");

   return program;
}
