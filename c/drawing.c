#include "drawing.h"
#include "shaderprogram.h"
#include "platform.h"
#include <stdlib.h>

void checkGLError()
{
    for (int e = glGetError(); e != GL_NO_ERROR; e = glGetError())
    {
        dlog("Error: GL Error");
        switch (e)
        {
            case GL_INVALID_VALUE:  dlog("OpenGL: Invalid value"); break;
            case GL_INVALID_ENUM:  dlog("OpenGL: Invalid enum"); break;
            case GL_INVALID_OPERATION:  dlog("OpenGL: Invalid operation"); break;
            case GL_OUT_OF_MEMORY:  dlog("OpenGL: Out of memory"); break;
            default: dlog("OpenGL: Unknown error");
        }
    }
}

const GLfloat boxVertices[] = {
    -0.5, 0.5,
    -0.5, -0.5,
    0.5, -0.5,
    0.5, 0.5
};

const GLushort boxIndices[] = {
    0,1,2,3
};

const GLshort boxTextureCoords[] = {
    0, 0,
    0, 1,
    1, 1,
    1, 0,
};

GLuint    vertexBuffer;
GLuint    indexBuffer;
GLuint    texCoordBuffer;

void setupSquareDrawing()
{
   glGenBuffers(1, &vertexBuffer);
   glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
   glBufferData(GL_ARRAY_BUFFER, sizeof(boxVertices), boxVertices, GL_STATIC_DRAW);

   glGenBuffers(1, &texCoordBuffer);
   glBindBuffer(GL_ARRAY_BUFFER, texCoordBuffer);
   glBufferData(GL_ARRAY_BUFFER, sizeof(boxTextureCoords), boxTextureCoords, GL_STATIC_DRAW);

   glGenBuffers(1, &indexBuffer);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer);
   glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(boxIndices), boxIndices, GL_STATIC_DRAW);
}

void rebindSquareDrawing()
{
    glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer);
}

void rebindTC(ShaderProgram * program)
{
   glBindBuffer(GL_ARRAY_BUFFER, texCoordBuffer);
   glVertexAttribPointer(shaderProgramAttrLoc(program, SP_ATTR_TEX_COORDS), 2, GL_SHORT, 0, 0, 0);
}

void changeTexCoords(const GLfloat * coords, ShaderProgram * program)
{
    if (coords == NULL)
    {
        glBindBuffer(GL_ARRAY_BUFFER, texCoordBuffer);
        glVertexAttribPointer(shaderProgramAttrLoc(program, SP_ATTR_TEX_COORDS), 2, GL_SHORT, 0, 0, 0);
    }
    else
    {
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glVertexAttribPointer(shaderProgramAttrLoc(program, SP_ATTR_TEX_COORDS), 2, GL_FLOAT, 0, 0, coords);
    }

}

void setupSquareDrawingForProgram(ShaderProgram * program)
{
   int pidx = shaderProgramAttrLoc(program, SP_ATTR_POSITION);
   // Update attribute values.
   glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
   glVertexAttribPointer(pidx, 2, GL_FLOAT, 0, 0, 0);
   glEnableVertexAttribArray(pidx);
   
   int tcidx = shaderProgramAttrLoc(program, SP_ATTR_TEX_COORDS);
   
   if (tcidx >= 0)
   {
      // Update attribute values.
      glBindBuffer(GL_ARRAY_BUFFER, texCoordBuffer);
      glVertexAttribPointer(tcidx, 2, GL_SHORT, 0, 0, 0);
      glEnableVertexAttribArray(tcidx);      
   }
   
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer);
}

void drawLineRect(ShaderProgram * program, float width, float height, float thickness)
{
   glLineWidth(thickness);
   float offset = thickness / 2;
   
   float hw = width / 2;
   float hh = height / 2;
   
   float ohw = hw + offset;
   float ohh = hh + offset;
   
   GLfloat vertices[] =
   {
      -hw, ohh,
      -hw, -ohh,
      
      -ohw, -hh,
      ohw, -hh,
      
      hw, -ohh,
      hw, ohh,
      
      ohw, hh,
      -ohw, hh
   };
   
   glBindBuffer(GL_ARRAY_BUFFER, 0);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
   
   glEnableVertexAttribArray(shaderProgramAttrLoc(program, SP_ATTR_POSITION));
   glVertexAttribPointer(shaderProgramAttrLoc(program, SP_ATTR_POSITION), 2, GL_FLOAT, GL_FALSE, 0, vertices);
   
   glDrawArrays(GL_LINES, 0, 8);
   
   setupSquareDrawingForProgram(program);
}

void drawPoints(GLfloat * vertices, unsigned int num, ShaderProgram * program, int psize)
{
   glBindBuffer(GL_ARRAY_BUFFER, 0);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
   
   glEnableVertexAttribArray(shaderProgramAttrLoc(program, SP_ATTR_POSITION));
   glVertexAttribPointer(shaderProgramAttrLoc(program, SP_ATTR_POSITION), psize, GL_FLOAT, GL_FALSE, 0, vertices);
   
   glDrawArrays(GL_POINTS, 0, num);
}

