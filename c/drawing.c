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
