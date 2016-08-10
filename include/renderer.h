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

ShaderProgram * getShader(unsigned int num);

int load_shader(const char * inVertShaderPath, const char * inFragShaderPath);

GLuint make_vao();
GLuint attach_index_array();

void use_shader(unsigned int shader);

#endif
