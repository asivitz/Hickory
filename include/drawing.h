/*
 *  Drawing.h
 *
 *  Created by Axis Sivitz on 10/19/10.
 *  Copyright 2010 Axis Sivitz. All rights reserved.
 *
 */

#include "shaderprogram.h"

void checkGLError();
void setupSquareDrawing();
void rebindSquareDrawing();
void drawPoints(GLfloat * vertices, unsigned int num, ShaderProgram * program, int psize);
void changeTexCoords(const GLfloat * coords, ShaderProgram * program);
void setupSquareDrawingForProgram(ShaderProgram * program);
void drawLineRect(ShaderProgram * program, float width, float height, float thickness);
void rebindTC(ShaderProgram * program);
