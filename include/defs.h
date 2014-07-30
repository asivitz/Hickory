//
//  defs.h
//
//  Created by Axis Sivitz on 12/19/13.
//  Copyright (c) 2013 Axis Sivitz. All rights reserved.
//

#ifndef flip_defs_h
#define flip_defs_h

#if TARGET_OS_IPHONE

#define glMapBuffer glMapBufferOES
#define glUnmapBuffer glUnmapBufferOES
#define GL_WRITE_ONLY GL_WRITE_ONLY_OES
#define glBindVertexArray glBindVertexArrayOES
#define glGenVertexArrays glGenVertexArraysOES
#define glDeleteVertexArrays glDeleteVertexArraysOES

#else

//#define glBindVertexArray glBindVertexArrayAPPLE
//#define glGenVertexArrays glGenVertexArraysAPPLE

#endif

#endif
