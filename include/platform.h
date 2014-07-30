//
//  platform.h
//
//  Created by Axis Sivitz on 4/24/14.
//  Copyright (c) 2014 Axis Sivitz. All rights reserved.
//

#ifndef flip_platform_h
#define flip_platform_h

void getResourcePath(char * buf, int maxLength);
const char * documentsPath();

void dlog(const char *format , ... );
void logError(const char * errorMessage);

#endif
