//
//  main.m
//  PlayBook
//
//  Created by Axis Sivitz on 10/10/14.
//  Copyright (c) 2014 Axis Sivitz. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "AppDelegate.h"
#import "HsFFI.h"

void getResourcePath(char * buf, int maxLength)
{
   NSString *filePath = [[NSBundle mainBundle] resourcePath];
   [filePath getCString:buf maxLength:maxLength encoding:NSASCIIStringEncoding];
}

void (*touchDown)(int, double, double);
void (*touchUp)(int, double, double);
void (*touchMoved)(int, double, double);
void (*initDraw)(int, int);
void (*drawFrame)(void);

void register_touch_began_callback(void (*_touchDown)(int, double, double))
{
   touchDown = _touchDown;
}
void register_touch_ended_callback(void (*_touchUp)(int, double, double))
{
   touchUp = _touchUp;
}
void register_touch_moved_callback(void (*_touchMoved)(int, double, double))
{
   touchMoved = _touchMoved;
}
void register_init_draw_callback(void (*_initDraw)(int, int))
{
   initDraw = _initDraw;
}
void register_draw_frame_callback(void (*_drawFrame)(void))
{
   drawFrame = _drawFrame;
}

void c_main() {
   int argc = 1;
   char* argv[2];

   argv[0] = "dummy";
   argv[1] = "";
   @autoreleasepool {
      UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
   }
}