//
//  GameViewController.m
//  PlayBook
//
//  Created by Axis Sivitz on 10/10/14.
//  Copyright (c) 2014 Axis Sivitz. All rights reserved.
//

#import "GameViewController.h"
#import <OpenGLES/ES2/glext.h>

@interface GameViewController () {
}
@property (strong, nonatomic) EAGLContext *context;

- (void)setupGL;
- (void)tearDownGL;

@end

@implementation GameViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];

    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }
    
    GLKView *view = (GLKView *)self.view;
    view.context = self.context;
    view.drawableDepthFormat = GLKViewDrawableDepthFormat24;
    
    [self setupGL];
   
   int w = ((GLKView *)self.view).frame.size.width;
   int h = ((GLKView *)self.view).frame.size.height;
   
   extern void (*initDraw)(int,int);
   initDraw(w, h);
}

- (void)dealloc
{    
    [self tearDownGL];
    
    if ([EAGLContext currentContext] == self.context) {
        [EAGLContext setCurrentContext:nil];
    }
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];

    if ([self isViewLoaded] && ([[self view] window] == nil)) {
        self.view = nil;
        
        [self tearDownGL];
        
        if ([EAGLContext currentContext] == self.context) {
            [EAGLContext setCurrentContext:nil];
        }
        self.context = nil;
    }

    // Dispose of any resources that can be recreated.
}

- (void)setupGL
{
    [EAGLContext setCurrentContext:self.context];
    
    glEnable(GL_DEPTH_TEST);
}

- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];
}

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
   extern void (*touchDown)(int, double, double);
   
   for (UITouch * touch in touches)
   {
      CGPoint loc = [touch locationInView:self.view];
      touchDown((int)touch, loc.x, (self.view.frame.size.height - loc.y));
   }
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
   extern void (*touchUp)(int, double, double);
   
   for (UITouch * touch in touches)
   {
      CGPoint loc = [touch locationInView:self.view];
      touchUp((int)touch, loc.x, (self.view.frame.size.height - loc.y));
   }
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
   extern void (*touchMoved)(int, double, double);
   
   for (UITouch * touch in touches)
   {
      CGPoint loc = [touch locationInView:self.view];
      touchMoved((int)touch, loc.x, (self.view.frame.size.height - loc.y));
   }
}

#pragma mark - GLKView and GLKViewController delegate methods

- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
   extern void (*drawFrame)(void);
   
   drawFrame();
}

@end
